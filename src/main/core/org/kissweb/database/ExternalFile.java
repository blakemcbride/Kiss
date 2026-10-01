/*
 *  Copyright (c) 2015 Blake McBride (blake@mcbridemail.com)
 *  All rights reserved.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright
 *  notice, this list of conditions, and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.kissweb.database;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.FileUtils;
import org.kissweb.Image;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.sql.SQLException;
import java.sql.Types;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * Supports storing files external to the SQL database, associating each file with a row of any
 * SQL table.
 * <br><br>
 * Any row of any table may have one or more external files associated with it.  This class keeps
 * that association intact, computing the location of a row's file(s) on demand from the table
 * name, the row's primary key, and (when a row has more than one associated file) a "fictitious"
 * field name distinguishing them &mdash; nothing about the file's location is ever stored in the
 * database.  Because the location is always recomputed rather than persisted, moving the
 * underlying storage to another machine or path works without touching any database row.
 * <br><br>
 * This class knows nothing about any particular application's primary-key format, table layout,
 * or configuration mechanism.  Two things must be supplied by the application, once, before this
 * class is used:
 * <ol>
 *     <li>{@link #setRootSupplier(Supplier)} &mdash; a lambda returning the current root
 *     directory under which all external files are stored.</li>
 *     <li>{@link #setDirectoryMapper(BiFunction)} &mdash; a lambda that, given a table name and a
 *     primary key, returns the path (relative to the root) of the directory in which that row's
 *     file(s) live.  This is what frees this class from any assumption about primary-key shape or
 *     sharding scheme; the application decides how primary keys map to directories.</li>
 * </ol>
 * Every other operation &mdash; save, read, delete, existence checks, multiple files per row, and
 * participation in {@link Connection#setDeleteCallback} cascade-delete &mdash; is generic and
 * needs no further configuration.
 * <br><br>
 * <b>This class does not participate in SQL transactions.</b> A file write or delete happens
 * immediately and independently of any surrounding database transaction; if that transaction is
 * later rolled back, a newly written external file is not automatically removed. An application
 * that needs correctness under rollback must handle that itself (e.g. by deferring the file write
 * until after commit, or by tolerating an orphaned file).
 * <br><br>
 * <b>Hybrid inline/external column storage.</b> Everything above is the "virtual column" pattern:
 * the file's association with a row is entirely computed (table + primary key + fictitious field
 * name) and no SQL column is involved at all. {@link #saveHybridColumn} / {@link #getHybridColumn}
 * instead back a real, existing SQL {@code varchar} column: data that fits in the column is stored
 * in it directly, and data too large for the column is written to an external file (using the same
 * root/directory-mapper configuration as above) with a sentinel value left in the column so a
 * single {@link #getHybridColumn} call transparently returns the real data regardless of where it
 * actually lives. This is the right tool when a column's content is normally small but occasionally
 * large, and the schema already has (or is willing to add) a real column for it; use the virtual
 * column methods above instead when there is no real column at all.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 9/27/26
 */
public class ExternalFile {

    private static final Logger logger = LogManager.getLogger(ExternalFile.class);

    /** Application-supplied lambda returning the current external-file root directory. */
    private static volatile Supplier<String> rootSupplier;

    /**
     * Application-supplied lambda mapping (tableName, primaryKey) to the directory &mdash;
     * relative to the root returned by {@link #rootSupplier} &mdash; in which that row's file(s)
     * are stored.
     */
    private static volatile BiFunction<String, String, String> directoryMapper;

    private static final Map<String, ExternalField> fields = new ConcurrentHashMap<>();

    /**
     * Tables (lower-cased) whose {@link #deleteCallback} is allowed to remove the row's external
     * file(s) automatically &mdash; see {@link #cascadeDeleteFor(String)}'s javadoc for why this
     * is opt-in, and empty by default.
     */
    private static final Set<String> cascadeDeleteTables = ConcurrentHashMap.newKeySet();

    /** Utility class &mdash; not intended for instantiation. */
    private ExternalFile() {
    }

    // ------------------------------------------------------------------------------------------
    // Application configuration -- must be called once at application startup, before any other
    // method on this class is used.
    // ------------------------------------------------------------------------------------------

    /**
     * Registers the application-supplied lambda that returns the current external-file root
     * directory (for example, a value read from a configuration file, a system property, or a
     * per-tenant setting).
     * <br><br>
     * The lambda is invoked fresh every time a file path is computed &mdash; its result is never
     * cached by this class &mdash; so an application whose root can legitimately change at
     * runtime (e.g. a different value per tenant, resolved from whatever the application
     * considers "current" at the moment of the call) is reflected immediately, with no need to
     * notify this class of the change.
     *
     * @param supplier lambda taking no arguments and returning the root directory path, or
     *                  null/empty if not currently configured (in which case any operation that
     *                  needs a path throws {@link IllegalStateException})
     */
    public static void setRootSupplier(Supplier<String> supplier) {
        rootSupplier = supplier;
    }

    /**
     * Registers the application-supplied lambda that maps a table name and a row's primary key
     * to the directory &mdash; relative to the root returned by the lambda passed to
     * {@link #setRootSupplier(Supplier)} &mdash; in which that row's external file(s) are stored.
     * <br><br>
     * This is what allows this class to remain free of any assumption about primary-key format or
     * sharding scheme: the application alone decides how a (tableName, primaryKey) pair turns
     * into a directory path. A typical implementation shards by some portion of the primary key
     * (to keep any single directory from growing unbounded as rows accumulate) and includes the
     * table name so different tables never collide; a small application might simply return
     * {@code tableName + "/" + primaryKey} with no sharding at all.
     * <br><br>
     * The returned path is interpreted relative to the current root (see
     * {@link #setRootSupplier(Supplier)}); a leading or trailing slash is tolerated but not
     * required.  The lambda is invoked fresh on every call needing a path &mdash; its result is
     * never cached by this class.
     *
     * @param mapper lambda taking the table name and the primary key value and returning the
     *               directory path (relative to the root) in which that row's file(s) are stored;
     *               must never return null/empty for a table/primary-key pair this class is asked
     *               to compute a path for
     */
    public static void setDirectoryMapper(BiFunction<String, String, String> mapper) {
        directoryMapper = mapper;
    }

    private static void requireConfigured() {
        if (rootSupplier == null)
            throw new IllegalStateException("ExternalFile.setRootSupplier(...) has not been called; the application must configure the external-file root before this class can be used.");
        if (directoryMapper == null)
            throw new IllegalStateException("ExternalFile.setDirectoryMapper(...) has not been called; the application must configure how a table name and primary key map to a storage directory before this class can be used.");
    }

    // ------------------------------------------------------------------------------------------
    // Registry of logical file types -- optional convenience so callers can refer to a
    // (tableName, fieldName) pair by a single logical name instead of repeating both every time.
    // ------------------------------------------------------------------------------------------

    /**
     * Registers a logical file type under {@code name}, associating it with a SQL table and a
     * (possibly fictitious) field name.  Typically called once per logical file type at
     * application startup.
     *
     * @param name      the logical name under which this file type is looked up by the
     *                  {@code String name}-based overloads in this class
     * @param tableName the SQL table name this file type belongs to
     * @param fieldName the fictitious field name distinguishing this file type from any other
     *                  file(s) associated with the same table, or null if the table only ever has
     *                  one associated file
     */
    public static void addField(String name, String tableName, String fieldName) {
        fields.put(name, new ExternalField(tableName, fieldName));
    }

    /**
     * Looks up a previously registered file type.
     *
     * @param name the logical name passed to {@link #addField(String, String, String)}
     * @return the registered {@link ExternalField}, or null if {@code name} was never registered
     */
    public static ExternalField get(String name) {
        return fields.get(name);
    }

    // ------------------------------------------------------------------------------------------
    // Path computation
    // ------------------------------------------------------------------------------------------

    /**
     * Computes the absolute path to be used when storing/reading the file for this record,
     * creating the containing directory if it doesn't already exist.  Requires an exact, known
     * extension.  This does not check whether the file itself already exists; it is the method
     * used before writing a file.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension (with or without leading period)
     * @return the absolute path of the file on disk
     * @see #makeExternalFilePath(ExternalField, String, String)
     */
    public static String makeExternalFilePath(String name, String primaryKey, String extension) {
        return makeExternalFilePath(get(name), primaryKey, extension);
    }

    /**
     * Computes the absolute path to be used when storing/reading the file for this record,
     * creating the containing directory if it doesn't already exist.  Requires an exact, known
     * extension.  This does not check whether the file itself already exists; it is the method
     * used before writing a file.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension (with or without leading period)
     * @return the absolute path of the file on disk
     */
    public static String makeExternalFilePath(ExternalField field, String primaryKey, String extension) {
        return makeExternalFilePath(field.tableName, primaryKey, field.fieldName, extension, true, false);
    }

    /**
     * Returns the full path to a file that was already saved for this record, without creating
     * any directory and without requiring the caller to know the extension: it looks for any file
     * whose name starts with {@code primaryKey + fieldName + "."} in the row's directory and
     * returns the first match's absolute path, or null if none exists or the directory doesn't
     * exist.  Match order among multiple files sharing that prefix but differing only in
     * extension is filesystem-dependent.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @return the path to the existing file, or null if not found
     * @see #getExternalFilePath(ExternalField, String)
     */
    public static String getExternalFilePath(String name, String primaryKey) {
        return getExternalFilePath(get(name), primaryKey);
    }

    /**
     * Returns the full path to a file that was already saved for this record, without creating
     * any directory and without requiring the caller to know the extension: it looks for any file
     * whose name starts with {@code primaryKey + fieldName + "."} in the row's directory and
     * returns the first match's absolute path, or null if none exists or the directory doesn't
     * exist.  Match order among multiple files sharing that prefix but differing only in
     * extension is filesystem-dependent.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @return the path to the existing file, or null if not found
     */
    public static String getExternalFilePath(ExternalField field, String primaryKey) {
        return makeExternalFilePath(field.tableName, primaryKey, field.fieldName, null, false, true);
    }

    private static String makeExternalFilePath(String tableName, final String primaryKey, String fieldName, String extension, boolean create, boolean anyExtension) {
        tableName = tableName.toLowerCase();
        if (fieldName == null)
            fieldName = "";
        if (!fieldName.isEmpty() && fieldName.charAt(0) != '-')
            fieldName = "-" + fieldName;
        extension = fileExtension(extension);
        final String dir = getExternalFileDir(tableName, primaryKey, create);
        if (anyExtension) {
            final File parentDir = new File(dir);
            final String fnamePrefix = primaryKey + fieldName + ".";
            final File[] matches = parentDir.listFiles((FilenameFilter) (d, nm) -> nm.startsWith(fnamePrefix));
            if (matches != null && matches.length > 0)
                return matches[0].getAbsolutePath();
            return null;
        } else
            return dir + "/" + primaryKey + fieldName + extension;
    }

    /**
     * Calculates the directory in which an external file for the given table/primary key will be
     * (or already is) stored, by combining the current root ({@link #setRootSupplier(Supplier)})
     * with the application's directory mapper ({@link #setDirectoryMapper(BiFunction)}).
     *
     * @param tableName  the SQL table name (already lower-cased by the caller)
     * @param primaryKey primary key value for the given table
     * @param create     if true, create the directory if it doesn't exist
     * @return the absolute directory path
     */
    private static String getExternalFileDir(final String tableName, final String primaryKey, boolean create) {
        requireConfigured();
        String rootPath = rootSupplier.get();
        if (rootPath == null || rootPath.isEmpty())
            throw new IllegalStateException("External file root is not configured (the application-supplied root supplier returned null/empty).");
        if (rootPath.charAt(rootPath.length() - 1) != '/')
            rootPath += '/';
        String rel = directoryMapper.apply(tableName, primaryKey);
        if (rel == null || rel.isEmpty())
            throw new IllegalStateException("ExternalFile directory mapper returned a null/empty path for table \"" + tableName + "\", primary key \"" + primaryKey + "\".");
        while (rel.charAt(0) == '/')
            rel = rel.substring(1);
        while (!rel.isEmpty() && rel.charAt(rel.length() - 1) == '/')
            rel = rel.substring(0, rel.length() - 1);
        final String spath = rootPath + rel;
        final File dirfp = new File(spath);
        if (create && !dirfp.exists())
            dirfp.mkdirs();
        return spath;
    }

    // ------------------------------------------------------------------------------------------
    // Save / read / delete
    // ------------------------------------------------------------------------------------------

    /**
     * Saves string data to an external file.  Overwrites any existing file at that path.
     * <br><br>
     * This method has been superseded by {@link #saveInputStream} for uploaded content, which
     * additionally normalizes image orientation; use it instead when possible.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(String name, String primaryKey, String extension, String data) throws IOException {
        saveData(get(name), primaryKey, extension, data);
    }

    /**
     * Saves binary data to an external file.  Overwrites any existing file at that path.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(String name, String primaryKey, String extension, byte[] data) throws IOException {
        saveData(get(name), primaryKey, extension, data);
    }

    /**
     * Saves boxed binary data to an external file.  Overwrites any existing file at that path.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(String name, String primaryKey, String extension, Byte[] data) throws IOException {
        saveData(get(name), primaryKey, extension, data);
    }

    /**
     * Saves string data to an external file.  Overwrites any existing file at that path.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(ExternalField field, String primaryKey, String extension, String data) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        FileUtils.write(fname, data);
    }

    /**
     * Saves binary data to an external file.  Overwrites any existing file at that path.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(ExternalField field, String primaryKey, String extension, byte[] data) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        FileUtils.write(fname, data);
    }

    /**
     * Saves boxed binary data to an external file.  Overwrites any existing file at that path.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @param data       the data to be saved
     * @throws IOException if there is an error writing the data to the file
     */
    public static void saveData(ExternalField field, String primaryKey, String extension, Byte[] data) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        FileUtils.write(fname, data);
    }

    /**
     * Deletes an external file.  Silently does nothing (no exception) if the file does not exist
     * or could not be deleted (e.g. due to permissions); the underlying {@code File.delete()}
     * result is not checked.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @throws IOException if the containing directory could not be prepared
     */
    public static void deleteExternalFile(String name, String primaryKey, String extension) throws IOException {
        deleteExternalFile(get(name), primaryKey, extension);
    }

    /**
     * Deletes an external file.  Silently does nothing (no exception) if the file does not exist
     * or could not be deleted (e.g. due to permissions); the underlying {@code File.delete()}
     * result is not checked.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @throws IOException if the containing directory could not be prepared
     */
    public static void deleteExternalFile(ExternalField field, String primaryKey, String extension) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        (new File(fname)).delete();
    }

    /**
     * Retrieves the contents of an external file as text.  Returns a zero-length string (not an
     * exception) if the file does not exist.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @return the data from the external file, or "" if it does not exist
     * @throws IOException if there is an error reading the file
     */
    public static String getString(String name, String primaryKey, String extension) throws IOException {
        return getString(get(name), primaryKey, extension);
    }

    /**
     * Retrieves the contents of an external file as text.  Returns a zero-length string (not an
     * exception) if the file does not exist.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @return the data from the external file, or "" if it does not exist
     * @throws IOException if there is an error reading the file
     */
    public static String getString(ExternalField field, String primaryKey, String extension) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        if (!new File(fname).exists())
            return "";
        return FileUtils.readFile(fname);
    }

    /**
     * Retrieves the contents of an external file as bytes.  Returns a zero-length byte array (not
     * an exception) if the file does not exist.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @return the data from the external file, or a zero-length array if it does not exist
     * @throws IOException if there is an error reading the file
     */
    public static byte[] getBinary(String name, String primaryKey, String extension) throws IOException {
        return getBinary(get(name), primaryKey, extension);
    }

    /**
     * Retrieves the contents of an external file as bytes.  Returns a zero-length byte array (not
     * an exception) if the file does not exist.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param extension  the file extension, or the file name (from which the extension is
     *                   calculated)
     * @return the data from the external file, or a zero-length array if it does not exist
     * @throws IOException if there is an error reading the file
     */
    public static byte[] getBinary(ExternalField field, String primaryKey, String extension) throws IOException {
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        if (!new File(fname).exists())
            return new byte[0];
        return FileUtils.readFileBytes(fname);
    }

    /**
     * Checks whether an external file exists for this row, matching on the primary key prefix
     * alone (independent of field name).
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @return true if at least one external file exists for this row
     */
    public static boolean externalFileExists(String name, String primaryKey) {
        return externalFileExists(get(name), primaryKey);
    }

    /**
     * Checks whether an external file exists for this row, matching on the primary key prefix
     * alone (independent of field name).
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @return true if at least one external file exists for this row
     */
    public static boolean externalFileExists(final ExternalField field, String primaryKey) {
        final String dirf = getExternalFileDir(field.tableName.toLowerCase(), primaryKey, false);
        final File directory = new File(dirf);
        if (!directory.exists() || !directory.isDirectory())
            return false;
        final String[] matchingFiles = directory.list((dir, nm) -> nm.startsWith(primaryKey));
        return matchingFiles != null && matchingFiles.length > 0;
    }

    /**
     * Checks whether any file exists in the record's directory whose name starts with the
     * concatenation of the primary key and (normalized) field name.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param fieldName  the fictitious field name, or null to match on the primary key alone
     * @return true if a matching file exists, false otherwise
     */
    public static boolean anyStartsWith(String name, String primaryKey, String fieldName) {
        return anyStartsWith(get(name), primaryKey, fieldName);
    }

    /**
     * Checks whether any file exists in the record's directory whose name starts with the
     * concatenation of the primary key and (normalized) field name.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param fieldName  the fictitious field name, or null to match on the primary key alone
     * @return true if a matching file exists, false otherwise
     */
    public static boolean anyStartsWith(final ExternalField field, final String primaryKey, String fieldName) {
        final String dirName = getExternalFileDir(field.tableName.toLowerCase(), primaryKey, false);
        final File[] files = new File(dirName).listFiles();
        if (files != null) {
            if (fieldName == null)
                fieldName = "";
            if (!fieldName.isEmpty() && fieldName.charAt(0) != '-')
                fieldName = "-" + fieldName;
            final String str = primaryKey + fieldName;
            for (File file : files)
                if (file.getName().startsWith(str))
                    return true;
        }
        return false;
    }

    /**
     * Deletes every file in the record's directory whose name starts with {@code primaryKey}
     * &mdash; i.e. all fields/variants for that record, regardless of fictitious field name or
     * extension.  Does not throw; individual deletion failures are silently ignored.
     *
     * @param tableName  the SQL table name
     * @param primaryKey primary key value for the given table
     */
    public static void deleteAllExternalFiles(final String tableName, final String primaryKey) {
        final String dir = getExternalFileDir(tableName.toLowerCase(), primaryKey, false);
        (new File(dir)).listFiles((dir1, nm) -> {
            if (nm.startsWith(primaryKey))
                (new File(dir1, nm)).delete();
            return false;
        });
    }

    // ------------------------------------------------------------------------------------------
    // Hybrid inline/external column storage -- a real SQL varchar column that transparently
    // overflows to an external file (using the same root/directoryMapper configured above) when
    // the data is too large to fit.  See the class-level javadoc for how this differs from the
    // virtual-column methods above.
    // ------------------------------------------------------------------------------------------

    /**
     * Sentinel value stored verbatim in the column to mean "the real data lives in an external
     * file, not in this column."  Begins and ends with ASCII SOH (0x01), a control character that
     * essentially never occurs in ordinary text data, so real data colliding with this sequence is
     * rare &mdash; but see {@link #INLINE_ESCAPE} for how that rare case is still handled
     * correctly rather than merely assumed away.
     */
    private static final String EXTERNAL_MARKER = "\u0001KFX\u0001";

    /**
     * Escape prefix. If a caller's real inline data happens to already start with
     * {@link #EXTERNAL_MARKER} or with this prefix itself, {@link #saveHybridColumn} prepends this
     * marker before storing it, and {@link #getHybridColumn} strips exactly one leading occurrence
     * of it before returning the value. Data that does not collide with either sentinel is stored
     * completely unescaped, with no overhead.
     */
    private static final String INLINE_ESCAPE = "\u0001KFI\u0001";

    /** Fixed extension used for hybrid-column overflow files; the content is opaque text, so the
     *  extension carries no meaning beyond keeping these files visually distinct on disk. */
    private static final String HYBRID_EXTENSION = ".col";

    /**
     * Per (schema, table, column) cache of a varchar column's declared size (in characters),
     * populated by {@link #getColumnSize}.  Keyed by the connection's current schema/catalog
     * (see {@link Connection#metadataScopeKey}) because the same table name can have different
     * column sizes in different schemas.  A table's definition does not change while the
     * application is running, so entries are never invalidated.
     */
    private static final Map<String, Integer> hybridColumnSizeCache = new ConcurrentHashMap<>();

    /**
     * Looks up {@code tableName.columnName}'s declared size (in characters &mdash; the unit JDBC
     * column metadata already reports character types in, e.g. PostgreSQL {@code varchar(n)} is
     * measured in characters, not bytes) via {@link Connection#getColumnInfo}, the same metadata
     * facility {@link Connection#getPrimaryColumns} and friends are built on. The result is cached
     * per schema+table+column for the life of the JVM (see {@link #hybridColumnSizeCache}), so repeated
     * saves/gets against the same column cost one metadata lookup, not one per call.
     */
    private static int getColumnSize(Connection db, String tableName, String columnName) throws SQLException {
        final String key = db.metadataScopeKey() + "|" + tableName.toLowerCase() + "." + columnName.toLowerCase();
        final Integer cached = hybridColumnSizeCache.get(key);
        if (cached != null)
            return cached;
        final HashMap<String, ColumnInfo> cols = db.getColumnInfo(tableName);
        ColumnInfo ci = cols == null ? null : cols.get(columnName);
        if (ci == null && cols != null)
            for (final Map.Entry<String, ColumnInfo> e : cols.entrySet())
                if (e.getKey().equalsIgnoreCase(columnName)) {
                    ci = e.getValue();
                    break;
                }
        if (ci == null)
            throw new SQLException("Column \"" + columnName + "\" not found in table \"" + tableName + "\".");
        final int size = ci.getColumnSize();
        hybridColumnSizeCache.put(key, size);
        return size;
    }

    /**
     * Converts a primary key supplied as a {@code String} into the Java type its column's declared
     * JDBC type expects, so it binds correctly regardless of database vendor. Integer/bigint
     * primary keys are converted to {@code Integer}/{@code Long}/{@code BigDecimal}; every other
     * type (including character and UUID primary keys) is bound as the {@code String} unchanged,
     * which is already correct for those.  This uses the same {@link Connection#getColumnInfo}
     * metadata as {@link #getColumnSize}, just against the primary key column instead of the data
     * column.
     */
    private static Object convertPrimaryKeyForBind(Connection db, String tableName, String primaryKey, String pkColumn) throws SQLException {
        if (primaryKey == null)
            return null;
        final HashMap<String, ColumnInfo> cols = db.getColumnInfo(tableName);
        ColumnInfo pkInfo = cols == null ? null : cols.get(pkColumn);
        if (pkInfo == null && cols != null)
            for (final Map.Entry<String, ColumnInfo> e : cols.entrySet())
                if (e.getKey().equalsIgnoreCase(pkColumn)) {
                    pkInfo = e.getValue();
                    break;
                }
        if (pkInfo == null)
            return primaryKey;
        switch (pkInfo.getDataType()) {
            case Types.INTEGER:
            case Types.SMALLINT:
            case Types.TINYINT:
                return Integer.valueOf(primaryKey);
            case Types.BIGINT:
                return Long.valueOf(primaryKey);
            case Types.NUMERIC:
            case Types.DECIMAL:
                return new BigDecimal(primaryKey);
            default:
                return primaryKey;
        }
    }

    /**
     * Computes the deterministic path of the external overflow file for {@code (tableName,
     * primaryKey, columnName)}, reusing the same private path machinery (and therefore the same
     * root/directoryMapper configuration) as the virtual-column methods, with {@code columnName}
     * used as the fictitious field name and {@link #HYBRID_EXTENSION} as the fixed extension.
     */
    private static String hybridExternalPath(String tableName, String primaryKey, String columnName, boolean create) {
        return makeExternalFilePath(tableName, primaryKey, columnName, HYBRID_EXTENSION, create, false);
    }

    /** Deletes the external overflow file for this row/column, if any. Safe to call unconditionally
     *  &mdash; a no-op when no such file exists. */
    private static void deleteHybridExternalFile(String tableName, String primaryKey, String columnName) {
        final String path = hybridExternalPath(tableName, primaryKey, columnName, false);
        new File(path).delete();
    }

    /**
     * Saves {@code data} into a real SQL {@code varchar} column, transparently storing it inline in
     * the column when it fits and in an external file (falling back to the same
     * root/{@link #setDirectoryMapper} configuration used by the rest of this class) when it does
     * not &mdash; in a single call.
     * <br><br>
     * <b>Inline-vs-external decision.</b> {@code columnName}'s declared size is read from database
     * metadata (see {@link #getColumnSize}) &mdash; never hard-coded and never supplied by the
     * caller. If {@code data} (plus one {@link #INLINE_ESCAPE} prefix, only when needed &mdash; see
     * below) fits within that size, it is written straight into the column; otherwise it is written
     * to an external file and {@link #EXTERNAL_MARKER} is written into the column in its place.
     * <br><br>
     * <b>Sentinel and escaping.</b> {@link #EXTERNAL_MARKER} in the column means "the data is
     * external." Because real inline data could coincidentally begin with that exact sequence (or
     * with the escape prefix itself), any data starting with either one is stored with
     * {@link #INLINE_ESCAPE} prepended, so it never gets misread as the external marker on a later
     * {@link #getHybridColumn} call; ordinary data that does not collide with either sentinel is
     * stored verbatim, with no overhead. This makes the round trip exact for every possible input,
     * including a value that merely looks like the sentinel.
     * <br><br>
     * <b>Transitions.</b> An update may flip storage direction either way. Going external&rarr;inline
     * or storing null/empty deletes the row's now-stale external file (harmless no-op if none
     * exists); going external&rarr;external simply overwrites the same deterministic file path.
     * Because the external file's path is entirely determined by table name + primary key +
     * {@code columnName} (the same scheme the rest of this class uses), a table already opted into
     * {@link #cascadeDeleteFor} continues to have these files removed automatically on row delete
     * &mdash; no separate cascade wiring is needed for this mechanism.
     * <br><br>
     * <b>Requires a single-column primary key</b> (see {@link Connection#getPrimaryColumnName}) and,
     * like every other method on this class, requires {@link #setRootSupplier} and
     * {@link #setDirectoryMapper} to already be configured &mdash; but only actually needs them when
     * an external file must be written or cleaned up; a save that stays inline never touches them.
     * <br><br>
     * <b>Not transactional with the SQL update.</b> Just as with the rest of this class, a file
     * write/delete happens immediately and independently of any surrounding database transaction. To
     * minimize the chance of an inconsistent result, the file operation (if any) is performed
     * <i>before</i> the column is updated, so a failure in the database step leaves at worst an
     * orphaned/stale external file &mdash; never a column pointing at a file that was never written.
     *
     * @param db         the database connection
     * @param tableName  the SQL table name
     * @param columnName the {@code varchar} column name backing this data
     * @param primaryKey the primary key value for the row, as a string
     * @param data       the data to save, or null/empty to clear the column and remove any external
     *                   file for this row/column. The value written to the column in that case is
     *                   exactly {@code data} as passed: null stores SQL NULL (which fails on a NOT NULL
     *                   column) and {@code ""} stores an empty string. To clear a column without having
     *                   to know which is appropriate, use {@link #releaseHybridColumn}.
     * @throws SQLException if a database access error occurs, the table's primary key is
     *                       composite, or the column does not exist
     * @throws IOException  if there is an error writing or deleting the external file
     * @see #getHybridColumn(Connection, String, String, String)
     */
    public static void saveHybridColumn(Connection db, String tableName, String columnName, String primaryKey, String data) throws SQLException, IOException {
        final String pkColumn = db.getPrimaryColumnName(tableName);
        final Object pkBind = convertPrimaryKeyForBind(db, tableName, primaryKey, pkColumn);
        final int maxLen = getColumnSize(db, tableName, columnName);
        if (maxLen < EXTERNAL_MARKER.length())
            throw new SQLException("Column \"" + tableName + "." + columnName + "\" (size " + maxLen + ") is too narrow to support ExternalFile hybrid storage; it must be at least " + EXTERNAL_MARKER.length() + " characters.");

        final String updateSql = "UPDATE " + tableName + " SET " + columnName + " = ? WHERE " + pkColumn + " = ?";

        if (data == null || data.isEmpty()) {
            deleteHybridExternalFile(tableName, primaryKey, columnName);
            db.execute(updateSql, data, pkBind);
            return;
        }

        final boolean needsEscape = data.startsWith(EXTERNAL_MARKER) || data.startsWith(INLINE_ESCAPE);
        final String encoded = needsEscape ? INLINE_ESCAPE + data : data;

        if (encoded.length() <= maxLen) {
            deleteHybridExternalFile(tableName, primaryKey, columnName);
            db.execute(updateSql, encoded, pkBind);
        } else {
            final String path = hybridExternalPath(tableName, primaryKey, columnName, true);
            FileUtils.write(path, data);
            db.execute(updateSql, EXTERNAL_MARKER, pkBind);
        }
    }

    /**
     * Retrieves the data previously saved by {@link #saveHybridColumn}, transparently returning the
     * real value regardless of whether it is stored inline in the column or in an external file
     * &mdash; in a single call.
     * <br><br>
     * The column is read, and: a SQL NULL returns null; the exact {@link #EXTERNAL_MARKER} sentinel
     * causes the associated external file to be read and returned instead (or {@code ""} if that
     * file is unexpectedly missing, matching this class's other read methods); a value starting
     * with {@link #INLINE_ESCAPE} has that one prefix stripped before being returned; any other
     * value is returned exactly as stored.
     *
     * @param db         the database connection
     * @param tableName  the SQL table name
     * @param columnName the {@code varchar} column name backing this data
     * @param primaryKey the primary key value for the row, as a string
     * @return the real data, or null if the row does not exist or the column is SQL NULL
     * @throws SQLException if a database access error occurs or the table's primary key is
     *                       composite
     * @throws IOException  if there is an error reading the external file
     * @see #saveHybridColumn(Connection, String, String, String, String)
     */
    public static String getHybridColumn(Connection db, String tableName, String columnName, String primaryKey) throws SQLException, IOException {
        final String pkColumn = db.getPrimaryColumnName(tableName);
        final Object pkBind = convertPrimaryKeyForBind(db, tableName, primaryKey, pkColumn);

        final Record rec;
        try {
            rec = db.fetchOne("SELECT " + columnName + " FROM " + tableName + " WHERE " + pkColumn + " = ?", pkBind);
        } catch (Exception e) {
            if (e instanceof SQLException se)
                throw se;
            throw new SQLException(e);
        }
        if (rec == null)
            return null;

        return resolveHybridColumn(db, tableName, columnName, primaryKey, rec.getString(columnName));
    }

    /**
     * Tells whether a raw column value, exactly as fetched from the database, is the marker meaning
     * "the real data lives in an external file" rather than being the data itself.
     * <br><br>
     * Only the exact marker qualifies; a value carrying the inline escape prefix is real (inline)
     * data and returns false. No database or file access is performed.
     *
     * @param raw the raw value of a hybrid column as stored (may be null)
     * @return true exactly when {@code raw} is the external-storage marker
     * @see #resolveHybridColumn(Connection, String, String, String, String)
     */
    public static boolean isExternal(String raw) {
        return EXTERNAL_MARKER.equals(raw);
    }

    /**
     * Resolves an already-fetched raw hybrid column value to the real data, without issuing any
     * SELECT. This is what {@link #getHybridColumn} does after reading the column, and
     * {@link #getHybridColumn} delegates to it so the two can never drift.
     * <br><br>
     * Use it when the raw value arrives as part of a larger query (a list, a join) and a per-row
     * {@code getHybridColumn} round trip would be wasteful. Behavior: null returns null; a value
     * that does not begin with the internal sentinel character is returned as-is; a value with the
     * {@link #INLINE_ESCAPE} prefix has that one prefix stripped (no file is read); the exact
     * {@link #EXTERNAL_MARKER} causes the external file to be read and returned ({@code ""} if the
     * file is unexpectedly missing, matching {@link #getHybridColumn}).
     * <br><br>
     * {@code db} is not used to query anything; {@code primaryKey} (together with {@code tableName}
     * and {@code columnName}) is needed only to compute the external file's path, and is ignored
     * unless {@code raw} is the external marker.
     *
     * @param db         the database connection (not queried; retained for signature symmetry)
     * @param tableName  the SQL table name
     * @param columnName the {@code varchar} column name backing this data
     * @param primaryKey the primary key value for the row, as a string (used only for the file path)
     * @param raw        the raw column value as fetched, possibly null
     * @return the real data, or null if {@code raw} is null
     * @throws IOException if there is an error reading the external file
     * @see #isExternal(String)
     */
    public static String resolveHybridColumn(Connection db, String tableName, String columnName, String primaryKey, String raw) throws IOException {
        if (raw == null)
            return null;
        if (raw.isEmpty() || raw.charAt(0) != '\u0001')
            return raw;
        if (raw.equals(EXTERNAL_MARKER)) {
            final String path = hybridExternalPath(tableName, primaryKey, columnName, false);
            if (!new File(path).exists())
                return "";
            return FileUtils.readFile(path);
        }
        if (raw.startsWith(INLINE_ESCAPE))
            return raw.substring(INLINE_ESCAPE.length());
        return raw;
    }

    /**
     * Releases a hybrid column's data: removes any external file for this row/column and clears the
     * column &mdash; to SQL NULL when {@code nullable} is true, to an empty string when false (for a
     * NOT NULL column). This replaces guessing between {@code saveHybridColumn(..., null)} and
     * {@code saveHybridColumn(..., "")}. Safe to call when no external file exists.
     *
     * @param db         the database connection
     * @param tableName  the SQL table name
     * @param columnName the {@code varchar} column name backing this data
     * @param primaryKey the primary key value for the row, as a string
     * @param nullable   true if the column permits NULL (clear to NULL), false if NOT NULL (clear to "")
     * @throws SQLException if a database access error occurs, the table's primary key is composite,
     *                       or the column does not exist
     * @throws IOException  if there is an error deleting the external file
     * @see #saveHybridColumn(Connection, String, String, String, String)
     */
    public static void releaseHybridColumn(Connection db, String tableName, String columnName, String primaryKey, boolean nullable) throws SQLException, IOException {
        saveHybridColumn(db, tableName, columnName, primaryKey, nullable ? null : "");
    }

    // ------------------------------------------------------------------------------------------
    // Cascade-delete opt-in -- wire ExternalFile::deleteCallback into Connection.setDeleteCallback
    // to have this class participate in Kiss's row-delete callback.
    // ------------------------------------------------------------------------------------------

    /**
     * Opts {@code tableName} into {@link #deleteCallback} actually removing its files, one table
     * at a time (case-insensitive).
     * <br><br>
     * Cascade delete is opt-in, not automatic, and defaults to off for every table.  An
     * application that has been deleting rows for a table without ever removing that row's
     * external files must not have that behavior silently change the moment this class's delete
     * callback is wired up (via {@link Connection#setDeleteCallback}); it must opt each table in
     * explicitly, one at a time, once it has confirmed cascading file deletion is the behavior it
     * wants for that table. An application that genuinely needs a row's files removed immediately,
     * without opting the table in generally, can call {@link #deleteAllExternalFiles} explicitly
     * at the point of deletion instead.
     *
     * @param tableName the SQL table name to opt into cascade delete
     */
    public static void cascadeDeleteFor(String tableName) {
        if (tableName != null)
            cascadeDeleteTables.add(tableName.toLowerCase());
    }

    /**
     * Row-delete callback compatible with {@link Connection#setDeleteCallback}. Removes the
     * row's external file(s) only if {@code table} was previously opted in via
     * {@link #cascadeDeleteFor(String)}; otherwise this is a no-op.
     *
     * @param table  the SQL table name a row was deleted from
     * @param pkval  the deleted row's primary key value (converted with {@code toString()} before
     *               use)
     */
    public static void deleteCallback(String table, Object pkval) {
        if (table == null || !cascadeDeleteTables.contains(table.toLowerCase())) {
            logger.debug("ExternalFile: delete callback skipped for table \"" + table + "\" (not registered for cascade delete)");
            return;
        }
        deleteAllExternalFiles(table, pkval == null ? null : pkval.toString());
        logger.info("ExternalFile: cascade-deleted files for " + table + " " + pkval);
    }

    // ------------------------------------------------------------------------------------------
    // Uploaded-file handling, with EXIF orientation normalization for image uploads
    // ------------------------------------------------------------------------------------------

    private static boolean isImageFile(String extension) {
        if (extension == null || extension.isEmpty())
            return false;
        String ext = extension.toLowerCase();
        if (ext.charAt(0) == '.')
            ext = ext.substring(1);
        return ext.equals("jpg") || ext.equals("jpeg") || ext.equals("png") ||
               ext.equals("bmp") || ext.equals("gif") || ext.equals("tiff") || ext.equals("tif");
    }

    private static byte[] normalizeImageOrientation(byte[] imageData, String extension) {
        if (!isImageFile(extension))
            return imageData;
        try {
            int orientation = Image.getExifOrientation(imageData);
            if (orientation == 1)
                return imageData;
            String format = extension.toLowerCase();
            if (format.charAt(0) == '.')
                format = format.substring(1);
            if (format.equals("jpg"))
                format = "jpeg";
            return Image.applyExifOrientation(imageData, orientation, format);
        } catch (Exception e) {
            return imageData;
        }
    }

    /**
     * Saves an uploaded file to disk, always closing {@code is}.  If the extension identifies a
     * supported image type (jpg, jpeg, png, bmp, gif, tiff/tif), the stream is first fully
     * buffered into memory, its EXIF orientation is normalized via {@link Image#getExifOrientation}
     * / {@link Image#applyExifOrientation}, and the normalized bytes are written; otherwise the
     * stream is copied straight through. This is the preferred way to persist an uploaded file.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param ext        the file extension
     * @param is         the input stream to save; always closed by this method
     * @throws IOException if an I/O error occurs
     */
    public static void saveInputStream(String name, String primaryKey, String ext, BufferedInputStream is) throws IOException {
        saveInputStream(get(name), primaryKey, ext, is);
    }

    /**
     * Saves an uploaded file to disk, always closing {@code is}.  If the extension identifies a
     * supported image type (jpg, jpeg, png, bmp, gif, tiff/tif), the stream is first fully
     * buffered into memory, its EXIF orientation is normalized via {@link Image#getExifOrientation}
     * / {@link Image#applyExifOrientation}, and the normalized bytes are written; otherwise the
     * stream is copied straight through. This is the preferred way to persist an uploaded file.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param ext        the file extension
     * @param is         the input stream to save; always closed by this method
     * @throws IOException if an I/O error occurs
     */
    public static void saveInputStream(final ExternalField field, String primaryKey, String ext, BufferedInputStream is) throws IOException {
        if (isImageFile(ext)) {
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final byte[] buf = new byte[1024];
            int len;
            while ((len = is.read(buf)) > 0)
                baos.write(buf, 0, len);
            is.close();

            final byte[] imageData = baos.toByteArray();
            final byte[] normalizedData = normalizeImageOrientation(imageData, ext);

            try (BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(makeExternalFilePath(field, primaryKey, ext)))) {
                os.write(normalizedData);
            }
        } else {
            try (BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(makeExternalFilePath(field, primaryKey, ext)))) {
                final byte[] buf = new byte[1024];
                int len;
                while ((len = is.read(buf)) > 0)
                    os.write(buf, 0, len);
            }
            is.close();
        }
    }

    // ------------------------------------------------------------------------------------------
    // Extension normalization
    // ------------------------------------------------------------------------------------------

    /**
     * Returns the file extension from the given file name (or, if already a bare extension,
     * returns it normalized).  The result always starts with "." unless empty.
     *
     * @param fname the file name, or a file extension
     * @return the file type extension (lower-cased, dot-prefixed), or an empty string if
     *         {@code fname} is null/empty or has no recognizable extension
     */
    public static String fileExtension(String fname) {
        if (fname == null || fname.isEmpty())
            return "";
        if (fname.charAt(0) == '.')
            return fname;
        int lastPeriodIndex = fname.lastIndexOf('.');
        if (lastPeriodIndex == -1)
            return fname.length() > 4 ? "" : "." + fname.toLowerCase();
        return fname.substring(lastPeriodIndex).toLowerCase();
    }

    // ------------------------------------------------------------------------------------------
    // Front-end-servable temporary copies
    // ------------------------------------------------------------------------------------------

    /**
     * Generates a URL for an external field suitable for the front-end: a temporary, randomly
     * named copy of the stored file is created (via {@link FileUtils#createReportFile(String, String)})
     * and its HTTP-relative path is returned. See {@link #getURL2} for the case where the caller
     * wants to supply its own base destination file name instead of a separate prefix/suffix pair.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param prefix     the temporary file name prefix
     * @param extension  the file name extension
     * @return the generated URL
     * @throws IOException if an I/O error occurs
     */
    public static String getURL(String name, String primaryKey, String prefix, String extension) throws IOException {
        return getURL(get(name), primaryKey, prefix, extension);
    }

    /**
     * Generates a URL for an external field suitable for the front-end: a temporary, randomly
     * named copy of the stored file is created (via {@link FileUtils#createReportFile(String, String)})
     * and its HTTP-relative path is returned. See {@link #getURL2} for the case where the caller
     * wants to supply its own base destination file name instead of a separate prefix/suffix pair.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param prefix     the temporary file name prefix
     * @param extension  the file name extension
     * @return the generated URL
     * @throws IOException if an I/O error occurs
     */
    public static String getURL(ExternalField field, String primaryKey, String prefix, String extension) throws IOException {
        extension = fileExtension(extension);
        final String fname = makeExternalFilePath(field, primaryKey, extension);
        final File tfp = FileUtils.createReportFile(prefix, extension);
        FileUtils.copy(fname, tfp.getAbsolutePath());
        return FileUtils.getHTTPPath(tfp);
    }

    /**
     * Generates a URL for an external field suitable for the front-end, using a caller-supplied
     * base destination file name (via {@link FileUtils#createReportFile(String)}) instead of a
     * separate prefix/suffix pair. {@link FileUtils#createReportFile(String)} itself randomizes
     * the actual file name it hands back, so concurrent callers passing the same {@code fname}
     * never collide on the same path; this method additionally copies the source into a unique
     * temporary file first and then atomically moves it into the final name, so a partially
     * written file is never visible at the returned path.
     *
     * @param name       the logical name registered via {@link #addField(String, String, String)}
     * @param primaryKey the primary key value for the row
     * @param fname      the base destination file name
     * @param extension  the file name extension
     * @return the generated URL
     * @throws IOException if an I/O error occurs
     */
    public static String getURL2(String name, String primaryKey, String fname, String extension) throws IOException {
        return getURL2(get(name), primaryKey, fname, extension);
    }

    /**
     * Generates a URL for an external field suitable for the front-end, using a caller-supplied
     * base destination file name (via {@link FileUtils#createReportFile(String)}) instead of a
     * separate prefix/suffix pair. {@link FileUtils#createReportFile(String)} itself randomizes
     * the actual file name it hands back, so concurrent callers passing the same {@code fname}
     * never collide on the same path; this method additionally copies the source into a unique
     * temporary file first and then atomically moves it into the final name, so a partially
     * written file is never visible at the returned path.
     *
     * @param field      the file type
     * @param primaryKey the primary key value for the row
     * @param fname      the base destination file name
     * @param extension  the file name extension
     * @return the generated URL
     * @throws IOException if an I/O error occurs
     */
    public static String getURL2(ExternalField field, String primaryKey, String fname, String extension) throws IOException {
        final File tfp = FileUtils.createReportFile(fname);
        final String fname1 = makeExternalFilePath(field, primaryKey, extension);
        // tfp's name is already randomized and guaranteed not to exist yet (FileUtils.createReportFile),
        // so two calls never target the same path. Still copy to a unique temp file first and atomically
        // rename it into place, so a caller can never observe a partially-copied file at the returned path.
        final Path tmp = Files.createTempFile(tfp.getParentFile().toPath(), tfp.getName() + ".", ".tmp");
        try {
            Files.copy(new File(fname1).toPath(), tmp, StandardCopyOption.REPLACE_EXISTING);
            Files.move(tmp, tfp.toPath(), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException e) {
            Files.deleteIfExists(tmp);
            throw e;
        }
        return FileUtils.getHTTPPath(tfp);
    }

    /**
     * A registered file type: the SQL table it belongs to and the fictitious field name
     * distinguishing it from other files associated with the same table.  Obtained via
     * {@link #get(String)} (for a name registered with {@link #addField}) or constructed
     * directly to bypass the registry.
     */
    public static class ExternalField {
        private final String tableName;
        private final String fieldName;

        /**
         * Constructs a file type directly, bypassing the {@link #addField}/{@link #get} registry.
         *
         * @param tableName the SQL table name this file type belongs to
         * @param fieldName the fictitious field name distinguishing this file type from any other
         *                  file(s) associated with the same table, or null if the table only ever
         *                  has one associated file
         */
        public ExternalField(String tableName, String fieldName) {
            this.tableName = tableName;
            this.fieldName = fieldName;
        }

        /**
         * @return the SQL table name this file type belongs to
         */
        public String getTableName() {
            return tableName;
        }

        /**
         * @return the fictitious field name distinguishing this file type from any other file(s)
         *         associated with the same table, or null
         */
        public String getFieldName() {
            return fieldName;
        }
    }
}
