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


import org.json.JSONArray;
import org.json.JSONObject;
import org.kissweb.ArrayUtils;

import java.io.*;
import java.sql.*;
import java.time.ZonedDateTime;
import java.util.*;


/**
 * Instances of this class represent a row into a result set.  If a single table is selected from, rows may be updated
 * or deleted via these facilities.
 * <br><br>
 * It is important to understand that this class represents a Kiss Cursor and not a true database cursor!  When queries
 * occur, Kiss downloads all the records in the result set either in memory or a local disk file and then closes any
 * database cursor.  The advantage of this approach is that there is never a problem with multiple database cursors
 * being open simultaneously or nested in code (which often causes database performance issues or interference).
 * However, from the application's perspective, a Kiss Cursor works just like a database cursor and has all the same
 * functionality.
 * <br><br>
 * A new cursor is created with the following code:
 * <br><br>
 *  &nbsp;&nbsp;&nbsp;&nbsp;   <code>Cursor cursor = cmd.query("select .....</code>
 * <br><br>
 *     where <code>cmd</code> is an instance of Command.
 * @see Command#query(String, Object...)
 *
 * @author Blake McBride
 */
public class Cursor implements AutoCloseable {
    private static final int BATCH_SIZE = 100;  // number of records retrieved from the database server at a time
    Command cmd;
    String tname;
    PreparedStatement pstmt;
    StringBuilder prevsql;  // this is the sql last used by pstmt
    private final ResultSetMetaData mdata;
    private Record lastRec;
    private File cacheFile;
    private ObjectInputStream cacheStream;
    private ColumnInfo [] columnInfo;
    private ArrayList<Record> memoryCache;
    private long size;

    /**
     * Read in the entire result set and cache locally.  This can be done via a temporary disk file or in-memory.
     * In-memory cache is used if <code>useMemoryCache</code>
     * is <code>true</code> or if <code>max</code> is greater than zero and less than <code>BATCH_SIZE</code>.
     * The returned <code>Cursor</code> cursors through this locally cached result set.
     *
     * @param useMemoryCache
     * @param max
     * @param cmd
     * @throws SQLException
     * @throws IOException
     */
    Cursor(boolean useMemoryCache, int max, Command cmd) throws SQLException, IOException {
        this.cmd = cmd;
        final int bs = max == 0 ? BATCH_SIZE : max;
        cmd.pstat.setFetchSize(bs);
        ResultSet rset = cmd.pstat.executeQuery();
        mdata = rset.getMetaData();
        cmd.isSelect = true;
        cacheAllRecords(useMemoryCache, max, rset);
    }

    /**
     * Returns the name of the table associated with the select associated with this cursor.
     *
     * @return
     */
    public String getTableName() {
        return tname;
    }

    /**
     * Returns the <code>Record</code> instance representing the current row.  This method does not advance the row pointer.
     *
     * @return
     *
     * @see #next()
     * @see #isNext()
     */
    public Record getRecord() {
        return lastRec;
    }

    /**
     * This method advances the row pointer and returns <code>true</code> if there is a next record.
     * @return
     * @throws SQLException
     *
     * @see #getRecord()
     * @see #next()
     */
    public boolean isNext() throws Exception {
        return null != next();
    }

    /**
     * This method is used to advance the row pointer first and then return the (next) <code>Record</code>
     * instance representing the next row.  If there are no more records, <code>null</code>
     * is returned and the cursor is closed.
     *
     * @return
     * @throws SQLException
     *
     * @see #getRecord()
     * @see #isNext()
     */
    public Record next() throws Exception {
        return lastRec = nextCachedRecord();
    }

    private static class ColumnInfo {
        String name;
        int type;
        int size;

        ColumnInfo(ResultSetMetaData mdata, int i) throws SQLException {
            name = mdata.getColumnName(i).toLowerCase();
            type = mdata.getColumnType(i);
            size = mdata.getPrecision(i);
        }
    }

    private static final int RECORD_BEGINNING = 9990;
    private static final int END_OF_RECORD    = 9991;
    private static final int END_OF_CACHE     = 9992;
    private static final int NULL_FIELD       = 9993;
    private static final int FIELD_HAS_VALUE  = 9994;

    /**
     * Read all records in, cache locally, close result set and statement.
     * <br><br>
     * This can be done via a temporary disk file or in-memory.
     * In-memory cache is used if <code>useMemoryCache</code>
     * is <code>true</code> or if <code>max</code> is greater than zero and less than <code>BATCH_SIZE</code>.
     *
     * <br><br>
     * <pre>
     * Format is:<br>
     *     Entry type: (int)
     *         1=record beginning
     *         3=end of cache (all records)
     *         4=null field
     *         5=field not null
     *     Field type - Types class (int) or 9999=end of record
     *     Field length: if not null and only for string or binary fields (short or int)
     *     Field value: if not null
     * </pre>
     */
    private void cacheAllRecords(boolean useMemoryCache, int maxRecords, ResultSet rset) throws IOException, SQLException {
        try {
            tname = mdata.getTableName(1).toLowerCase();
        } catch (SQLException e) {
            tname = null;
        }
        size = 0;
        if (useMemoryCache || maxRecords > 0  &&  maxRecords <= BATCH_SIZE) {
            cacheFile = null;
            cacheStream = null;
            memoryCache = new ArrayList<>();

            while (rset.next() && (maxRecords == 0 || size < maxRecords)) {
                size++;
                HashMap<String,Object> ocols = new HashMap<>();
                LinkedHashMap<String,Object> cols = new LinkedHashMap<>();
                int ncols = mdata.getColumnCount();
                for (int i=1 ; i <= ncols ; i++) {
                    Object val;
                    if (mdata.getColumnType(i) == Types.TIMESTAMP_WITH_TIMEZONE)
                        val = rset.getObject(i, ZonedDateTime.class);
                    else
                        val = rset.getObject(i);
                    String name = mdata.getColumnName(i).toLowerCase();
                    cols.put(name, val);
                    ocols.put(name, val);
                }
                memoryCache.add(new Record(cmd.conn, this, ocols, cols));
            }
        } else {
            memoryCache = null;
            cacheFile = File.createTempFile("cache-", ".dat");
            int ncols = -1;
            columnInfo = null;  //  initialized just to keep the IDE happy
            try (FileOutputStream fos = new FileOutputStream(cacheFile);
                 BufferedOutputStream bos = new BufferedOutputStream(fos);
                 ObjectOutputStream oos = new ObjectOutputStream(bos)) {
                while (rset.next() && (maxRecords == 0 || size < maxRecords)) {
                    size++;
                    int len;
                    if (ncols == -1) {
                        ncols = mdata.getColumnCount();
                        columnInfo = new ColumnInfo[ncols];
                        for (int i = 0; i < ncols; i++)
                            columnInfo[i] = new ColumnInfo(mdata, i+1);
                    }
                    oos.writeInt(RECORD_BEGINNING);
                    for (int i = 0; i < ncols; i++) {
                        ColumnInfo ci = columnInfo[i];
                        Object val;
                        if (ci.type == Types.TIMESTAMP_WITH_TIMEZONE)
                            val = rset.getObject(i+1, ZonedDateTime.class);
                        else
                            val = rset.getObject(i+1);
                        oos.writeInt(ci.type);
                        if (val == null) {
                            oos.writeInt(NULL_FIELD);
                            continue;
                        } else
                            oos.writeInt(FIELD_HAS_VALUE);
                        switch (ci.type) {
                            case Types.SMALLINT:
                                short s = (short)(int)(Integer) val;
                                oos.writeShort(s);
                                break;
                            case Types.INTEGER:
                                oos.writeInt((Integer) val);
                                break;
                            case Types.BIGINT:
                                oos.writeLong((Long) val);
                                break;
                            case Types.REAL:
                            case Types.FLOAT:
                                oos.writeFloat((Float) val);
                                break;
                            case Types.DOUBLE:
                                oos.writeDouble((Double) val);
                                break;
                            case Types.CHAR:
                            case Types.VARCHAR:
                            case Types.LONGVARCHAR:
                                len = ((String) val).length();
                                oos.writeInt(len);
                                if (len > 0)
                                    oos.writeObject(val);
                                break;
                            case Types.TIMESTAMP:
                            case Types.TIMESTAMP_WITH_TIMEZONE:
                            case Types.DATE:
                            case Types.TIME:
                            case Types.TIME_WITH_TIMEZONE:
                            case Types.NCHAR:
                            case Types.NVARCHAR:
                            case Types.LONGNVARCHAR:
                                oos.writeObject(val);
                                break;
                            case Types.BINARY:
                            case Types.VARBINARY:
                            case Types.LONGVARBINARY:
                            case Types.BLOB:
                                byte [] bytes = (byte[]) val;
                                len = bytes.length;
                                oos.writeInt(len);
                                if (len > 0)
                                    oos.write(bytes);
                                break;
                            default:
                                throw new SQLException("Unhandled data type " + ci.type);
                        }
                    }
                    oos.writeInt(END_OF_RECORD);
                }
                oos.writeInt(END_OF_CACHE);
            }
            FileInputStream fis = new FileInputStream(cacheFile);
            BufferedInputStream bis = new BufferedInputStream(fis);
            cacheStream = new ObjectInputStream(bis);
        }
        rset.close();
    }

    private Record closeCache() {
        try {
            if (cacheStream != null)
                cacheStream.close();
        } catch (Exception ignore) {

        } finally {
            if (cacheFile != null)
                cacheFile.delete();
            cacheStream = null;
            cacheFile = null;
            columnInfo = null;
            memoryCache = null;
        }
        return null;
    }

    private Record nextCachedRecord() throws Exception {
        if (memoryCache != null) {
            if (memoryCache.isEmpty())
                return null;
            return memoryCache.remove(0);
        }
        if (cacheFile == null || cacheStream == null)
            return closeCache();
        int recType = cacheStream.readInt();
        if (recType == END_OF_CACHE)
            return closeCache();
        if (recType != RECORD_BEGINNING)
            throw new Exception("Cursor: JDBC Cache: expected RECORD_BEGINNING");
        final LinkedHashMap<String,Object> cols = new LinkedHashMap<>();
        final HashMap<String,Object> ocols = new HashMap<>();
        int i = 0;
        while (true) {
            Object obj;
            int len;
            int fieldType = cacheStream.readInt();
            if (fieldType == END_OF_RECORD)
                break;
            ColumnInfo ci = columnInfo[i++];
            int nullField = cacheStream.readInt();
            if (nullField == NULL_FIELD) {
                cols.put(ci.name, null);
                ocols.put(ci.name, null);
                continue;
            }
            switch (fieldType) {
                case Types.SMALLINT:
                    cols.put(ci.name, obj=cacheStream.readShort());
                    ocols.put(ci.name, obj);
                    break;
                case Types.INTEGER:
                    cols.put(ci.name, obj=cacheStream.readInt());
                    ocols.put(ci.name, obj);
                    break;
                case Types.BIGINT:
                    cols.put(ci.name, obj=cacheStream.readLong());
                    ocols.put(ci.name, obj);
                    break;
                case Types.REAL:
                case Types.FLOAT:
                    cols.put(ci.name, obj=cacheStream.readFloat());
                    ocols.put(ci.name, obj);
                    break;
                case Types.DOUBLE:
                    cols.put(ci.name, obj=cacheStream.readDouble());
                    ocols.put(ci.name, obj);
                    break;
                case Types.CHAR:
                case Types.VARCHAR:
                case Types.LONGVARCHAR:
                    len = cacheStream.readInt();
                    cols.put(ci.name, obj=len > 0 ? cacheStream.readObject() : "");
                    ocols.put(ci.name, obj);
                    break;
                case Types.TIMESTAMP:
                case Types.TIMESTAMP_WITH_TIMEZONE:
                case Types.DATE:
                case Types.TIME:
                case Types.TIME_WITH_TIMEZONE:
                case Types.NCHAR:
                case Types.NVARCHAR:
                case Types.LONGNVARCHAR:
                    cols.put(ci.name, obj=cacheStream.readObject());
                    ocols.put(ci.name, obj);
                    break;
                case Types.BINARY:
                case Types.VARBINARY:
                case Types.LONGVARBINARY:
                case Types.BLOB:
                    len = cacheStream.readInt();
                    if (len > 0) {
                        byte[] bytes = new byte[len];
                        int total_read = 0;
                        while (total_read < len)
                            total_read += cacheStream.read(bytes, total_read, len-total_read);
                        Byte [] bytes2 = new Byte[bytes.length];
                        Arrays.setAll(bytes2, n -> bytes[n]);
                        cols.put(ci.name, bytes2);
                        ocols.put(ci.name, bytes2);
                    }
                    break;
                default:
                    throw new SQLException("Unhandled data type " + fieldType);
            }
        }
        return new Record(cmd.conn, this, ocols, cols);
    }

    /**
     * Read in a single record and close the read cursor (no more records can be read).
     * The record read can be updated or deleted.  <code>null</code> is returned if
     * there is no record.
     *
     * @return the Record or null if none
     * @throws SQLException
     */
    public Record fetchOne() throws Exception {
        lastRec = next();
        return lastRec;
    }

    /**
     * This method works exactly like <code>fetchOne</code> except that it return a JSON object representing the record.
     * <code>null</code> is returned if there is no record.
     *
     * @return the JSON object or <code>null</code> if none
     * @throws SQLException
     *
     * @see #fetchOne()
     */
    public JSONObject fetchOneJSON() throws Exception {
        Record rec = fetchOne();
        return rec != null ? rec.toJSON() : null;
    }

    /**
     * This method works like <code>fetchOne</code> except that it adds the record columns to an existing JSON object
     * passed in.
     *
     * @param obj
     * @return the JSON object passed in
     * @throws SQLException
     */
    public JSONObject fetchOneJSON(JSONObject obj) throws Exception {
        Record rec = fetchOne();
        if (rec != null)
            rec.addToJSON(obj);
        return obj;
    }

    /**
     * Read in all of the records and close the cursor.
     * Records can be updated or deleted.
     *
     * If no records are found, an empty list is returned.
     *
     * @return
     * @throws SQLException
     */
    public List<Record> fetchAll() throws Exception {
        if (memoryCache != null) {
            final List<Record> recs = memoryCache;
            closeCache();
            lastRec = null;
            return recs;
        }
        final List<Record> r = new ArrayList<>();
        Record rec;
        while (null != (rec=next()))
            r.add(rec);
        closeCache();
        lastRec = null;
        return r;
    }

    /**
     * This method does the same thing as <code>fetchAll</code> except that it return a
     * JSON array representing all of the records.
     *
     * @return
     * @throws SQLException
     *
     * @see #fetchAll()
     */
    public JSONArray fetchAllJSON() throws Exception {
        return Record.toJSONArray(fetchAll());
    }

    /**
     * Close the entire cursor.  No more read, edit, or deletes.
     * Since this class implement AutoCloseable, this method is rarely needed.
     *
     * @throws SQLException
     */
    @Override
    public void close() throws SQLException {
        closeCache();
        if (pstmt != null) {
            pstmt.close();
            pstmt = null;
        }
        prevsql = null;
        lastRec = null;
    }

    /**
     * Set the value of a column in the current record.
     * This method should not be used with dates or times.
     *
     * @param name the column name
     * @param val the value to set.  Can be any type.
     * @return
     *
     * @see Record#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setDateTime(String, java.util.Date)
     */
    public Cursor set(String name, Object val) {
        lastRec.cols.put(name.toLowerCase(), val);
        return this;
    }

    /**
     * Set the date portion of the Date only (no time info) value of a column in the record.
     *
     * @param name the column name
     * @param val the value to set.
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     * @see #setDateOnly(String, int)
     * @see #setTime(String, long)
     */
    public Cursor setDateOnly(String name, java.util.Date val) {
        lastRec.cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
        return this;
    }

    /**
     * Set the time value in milliseconds
     *
     * @param name the column name
     * @param dat the value to set in milliseconds
     * @return
     *
     * @see #setDateOnly(String, java.util.Date)
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     */
    public Cursor setTime(String name, long dat) {
        if (dat == 0) {
            lastRec.cols.put(name.toLowerCase(), null);
            return this;
        }
        java.util.Date val = new java.util.Date(dat);
        lastRec.cols.put(name.toLowerCase(), new java.sql.Time(val.getTime()));
        return this;
    }

    /**
     * Set the date portion of the Date only (no time info) value of a column in the record.
     *
     * @param name the column name
     * @param dat the value to set. Format is YYYYMMDD
     * @return
     *
     * @see #setDateOnly(String, java.util.Date)
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     * @see #setTime(String, long)
     */
    public Cursor setDateOnly(String name, int dat) {
        if (dat == 0) {
            lastRec.cols.put(name.toLowerCase(), null);
            return this;
        }
        int y = dat / 10000;
        int m = (dat % 10000) / 100;
        int d = dat % 100;
        java.util.Date val = new GregorianCalendar(y, m-1, d).getTime();
        lastRec.cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
        return this;
    }

    /**
     * Set the date and time value of a column in the record.
     *
     * @param name the column name
     * @param val the value to set.
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see #set(String, Object)
     */
    public Cursor setDateTime(String name, java.util.Date val) {
        lastRec.cols.put(name.toLowerCase(), val == null ? null : new java.sql.Timestamp(val.getTime()));
        return this;
    }

    /**
     * Set the date and time value of a column in the record.
     *
     * @param name the column name
     * @param val the number of milliseconds since 1970 UTC
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see #set(String, Object)
     */
    public Cursor setDateTime(String name, long val) {
        lastRec.cols.put(name.toLowerCase(), val == 0 ? null : new java.sql.Timestamp(val));
        return this;
    }

    /**
     * Get the value of a column as an <code>Object</code>.  Other methods that get
     * expected types are typically used over this method.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#get(String)
     * @see #getShort(String)
     * @see #getLong(String)
     * @see #getString(String)
     * etc.
     */
    public Object get(String cname) throws SQLException {
        cname = cname.toLowerCase();
        if (lastRec.cols.containsKey(cname))
            return lastRec.cols.get(cname);
        else
            throw new SQLException("Column " + cname + " not found.");
    }

    /**
     * Return the <code>Short</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getShort(String)
     */
    public Short getShort(String cname) throws SQLException {
        // some databases return an Integer even though the field is a smallint
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Short.class)
            return (Short) obj;
        else if (obj.getClass() == Integer.class) {
            int i = (Integer) obj;
            return (short) i;
        }
        throw new SQLException("column " + cname + " is not a short or integer");
    }

    /**
     * Return the <code>Integer</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getInt(String)
     */
    public Integer getInt(String cname) throws SQLException {
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Integer.class)
            return (Integer) obj;
        if (obj.getClass() == Short.class)
            return ((Short) obj).intValue();
        throw new SQLException("column " + cname + " is not an integer or short");
    }

    /**
     * Return the <code>Long</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getLong(String)
     */
    public Long getLong(String cname) throws SQLException {
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Long.class)
            return (Long) obj;
        if (obj.getClass() == Integer.class)
            return ((Integer) obj).longValue();
        if (obj.getClass() == Short.class)
            return ((Short) obj).longValue();
        throw new SQLException("column " + cname + " is not a long or integer or short");
    }

    /**
     * Return the <code>Float</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getFloat(String)
     */
    public Float getFloat(String cname) throws SQLException {
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Float.class)
            return (Float) obj;
        if (obj.getClass() == Double.class)
            return ((Double) obj).floatValue();
        if (obj.getClass() == Integer.class)
            return ((Integer) obj).floatValue();
        if (obj.getClass() == Short.class)
            return ((Short) obj).floatValue();
        if (obj.getClass() == Long.class)
            return ((Long) obj).floatValue();
        throw new SQLException("column " + cname + " is not a float or double or integer or short or long");
    }

    /**
     * Return the <code>Double</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getDouble(String)
     */
    public Double getDouble(String cname) throws SQLException {
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Double.class)
            return (Double) obj;
        if (obj.getClass() == Float.class)
            return ((Float) obj).doubleValue();
        if (obj.getClass() == Integer.class)
            return ((Integer) obj).doubleValue();
        if (obj.getClass() == Short.class)
            return ((Short) obj).doubleValue();
        if (obj.getClass() == Long.class)
            return ((Long) obj).doubleValue();
        throw new SQLException("column " + cname + " is not a float or double or integer or short or long");
    }

    /**
     * Return the <code>java.util.Date</code> value of the named column.
     * That is a date without a time.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateAsInt(String)
     * @see Record#getDateOnly(String)
     * @see #getDateTime(String)
     */
    public java.util.Date getDateOnly(String cname) throws SQLException {
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj instanceof java.sql.Date) {
            java.sql.Date dt = (java.sql.Date) obj;
            return new java.util.Date(dt.getTime());
        } else {
            // assume Timestamp
            java.sql.Timestamp ts = (java.sql.Timestamp) obj;
            Calendar cal = Calendar.getInstance();
            cal.setTime(ts);
            cal.set(Calendar.HOUR, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            return cal.getTime();
        }
    }

    /**
     * Return the date in an int formatted as YYYYMMDD for the named column.
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return date format YYYYMMDD
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     * @see Cursor#getDateOnly(String)
     * @see #getDateTime(String)
     */
    public int getDateAsInt(String cname) throws SQLException {
        Calendar cal;
        Object obj = get(cname);
        if (obj == null)
            return 0;
        if (obj instanceof java.sql.Date) {
            java.sql.Date dt = (java.sql.Date) obj;
            cal = Calendar.getInstance();
            cal.setTime(dt);
        } else {
            // assume Timestamp
            java.sql.Timestamp ts = (java.sql.Timestamp) obj;
            cal = Calendar.getInstance();
            cal.setTime(ts);
        }
        return cal.get(Calendar.DAY_OF_MONTH) + ((cal.get(Calendar.MONTH) + 1) * 100) + ((cal.get(Calendar.YEAR)) * 10000);
    }

    /**
     * Return the <code>java.util.Date</code> value of the named column.
     * Date and time information.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     * @see #getTime(String)
     * @see #getDateTimeMS(String)
     */
    public java.util.Date getDateTime(String cname) throws SQLException {
        Timestamp ts = (Timestamp) get(cname);
        if (ts == null)
            return null;
        return new java.util.Date(ts.getTime());
    }

    /**
     * Return the date/time value as the number of milliseconds since 1970 UTC.
     *
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     * @see #getTime(String)
     * @see #getDateTime(String)
     */
    public long getDateTimeMS(String cname) throws SQLException {
        Timestamp ts = (Timestamp) get(cname);
        if (ts == null)
            return 0L;
        return ts.getTime();
    }

    /**
     * Return the <code>long</code> value of the named column.
     * Only time information is returned.
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return time in milliseconds
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     */
    public long getTime(String cname) throws SQLException {
        java.sql.Time ts = (java.sql.Time) get(cname);
        if (ts == null)
            return 0;
        return ts.getTime();
    }

    /**
     * Return the <code>String</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getString(String)
     */
    public String getString(String cname) throws SQLException {
        return (String) get(cname);
    }

    /**
     * Return the <code>Character</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#getChar(String)
     */
    public Character getChar(String cname) throws SQLException {
        String s = (String) get(cname);
        if (s == null)
            return null;
        if (s.length() != 1)
            throw new SQLException("Column \"" + cname + "\" not a single character");
        return s.charAt(0);
    }

    /**
     * Return the <code>byte[]</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Record#get(String)
     */
    public byte [] getByteArray(String cname) throws SQLException {
        Object ba = get(cname);
        if (ba instanceof byte[])
            return (byte[]) ba;
        return ArrayUtils.toPrimitiveByteArray((Byte[]) ba);
    }

    /**
     * Returns the number of records in the cursor.
     *
     * @return
     */
    public long size() {
        return size;
    }

}