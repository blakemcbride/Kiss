package org.kissweb.database;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Path;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DB-backed round-trip tests for {@link ExternalFile#saveHybridColumn} / {@link ExternalFile#getHybridColumn}
 * (the real-column, inline/external hybrid storage mechanism). Covers: a small value staying
 * inline, a large value overflowing to an external file, a value that collides with the internal
 * sentinel/escape sequences, every storage-direction transition, an external-to-external overwrite,
 * null clearing both the column and any external file, and cascade-delete removing a hybrid file
 * via the existing {@code deleteCallback}/{@code cascadeDeleteFor} mechanism.
 * <br><br>
 * <b>Disabled by default</b> -- follows the same convention as {@link RecordMultiSchemaRegressionTest}:
 * requires a reachable PostgreSQL server and creates/drops its own throwaway schema (named with a
 * random UUID, never touching any real tenant schema or the {@code stack360} application database).
 * Enable explicitly with {@code -Dkiss.test.postgres=true} (optionally override
 * {@code -Dkiss.test.postgres.url}, {@code -Dkiss.test.postgres.user}, {@code -Dkiss.test.postgres.password}).
 * <br><br>
 * Every test uses the same table name and the same column size ({@code data_col varchar(20)}) so
 * that {@code ExternalFile}'s JVM-wide table+column size cache can never return a stale answer
 * across test methods, even though each method runs in its own freshly created/dropped schema.
 */
@EnabledIfSystemProperty(named = "kiss.test.postgres", matches = "true")
class ExternalFileHybridColumnTest {

    private static final String TABLE = "hybrid_test";
    private static final String COLUMN = "data_col";
    private static final int COLUMN_SIZE = 20;

    private String schema;
    private Connection db;
    private java.sql.Connection raw;

    @TempDir
    Path tempDir;

    @BeforeEach
    void setUp() throws Exception {
        String url = System.getProperty("kiss.test.postgres.url", "jdbc:postgresql://localhost/postgres");
        String user = System.getProperty("kiss.test.postgres.user", "postgres");
        String password = System.getProperty("kiss.test.postgres.password", "");
        Class.forName("org.postgresql.Driver");
        raw = DriverManager.getConnection(url, user, password);
        raw.setAutoCommit(true);
        schema = "kiss_test_hybrid_" + UUID.randomUUID().toString().replace("-", "_");
        try (Statement st = raw.createStatement()) {
            st.execute("CREATE SCHEMA " + schema);
            st.execute("CREATE TABLE " + schema + "." + TABLE + " (id INTEGER PRIMARY KEY, " + COLUMN + " VARCHAR(" + COLUMN_SIZE + "))");
            st.execute("set search_path to " + schema);
        }
        db = new Connection(raw);

        ExternalFile.setRootSupplier(() -> tempDir.toString());
        ExternalFile.setDirectoryMapper((table, primaryKey) -> table + "/" + primaryKey);
    }

    @AfterEach
    void tearDown() throws Exception {
        if (raw != null) {
            try (Statement st = raw.createStatement()) {
                st.execute("DROP SCHEMA IF EXISTS " + schema + " CASCADE");
            } finally {
                raw.close();
            }
        }
    }

    private void insertRow(int id) throws Exception {
        db.execute("INSERT INTO " + TABLE + " (id) VALUES (?)", id);
    }

    /** Reads the raw column value directly, bypassing {@code getHybridColumn}'s inline/external logic. */
    private String rawColumnValue(int id) throws Exception {
        Record rec = db.fetchOne("SELECT " + COLUMN + " FROM " + TABLE + " WHERE id = ?", id);
        return rec == null ? null : rec.getString(COLUMN);
    }

    /** Independently recomputes the deterministic external-file path this test's directoryMapper implies. */
    private File expectedExternalFile(int id) {
        return new File(tempDir.toFile(), TABLE + "/" + id + "/" + id + "-" + COLUMN + ".col");
    }

    @Test
    void testReleaseHybridColumn() throws Exception {
        insertRow(9);
        String big = "x".repeat(100);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "9", big);
        assertTrue(expectedExternalFile(9).exists());
        ExternalFile.releaseHybridColumn(db, TABLE, COLUMN, "9", true);
        assertFalse(expectedExternalFile(9).exists());
        assertNull(rawColumnValue(9));
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "9", big);
        ExternalFile.releaseHybridColumn(db, TABLE, COLUMN, "9", false);
        assertFalse(expectedExternalFile(9).exists());
        assertEquals("", rawColumnValue(9));
    }

    @Test
    void testSmallValueStoredInline() throws Exception {
        insertRow(1);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "1", "hello");
        assertEquals("hello", ExternalFile.getHybridColumn(db, TABLE, COLUMN, "1"));
        assertEquals("hello", rawColumnValue(1), "short data must be stored verbatim, with no sentinel/escape overhead");
        assertFalse(expectedExternalFile(1).exists(), "no external file should be created for inline data");
    }

    @Test
    void testLargeValueStoredExternally() throws Exception {
        insertRow(2);
        String big = "x".repeat(100);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "2", big);
        assertEquals(big, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "2"));
        String stored = rawColumnValue(2);
        assertNotEquals(big, stored, "data too large for the column must not be stored verbatim in it");
        assertTrue(stored.length() <= COLUMN_SIZE, "the sentinel left in the column must itself fit the column");
        assertTrue(expectedExternalFile(2).isFile(), "an external file must have been created");
        assertEquals(big, java.nio.file.Files.readString(expectedExternalFile(2).toPath()));
    }

    @Test
    void testSentinelLookalikeValueRoundTrips() throws Exception {
        insertRow(3);
        // Mirrors ExternalFile's private EXTERNAL_MARKER sentinel ("\u0001KFX\u0001") followed by
        // genuine trailing data -- must round-trip exactly, not be misread as an external reference.
        String lookalike = "\u0001KFX\u0001hi";
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "3", lookalike);
        assertEquals(lookalike, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "3"));
        assertNotEquals(lookalike, rawColumnValue(3), "a sentinel-lookalike value must be escaped before storage");
        assertFalse(expectedExternalFile(3).exists(), "a sentinel-lookalike value that still fits must stay inline, escaped");

        // Also mirrors the escape prefix itself ("\u0001KFI\u0001") -- must also round-trip exactly.
        insertRow(4);
        String escapeLookalike = "\u0001KFI\u0001yo";
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "4", escapeLookalike);
        assertEquals(escapeLookalike, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "4"));
        assertNotEquals(escapeLookalike, rawColumnValue(4));
    }

    @Test
    void testTransitionsInlineToExternalAndBackToInline() throws Exception {
        insertRow(5);

        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "5", "abc");
        assertEquals("abc", rawColumnValue(5));
        assertFalse(expectedExternalFile(5).exists());

        String big = "y".repeat(50);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "5", big);
        assertEquals(big, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "5"));
        assertTrue(expectedExternalFile(5).isFile(), "inline -> external must create the file");

        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "5", "xyz");
        assertEquals("xyz", ExternalFile.getHybridColumn(db, TABLE, COLUMN, "5"));
        assertEquals("xyz", rawColumnValue(5));
        assertFalse(expectedExternalFile(5).exists(), "external -> inline must delete the now-stale external file");
    }

    @Test
    void testExternalToExternalOverwrite() throws Exception {
        insertRow(6);
        String valueA = "a".repeat(40);
        String valueB = "b".repeat(60);

        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "6", valueA);
        assertEquals(valueA, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "6"));

        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "6", valueB);
        assertEquals(valueB, ExternalFile.getHybridColumn(db, TABLE, COLUMN, "6"), "overwrite must replace, not append to, the external file");
        assertEquals(valueB, java.nio.file.Files.readString(expectedExternalFile(6).toPath()));
    }

    @Test
    void testNullClearsColumnAndRemovesExternalFile() throws Exception {
        insertRow(7);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "7", "z".repeat(50));
        assertTrue(expectedExternalFile(7).isFile());

        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "7", null);
        assertNull(ExternalFile.getHybridColumn(db, TABLE, COLUMN, "7"));
        assertNull(rawColumnValue(7));
        assertFalse(expectedExternalFile(7).exists(), "saving null must remove any external file");
    }

    @Test
    void testCascadeDeleteRemovesHybridExternalFile() throws Exception {
        db.setDeleteCallback(ExternalFile::deleteCallback);
        ExternalFile.cascadeDeleteFor(TABLE);

        insertRow(8);
        ExternalFile.saveHybridColumn(db, TABLE, COLUMN, "8", "w".repeat(50));
        assertTrue(expectedExternalFile(8).isFile());

        Record rec = db.fetchOne("SELECT id FROM " + TABLE + " WHERE id = ?", 8);
        assertNotNull(rec);
        rec.delete();

        assertFalse(expectedExternalFile(8).exists(), "cascade delete must remove the hybrid-column's external file too");
    }
}
