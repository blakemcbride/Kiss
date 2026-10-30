package org.kissweb.database;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link Connection#getColumnWidth(String, String)} against a SQLite database, and for
 * {@link ExternalFile}'s use of the same lookup.  The width cache is JVM-wide, so each test uses its own table names.
 */
class ConnectionColumnWidthTest {

    @TempDir
    Path tempDir;

    private String url;
    private java.sql.Connection raw;
    private Connection db;

    @BeforeEach
    void setUp() throws Exception {
        Class.forName("org.sqlite.JDBC");
        url = "jdbc:sqlite:" + tempDir.resolve("w.db");
        raw = DriverManager.getConnection(url);
        db = new Connection(raw);
        ExternalFile.setRootSupplier(() -> tempDir.resolve("files").toString());
        ExternalFile.setDirectoryMapper((table, pk) -> table + "/" + pk);
    }

    @AfterEach
    void tearDown() throws Exception {
        ExternalFile.setRootSupplier(null);
        ExternalFile.setDirectoryMapper(null);
        raw.close();
    }

    private void sql(String... statements) throws Exception {
        try (Statement st = raw.createStatement()) {
            for (String s : statements)
                st.execute(s);
        }
    }

    @Test
    void boundedColumnReportsItsDeclaredWidth() throws Exception {
        sql("create table cw_basic (id integer primary key, name varchar(20), code char(5))");
        assertEquals(20, db.getColumnWidth("cw_basic", "name"));
        assertEquals(5, db.getColumnWidth("cw_basic", "code"));
    }

    @Test
    void namesAreCaseInsensitive() throws Exception {
        sql("create table cw_case (id integer primary key, First_Name varchar(12))");
        assertEquals(12, db.getColumnWidth("cw_case", "first_name"));
        assertEquals(12, db.getColumnWidth("cw_case", "FIRST_NAME"));
        assertEquals(12, db.getColumnWidth("CW_CASE", "First_Name"));
    }

    @Test
    void unboundedColumnIsEffectivelyUnlimited() throws Exception {
        sql("create table cw_text (id integer primary key, body text)");
        int w = db.getColumnWidth("cw_text", "body");
        assertTrue(w >= 1_000_000_000, "an unbounded text column reports a huge width, got " + w);
    }

    @Test
    void missingColumnOrTableIsUnlimitedAndDoesNotThrow() throws Exception {
        sql("create table cw_missing (id integer primary key, name varchar(8))");
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("cw_missing", "nope"));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("cw_no_such_table", "name"));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth(null, "name"));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("cw_missing", null));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("", ""));
        assertEquals(Integer.MAX_VALUE, Connection.UNLIMITED_COLUMN_WIDTH);
    }

    @Test
    void underscoreIsACharacterNotAWildcard() throws Exception {
        //'_' is the single-character wildcard of a DatabaseMetaData pattern: a_b would also match aXb.
        sql("create table cw_wild (id integer primary key, a_b varchar(10), aXb varchar(30))");
        assertEquals(10, db.getColumnWidth("cw_wild", "a_b"));
        assertEquals(30, db.getColumnWidth("cw_wild", "aXb"));
    }

    @Test
    void lookAlikeTableNamesAreNotMixed() throws Exception {
        //The table name is a pattern too: wid_t would also match widXt, whose column c has another width.
        sql("create table cw_t (id integer primary key, c varchar(5))",
                "create table cwXt (id integer primary key, c varchar(7))");
        assertEquals(5, db.getColumnWidth("cw_t", "c"));
        assertEquals(7, db.getColumnWidth("cwXt", "c"));
    }

    @Test
    void anAnswerIsCachedAcrossConnections() throws Exception {
        sql("create table cw_cache (id integer primary key, name varchar(20))");
        assertEquals(20, db.getColumnWidth("cw_cache", "name"));
        sql("drop table cw_cache");
        //A different Connection object has no per-connection metadata cache of its own; only the shared width cache can answer.
        assertEquals(20, new Connection(raw).getColumnWidth("cw_cache", "name"));
    }

    @Test
    void aMissingColumnIsNotCachedSoALaterColumnIsFound() throws Exception {
        sql("create table cw_late (id integer primary key)");
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("cw_late", "added"));
        sql("alter table cw_late add column added varchar(9)");
        assertEquals(9, new Connection(raw).getColumnWidth("cw_late", "added"));
    }

    @Test
    void aMetadataFailurePropagates() throws Exception {
        sql("create table cw_closed (id integer primary key, name varchar(4))");
        java.sql.Connection other = DriverManager.getConnection(url);
        Connection closed = new Connection(other);
        other.close();
        assertThrows(SQLException.class, () -> closed.getColumnWidth("cw_closed", "name"));
    }

    @Test
    void concurrentLookupsAgree() throws Exception {
        sql("create table cw_threads (id integer primary key, name varchar(20))");
        ExecutorService pool = Executors.newFixedThreadPool(8);
        try {
            List<Future<Integer>> results = new ArrayList<>();
            for (int t = 0; t < 8; t++)
                results.add(pool.submit((Callable<Integer>) () -> {
                    try (java.sql.Connection c = DriverManager.getConnection(url)) {
                        Connection mine = new Connection(c);
                        int last = 0;
                        for (int i = 0; i < 200; i++)
                            last = mine.getColumnWidth("cw_threads", "NAME");
                        return last;
                    }
                }));
            for (Future<Integer> f : results)
                assertEquals(20, f.get());
        } finally {
            pool.shutdownNow();
        }
    }

    // ---------------------------------------------------------------- ExternalFile keeps its own behavior

    @Test
    void externalFileStillThrowsForAMissingColumn() throws Exception {
        sql("create table cw_ef_missing (id integer primary key, data_col varchar(40))");
        SQLException e = assertThrows(SQLException.class,
                () -> ExternalFile.saveHybridColumn(db, "cw_ef_missing", "no_such_col", "1", "x"));
        assertEquals("Column \"no_such_col\" not found in table \"cw_ef_missing\".", e.getMessage());
    }

    @Test
    void externalFileStillRejectsAColumnTooNarrowForTheMarker() throws Exception {
        sql("create table cw_ef_narrow (id integer primary key, data_col varchar(3))");
        SQLException e = assertThrows(SQLException.class,
                () -> ExternalFile.saveHybridColumn(db, "cw_ef_narrow", "data_col", "1", "x"));
        assertTrue(e.getMessage().contains("too narrow"), e.getMessage());
        assertTrue(e.getMessage().contains("(size 3)"), e.getMessage());
    }

    @Test
    void externalFileStillStoresAValueThatFitsInline() throws Exception {
        sql("create table cw_ef_ok (id integer primary key, data_col varchar(40))",
                "insert into cw_ef_ok (id) values (1)");
        ExternalFile.saveHybridColumn(db, "cw_ef_ok", "data_col", "1", "hello");
        assertEquals("hello", ExternalFile.getHybridColumn(db, "cw_ef_ok", "data_col", "1"));
    }
}
