package org.kissweb.database;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;

import java.sql.DriverManager;
import java.sql.Statement;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * {@link Connection#getColumnWidth(String, String)} against PostgreSQL, where the table and column names handed to
 * {@code DatabaseMetaData.getColumns} are patterns in which {@code _} matches any single character, and where text and
 * unbounded character columns are reported as {@link Integer#MAX_VALUE}.
 * <br><br>
 * <b>Disabled by default</b>, with the same convention as {@link ExternalFileHybridColumnTest}: it needs a reachable
 * PostgreSQL server and creates and drops its own throwaway schema (named with a random UUID).  Enable with
 * {@code -Dkiss.test.postgres=true} (optionally {@code -Dkiss.test.postgres.url}, {@code .user}, {@code .password}).
 */
@EnabledIfSystemProperty(named = "kiss.test.postgres", matches = "true")
class ConnectionColumnWidthPostgresTest {

    private String schema;
    private java.sql.Connection raw;
    private Connection db;

    @BeforeEach
    void setUp() throws Exception {
        String url = System.getProperty("kiss.test.postgres.url", "jdbc:postgresql://localhost/postgres");
        String user = System.getProperty("kiss.test.postgres.user", "postgres");
        String password = System.getProperty("kiss.test.postgres.password", "");
        Class.forName("org.postgresql.Driver");
        raw = DriverManager.getConnection(url, user, password);
        raw.setAutoCommit(true);
        schema = "kiss_test_width_" + UUID.randomUUID().toString().replace("-", "_");
        try (Statement st = raw.createStatement()) {
            st.execute("CREATE SCHEMA " + schema);
            st.execute("set search_path to " + schema);
            st.execute("CREATE TABLE pw_t (id integer primary key, a_b varchar(10), aXb varchar(30), body text, free varchar)");
            st.execute("CREATE TABLE pwXt (id integer primary key, a_b varchar(77))");
        }
        db = new Connection(raw);
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

    @Test
    void widthsAreExactAndCaseInsensitive() throws Exception {
        assertEquals(10, db.getColumnWidth("pw_t", "a_b"));
        assertEquals(30, db.getColumnWidth("pw_t", "axb"));       //PostgreSQL folds the unquoted aXb to axb
        assertEquals(10, db.getColumnWidth("pw_t", "A_B"));
        assertEquals(77, db.getColumnWidth("pwxt", "a_b"));
    }

    @Test
    void lookAlikeTableIsNotMixedIn() throws Exception {
        //pw_t as a pattern also matches pwxt, which has a different a_b; each table must answer for itself.
        assertEquals(10, db.getColumnWidth("pw_t", "a_b"));
        assertEquals(77, db.getColumnWidth("pwxt", "a_b"));
    }

    @Test
    void textAndUnboundedCharacterColumnsAreUnlimited() throws Exception {
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("pw_t", "body"));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("pw_t", "free"));
    }

    @Test
    void aMissingColumnIsUnlimited() throws Exception {
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("pw_t", "nope"));
        assertEquals(Connection.UNLIMITED_COLUMN_WIDTH, db.getColumnWidth("no_such_table", "id"));
    }
}
