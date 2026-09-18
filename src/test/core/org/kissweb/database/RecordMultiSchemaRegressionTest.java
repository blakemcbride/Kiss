package org.kissweb.database;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;

import java.sql.DriverManager;
import java.sql.Statement;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DB-backed regression test for the multi-schema primary-key resolution fix.  Reproduces the
 * production failure directly: the same table name deployed in more than one PostgreSQL schema,
 * then an {@code update()} and {@code delete()} of a row fetched via a SELECT cursor -- exactly
 * the code path that broke when {@code Command.getPriColumns()} resolved a table's primary-key
 * columns independently of, and inconsistently with, {@code Connection.getPrimaryColumns()}:
 * {@code Record.update()}/{@code delete()} build their WHERE-clause SQL text from the latter (1
 * column, correctly scoped) but bound WHERE values -- for a cursor-backed record -- from the
 * former (N columns, one per schema, unscoped), throwing a JDBC "column index out of range"
 * error on every update/delete of a fetched row from such a table. Now that
 * {@code Command.getPriColumns()} delegates to {@code Connection.getPrimaryColumns()} (see that
 * method's javadoc), both call sites agree by construction.
 * <br><br>
 * <b>Disabled by default.</b> This test requires a reachable PostgreSQL server and creates/drops
 * its own throwaway schemas (named with a random UUID, never touching any real tenant schema or
 * the {@code stack360} application database), which is inappropriate to run unattended against a
 * shared server on every unit-test invocation. Enable explicitly with
 * {@code -Dkiss.test.postgres=true} (optionally override
 * {@code -Dkiss.test.postgres.url=jdbc:postgresql://host/db},
 * {@code -Dkiss.test.postgres.user=...}, {@code -Dkiss.test.postgres.password=...} -- defaults
 * connect to the local {@code postgres} admin database, not any application database).
 */
@EnabledIfSystemProperty(named = "kiss.test.postgres", matches = "true")
class RecordMultiSchemaRegressionTest {

    private String schemaA;
    private String schemaB;
    private Connection db;
    private java.sql.Connection raw;

    @BeforeEach
    void setUp() throws Exception {
        String url = System.getProperty("kiss.test.postgres.url", "jdbc:postgresql://localhost/postgres");
        String user = System.getProperty("kiss.test.postgres.user", "postgres");
        String password = System.getProperty("kiss.test.postgres.password", "");
        Class.forName("org.postgresql.Driver");
        raw = DriverManager.getConnection(url, user, password);
        raw.setAutoCommit(true);
        String suffix = UUID.randomUUID().toString().replace("-", "_");
        schemaA = "kiss_test_a_" + suffix;
        schemaB = "kiss_test_b_" + suffix;
        try (Statement st = raw.createStatement()) {
            // Two schemas, each with an identically-named table with the SAME single-column
            // primary key -- this is what makes DatabaseMetaData.getPrimaryKeys(null, null,
            // "widget") return one row per schema instead of one.
            st.execute("CREATE SCHEMA " + schemaA);
            st.execute("CREATE SCHEMA " + schemaB);
            st.execute("CREATE TABLE " + schemaA + ".widget (widget_id INTEGER PRIMARY KEY, name TEXT)");
            st.execute("CREATE TABLE " + schemaB + ".widget (widget_id INTEGER PRIMARY KEY, name TEXT)");
            st.execute("INSERT INTO " + schemaA + ".widget (widget_id, name) VALUES (1, 'before')");
            st.execute("set search_path to " + schemaA);
        }
        db = new Connection(raw);
    }

    @AfterEach
    void tearDown() throws Exception {
        if (raw != null) {
            try (Statement st = raw.createStatement()) {
                st.execute("DROP SCHEMA IF EXISTS " + schemaA + " CASCADE");
                st.execute("DROP SCHEMA IF EXISTS " + schemaB + " CASCADE");
            } finally {
                raw.close();
            }
        }
    }

    @Test
    void testUpdateThenDeleteOfFetchedRowInMultiSchemaTable() throws Exception {
        List<String> pk = db.getPrimaryColumns("widget");
        assertEquals(List.of("widget_id"), pk, "a single-column key must not be duplicated across schemas");

        // update() of a row fetched via a SELECT cursor -- this is the path that went through
        // Command.getPriColumns() and threw before the fix.
        Record rec = db.fetchOne("select * from widget where widget_id = ?", 1);
        assertNotNull(rec, "fixture row must be fetchable");
        rec.set("name", "after");
        assertDoesNotThrow(rec::update, "update() of a fetched row must not throw a column-index error");

        Record rec2 = db.fetchOne("select * from widget where widget_id = ?", 1);
        assertNotNull(rec2);
        assertEquals("after", rec2.getString("name"), "update() must actually have applied");

        // delete() of a row fetched via a SELECT cursor -- same path, same previously-broken bind.
        assertDoesNotThrow(rec2::delete, "delete() of a fetched row must not throw a column-index error");
        assertNull(db.fetchOne("select * from widget where widget_id = ?", 1), "delete() must actually have applied");
    }
}
