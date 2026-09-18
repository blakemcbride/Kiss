package org.kissweb.database;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link Connection#resolvePrimaryKeyColumns(List, String)}, the de-duplication
 * guard that collapses {@code DatabaseMetaData.getPrimaryKeys} rows spanning more than one
 * schema (as happens on a multi-tenant database with one schema per tenant, where the same
 * table name exists in every tenant schema plus a shared schema) down to a single schema's
 * primary key.  No database is needed -- the rows are hand-built.
 */
class ConnectionPrimaryKeyResolutionTest {

    private static Connection.PrimaryKeyRow row(String schema, String column, int keySeq) {
        return new Connection.PrimaryKeyRow(schema, column, keySeq);
    }

    @Test
    void testEmptyRowsReturnsEmptyList() {
        List<Connection.PrimaryKeyRow> rows = new ArrayList<>();
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, null);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSingleSchemaSingleColumnKey() {
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("tenant_a", "browser_job_step_id", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, "tenant_a");
        assertEquals(List.of("browser_job_step_id"), result);
    }

    @Test
    void testSameTableInMultipleSchemasCollapsesToCurrentSchema() {
        // Reproduces the live bug: browser_job_step exists in 5 schemas (master + 4 tenants),
        // so getPrimaryKeys(null, null, "browser_job_step") returns 5 rows for the one real
        // column. The connection's current schema is "tenant_b" -- only that row must survive.
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("master", "browser_job_step_id", 1),
                row("tenant_a", "browser_job_step_id", 1),
                row("tenant_b", "browser_job_step_id", 1),
                row("tenant_c", "browser_job_step_id", 1),
                row("tenant_d", "browser_job_step_id", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, "tenant_b");
        assertEquals(List.of("browser_job_step_id"), result);
    }

    @Test
    void testCurrentSchemaMatchIsCaseInsensitive() {
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("Tenant_B", "id", 1),
                row("tenant_a", "id", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, "TENANT_b");
        assertEquals(List.of("id"), result);
    }

    @Test
    void testNoCurrentSchemaMatchFallsBackToFirstSchemaSeen() {
        // The connection's current schema is unknown or doesn't appear among the rows (e.g.
        // getSchema() wasn't supported by the driver) -- fall back deterministically to
        // whichever schema's rows came back first, rather than arbitrarily merging columns
        // from different schemas.
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("master", "browser_job_step_id", 1),
                row("tenant_a", "browser_job_step_id", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, null);
        assertEquals(List.of("browser_job_step_id"), result);

        List<String> resultNoMatch = Connection.resolvePrimaryKeyColumns(rows, "some_other_schema");
        assertEquals(List.of("browser_job_step_id"), resultNoMatch);
    }

    @Test
    void testGenuineCompositeKeyInSingleSchemaReturnsAllColumnsInKeySeqOrder() {
        // Two DIFFERENT columns in the SAME schema must survive whole and in KEY_SEQ order,
        // even though the de-dup logic runs unconditionally.
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("tenant_a", "col_b", 2),
                row("tenant_a", "col_a", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, "tenant_a");
        assertEquals(List.of("col_a", "col_b"), result);
    }

    @Test
    void testCompositeKeyAcrossMultipleSchemasCollapsesPerSchemaAndKeepsBothColumns() {
        // A composite key (col_a, col_b) exists identically in two schemas; only the current
        // schema's two rows must survive, still both columns, still in KEY_SEQ order.
        List<Connection.PrimaryKeyRow> rows = List.of(
                row("master", "col_a", 1),
                row("master", "col_b", 2),
                row("tenant_a", "col_b", 2),
                row("tenant_a", "col_a", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, "tenant_a");
        assertEquals(List.of("col_a", "col_b"), result);
    }

    @Test
    void testNullSchemaRowsTreatedAsSingleSchema() {
        // Some drivers (e.g. MySQL, SQLite) report a null TABLE_SCHEM for every row; that must
        // not be treated as "multiple schemas" when it's really just one (null) schema repeated.
        List<Connection.PrimaryKeyRow> rows = List.of(
                row(null, "id", 1)
        );
        List<String> result = Connection.resolvePrimaryKeyColumns(rows, null);
        assertEquals(List.of("id"), result);
    }
}
