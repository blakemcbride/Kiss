package org.kissweb.database;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for SchemaGraph and QueryBuilder.
 * Tests use a programmatically-built schema graph (no database needed).
 */
class QueryBuilderTest {

    private SchemaGraph graph;

    /**
     * Build a test schema:
     *
     *   building <-- department <-- employee <-- project_assignment --> project
     *                                  |
     *                                  +--> employee_phone
     */
    @BeforeEach
    void setUp() {
        graph = new SchemaGraph();
        graph.addForeignKey("department", "building_id", "building", "building_id");
        graph.addForeignKey("employee", "department_id", "department", "department_id");
        graph.addForeignKey("project_assignment", "employee_id", "employee", "employee_id");
        graph.addForeignKey("project_assignment", "project_id", "project", "project_id");
        graph.addForeignKey("employee_phone", "employee_id", "employee", "employee_id");
    }

    // ---- SchemaGraph tests ----

    @Test
    void testAddTable() {
        SchemaGraph g = new SchemaGraph();
        g.addTable("foo");
        assertTrue(g.hasTable("foo"));
        assertFalse(g.hasTable("bar"));
    }

    @Test
    void testAddForeignKeyCreatesTableEntries() {
        assertTrue(graph.hasTable("building"));
        assertTrue(graph.hasTable("department"));
        assertTrue(graph.hasTable("employee"));
        assertTrue(graph.hasTable("project_assignment"));
        assertTrue(graph.hasTable("project"));
        assertTrue(graph.hasTable("employee_phone"));
    }

    @Test
    void testCaseInsensitivity() {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("Employee", "Department_ID", "Department", "Department_ID");
        assertTrue(g.hasTable("employee"));
        assertTrue(g.hasTable("department"));
        assertTrue(g.hasTable("EMPLOYEE"));
    }

    @Test
    void testFindJoinPathDirectRelationship() throws SQLException {
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("employee", "department"));
        List<SchemaGraph.Edge> path = graph.findJoinPath(tables, "employee");
        assertEquals(1, path.size());
        SchemaGraph.Edge e = path.get(0);
        assertEquals("employee", e.getFromTable());
        assertEquals("department_id", e.getFromColumn());
        assertEquals("department", e.getToTable());
    }

    @Test
    void testFindJoinPathTwoHops() throws SQLException {
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("employee", "building"));
        List<SchemaGraph.Edge> path = graph.findJoinPath(tables, "employee");
        assertEquals(2, path.size());
    }

    @Test
    void testFindJoinPathMultipleTables() throws SQLException {
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("project", "building"));
        List<SchemaGraph.Edge> path = graph.findJoinPath(tables, "project");
        assertEquals(4, path.size());
    }

    @Test
    void testFindJoinPathSingleTable() throws SQLException {
        Set<String> tables = new LinkedHashSet<>(Collections.singletonList("employee"));
        List<SchemaGraph.Edge> path = graph.findJoinPath(tables, "employee");
        assertTrue(path.isEmpty());
    }

    @Test
    void testFindJoinPathDisconnectedThrows() {
        SchemaGraph g = new SchemaGraph();
        g.addTable("island_a");
        g.addTable("island_b");
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("island_a", "island_b"));
        assertThrows(SQLException.class, () -> g.findJoinPath(tables, "island_a"));
    }

    @Test
    void testDuplicateEdgesIgnored() {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("a", "b_id", "b", "b_id");
        g.addForeignKey("a", "b_id", "b", "b_id");
        assertEquals(1, g.getEdges("a").size());
        assertEquals(1, g.getEdges("b").size());
    }

    // ---- Composite FK SchemaGraph tests ----

    @Test
    void testCompositeEdgeCreation() {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});
        assertTrue(g.hasTable("order_line"));
        assertTrue(g.hasTable("order_product"));
        List<SchemaGraph.Edge> edges = g.getEdges("order_line");
        assertEquals(1, edges.size());
        SchemaGraph.Edge e = edges.get(0);
        assertTrue(e.isComposite());
        assertEquals(2, e.getFromColumns().size());
        assertEquals("order_id", e.getFromColumns().get(0));
        assertEquals("product_id", e.getFromColumns().get(1));
        assertEquals("order_id", e.getToColumns().get(0));
        assertEquals("product_id", e.getToColumns().get(1));
    }

    @Test
    void testCompositeEdgeMismatchedColumnsThrows() {
        assertThrows(IllegalArgumentException.class, () ->
                new SchemaGraph.Edge("a", new String[]{"x", "y"}, "b", new String[]{"z"}));
    }

    @Test
    void testCompositeEdgeEmptyColumnsThrows() {
        assertThrows(IllegalArgumentException.class, () ->
                new SchemaGraph.Edge("a", new String[]{}, "b", new String[]{}));
    }

    @Test
    void testCompositeEdgeDuplicateIgnored() {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("a", new String[]{"x", "y"}, "b", new String[]{"x", "y"});
        g.addForeignKey("a", new String[]{"x", "y"}, "b", new String[]{"x", "y"});
        assertEquals(1, g.getEdges("a").size());
    }

    @Test
    void testCompositeBuildOnCondition() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});
        String on = e.buildOnCondition(null, null);
        assertEquals("order_line.order_id = order_product.order_id AND order_line.product_id = order_product.product_id", on);
    }

    @Test
    void testCompositeBuildOnConditionWithAliases() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});
        String on = e.buildOnCondition("ol", "op");
        assertEquals("ol.order_id = op.order_id AND ol.product_id = op.product_id", on);
    }

    @Test
    void testSingleColumnBuildOnCondition() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("employee", "dept_id", "department", "dept_id");
        String on = e.buildOnCondition(null, null);
        assertEquals("employee.dept_id = department.dept_id", on);
    }

    @Test
    void testCompositeEdgeIsComposite() {
        SchemaGraph.Edge single = new SchemaGraph.Edge("a", "x", "b", "x");
        assertFalse(single.isComposite());

        SchemaGraph.Edge composite = new SchemaGraph.Edge("a",
                new String[]{"x", "y"}, "b", new String[]{"x", "y"});
        assertTrue(composite.isComposite());
    }

    @Test
    void testCompositeEdgeGetColumnBackwardCompat() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("a",
                new String[]{"x", "y"}, "b", new String[]{"p", "q"});
        // getFromColumn()/getToColumn() return the first column
        assertEquals("x", e.getFromColumn());
        assertEquals("p", e.getToColumn());
    }

    @Test
    void testCompositeJoinPath() throws SQLException {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});
        g.addForeignKey("order_product", "order_id", "orders", "order_id");
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("order_line", "orders"));
        List<SchemaGraph.Edge> path = g.findJoinPath(tables, "order_line");
        assertEquals(2, path.size());
        assertTrue(path.get(0).isComposite());
        assertFalse(path.get(1).isComposite());
    }

    // ---- QueryBuilder tests ----

    @Test
    void testSimpleSingleTableQuery() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("employee.last_name")
                .where("employee.active = ?", "Y")
                .build();

        assertEquals("SELECT employee.first_name, employee.last_name FROM employee WHERE employee.active = ?", sql);
    }

    @Test
    void testTwoTableJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .build();

        assertTrue(sql.contains("SELECT employee.first_name, department.name"));
        assertTrue(sql.contains("JOIN"));
        assertTrue(sql.contains("employee.department_id = department.department_id"));
    }

    @Test
    void testThreeTableJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .build();

        assertTrue(sql.contains("JOIN"));
        assertTrue(sql.contains("employee.department_id = department.department_id"));
        assertTrue(sql.contains("department.building_id = building.building_id"));
    }

    @Test
    void testWhereWithParameters() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        qb.select("employee.first_name")
                .where("employee.salary > ?", 50000)
                .where("employee.active = ?", "Y");
        qb.build();

        List<Object> params = qb.getParameters();
        assertEquals(2, params.size());
        assertEquals(50000, params.get(0));
        assertEquals("Y", params.get(1));
    }

    @Test
    void testOrderBy() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name", "employee.last_name")
                .orderBy("employee.last_name")
                .orderByDesc("employee.first_name")
                .build();

        assertTrue(sql.contains("ORDER BY employee.last_name, employee.first_name DESC"));
    }

    @Test
    void testGroupByAndHaving() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("department.name")
                .select("COUNT(employee.employee_id)")
                .groupBy("department.name")
                .having("COUNT(employee.employee_id) > ?", 5)
                .build();

        assertTrue(sql.contains("GROUP BY department.name"));
        assertTrue(sql.contains("HAVING COUNT(employee.employee_id) > ?"));
        assertEquals(1, qb.getParameters().size());
        assertEquals(5, qb.getParameters().get(0));
    }

    @Test
    void testDistinct() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.department_id")
                .distinct()
                .build();

        assertTrue(sql.startsWith("SELECT DISTINCT"));
    }

    @Test
    void testRootTableFromWhere() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("project.project_id = ?", 123)
                .build();

        assertTrue(sql.contains("FROM project"));
        assertTrue(sql.contains("JOIN"));
    }

    @Test
    void testExplicitJoinWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("mgr.first_name AS manager_name")
                .join("employee", "manager_id", "employee", "employee_id", "mgr")
                .build();

        assertTrue(sql.contains("JOIN employee mgr ON employee.manager_id = mgr.employee_id"));
    }

    @Test
    void testNoSelectThrows() {
        assertThrows(SQLException.class, () -> new QueryBuilder(graph).build());
    }

    @Test
    void testGetParametersBeforeBuildThrows() {
        assertThrows(IllegalStateException.class, () -> new QueryBuilder(graph).getParameters());
    }

    @Test
    void testMultipleWhereCombinedWithAnd() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("employee.active = 'Y'")
                .where("employee.salary > 50000")
                .build();

        assertTrue(sql.contains("WHERE employee.active = 'Y' AND employee.salary > 50000"));
    }

    @Test
    void testSelectWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name AS fname")
                .build();

        assertTrue(sql.contains("employee.first_name AS fname"));
    }

    @Test
    void testExtractTable() {
        assertEquals("employee", QueryBuilder.extractTable("employee.first_name"));
        assertEquals("employee", QueryBuilder.extractTable("employee.first_name AS fname"));
        assertEquals("employee", QueryBuilder.extractTable("COUNT(employee.employee_id)"));
        assertEquals("dept", QueryBuilder.extractTable("SUM(dept.salary) AS total"));
        assertNull(QueryBuilder.extractTable("standalone_column"));
        assertNull(QueryBuilder.extractTable("42"));
    }

    @Test
    void testExtractAllTables() {
        Set<String> tables = QueryBuilder.extractAllTables(
                "employee.salary > department.min_salary AND employee.active = 'Y'");
        assertTrue(tables.contains("employee"));
        assertTrue(tables.contains("department"));
        assertEquals(2, tables.size());
    }

    @Test
    void testWhereReferencesTableNotInSelect() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("project.name = ?", "Alpha")
                .build();

        assertTrue(sql.contains("JOIN"));
    }

    @Test
    void testFiveTableQuery() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .select("project.name")
                .select("employee_phone.phone_number")
                .build();

        int joinCount = countOccurrences(sql, " JOIN ");
        assertTrue(joinCount >= 4, "Expected at least 4 joins, got " + joinCount + ": " + sql);
    }

    // ---- Phase 2: Composite FK QueryBuilder tests ----

    @Test
    void testCompositeAutoJoin() throws SQLException {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});

        String sql = new QueryBuilder(g)
                .select("order_line.qty")
                .select("order_product.price")
                .build();

        assertTrue(sql.contains("order_line.order_id = order_product.order_id"));
        assertTrue(sql.contains("order_line.product_id = order_product.product_id"));
        assertTrue(sql.contains(" AND "));
    }

    @Test
    void testCompositeExplicitJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("audit.description")
                .join("employee",
                        new String[]{"company_id", "employee_id"},
                        "audit",
                        new String[]{"company_id", "ref_id"},
                        null)
                .build();

        assertTrue(sql.contains("employee.company_id = audit.company_id"));
        assertTrue(sql.contains("employee.employee_id = audit.ref_id"));
    }

    @Test
    void testCompositeExplicitJoinWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("a.description")
                .join("employee",
                        new String[]{"company_id", "employee_id"},
                        "audit",
                        new String[]{"company_id", "ref_id"},
                        "a")
                .build();

        assertTrue(sql.contains("JOIN audit a ON"));
        assertTrue(sql.contains("employee.company_id = a.company_id"));
        assertTrue(sql.contains("employee.employee_id = a.ref_id"));
    }

    @Test
    void testCompositeExplicitJoinMismatchThrows() {
        assertThrows(IllegalArgumentException.class, () ->
                new QueryBuilder(graph).join("a", new String[]{"x", "y"}, "b", new String[]{"z"}, null));
    }

    @Test
    void testCompositeThreeColumnJoin() throws SQLException {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("detail",
                new String[]{"a", "b", "c"},
                "master",
                new String[]{"x", "y", "z"});

        String sql = new QueryBuilder(g)
                .select("detail.val")
                .select("master.name")
                .build();

        assertTrue(sql.contains("detail.a = master.x"));
        assertTrue(sql.contains("detail.b = master.y"));
        assertTrue(sql.contains("detail.c = master.z"));
    }

    // ---- Phase 2: Alias / self-join / duplicate table tests ----

    @Test
    void testExplicitJoinWithoutAlias() throws SQLException {
        // Test the join() overload without alias parameter
        graph.addForeignKey("employee", "home_department_id", "department", "department_id");

        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .join("employee", "home_department_id", "department", "department_id")
                .build();

        assertTrue(sql.contains("employee.home_department_id = department.department_id"));
    }

    @Test
    void testSelfJoinWithAlias() throws SQLException {
        // employee.manager_id -> employee.employee_id  (self-join)
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("mgr.first_name AS manager_name")
                .join("employee", "manager_id", "employee", "employee_id", "mgr")
                .build();

        assertTrue(sql.contains("FROM employee"));
        assertTrue(sql.contains("JOIN employee mgr ON employee.manager_id = mgr.employee_id"));
        assertFalse(sql.contains("FROM mgr"));
    }

    @Test
    void testSelfJoinPlusAutoJoin() throws SQLException {
        // Self-join on employee PLUS auto-join to department
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("mgr.first_name AS manager_name")
                .select("department.name")
                .join("employee", "manager_id", "employee", "employee_id", "mgr")
                .build();

        assertTrue(sql.contains("JOIN employee mgr ON employee.manager_id = mgr.employee_id"));
        assertTrue(sql.contains("employee.department_id = department.department_id"));
    }

    @Test
    void testMultipleExplicitJoins() throws SQLException {
        // Two separate explicit joins
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("mgr.first_name AS manager")
                .select("mentor.first_name AS mentor_name")
                .join("employee", "manager_id", "employee", "employee_id", "mgr")
                .join("employee", "mentor_id", "employee", "employee_id", "mentor")
                .build();

        assertTrue(sql.contains("JOIN employee mgr ON employee.manager_id = mgr.employee_id"));
        assertTrue(sql.contains("JOIN employee mentor ON employee.mentor_id = mentor.employee_id"));
    }

    @Test
    void testAliasedTableInWhereClause() throws SQLException {
        // WHERE references alias, not real table name
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("mgr.active = ?", "Y")
                .join("employee", "manager_id", "employee", "employee_id", "mgr")
                .build();

        // mgr should be treated as alias, not trigger path finding
        assertTrue(sql.contains("JOIN employee mgr ON"));
        assertTrue(sql.contains("WHERE mgr.active = ?"));
    }

    @Test
    void testExplicitJoinNoAliasWithAutoJoin() throws SQLException {
        // Explicit join without alias, plus auto-join to another table
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .join("employee", "department_id", "department", "department_id")
                .build();

        // The explicit join replaces the auto-join for employee->department
        assertTrue(sql.contains("employee.department_id = department.department_id"));
        assertTrue(sql.contains("department.building_id = building.building_id"));
    }

    // ---- Edge.buildOnCondition tests ----

    @Test
    void testBuildOnConditionSingleNoAlias() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("employee", "dept_id", "department", "dept_id");
        assertEquals("employee.dept_id = department.dept_id", e.buildOnCondition(null, null));
    }

    @Test
    void testBuildOnConditionSingleWithAlias() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("employee", "dept_id", "department", "dept_id");
        assertEquals("e.dept_id = d.dept_id", e.buildOnCondition("e", "d"));
    }

    @Test
    void testBuildOnConditionCompositeNoAlias() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("a",
                new String[]{"x", "y"}, "b", new String[]{"p", "q"});
        assertEquals("a.x = b.p AND a.y = b.q", e.buildOnCondition(null, null));
    }

    @Test
    void testBuildOnConditionCompositeWithAlias() {
        SchemaGraph.Edge e = new SchemaGraph.Edge("a",
                new String[]{"x", "y"}, "b", new String[]{"p", "q"});
        assertEquals("aa.x = bb.p AND aa.y = bb.q", e.buildOnCondition("aa", "bb"));
    }

    // ---- Phase 3: Left/Right Join tests ----

    @Test
    void testLeftJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("department.name")
                .selectCount("employee.employee_id", "emp_count")
                .leftJoin("department", "department_id", "employee", "department_id")
                .groupBy("department.name")
                .build();

        assertTrue(sql.contains("LEFT JOIN employee ON"));
        assertTrue(sql.contains("department.department_id = employee.department_id"));
        assertTrue(sql.contains("COUNT(employee.employee_id) AS emp_count"));
        assertTrue(sql.contains("GROUP BY department.name"));
    }

    @Test
    void testRightJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .rightJoin("employee", "department_id", "department", "department_id")
                .build();

        assertTrue(sql.contains("RIGHT JOIN department ON"));
        assertTrue(sql.contains("employee.department_id = department.department_id"));
    }

    @Test
    void testLeftJoinWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("mgr.first_name AS manager_name")
                .leftJoin("employee", "manager_id", "employee", "employee_id", "mgr")
                .build();

        assertTrue(sql.contains("LEFT JOIN employee mgr ON employee.manager_id = mgr.employee_id"));
    }

    @Test
    void testRightJoinWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("sub.first_name AS subordinate")
                .rightJoin("employee", "employee_id", "employee", "manager_id", "sub")
                .build();

        assertTrue(sql.contains("RIGHT JOIN employee sub ON employee.employee_id = sub.manager_id"));
    }

    @Test
    void testLeftJoinComposite() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("audit.description")
                .leftJoin("employee",
                        new String[]{"company_id", "employee_id"},
                        "audit",
                        new String[]{"company_id", "ref_id"},
                        null)
                .build();

        assertTrue(sql.contains("LEFT JOIN audit ON"));
        assertTrue(sql.contains("employee.company_id = audit.company_id"));
        assertTrue(sql.contains("employee.employee_id = audit.ref_id"));
    }

    @Test
    void testMixedJoinTypes() throws SQLException {
        // INNER auto-join to department, LEFT explicit join to employee_phone
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("employee_phone.phone_number")
                .leftJoin("employee", "employee_id", "employee_phone", "employee_id")
                .build();

        // Auto-join to department should be INNER (i.e. just "JOIN")
        assertTrue(sql.contains("JOIN department ON"));
        // Explicit join to employee_phone should be LEFT
        assertTrue(sql.contains("LEFT JOIN employee_phone ON"));
    }

    @Test
    void testAutoJoinsAreInner() throws SQLException {
        // Verify auto-discovered joins emit "JOIN" (INNER), not "LEFT JOIN" or "RIGHT JOIN"
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .build();

        // Should contain "JOIN department" and "JOIN building" but NOT "LEFT" or "RIGHT"
        assertFalse(sql.contains("LEFT JOIN"));
        assertFalse(sql.contains("RIGHT JOIN"));
        assertTrue(sql.contains("JOIN department ON"));
        assertTrue(sql.contains("JOIN building ON"));
    }

    // ---- Phase 3: Aggregate helper tests ----

    @Test
    void testSelectCount() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectCount("employee.employee_id")
                .build();

        assertTrue(sql.contains("SELECT COUNT(employee.employee_id)"));
    }

    @Test
    void testSelectCountWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectCount("employee.employee_id", "total")
                .build();

        assertTrue(sql.contains("COUNT(employee.employee_id) AS total"));
    }

    @Test
    void testSelectSum() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectSum("employee.salary")
                .build();

        assertTrue(sql.contains("SUM(employee.salary)"));
    }

    @Test
    void testSelectSumWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectSum("employee.salary", "total_salary")
                .build();

        assertTrue(sql.contains("SUM(employee.salary) AS total_salary"));
    }

    @Test
    void testSelectAvg() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectAvg("employee.salary")
                .build();

        assertTrue(sql.contains("AVG(employee.salary)"));
    }

    @Test
    void testSelectAvgWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectAvg("employee.salary", "avg_salary")
                .build();

        assertTrue(sql.contains("AVG(employee.salary) AS avg_salary"));
    }

    @Test
    void testSelectMin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectMin("employee.salary")
                .build();

        assertTrue(sql.contains("MIN(employee.salary)"));
    }

    @Test
    void testSelectMinWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectMin("employee.salary", "lowest")
                .build();

        assertTrue(sql.contains("MIN(employee.salary) AS lowest"));
    }

    @Test
    void testSelectMax() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectMax("employee.salary")
                .build();

        assertTrue(sql.contains("MAX(employee.salary)"));
    }

    @Test
    void testSelectMaxWithAlias() throws SQLException {
        String sql = new QueryBuilder(graph)
                .selectMax("employee.salary", "highest")
                .build();

        assertTrue(sql.contains("MAX(employee.salary) AS highest"));
    }

    @Test
    void testMultipleAggregatesWithGroupBy() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("department.name")
                .selectCount("employee.employee_id", "emp_count")
                .selectAvg("employee.salary", "avg_salary")
                .selectSum("employee.salary", "total_salary")
                .selectMin("employee.salary", "min_salary")
                .selectMax("employee.salary", "max_salary")
                .groupBy("department.name")
                .build();

        assertTrue(sql.contains("department.name"));
        assertTrue(sql.contains("COUNT(employee.employee_id) AS emp_count"));
        assertTrue(sql.contains("AVG(employee.salary) AS avg_salary"));
        assertTrue(sql.contains("SUM(employee.salary) AS total_salary"));
        assertTrue(sql.contains("MIN(employee.salary) AS min_salary"));
        assertTrue(sql.contains("MAX(employee.salary) AS max_salary"));
        assertTrue(sql.contains("GROUP BY department.name"));
    }

    @Test
    void testAggregateTriggersJoin() throws SQLException {
        // An aggregate on a different table should trigger a join
        String sql = new QueryBuilder(graph)
                .select("department.name")
                .selectCount("employee.employee_id", "emp_count")
                .groupBy("department.name")
                .build();

        assertTrue(sql.contains("JOIN"));
        assertTrue(sql.contains("employee.department_id = department.department_id"));
    }

    // ---- Phase 3: Schema caching tests ----

    @Test
    void testSaveAndLoadCacheFile(@TempDir Path tempDir) throws IOException, SQLException {
        String filePath = tempDir.resolve("schema-cache.txt").toString();

        // Save the graph
        graph.saveToFile(filePath);

        // Load it back
        SchemaGraph loaded = SchemaGraph.loadFromFile(filePath);

        // Verify all tables exist
        assertTrue(loaded.hasTable("building"));
        assertTrue(loaded.hasTable("department"));
        assertTrue(loaded.hasTable("employee"));
        assertTrue(loaded.hasTable("project_assignment"));
        assertTrue(loaded.hasTable("project"));
        assertTrue(loaded.hasTable("employee_phone"));

        // Verify edges work for path finding
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("employee", "building"));
        List<SchemaGraph.Edge> path = loaded.findJoinPath(tables, "employee");
        assertEquals(2, path.size());
    }

    @Test
    void testSaveAndLoadWithCompositeFK(@TempDir Path tempDir) throws IOException, SQLException {
        SchemaGraph g = new SchemaGraph();
        g.addForeignKey("order_line",
                new String[]{"order_id", "product_id"},
                "order_product",
                new String[]{"order_id", "product_id"});
        g.addForeignKey("order_product", "order_id", "orders", "order_id");
        g.addTable("standalone_table");

        String filePath = tempDir.resolve("composite-cache.txt").toString();
        g.saveToFile(filePath);

        SchemaGraph loaded = SchemaGraph.loadFromFile(filePath);

        assertTrue(loaded.hasTable("order_line"));
        assertTrue(loaded.hasTable("order_product"));
        assertTrue(loaded.hasTable("orders"));
        assertTrue(loaded.hasTable("standalone_table"));

        // Verify composite FK survived round-trip
        List<SchemaGraph.Edge> edges = loaded.getEdges("order_line");
        assertEquals(1, edges.size());
        assertTrue(edges.get(0).isComposite());
        assertEquals(2, edges.get(0).getFromColumns().size());

        // Verify path finding works
        Set<String> tables = new LinkedHashSet<>(Arrays.asList("order_line", "orders"));
        List<SchemaGraph.Edge> path = loaded.findJoinPath(tables, "order_line");
        assertEquals(2, path.size());
    }

    @Test
    void testLoadCacheMissingHeaderThrows(@TempDir Path tempDir) throws IOException {
        String filePath = tempDir.resolve("bad-cache.txt").toString();
        java.nio.file.Files.writeString(java.nio.file.Path.of(filePath), "not a valid header\n");

        assertThrows(IOException.class, () -> SchemaGraph.loadFromFile(filePath));
    }

    @Test
    void testLoadCacheInvalidLineThrows(@TempDir Path tempDir) throws IOException {
        String filePath = tempDir.resolve("bad-line.txt").toString();
        java.nio.file.Files.writeString(java.nio.file.Path.of(filePath),
                "# SchemaGraph cache v1\nGARBAGE line here\n");

        assertThrows(IOException.class, () -> SchemaGraph.loadFromFile(filePath));
    }

    @Test
    void testLoadCacheEmptyFileThrows(@TempDir Path tempDir) throws IOException {
        String filePath = tempDir.resolve("empty.txt").toString();
        java.nio.file.Files.writeString(java.nio.file.Path.of(filePath), "");

        assertThrows(IOException.class, () -> SchemaGraph.loadFromFile(filePath));
    }

    @Test
    void testCacheRoundTripPreservesQueryBuilderBehavior(@TempDir Path tempDir) throws IOException, SQLException {
        String filePath = tempDir.resolve("roundtrip.txt").toString();
        graph.saveToFile(filePath);
        SchemaGraph loaded = SchemaGraph.loadFromFile(filePath);

        // Build the same query with original and loaded graph — should produce identical SQL
        String sql1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .where("employee.active = ?", "Y")
                .orderBy("employee.last_name")
                .build();

        String sql2 = new QueryBuilder(loaded)
                .select("employee.first_name")
                .select("department.name")
                .select("building.address")
                .where("employee.active = ?", "Y")
                .orderBy("employee.last_name")
                .build();

        assertEquals(sql1, sql2);
    }

    @Test
    void testCacheWithCommentsAndBlankLines(@TempDir Path tempDir) throws IOException {
        String filePath = tempDir.resolve("comments.txt").toString();
        java.nio.file.Files.writeString(java.nio.file.Path.of(filePath),
                "# SchemaGraph cache v1\n" +
                "\n" +
                "# This is a comment\n" +
                "TABLE employee\n" +
                "TABLE department\n" +
                "\n" +
                "FK employee department_id department department_id\n");

        SchemaGraph loaded = SchemaGraph.loadFromFile(filePath);
        assertTrue(loaded.hasTable("employee"));
        assertTrue(loaded.hasTable("department"));
        assertEquals(1, loaded.getEdges("employee").size());
    }

    // ---- Phase 4: OR / AND grouping tests ----

    @Test
    void testSimpleOrGroup() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                    .where("employee.department_id = ?", 10)
                    .where("employee.department_id = ?", 20)
                .endOr()
                .build();

        assertTrue(sql.contains("WHERE (employee.department_id = ? OR employee.department_id = ?)"));
    }

    @Test
    void testOrGroupWithAndCondition() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .startOr()
                    .where("employee.department_id = ?", 10)
                    .where("employee.department_id = ?", 20)
                .endOr()
                .where("employee.active = 'Y'")
                .build();

        assertTrue(sql.contains("(employee.department_id = ? OR employee.department_id = ?)"));
        assertTrue(sql.contains("AND employee.active = 'Y'"));
        assertEquals(2, qb.getParameters().size());
        assertEquals(10, qb.getParameters().get(0));
        assertEquals(20, qb.getParameters().get(1));
    }

    @Test
    void testNestedAndInsideOr() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                    .where("employee.salary > ?", 100000)
                    .startAnd()
                        .where("employee.department_id = ?", 5)
                        .where("employee.level = ?", "senior")
                    .endAnd()
                .endOr()
                .build();

        assertTrue(sql.contains("(employee.salary > ? OR (employee.department_id = ? AND employee.level = ?))"));
    }

    @Test
    void testEmptyOrGroupOmitted() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                .endOr()
                .where("employee.active = 'Y'")
                .build();

        assertTrue(sql.contains("WHERE employee.active = 'Y'"));
        assertFalse(sql.contains("OR"));
        assertFalse(sql.contains("()"));
    }

    @Test
    void testOrGroupSingleCondition() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                    .where("employee.department_id = ?", 10)
                .endOr()
                .build();

        // Single condition inside OR group should not have extra parens
        assertTrue(sql.contains("WHERE employee.department_id = ?"));
    }

    @Test
    void testOrGroupTriggersJoin() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                    .where("department.name = ?", "Engineering")
                    .where("department.name = ?", "Sales")
                .endOr()
                .build();

        assertTrue(sql.contains("JOIN"));
        assertTrue(sql.contains("(department.name = ? OR department.name = ?)"));
    }

    @Test
    void testEndOrWithoutStartOrThrows() {
        assertThrows(IllegalStateException.class, () ->
                new QueryBuilder(graph).select("employee.first_name").endOr());
    }

    @Test
    void testEndAndWithoutStartAndThrows() {
        assertThrows(IllegalStateException.class, () ->
                new QueryBuilder(graph).select("employee.first_name").endAnd());
    }

    @Test
    void testMultipleOrGroups() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .startOr()
                    .where("employee.department_id = ?", 10)
                    .where("employee.department_id = ?", 20)
                .endOr()
                .startOr()
                    .where("employee.level = ?", "junior")
                    .where("employee.level = ?", "senior")
                .endOr()
                .build();

        assertTrue(sql.contains("(employee.department_id = ? OR employee.department_id = ?)"));
        assertTrue(sql.contains("(employee.level = ? OR employee.level = ?)"));
        assertTrue(sql.contains(" AND "));
        assertEquals(4, qb.getParameters().size());
    }

    // ---- Phase 4: Subquery tests ----

    @Test
    void testWhereInSubquery() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("project_assignment.employee_id")
                .where("project_assignment.project_id = ?", 42);

        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .whereIn("employee.employee_id", sub)
                .build();

        assertTrue(sql.contains("employee.employee_id IN (SELECT project_assignment.employee_id FROM project_assignment WHERE project_assignment.project_id = ?)"));
        assertEquals(1, qb.getParameters().size());
        assertEquals(42, qb.getParameters().get(0));
    }

    @Test
    void testWhereNotInSubquery() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("project_assignment.employee_id")
                .where("project_assignment.project_id = ?", 42);

        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .whereNotIn("employee.employee_id", sub)
                .build();

        assertTrue(sql.contains("employee.employee_id NOT IN (SELECT"));
    }

    @Test
    void testWhereInRawSQL() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .whereIn("employee.employee_id",
                        "SELECT person_id FROM timesheet WHERE work_date = ?", 20260101)
                .build();

        assertTrue(sql.contains("employee.employee_id IN (SELECT person_id FROM timesheet WHERE work_date = ?)"));
        assertEquals(1, qb.getParameters().size());
        assertEquals(20260101, qb.getParameters().get(0));
    }

    @Test
    void testWhereNotInRawSQL() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .whereNotIn("employee.employee_id",
                        "SELECT person_id FROM terminated")
                .build();

        assertTrue(sql.contains("employee.employee_id NOT IN (SELECT person_id FROM terminated)"));
    }

    @Test
    void testWhereExistsSubquery() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("1")
                .where("project_assignment.employee_id = employee.employee_id")
                .where("project_assignment.project_id = ?", 42);

        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .whereExists(sub)
                .build();

        assertTrue(sql.contains("EXISTS (SELECT 1 FROM project_assignment"));
        assertEquals(1, qb.getParameters().size());
        assertEquals(42, qb.getParameters().get(0));
    }

    @Test
    void testWhereNotExistsSubquery() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("1")
                .where("project_assignment.employee_id = employee.employee_id");

        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .whereNotExists(sub)
                .build();

        assertTrue(sql.contains("NOT EXISTS (SELECT 1 FROM project_assignment"));
    }

    @Test
    void testWhereExistsRawSQL() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("department.name")
                .whereExists("SELECT 1 FROM employee WHERE employee.department_id = department.department_id AND employee.active = 'Y'")
                .build();

        assertTrue(sql.contains("EXISTS (SELECT 1 FROM employee WHERE"));
    }

    @Test
    void testWhereNotExistsRawSQL() throws SQLException {
        String sql = new QueryBuilder(graph)
                .select("department.name")
                .whereNotExists("SELECT 1 FROM employee WHERE employee.department_id = department.department_id")
                .build();

        assertTrue(sql.contains("NOT EXISTS (SELECT 1 FROM employee WHERE"));
    }

    @Test
    void testSubqueryWithOtherWhereConditions() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("project_assignment.employee_id")
                .where("project_assignment.project_id = ?", 42);

        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .select("employee.first_name")
                .where("employee.active = ?", "Y")
                .whereIn("employee.employee_id", sub)
                .where("employee.salary > ?", 50000)
                .build();

        assertTrue(sql.contains("employee.active = ?"));
        assertTrue(sql.contains("employee.employee_id IN (SELECT"));
        assertTrue(sql.contains("employee.salary > ?"));
        assertEquals(3, qb.getParameters().size());
        assertEquals("Y", qb.getParameters().get(0));
        assertEquals(42, qb.getParameters().get(1));
        assertEquals(50000, qb.getParameters().get(2));
    }

    @Test
    void testSubqueryInsideOrGroup() throws SQLException {
        QueryBuilder sub = new QueryBuilder(graph)
                .select("project_assignment.employee_id")
                .where("project_assignment.project_id = ?", 42);

        String sql = new QueryBuilder(graph)
                .select("employee.first_name")
                .startOr()
                    .where("employee.department_id = ?", 10)
                    .whereIn("employee.employee_id", sub)
                .endOr()
                .build();

        assertTrue(sql.contains("(employee.department_id = ? OR employee.employee_id IN (SELECT"));
    }

    // ---- Phase 4: CTE tests ----

    @Test
    void testCTERawSQL() throws SQLException {
        String sql = new QueryBuilder(graph)
                .with("active_emps",
                        "SELECT employee_id, first_name FROM employee WHERE active = 'Y'")
                .select("active_emps.employee_id")
                .select("active_emps.first_name")
                .build();

        assertTrue(sql.startsWith("WITH active_emps AS (SELECT employee_id, first_name FROM employee WHERE active = 'Y')"));
        assertTrue(sql.contains("SELECT active_emps.employee_id, active_emps.first_name"));
        assertTrue(sql.contains("FROM active_emps"));
    }

    @Test
    void testCTERawSQLWithParams() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .with("recent_tasks",
                        "SELECT employee_id, MAX(task_date) last_task FROM task WHERE task_date > ? GROUP BY employee_id",
                        20260101)
                .select("employee.first_name")
                .select("recent_tasks.last_task")
                .leftJoin("employee", "employee_id", "recent_tasks", "employee_id")
                .build();

        assertTrue(sql.startsWith("WITH recent_tasks AS ("));
        assertTrue(sql.contains("task_date > ?"));
        assertEquals(1, qb.getParameters().size());
        assertEquals(20260101, qb.getParameters().get(0));
    }

    @Test
    void testCTEWithSubquery() throws SQLException {
        QueryBuilder cteSub = new QueryBuilder(graph)
                .select("employee.employee_id")
                .where("employee.active = ?", "Y");

        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .with("active_emps", cteSub)
                .select("active_emps.employee_id")
                .build();

        assertTrue(sql.startsWith("WITH active_emps AS (SELECT employee.employee_id FROM employee WHERE employee.active = ?)"));
        assertEquals(1, qb.getParameters().size());
        assertEquals("Y", qb.getParameters().get(0));
    }

    @Test
    void testMultipleCTEs() throws SQLException {
        String sql = new QueryBuilder(graph)
                .with("cte1", "SELECT employee_id FROM employee WHERE active = 'Y'")
                .with("cte2", "SELECT employee_id, MAX(work_date) last_date FROM timesheet GROUP BY employee_id")
                .select("cte1.employee_id")
                .select("cte2.last_date")
                .leftJoin("cte1", "employee_id", "cte2", "employee_id")
                .build();

        assertTrue(sql.startsWith("WITH cte1 AS ("));
        assertTrue(sql.contains("), cte2 AS ("));
        assertTrue(sql.contains("LEFT JOIN cte2 ON"));
    }

    @Test
    void testCTEWithAutoJoinToRealTable() throws SQLException {
        // CTE referenced alongside a real table — CTE name should not go to graph path finding
        String sql = new QueryBuilder(graph)
                .with("active_emps",
                        "SELECT employee_id, department_id FROM employee WHERE active = 'Y'")
                .select("active_emps.employee_id")
                .select("department.name")
                .leftJoin("active_emps", "department_id", "department", "department_id")
                .build();

        assertTrue(sql.startsWith("WITH active_emps AS ("));
        assertTrue(sql.contains("LEFT JOIN department ON"));
        // Should not try to auto-join active_emps through the schema graph
        assertFalse(sql.contains("JOIN active_emps ON"));
    }

    @Test
    void testCTEParamsBeforeWhereParams() throws SQLException {
        QueryBuilder qb = new QueryBuilder(graph);
        String sql = qb
                .with("filtered",
                        "SELECT employee_id FROM employee WHERE salary > ?", 80000)
                .select("filtered.employee_id")
                .select("department.name")
                .leftJoin("filtered", "employee_id", "department", "department_id")
                .where("department.active = ?", "Y")
                .build();

        assertEquals(2, qb.getParameters().size());
        assertEquals(80000, qb.getParameters().get(0));
        assertEquals("Y", qb.getParameters().get(1));
    }

    // ---- Phase 4: UNION tests ----

    @Test
    void testUnionAll() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("employee.last_name")
                .where("employee.active = 'Y'");

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("employee.last_name")
                .where("employee.active = 'N'");

        String sql = q1.unionAll(q2).build();

        assertTrue(sql.contains("UNION ALL"));
        assertTrue(sql.contains("WHERE employee.active = 'Y'"));
        assertTrue(sql.contains("WHERE employee.active = 'N'"));
    }

    @Test
    void testUnion() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("employee.department_id = ?", 10);

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("employee.department_id = ?", 20);

        QueryBuilder qb = q1.union(q2);
        String sql = qb.build();

        assertTrue(sql.contains(" UNION SELECT"));
        assertFalse(sql.contains("UNION ALL"));
        assertEquals(2, qb.getParameters().size());
        assertEquals(10, qb.getParameters().get(0));
        assertEquals(20, qb.getParameters().get(1));
    }

    @Test
    void testUnionWithOrderBy() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name", "employee.last_name")
                .where("employee.active = 'Y'");

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name", "employee.last_name")
                .where("employee.active = 'N'");

        String sql = q1.unionAll(q2)
                .orderBy("last_name")
                .build();

        // ORDER BY should come after the UNION ALL
        int unionPos = sql.indexOf("UNION ALL");
        int orderPos = sql.indexOf("ORDER BY");
        assertTrue(orderPos > unionPos);
    }

    @Test
    void testMultipleUnions() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("'A' AS type");

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("'B' AS type");

        QueryBuilder q3 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("'C' AS type");

        String sql = q1.unionAll(q2).unionAll(q3).build();

        int count = countOccurrences(sql, "UNION ALL");
        assertEquals(2, count);
    }

    @Test
    void testUnionAllWithParams() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("employee.department_id = ?", 10)
                .where("employee.salary > ?", 50000);

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name")
                .where("employee.department_id = ?", 20)
                .where("employee.salary > ?", 60000);

        QueryBuilder qb = q1.unionAll(q2);
        qb.build();

        assertEquals(4, qb.getParameters().size());
        assertEquals(10, qb.getParameters().get(0));
        assertEquals(50000, qb.getParameters().get(1));
        assertEquals(20, qb.getParameters().get(2));
        assertEquals(60000, qb.getParameters().get(3));
    }

    @Test
    void testUnionWithJoins() throws SQLException {
        QueryBuilder q1 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .where("employee.active = 'Y'");

        QueryBuilder q2 = new QueryBuilder(graph)
                .select("employee.first_name")
                .select("department.name")
                .where("employee.active = 'N'");

        String sql = q1.unionAll(q2).build();

        // Both halves should have joins
        int joinCount = countOccurrences(sql, " JOIN ");
        assertTrue(joinCount >= 2, "Expected at least 2 JOINs, got " + joinCount);
    }

    // ---- Utility ----

    private int countOccurrences(String str, String sub) {
        int count = 0;
        int idx = 0;
        while ((idx = str.indexOf(sub, idx)) != -1) {
            count++;
            idx += sub.length();
        }
        return count;
    }
}
