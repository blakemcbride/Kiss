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

import org.kissweb.json.JSONArray;

import java.sql.SQLException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Fluent SQL query builder that uses a {@link SchemaGraph} to automatically
 * determine the JOIN path between tables.  The developer specifies what
 * columns to select, what conditions to apply, and how to order the results.
 * The builder figures out the necessary joins.
 * <br><br>
 * Supports single-column and composite (multi-column) foreign keys,
 * explicit join hints (INNER, LEFT, RIGHT), table aliases, self-joins,
 * and aggregate helper methods.
 * <br><br>
 * <b>Basic usage:</b>
 * <pre>
 *     String sql = new QueryBuilder(graph)
 *         .select("employee.first_name")
 *         .select("employee.last_name")
 *         .select("department.name")
 *         .where("project.project_id = ?", projectId)
 *         .orderBy("employee.last_name")
 *         .build();
 * </pre>
 *
 * <b>Left join with aggregates:</b>
 * <pre>
 *     String sql = new QueryBuilder(graph)
 *         .select("department.name")
 *         .selectCount("employee.employee_id", "emp_count")
 *         .leftJoin("department", "department_id", "employee", "department_id")
 *         .groupBy("department.name")
 *         .build();
 * </pre>
 *
 * @author Blake McBride
 *
 * @see SchemaGraph
 */
public class QueryBuilder {

    /** Join types for explicit joins */
    private enum JoinType {
        INNER("JOIN"),
        LEFT("LEFT JOIN"),
        RIGHT("RIGHT JOIN");

        final String sql;

        JoinType(String sql) {
            this.sql = sql;
        }
    }

    private final SchemaGraph graph;
    private Connection storedConn;  // optional, set by Connection-based constructor
    private Command storedCmd;      // optional, set by Command-based constructor
    private final List<String> selectColumns = new ArrayList<>();
    private final List<String> orderByColumns = new ArrayList<>();
    private final List<String> groupByColumns = new ArrayList<>();
    private final List<WhereClause> havingClauses = new ArrayList<>();
    private final List<ExplicitJoin> explicitJoins = new ArrayList<>();
    private final List<CTE> ctes = new ArrayList<>();
    private final List<UnionClause> unions = new ArrayList<>();
    private final Map<String, JoinType> joinTypeOverrides = new HashMap<>();
    private final Map<String, String> aliasToTable = new HashMap<>();   // alias -> real table
    private final Map<String, String> tableToAlias = new HashMap<>();   // real table -> alias
    private boolean distinct = false;
    private int maxRows = 0;

    // Where clause tree — supports AND/OR grouping
    private WhereGroup rootWhereGroup = new WhereGroup(GroupType.AND);
    private WhereGroup currentWhereGroup;

    // Populated by build()
    private String builtSQL;
    private List<Object> parameters;

    /** Grouping type for WHERE clause trees */
    private enum GroupType { AND, OR }

    /**
     * Create a new query builder that will use the given schema graph
     * for automatic join resolution.
     *
     * @param graph the schema graph describing table relationships
     */
    public QueryBuilder(SchemaGraph graph) {
        this.graph = graph;
        this.currentWhereGroup = rootWhereGroup;
    }

    /**
     * Create a new query builder using the schema graph associated with
     * the given connection.  The connection is also stored so that the
     * no-argument {@link #fetchAll()}, {@link #fetchOne()}, and
     * {@link #fetchAllJSON()} methods can be used without passing a
     * connection explicitly.
     *
     * @param conn a Kiss database connection (must have a schema graph available)
     * @throws SQLException if the schema graph cannot be obtained from the connection
     */
    public QueryBuilder(Connection conn) throws SQLException {
        this.graph = conn.getSchemaGraph();
        this.storedConn = conn;
        this.currentWhereGroup = rootWhereGroup;
    }

    /**
     * Create a new query builder using the schema graph associated with
     * the given command's connection.  The command is stored so that the
     * no-argument {@link #fetchAll()}, {@link #fetchOne()}, and
     * {@link #fetchAllJSON()} methods execute through this specific
     * command instance rather than creating a new one.
     * <br><br>
     * This is useful when multiple queries must be active simultaneously
     * on the same connection, each using its own command to avoid
     * interference.
     *
     * @param cmd a Kiss database command
     * @throws SQLException if the schema graph cannot be obtained from the command's connection
     *
     * @see Command#newQueryBuilder()
     */
    public QueryBuilder(Command cmd) throws SQLException {
        this.graph = cmd.conn.getSchemaGraph();
        this.storedConn = cmd.conn;
        this.storedCmd = cmd;
        this.currentWhereGroup = rootWhereGroup;
    }

    // ---- TABLE ALIASES ---------------------------------------------------------

    /**
     * Define an alias for a table name.  Once defined, the alias can be used
     * in place of the full table name in {@code select()}, {@code where()},
     * {@code orderBy()}, and all other clause methods.  The builder resolves
     * the alias to the real table name for join path finding, and emits
     * {@code real_table alias} in the FROM and JOIN clauses so the database
     * recognizes the short name.
     * <br><br>
     * Example:
     * <pre>
     *     db.newQueryBuilder()
     *         .alias("employee", "e")
     *         .alias("department", "d")
     *         .select("e.first_name", "e.last_name", "d.name")
     *         .where("e.active = ?", "Y")
     *         .orderBy("e.last_name")
     *         .fetchAll();
     * </pre>
     *
     * @param tableName the real table name
     * @param alias     the alias to use in query clauses
     * @return this builder for chaining
     */
    public QueryBuilder alias(String tableName, String alias) {
        aliasToTable.put(alias.toLowerCase(), tableName.toLowerCase());
        tableToAlias.put(tableName.toLowerCase(), alias.toLowerCase());
        return this;
    }

    /**
     * Resolve a name that may be an alias to the real table name.
     * Returns the input unchanged (lowercased) if it is not a known alias.
     */
    private String resolveAlias(String nameOrAlias) {
        if (nameOrAlias == null)
            return null;
        String lower = nameOrAlias.toLowerCase();
        return aliasToTable.getOrDefault(lower, lower);
    }

    /**
     * Return the alias for a real table name, or null if no alias was defined.
     */
    private String getAlias(String tableName) {
        if (tableName == null)
            return null;
        return tableToAlias.get(tableName.toLowerCase());
    }

    /**
     * Return the display name for a table — its alias if one was defined,
     * otherwise the table name itself.
     */
    private String displayName(String tableName) {
        if (tableName == null)
            return null;
        String alias = tableToAlias.get(tableName.toLowerCase());
        return alias != null ? alias : tableName;
    }

    // ---- SELECT ----------------------------------------------------------------

    /**
     * Add a column to the SELECT list.
     * <br><br>
     * Accepts formats like:
     * <ul>
     *   <li>{@code "table.column"}</li>
     *   <li>{@code "table.column AS alias"}</li>
     *   <li>{@code "COUNT(table.column)"}</li>
     *   <li>{@code "SUM(table.column) AS total"}</li>
     * </ul>
     *
     * @param tableColumn the column specification
     * @return this builder for chaining
     */
    public QueryBuilder select(String tableColumn) {
        selectColumns.add(tableColumn.trim());
        return this;
    }

    /**
     * Add multiple columns to the SELECT list.
     *
     * @param tableColumns the column specifications
     * @return this builder for chaining
     */
    public QueryBuilder select(String... tableColumns) {
        for (String tc : tableColumns)
            selectColumns.add(tc.trim());
        return this;
    }

    /**
     * Enable SELECT DISTINCT.
     *
     * @return this builder for chaining
     */
    public QueryBuilder distinct() {
        this.distinct = true;
        return this;
    }

    // ---- Aggregate helpers -----------------------------------------------------

    /**
     * Add {@code COUNT(table.column)} to the SELECT list.
     *
     * @param tableColumn the column to count (e.g. {@code "employee.employee_id"})
     * @return this builder for chaining
     */
    public QueryBuilder selectCount(String tableColumn) {
        selectColumns.add("COUNT(" + tableColumn.trim() + ")");
        return this;
    }

    /**
     * Add {@code COUNT(table.column) AS alias} to the SELECT list.
     *
     * @param tableColumn the column to count
     * @param alias       the result alias
     * @return this builder for chaining
     */
    public QueryBuilder selectCount(String tableColumn, String alias) {
        selectColumns.add("COUNT(" + tableColumn.trim() + ") AS " + alias.trim());
        return this;
    }

    /**
     * Add {@code SUM(table.column)} to the SELECT list.
     *
     * @param tableColumn the column to sum
     * @return this builder for chaining
     */
    public QueryBuilder selectSum(String tableColumn) {
        selectColumns.add("SUM(" + tableColumn.trim() + ")");
        return this;
    }

    /**
     * Add {@code SUM(table.column) AS alias} to the SELECT list.
     *
     * @param tableColumn the column to sum
     * @param alias       the result alias
     * @return this builder for chaining
     */
    public QueryBuilder selectSum(String tableColumn, String alias) {
        selectColumns.add("SUM(" + tableColumn.trim() + ") AS " + alias.trim());
        return this;
    }

    /**
     * Add {@code AVG(table.column)} to the SELECT list.
     *
     * @param tableColumn the column to average
     * @return this builder for chaining
     */
    public QueryBuilder selectAvg(String tableColumn) {
        selectColumns.add("AVG(" + tableColumn.trim() + ")");
        return this;
    }

    /**
     * Add {@code AVG(table.column) AS alias} to the SELECT list.
     *
     * @param tableColumn the column to average
     * @param alias       the result alias
     * @return this builder for chaining
     */
    public QueryBuilder selectAvg(String tableColumn, String alias) {
        selectColumns.add("AVG(" + tableColumn.trim() + ") AS " + alias.trim());
        return this;
    }

    /**
     * Add {@code MIN(table.column)} to the SELECT list.
     *
     * @param tableColumn the column to find minimum of
     * @return this builder for chaining
     */
    public QueryBuilder selectMin(String tableColumn) {
        selectColumns.add("MIN(" + tableColumn.trim() + ")");
        return this;
    }

    /**
     * Add {@code MIN(table.column) AS alias} to the SELECT list.
     *
     * @param tableColumn the column to find minimum of
     * @param alias       the result alias
     * @return this builder for chaining
     */
    public QueryBuilder selectMin(String tableColumn, String alias) {
        selectColumns.add("MIN(" + tableColumn.trim() + ") AS " + alias.trim());
        return this;
    }

    /**
     * Add {@code MAX(table.column)} to the SELECT list.
     *
     * @param tableColumn the column to find maximum of
     * @return this builder for chaining
     */
    public QueryBuilder selectMax(String tableColumn) {
        selectColumns.add("MAX(" + tableColumn.trim() + ")");
        return this;
    }

    /**
     * Add {@code MAX(table.column) AS alias} to the SELECT list.
     *
     * @param tableColumn the column to find maximum of
     * @param alias       the result alias
     * @return this builder for chaining
     */
    public QueryBuilder selectMax(String tableColumn, String alias) {
        selectColumns.add("MAX(" + tableColumn.trim() + ") AS " + alias.trim());
        return this;
    }

    // ---- WHERE -----------------------------------------------------------------

    /**
     * Add a WHERE condition.  Multiple calls at the same level are combined
     * with the current group's operator (AND by default).
     * Use {@code ?} for parameter placeholders.
     *
     * @param condition the condition expression (e.g. {@code "employee.active = ?"})
     * @param params    values for the {@code ?} placeholders
     * @return this builder for chaining
     */
    public QueryBuilder where(String condition, Object... params) {
        currentWhereGroup.children.add(new WhereLeaf(condition.trim(), params));
        return this;
    }

    // ---- OR / AND grouping ----------------------------------------------------

    /**
     * Begin an OR group.  Conditions added between {@code startOr()} and
     * {@code endOr()} are combined with OR.
     *
     * @return this builder for chaining
     */
    public QueryBuilder startOr() {
        WhereGroup group = new WhereGroup(GroupType.OR);
        group.parent = currentWhereGroup;
        currentWhereGroup.children.add(group);
        currentWhereGroup = group;
        return this;
    }

    /**
     * End the current OR group and return to the parent group.
     *
     * @return this builder for chaining
     */
    public QueryBuilder endOr() {
        if (currentWhereGroup.parent == null)
            throw new IllegalStateException("endOr() called without matching startOr()");
        currentWhereGroup = currentWhereGroup.parent;
        return this;
    }

    /**
     * Begin an AND group.  Useful inside an OR group to explicitly group
     * conditions with AND.
     *
     * @return this builder for chaining
     */
    public QueryBuilder startAnd() {
        WhereGroup group = new WhereGroup(GroupType.AND);
        group.parent = currentWhereGroup;
        currentWhereGroup.children.add(group);
        currentWhereGroup = group;
        return this;
    }

    /**
     * End the current AND group and return to the parent group.
     *
     * @return this builder for chaining
     */
    public QueryBuilder endAnd() {
        if (currentWhereGroup.parent == null)
            throw new IllegalStateException("endAnd() called without matching startAnd()");
        currentWhereGroup = currentWhereGroup.parent;
        return this;
    }

    // ---- Subqueries in WHERE --------------------------------------------------

    /**
     * Add a {@code column IN (SELECT ...)} condition using a subquery.
     *
     * @param column   the column to test (e.g. {@code "employee.employee_id"})
     * @param subquery a QueryBuilder that produces the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereIn(String column, QueryBuilder subquery) {
        currentWhereGroup.children.add(new WhereSubquery(column.trim() + " IN ", subquery));
        return this;
    }

    /**
     * Add a {@code column IN (raw SQL)} condition.
     *
     * @param column the column to test
     * @param rawSQL the subquery SQL
     * @param params parameter values for the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereIn(String column, String rawSQL, Object... params) {
        currentWhereGroup.children.add(new WhereRawSubquery(column.trim() + " IN (" + rawSQL.trim() + ")", params));
        return this;
    }

    /**
     * Add a {@code column NOT IN (SELECT ...)} condition using a subquery.
     *
     * @param column   the column to test
     * @param subquery a QueryBuilder that produces the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereNotIn(String column, QueryBuilder subquery) {
        currentWhereGroup.children.add(new WhereSubquery(column.trim() + " NOT IN ", subquery));
        return this;
    }

    /**
     * Add a {@code column NOT IN (raw SQL)} condition.
     *
     * @param column the column to test
     * @param rawSQL the subquery SQL
     * @param params parameter values for the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereNotIn(String column, String rawSQL, Object... params) {
        currentWhereGroup.children.add(new WhereRawSubquery(column.trim() + " NOT IN (" + rawSQL.trim() + ")", params));
        return this;
    }

    /**
     * Add an {@code EXISTS (SELECT ...)} condition using a subquery.
     *
     * @param subquery a QueryBuilder that produces the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereExists(QueryBuilder subquery) {
        currentWhereGroup.children.add(new WhereSubquery("EXISTS ", subquery));
        return this;
    }

    /**
     * Add an {@code EXISTS (raw SQL)} condition.
     *
     * @param rawSQL the subquery SQL
     * @param params parameter values for the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereExists(String rawSQL, Object... params) {
        currentWhereGroup.children.add(new WhereRawSubquery("EXISTS (" + rawSQL.trim() + ")", params));
        return this;
    }

    /**
     * Add a {@code NOT EXISTS (SELECT ...)} condition using a subquery.
     *
     * @param subquery a QueryBuilder that produces the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereNotExists(QueryBuilder subquery) {
        currentWhereGroup.children.add(new WhereSubquery("NOT EXISTS ", subquery));
        return this;
    }

    /**
     * Add a {@code NOT EXISTS (raw SQL)} condition.
     *
     * @param rawSQL the subquery SQL
     * @param params parameter values for the subquery
     * @return this builder for chaining
     */
    public QueryBuilder whereNotExists(String rawSQL, Object... params) {
        currentWhereGroup.children.add(new WhereRawSubquery("NOT EXISTS (" + rawSQL.trim() + ")", params));
        return this;
    }

    // ---- ORDER BY --------------------------------------------------------------

    /**
     * Add an ORDER BY column (ascending).
     *
     * @param tableColumn the column to order by (e.g. {@code "employee.last_name"})
     * @return this builder for chaining
     */
    public QueryBuilder orderBy(String tableColumn) {
        orderByColumns.add(tableColumn.trim());
        return this;
    }

    /**
     * Add an ORDER BY column (descending).
     *
     * @param tableColumn the column to order by
     * @return this builder for chaining
     */
    public QueryBuilder orderByDesc(String tableColumn) {
        orderByColumns.add(tableColumn.trim() + " DESC");
        return this;
    }

    // ---- GROUP BY / HAVING -----------------------------------------------------

    /**
     * Add a GROUP BY column.
     *
     * @param tableColumn the column to group by
     * @return this builder for chaining
     */
    public QueryBuilder groupBy(String tableColumn) {
        groupByColumns.add(tableColumn.trim());
        return this;
    }

    /**
     * Add a HAVING condition.  Multiple calls are combined with AND.
     *
     * @param condition the HAVING expression
     * @param params    values for the {@code ?} placeholders
     * @return this builder for chaining
     */
    public QueryBuilder having(String condition, Object... params) {
        havingClauses.add(new WhereClause(condition.trim(), params));
        return this;
    }

    // ---- INNER JOIN ------------------------------------------------------------

    /**
     * Force a specific single-column INNER JOIN without an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @return this builder for chaining
     */
    public QueryBuilder join(String fromTable, String fromColumn,
                             String toTable, String toColumn) {
        return addExplicitJoin(JoinType.INNER, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, null);
    }

    /**
     * Force a specific single-column INNER JOIN, optionally with an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @param alias      alias for the joined table (required for self-joins, may be null)
     * @return this builder for chaining
     */
    public QueryBuilder join(String fromTable, String fromColumn,
                             String toTable, String toColumn, String alias) {
        return addExplicitJoin(JoinType.INNER, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, alias);
    }

    /**
     * Force a specific composite (multi-column) INNER JOIN, optionally with an alias.
     *
     * @param fromTable   the table containing the FK columns
     * @param fromColumns the FK column names
     * @param toTable     the referenced table
     * @param toColumns   the referenced column names
     * @param alias       alias for the joined table (may be null)
     * @return this builder for chaining
     */
    public QueryBuilder join(String fromTable, String[] fromColumns,
                             String toTable, String[] toColumns, String alias) {
        return addExplicitJoin(JoinType.INNER, fromTable, fromColumns, toTable, toColumns, alias);
    }

    // ---- JOIN TYPE OVERRIDES ----------------------------------------------------

    /**
     * Override the join type to LEFT JOIN for an auto-discovered join to the
     * specified table.  The foreign key path is still resolved automatically
     * from the schema graph; only the join type changes.
     * <br><br>
     * The table may be specified by its real name or by an alias
     * defined via {@link #alias(String, String)}.
     *
     * @param table the table (or alias) whose auto-join should use LEFT JOIN
     * @return this builder for chaining
     */
    public QueryBuilder leftJoin(String table) {
        joinTypeOverrides.put(resolveAlias(table.toLowerCase()), JoinType.LEFT);
        return this;
    }

    /**
     * Override the join type to RIGHT JOIN for an auto-discovered join to the
     * specified table.  The foreign key path is still resolved automatically
     * from the schema graph; only the join type changes.
     * <br><br>
     * The table may be specified by its real name or by an alias
     * defined via {@link #alias(String, String)}.
     *
     * @param table the table (or alias) whose auto-join should use RIGHT JOIN
     * @return this builder for chaining
     */
    public QueryBuilder rightJoin(String table) {
        joinTypeOverrides.put(resolveAlias(table.toLowerCase()), JoinType.RIGHT);
        return this;
    }

    // ---- LEFT JOIN -------------------------------------------------------------

    /**
     * Force a specific single-column LEFT JOIN without an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @return this builder for chaining
     */
    public QueryBuilder leftJoin(String fromTable, String fromColumn,
                                 String toTable, String toColumn) {
        return addExplicitJoin(JoinType.LEFT, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, null);
    }

    /**
     * Force a specific single-column LEFT JOIN with an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @param alias      alias for the joined table (may be null)
     * @return this builder for chaining
     */
    public QueryBuilder leftJoin(String fromTable, String fromColumn,
                                 String toTable, String toColumn, String alias) {
        return addExplicitJoin(JoinType.LEFT, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, alias);
    }

    /**
     * Force a specific composite (multi-column) LEFT JOIN, optionally with an alias.
     *
     * @param fromTable   the table containing the FK columns
     * @param fromColumns the FK column names
     * @param toTable     the referenced table
     * @param toColumns   the referenced column names
     * @param alias       alias for the joined table (may be null)
     * @return this builder for chaining
     */
    public QueryBuilder leftJoin(String fromTable, String[] fromColumns,
                                 String toTable, String[] toColumns, String alias) {
        return addExplicitJoin(JoinType.LEFT, fromTable, fromColumns, toTable, toColumns, alias);
    }

    // ---- RIGHT JOIN ------------------------------------------------------------

    /**
     * Force a specific single-column RIGHT JOIN without an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @return this builder for chaining
     */
    public QueryBuilder rightJoin(String fromTable, String fromColumn,
                                  String toTable, String toColumn) {
        return addExplicitJoin(JoinType.RIGHT, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, null);
    }

    /**
     * Force a specific single-column RIGHT JOIN with an alias.
     *
     * @param fromTable  the table containing the FK column
     * @param fromColumn the FK column name
     * @param toTable    the referenced table
     * @param toColumn   the referenced column
     * @param alias      alias for the joined table (may be null)
     * @return this builder for chaining
     */
    public QueryBuilder rightJoin(String fromTable, String fromColumn,
                                  String toTable, String toColumn, String alias) {
        return addExplicitJoin(JoinType.RIGHT, fromTable,
                new String[]{fromColumn}, toTable, new String[]{toColumn}, alias);
    }

    /**
     * Force a specific composite (multi-column) RIGHT JOIN, optionally with an alias.
     *
     * @param fromTable   the table containing the FK columns
     * @param fromColumns the FK column names
     * @param toTable     the referenced table
     * @param toColumns   the referenced column names
     * @param alias       alias for the joined table (may be null)
     * @return this builder for chaining
     */
    public QueryBuilder rightJoin(String fromTable, String[] fromColumns,
                                  String toTable, String[] toColumns, String alias) {
        return addExplicitJoin(JoinType.RIGHT, fromTable, fromColumns, toTable, toColumns, alias);
    }

    // ---- Common explicit join builder ------------------------------------------

    private QueryBuilder addExplicitJoin(JoinType joinType, String fromTable,
                                          String[] fromColumns, String toTable,
                                          String[] toColumns, String alias) {
        if (fromColumns.length != toColumns.length || fromColumns.length == 0)
            throw new IllegalArgumentException("fromColumns and toColumns must have equal non-zero length");
        String[] fc = new String[fromColumns.length];
        String[] tc = new String[toColumns.length];
        for (int i = 0; i < fromColumns.length; i++) {
            fc[i] = fromColumns[i].trim().toLowerCase();
            tc[i] = toColumns[i].trim().toLowerCase();
        }
        explicitJoins.add(new ExplicitJoin(
                joinType,
                resolveAlias(fromTable.trim().toLowerCase()), fc,
                resolveAlias(toTable.trim().toLowerCase()), tc,
                alias != null ? alias.trim() : null));
        return this;
    }

    // ---- LIMIT -----------------------------------------------------------------

    /**
     * Limit the number of rows returned.
     *
     * @param max maximum number of rows
     * @return this builder for chaining
     */
    public QueryBuilder limit(int max) {
        this.maxRows = max;
        return this;
    }

    // ---- CTE (WITH ... AS) ----------------------------------------------------

    /**
     * Add a Common Table Expression (CTE) with raw SQL.
     *
     * @param name   the CTE name
     * @param rawSQL the CTE body SQL
     * @param params parameter values for the CTE body
     * @return this builder for chaining
     */
    public QueryBuilder with(String name, String rawSQL, Object... params) {
        ctes.add(new CTE(name.trim(), rawSQL.trim(), params));
        return this;
    }

    /**
     * Add a Common Table Expression (CTE) using a QueryBuilder as the body.
     *
     * @param name     the CTE name
     * @param subquery a QueryBuilder that produces the CTE body
     * @return this builder for chaining
     */
    public QueryBuilder with(String name, QueryBuilder subquery) {
        ctes.add(new CTE(name.trim(), subquery));
        return this;
    }

    // ---- UNION -----------------------------------------------------------------

    /**
     * Append a UNION (duplicate-removing) with another query.
     *
     * @param other the query to union with
     * @return this builder for chaining
     */
    public QueryBuilder union(QueryBuilder other) {
        unions.add(new UnionClause(false, other));
        return this;
    }

    /**
     * Append a UNION ALL (keeping duplicates) with another query.
     *
     * @param other the query to union with
     * @return this builder for chaining
     */
    public QueryBuilder unionAll(QueryBuilder other) {
        unions.add(new UnionClause(true, other));
        return this;
    }

    // ---- BUILD -----------------------------------------------------------------

    /**
     * Build the SQL query string.  After calling this method,
     * {@link #getParameters()} returns the ordered parameter values.
     *
     * @return the generated SQL string
     * @throws SQLException if no join path exists between the referenced tables
     */
    public String build() throws SQLException {
        if (selectColumns.isEmpty())
            throw new SQLException("QueryBuilder: at least one select column is required");

        parameters = new ArrayList<>();

        // 1. Collect all referenced tables
        Set<String> referencedTables = new LinkedHashSet<>();
        String firstWhereTable = null;
        String firstSelectTable = null;

        for (String col : selectColumns) {
            String table = resolveExtractTable(col);
            if (table != null) {
                referencedTables.add(table);
                if (firstSelectTable == null)
                    firstSelectTable = table;
            }
        }

        // Collect tables from where tree
        firstWhereTable = collectTablesFromWhereTree(rootWhereGroup, referencedTables);

        for (String col : orderByColumns) {
            String table = resolveExtractTable(col);
            if (table != null)
                referencedTables.add(table);
        }

        for (String col : groupByColumns) {
            String table = resolveExtractTable(col);
            if (table != null)
                referencedTables.add(table);
        }

        for (WhereClause hc : havingClauses)
            referencedTables.addAll(resolveExtractAllTables(hc.condition));

        // Collect explicit join aliases and toTables so we can exclude them
        // from auto-join resolution.  Explicit joins handle their own ON conditions,
        // so their target tables don't need to be connected via the schema graph.
        Set<String> explicitAliases = new HashSet<>();
        Set<String> explicitHandledTables = new HashSet<>();
        for (ExplicitJoin ej : explicitJoins) {
            if (ej.alias != null)
                explicitAliases.add(ej.alias.toLowerCase());
            // The fromTable must participate in auto-join (it's a real table that
            // needs to be reachable from the root)
            referencedTables.add(ej.fromTable);
            // The toTable is handled by the explicit join — exclude from graph path finding.
            // For self-joins, don't exclude the table (it's both from and to).
            if (!ej.fromTable.equals(ej.toTable))
                explicitHandledTables.add(ej.toTable);
        }

        // Collect CTE names so they are treated like aliases (not real DB tables)
        Set<String> cteNames = new HashSet<>();
        for (CTE cte : ctes)
            cteNames.add(cte.name.toLowerCase());

        // Remember which referenced names are CTE names (they become the FROM table directly)
        Set<String> referencedCTEs = new LinkedHashSet<>();
        for (String t : referencedTables)
            if (cteNames.contains(t.toLowerCase()))
                referencedCTEs.add(t);

        // Remove aliases and CTE names from referenced tables — they aren't real DB tables
        referencedTables.removeAll(explicitAliases);
        referencedTables.removeAll(cteNames);

        // Build the set of tables that need graph-based path finding
        Set<String> autoJoinTables = new LinkedHashSet<>(referencedTables);
        autoJoinTables.removeAll(explicitHandledTables);

        // If all references are CTEs/aliases (no real DB tables), use the first CTE as root
        boolean cteOnlyQuery = autoJoinTables.isEmpty() && referencedTables.isEmpty() && !referencedCTEs.isEmpty();
        if (autoJoinTables.isEmpty() && referencedTables.isEmpty() && referencedCTEs.isEmpty())
            throw new SQLException("QueryBuilder: no table references found in the query");

        String rootTable;
        List<SchemaGraph.Edge> joinEdges;

        if (cteOnlyQuery) {
            // Query only references CTEs — pick the first one as root, no graph path finding needed
            rootTable = referencedCTEs.iterator().next();
            joinEdges = Collections.emptyList();
        } else {
            if (autoJoinTables.isEmpty())
                autoJoinTables.add(referencedTables.iterator().next());

            // 2. Determine root table (prefer first WHERE table, then first SELECT table)
            rootTable = firstWhereTable != null ? firstWhereTable : firstSelectTable;
            if (rootTable == null || explicitAliases.contains(rootTable)
                    || cteNames.contains(rootTable != null ? rootTable.toLowerCase() : "")
                    || !autoJoinTables.contains(rootTable))
                rootTable = autoJoinTables.iterator().next();

            // 3. Find join path for tables in the graph
            if (autoJoinTables.size() <= 1)
                joinEdges = Collections.emptyList();
            else
                joinEdges = graph.findJoinPath(autoJoinTables, rootTable);
        }

        // 4. Build the ordered list of joins from the root (auto-joins are INNER)
        List<JoinClause> joins = buildJoinOrder(rootTable, joinEdges);

        // 5. Add explicit joins
        for (ExplicitJoin ej : explicitJoins) {
            String onCondition = buildExplicitOnCondition(ej);
            // Use explicit join alias if provided, otherwise use table alias from alias()
            String joinAlias = ej.alias != null ? ej.alias : getAlias(ej.toTable);
            joins.add(new JoinClause(ej.joinType, ej.toTable, joinAlias, onCondition));
        }

        // 6. Assemble SQL
        StringBuilder sql = new StringBuilder();

        // CTEs
        if (!ctes.isEmpty()) {
            sql.append("WITH ");
            for (int i = 0; i < ctes.size(); i++) {
                if (i > 0)
                    sql.append(", ");
                CTE cte = ctes.get(i);
                sql.append(cte.name).append(" AS (");
                if (cte.subquery != null) {
                    String subSQL = cte.subquery.build();
                    sql.append(subSQL);
                    parameters.addAll(cte.subquery.getParameters());
                } else {
                    sql.append(cte.rawSQL);
                    if (cte.params != null)
                        Collections.addAll(parameters, cte.params);
                }
                sql.append(")");
            }
            sql.append(" ");
        }

        // SELECT
        sql.append("SELECT ");
        if (distinct)
            sql.append("DISTINCT ");
        for (int i = 0; i < selectColumns.size(); i++) {
            if (i > 0)
                sql.append(", ");
            sql.append(selectColumns.get(i));
        }

        // FROM
        sql.append(" FROM ").append(rootTable);
        String rootAlias = getAlias(rootTable);
        if (rootAlias != null)
            sql.append(" ").append(rootAlias);

        // JOINs
        for (JoinClause jc : joins) {
            sql.append(" ").append(jc.joinType.sql).append(" ");
            sql.append(jc.table);
            if (jc.alias != null)
                sql.append(" ").append(jc.alias);
            sql.append(" ON ").append(jc.onCondition);
        }

        // WHERE
        String whereSQL = buildWhereTree(rootWhereGroup, parameters);
        if (whereSQL != null)
            sql.append(" WHERE ").append(whereSQL);

        // GROUP BY
        if (!groupByColumns.isEmpty()) {
            sql.append(" GROUP BY ");
            for (int i = 0; i < groupByColumns.size(); i++) {
                if (i > 0)
                    sql.append(", ");
                sql.append(groupByColumns.get(i));
            }
        }

        // HAVING
        if (!havingClauses.isEmpty()) {
            sql.append(" HAVING ");
            for (int i = 0; i < havingClauses.size(); i++) {
                if (i > 0)
                    sql.append(" AND ");
                WhereClause hc = havingClauses.get(i);
                sql.append(hc.condition);
                if (hc.params != null)
                    Collections.addAll(parameters, hc.params);
            }
        }

        // UNION
        for (UnionClause uc : unions) {
            sql.append(uc.all ? " UNION ALL " : " UNION ");
            String unionSQL = uc.query.build();
            sql.append(unionSQL);
            parameters.addAll(uc.query.getParameters());
        }

        // ORDER BY
        if (!orderByColumns.isEmpty()) {
            sql.append(" ORDER BY ");
            for (int i = 0; i < orderByColumns.size(); i++) {
                if (i > 0)
                    sql.append(", ");
                sql.append(orderByColumns.get(i));
            }
        }

        builtSQL = sql.toString();
        return builtSQL;
    }

    /**
     * Return a formatted, human-readable version of the built SQL for debugging.
     * Major SQL clauses appear on separate lines with joins indented for clarity.
     * Parameter values are appended at the end.
     * <br><br>
     * {@link #build()} is called automatically if it has not been called yet.
     *
     * @return formatted SQL string with parameter values
     */
    @Override
    public String toString() {
        if (builtSQL == null) {
            try {
                build();
            } catch (SQLException e) {
                return "QueryBuilder error: " + e.getMessage();
            }
        }

        // Keywords to break on — longer matches first to avoid partial matches
        String[][] breakpoints = {
            {" FROM ",       ""},
            {" LEFT JOIN ",  "    "},
            {" RIGHT JOIN ", "    "},
            {" JOIN ",       "    "},
            {" WHERE ",      ""},
            {" GROUP BY ",   ""},
            {" HAVING ",     ""},
            {" UNION ALL ",  ""},
            {" UNION ",      ""},
            {" ORDER BY ",   ""},
        };

        String sql = builtSQL;
        String upper = sql.toUpperCase();
        StringBuilder sb = new StringBuilder();
        int pos = 0;

        // Handle WITH (CTE) — put main SELECT on new line
        int withIdx = findKeywordOutsideParens(upper, pos, "WITH ");
        if (withIdx == 0) {
            int mainSelect = findMainSelectAfterWith(sql, upper);
            if (mainSelect > 0) {
                sb.append(sql, 0, mainSelect).append("\n");
                pos = mainSelect;
            }
        }

        while (pos < sql.length()) {
            // Find the earliest breakpoint keyword after current position
            String matchedKw = null;
            String matchedIndent = null;
            int matchIdx = Integer.MAX_VALUE;

            for (String[] bp : breakpoints) {
                int idx = findKeywordOutsideParens(upper, pos, bp[0]);
                if (idx >= 0 && idx < matchIdx) {
                    matchIdx = idx;
                    matchedKw = bp[0];
                    matchedIndent = bp[1];
                }
            }

            if (matchedKw == null) {
                // No more keywords — append the rest
                sb.append(sql, pos, sql.length());
                break;
            }

            // Append text before the keyword
            sb.append(sql, pos, matchIdx);
            // Start keyword on new line with indentation
            sb.append("\n").append(matchedIndent).append(matchedKw.trim()).append(" ");
            pos = matchIdx + matchedKw.length();
        }

        // Append parameter values
        if (parameters != null && !parameters.isEmpty()) {
            sb.append("\n-- Parameters: ").append(parameters);
        }

        return sb.toString();
    }

    /**
     * Find the position of the main SELECT keyword after a WITH clause,
     * skipping any SELECT keywords inside CTE subqueries (within parentheses).
     * Returns -1 if not found.
     */
    private static int findMainSelectAfterWith(String sql, String upper) {
        int depth = 0;
        for (int i = 0; i < sql.length() - 6; i++) {
            char c = sql.charAt(i);
            if (c == '(')
                depth++;
            else if (c == ')')
                depth--;
            else if (depth == 0 && i > 0 && upper.startsWith("SELECT ", i))
                return i;
        }
        return -1;
    }

    /**
     * Find the index of a keyword in the string starting from {@code from},
     * but only when it appears outside of parentheses.  Returns -1 if not found.
     */
    private static int findKeywordOutsideParens(String upper, int from, String keyword) {
        int depth = 0;
        for (int i = from; i <= upper.length() - keyword.length(); i++) {
            char c = upper.charAt(i);
            if (c == '(')
                depth++;
            else if (c == ')')
                depth--;
            else if (depth == 0 && upper.startsWith(keyword, i))
                return i;
        }
        return -1;
    }

    /**
     * Return the ordered list of parameter values for the {@code ?} placeholders
     * in the SQL produced by {@link #build()}.  Must be called after {@code build()}.
     *
     * @return the parameter list
     */
    public List<Object> getParameters() {
        if (parameters == null)
            throw new IllegalStateException("build() must be called before getParameters()");
        return Collections.unmodifiableList(parameters);
    }

    // ---- Execute convenience methods -------------------------------------------

    /**
     * Build and execute the query, returning all results as Records.
     *
     * @param conn the database connection
     * @return list of records
     * @throws Exception on database or query building errors
     */
    public List<Record> fetchAll(Connection conn) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = conn.limit(maxRows, sql);
        return conn.fetchAll(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning the first result or null.
     *
     * @param conn the database connection
     * @return the first matching record, or null
     * @throws Exception on database or query building errors
     */
    public Record fetchOne(Connection conn) throws Exception {
        String sql = build();
        sql = conn.limit(1, sql);
        return conn.fetchOne(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning results as a JSONArray.
     *
     * @param conn the database connection
     * @return JSONArray of results
     * @throws Exception on database or query building errors
     */
    public JSONArray fetchAllJSON(Connection conn) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = conn.limit(maxRows, sql);
        return conn.fetchAllJSON(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning all results as Records.
     *
     * @param cmd the database command
     * @return list of records
     * @throws Exception on database or query building errors
     */
    public List<Record> fetchAll(Command cmd) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = cmd.conn.limit(maxRows, sql);
        return cmd.fetchAll(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning the first result or null.
     *
     * @param cmd the database command
     * @return the first matching record, or null
     * @throws Exception on database or query building errors
     */
    public Record fetchOne(Command cmd) throws Exception {
        String sql = build();
        sql = cmd.conn.limit(1, sql);
        return cmd.fetchOne(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning results as a JSONArray.
     *
     * @param cmd the database command
     * @return JSONArray of results
     * @throws Exception on database or query building errors
     */
    public JSONArray fetchAllJSON(Command cmd) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = cmd.conn.limit(maxRows, sql);
        return cmd.fetchAllJSON(sql, parameters.toArray());
    }

    /**
     * Build and execute the query using the stored connection or command,
     * returning all results as {@code Record} objects.
     * <br><br>
     * If the builder was created with a {@link Command} (via
     * {@link #QueryBuilder(Command)} or {@link Command#newQueryBuilder()}),
     * the query executes through that command.  Otherwise, the stored
     * connection is used.
     *
     * @return list of matching records
     * @throws Exception on database or query building errors
     * @throws IllegalStateException if no connection or command was stored
     */
    public List<Record> fetchAll() throws Exception {
        if (storedCmd != null)
            return fetchAll(storedCmd);
        if (storedConn == null)
            throw new IllegalStateException("No connection available — use fetchAll(Connection) or create the QueryBuilder with a Connection or Command");
        return fetchAll(storedConn);
    }

    /**
     * Build and execute the query using the stored connection or command,
     * returning the first result or null.
     * <br><br>
     * If the builder was created with a {@link Command}, the query
     * executes through that command.  Otherwise, the stored connection
     * is used.
     *
     * @return the first matching record, or null
     * @throws Exception on database or query building errors
     * @throws IllegalStateException if no connection or command was stored
     */
    public Record fetchOne() throws Exception {
        if (storedCmd != null)
            return fetchOne(storedCmd);
        if (storedConn == null)
            throw new IllegalStateException("No connection available — use fetchOne(Connection) or create the QueryBuilder with a Connection or Command");
        return fetchOne(storedConn);
    }

    /**
     * Build and execute the query using the stored connection or command,
     * returning results as a {@code JSONArray}.
     * <br><br>
     * If the builder was created with a {@link Command}, the query
     * executes through that command.  Otherwise, the stored connection
     * is used.
     *
     * @return JSONArray of results
     * @throws Exception on database or query building errors
     * @throws IllegalStateException if no connection or command was stored
     */
    public JSONArray fetchAllJSON() throws Exception {
        if (storedCmd != null)
            return fetchAllJSON(storedCmd);
        if (storedConn == null)
            throw new IllegalStateException("No connection available — use fetchAllJSON(Connection) or create the QueryBuilder with a Connection or Command");
        return fetchAllJSON(storedConn);
    }

    /**
     * Build and execute the query, returning a {@code Cursor} for
     * row-by-row iteration.
     *
     * @param conn the database connection
     * @return a Cursor over the result set
     * @throws Exception on database or query building errors
     */
    public Cursor query(Connection conn) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = conn.limit(maxRows, sql);
        return conn.query(sql, parameters.toArray());
    }

    /**
     * Build and execute the query, returning a {@code Cursor} for
     * row-by-row iteration.
     *
     * @param cmd the database command
     * @return a Cursor over the result set
     * @throws Exception on database or query building errors
     */
    public Cursor query(Command cmd) throws Exception {
        String sql = build();
        if (maxRows > 0)
            sql = cmd.conn.limit(maxRows, sql);
        return cmd.query(sql, parameters.toArray());
    }

    /**
     * Build and execute the query using the stored connection or command,
     * returning a {@code Cursor} for row-by-row iteration.
     * <br><br>
     * If the builder was created with a {@link Command}, the query
     * executes through that command.  Otherwise, the stored connection
     * is used.
     *
     * @return a Cursor over the result set
     * @throws Exception on database or query building errors
     * @throws IllegalStateException if no connection or command was stored
     */
    public Cursor query() throws Exception {
        if (storedCmd != null)
            return query(storedCmd);
        if (storedConn == null)
            throw new IllegalStateException("No connection available — use query(Connection) or create the QueryBuilder with a Connection or Command");
        return query(storedConn);
    }

    // ---- Internal helpers -------------------------------------------------------

    // Pattern to match table.column references, accounting for aggregate functions
    // Matches: table.column, func(table.column), table.column AS alias
    // Does NOT match: standalone words, string literals, numeric literals
    private static final Pattern TABLE_DOT_COLUMN = Pattern.compile(
            "\\b([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\.\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\b");

    /**
     * Extract the first table name from an expression like "table.column",
     * "table.column AS alias", "COUNT(table.column)", etc.
     * Does not resolve aliases — returns the raw name as written.
     */
    static String extractTable(String expr) {
        Matcher m = TABLE_DOT_COLUMN.matcher(expr);
        if (m.find())
            return m.group(1).toLowerCase();
        return null;
    }

    /**
     * Extract the first table name from an expression, resolving any
     * alias to the real table name.
     */
    private String resolveExtractTable(String expr) {
        return resolveAlias(extractTable(expr));
    }

    /**
     * Extract all table names from an expression that may contain multiple
     * table.column references.  Does not resolve aliases.
     */
    static Set<String> extractAllTables(String expr) {
        Set<String> tables = new LinkedHashSet<>();
        String cleaned = removeSubqueries(expr);
        Matcher m = TABLE_DOT_COLUMN.matcher(cleaned);
        while (m.find())
            tables.add(m.group(1).toLowerCase());
        return tables;
    }

    /**
     * Extract all table names from an expression, resolving any
     * aliases to real table names.
     */
    private Set<String> resolveExtractAllTables(String expr) {
        Set<String> raw = extractAllTables(expr);
        Set<String> resolved = new LinkedHashSet<>();
        for (String t : raw)
            resolved.add(resolveAlias(t));
        return resolved;
    }

    /**
     * Remove all parenthesized SELECT subqueries from {@code sql} before
     * scanning for table.column references.  Table aliases used only inside a
     * subquery (e.g. {@code mt2} in {@code (SELECT ... FROM message_to mt2 ...)})
     * must not be mistaken for outer-query table references.
     *
     * <p>The scan is case-insensitive and handles nested parentheses correctly.
     * Every {@code (SELECT ...)} block — at any nesting depth — is replaced with
     * an empty string so that its contents are invisible to the caller's regex.
     */
    private static String removeSubqueries(String sql) {
        if (sql == null || sql.isEmpty())
            return sql;

        // Pattern: an opening '(' optionally followed by whitespace then SELECT
        Pattern subqueryStart = Pattern.compile("\\(\\s*SELECT\\b", Pattern.CASE_INSENSITIVE);

        StringBuilder result = new StringBuilder(sql);
        Matcher m = subqueryStart.matcher(result);

        // Each removal shifts indices, so restart the search from the
        // beginning after every removal.  In practice subqueries are rare
        // and short, so the cost is negligible.
        while (m.find()) {
            int start = m.start();          // position of the opening '('
            int depth = 1;
            int i = m.end();               // first character after the '('...SELECT match
            int len = result.length();

            while (i < len && depth > 0) {
                char c = result.charAt(i);
                if (c == '(')
                    depth++;
                else if (c == ')')
                    depth--;
                i++;
            }

            // i now points one past the closing ')' that matched the opening '('
            result.delete(start, i);

            // Reset matcher against the modified string
            m = subqueryStart.matcher(result);
        }

        return result.toString();
    }

    /**
     * Build the ON condition string for an explicit join.
     */
    private String buildExplicitOnCondition(ExplicitJoin ej) {
        // Use the explicit join alias if provided, otherwise use the table alias
        // from alias(), otherwise fall back to the real table name
        String fromRef = displayName(ej.fromTable);
        String toRef = ej.alias != null ? ej.alias : displayName(ej.toTable);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ej.fromColumns.length; i++) {
            if (i > 0)
                sb.append(" AND ");
            sb.append(fromRef).append('.').append(ej.fromColumns[i])
                    .append(" = ")
                    .append(toRef).append('.').append(ej.toColumns[i]);
        }
        return sb.toString();
    }

    /**
     * Given the root table and a set of join edges, produce an ordered list of
     * JoinClauses starting from the root.  Uses BFS on the join edges to determine
     * the correct order.  Auto-discovered joins are always INNER JOINs.
     */
    private List<JoinClause> buildJoinOrder(String rootTable, List<SchemaGraph.Edge> edges) {
        if (edges.isEmpty())
            return new ArrayList<>();

        // Build a mini adjacency list from just the join edges
        Map<String, List<SchemaGraph.Edge>> miniAdj = new HashMap<>();
        for (SchemaGraph.Edge e : edges) {
            miniAdj.computeIfAbsent(e.getFromTable(), k -> new ArrayList<>()).add(e);
            if (!e.getFromTable().equals(e.getToTable()))
                miniAdj.computeIfAbsent(e.getToTable(), k -> new ArrayList<>()).add(e);
        }

        // BFS from root to order the joins
        List<JoinClause> result = new ArrayList<>();
        Set<String> visited = new LinkedHashSet<>();
        visited.add(rootTable);
        Queue<String> queue = new LinkedList<>();
        queue.add(rootTable);

        while (!queue.isEmpty()) {
            String current = queue.poll();
            List<SchemaGraph.Edge> adj = miniAdj.get(current);
            if (adj == null)
                continue;
            for (SchemaGraph.Edge e : adj) {
                String neighbor = e.otherTable(current);
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    String fromAlias = getAlias(e.getFromTable());
                    String toAlias = getAlias(e.getToTable());
                    String onCondition = e.buildOnCondition(fromAlias, toAlias);
                    JoinType jt = joinTypeOverrides.getOrDefault(neighbor.toLowerCase(), JoinType.INNER);
                    String neighborAlias = getAlias(neighbor);
                    result.add(new JoinClause(jt, neighbor, neighborAlias, onCondition));
                    queue.add(neighbor);
                }
            }
        }

        return result;
    }

    // ---- Where tree helpers ----------------------------------------------------

    /**
     * Collect all table references from the WHERE clause tree.
     * Returns the first WHERE table found (for root table determination).
     */
    private String collectTablesFromWhereTree(WhereNode node, Set<String> tables) {
        String firstTable = null;
        if (node instanceof WhereLeaf) {
            WhereLeaf leaf = (WhereLeaf) node;
            Set<String> leafTables = resolveExtractAllTables(leaf.condition);
            tables.addAll(leafTables);
            if (!leafTables.isEmpty())
                firstTable = leafTables.iterator().next();
        } else if (node instanceof WhereSubquery) {
            WhereSubquery ws = (WhereSubquery) node;
            Set<String> prefixTables = resolveExtractAllTables(ws.prefix);
            tables.addAll(prefixTables);
            if (!prefixTables.isEmpty())
                firstTable = prefixTables.iterator().next();
            // Don't collect tables from the subquery itself — it's self-contained
        } else if (node instanceof WhereRawSubquery) {
            WhereRawSubquery wr = (WhereRawSubquery) node;
            Set<String> rawTables = resolveExtractAllTables(wr.sql);
            tables.addAll(rawTables);
            if (!rawTables.isEmpty())
                firstTable = rawTables.iterator().next();
        } else if (node instanceof WhereGroup) {
            WhereGroup group = (WhereGroup) node;
            for (WhereNode child : group.children) {
                String t = collectTablesFromWhereTree(child, tables);
                if (firstTable == null)
                    firstTable = t;
            }
        }
        return firstTable;
    }

    /**
     * Build the WHERE clause string from the WHERE tree, collecting parameters.
     * Returns null if the tree produces no conditions.
     */
    private String buildWhereTree(WhereNode node, List<Object> params) throws SQLException {
        if (node instanceof WhereLeaf) {
            WhereLeaf leaf = (WhereLeaf) node;
            if (leaf.params != null)
                Collections.addAll(params, leaf.params);
            return leaf.condition;
        } else if (node instanceof WhereSubquery) {
            WhereSubquery ws = (WhereSubquery) node;
            String subSQL = ws.subquery.build();
            params.addAll(ws.subquery.getParameters());
            return ws.prefix + "(" + subSQL + ")";
        } else if (node instanceof WhereRawSubquery) {
            WhereRawSubquery wr = (WhereRawSubquery) node;
            if (wr.params != null)
                Collections.addAll(params, wr.params);
            return wr.sql;
        } else if (node instanceof WhereGroup) {
            WhereGroup group = (WhereGroup) node;
            List<String> parts = new ArrayList<>();
            for (WhereNode child : group.children) {
                String part = buildWhereTree(child, params);
                if (part != null)
                    parts.add(part);
            }
            if (parts.isEmpty())
                return null;
            if (parts.size() == 1)
                return parts.get(0);
            String separator = group.type == GroupType.OR ? " OR " : " AND ";
            StringBuilder sb = new StringBuilder();
            // Add parentheses for non-root groups
            boolean needParens = group.parent != null;
            if (needParens)
                sb.append("(");
            for (int i = 0; i < parts.size(); i++) {
                if (i > 0)
                    sb.append(separator);
                sb.append(parts.get(i));
            }
            if (needParens)
                sb.append(")");
            return sb.toString();
        }
        return null;
    }

    // ---- Inner types -----------------------------------------------------------

    /** Base interface for WHERE clause tree nodes */
    private interface WhereNode {}

    /** A single WHERE condition (leaf node) */
    private static class WhereLeaf implements WhereNode {
        final String condition;
        final Object[] params;

        WhereLeaf(String condition, Object[] params) {
            this.condition = condition;
            this.params = params;
        }
    }

    /** A WHERE condition using a QueryBuilder subquery (IN, EXISTS, etc.) */
    private static class WhereSubquery implements WhereNode {
        final String prefix;  // e.g. "employee.id IN " or "EXISTS "
        final QueryBuilder subquery;

        WhereSubquery(String prefix, QueryBuilder subquery) {
            this.prefix = prefix;
            this.subquery = subquery;
        }
    }

    /** A WHERE condition using raw SQL for a subquery */
    private static class WhereRawSubquery implements WhereNode {
        final String sql;
        final Object[] params;

        WhereRawSubquery(String sql, Object[] params) {
            this.sql = sql;
            this.params = params;
        }
    }

    /** A group of WHERE conditions combined with AND or OR */
    private static class WhereGroup implements WhereNode {
        final GroupType type;
        final List<WhereNode> children = new ArrayList<>();
        WhereGroup parent;

        WhereGroup(GroupType type) {
            this.type = type;
        }
    }

    /** Backward-compatible WhereClause used for HAVING */
    private static class WhereClause {
        final String condition;
        final Object[] params;

        WhereClause(String condition, Object[] params) {
            this.condition = condition;
            this.params = params;
        }
    }

    private static class ExplicitJoin {
        final JoinType joinType;
        final String fromTable;
        final String[] fromColumns;
        final String toTable;
        final String[] toColumns;
        final String alias;

        ExplicitJoin(JoinType joinType, String fromTable, String[] fromColumns,
                     String toTable, String[] toColumns, String alias) {
            this.joinType = joinType;
            this.fromTable = fromTable;
            this.fromColumns = fromColumns;
            this.toTable = toTable;
            this.toColumns = toColumns;
            this.alias = alias;
        }
    }

    private static class JoinClause {
        final JoinType joinType;
        final String table;
        final String alias;
        final String onCondition;

        JoinClause(JoinType joinType, String table, String alias, String onCondition) {
            this.joinType = joinType;
            this.table = table;
            this.alias = alias;
            this.onCondition = onCondition;
        }
    }

    /** Common Table Expression */
    private static class CTE {
        final String name;
        final String rawSQL;
        final Object[] params;
        final QueryBuilder subquery;

        CTE(String name, String rawSQL, Object[] params) {
            this.name = name;
            this.rawSQL = rawSQL;
            this.params = params;
            this.subquery = null;
        }

        CTE(String name, QueryBuilder subquery) {
            this.name = name;
            this.rawSQL = null;
            this.params = null;
            this.subquery = subquery;
        }
    }

    /** UNION or UNION ALL clause */
    private static class UnionClause {
        final boolean all;
        final QueryBuilder query;

        UnionClause(boolean all, QueryBuilder query) {
            this.all = all;
            this.query = query;
        }
    }
}
