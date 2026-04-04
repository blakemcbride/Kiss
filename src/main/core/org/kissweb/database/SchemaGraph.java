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

import java.io.*;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Models a database schema as a graph where tables are nodes and foreign key
 * relationships are edges.  Given a set of tables, it can find the shortest
 * join path connecting them using BFS.
 * <br><br>
 * The graph can be built automatically from JDBC metadata, declared
 * programmatically, or both (hybrid approach).  Both single-column and
 * composite (multi-column) foreign keys are supported.
 * <br><br>
 * The graph can be saved to and loaded from a cache file to avoid re-reading
 * database metadata on every application restart.
 * <br><br>
 * <b>Usage:</b>
 * <pre>
 *     // From live database
 *     SchemaGraph graph = SchemaGraph.fromDatabase(connection);
 *
 *     // Save to cache file
 *     graph.saveToFile("schema-cache.txt");
 *
 *     // Load from cache file on next startup
 *     SchemaGraph graph = SchemaGraph.loadFromFile("schema-cache.txt");
 *
 *     // Or programmatically (single-column FK)
 *     SchemaGraph graph = new SchemaGraph();
 *     graph.addForeignKey("employee", "department_id", "department", "department_id");
 *
 *     // Composite FK
 *     graph.addForeignKey("order_line", new String[]{"order_id", "product_id"},
 *                         "order_product", new String[]{"order_id", "product_id"});
 *
 *     // Or hybrid
 *     SchemaGraph graph = SchemaGraph.fromDatabase(connection);
 *     graph.addForeignKey("audit_log", "user_id", "employee", "employee_id");
 * </pre>
 *
 * @author Blake McBride
 *
 * @see QueryBuilder
 */
public class SchemaGraph {

    /**
     * Represents a foreign key relationship between two tables.
     * Supports both single-column and composite (multi-column) foreign keys.
     */
    public static class Edge {
        private final String fromTable;
        private final List<String> fromColumns;
        private final String toTable;
        private final List<String> toColumns;

        /**
         * Create a new single-column foreign key edge.
         *
         * @param fromTable  the child table containing the FK column
         * @param fromColumn the FK column in the child table
         * @param toTable    the parent/referenced table
         * @param toColumn   the referenced column (usually the PK)
         */
        public Edge(String fromTable, String fromColumn, String toTable, String toColumn) {
            this.fromTable = fromTable.toLowerCase();
            this.fromColumns = Collections.singletonList(fromColumn.toLowerCase());
            this.toTable = toTable.toLowerCase();
            this.toColumns = Collections.singletonList(toColumn.toLowerCase());
        }

        /**
         * Create a new composite (multi-column) foreign key edge.
         *
         * @param fromTable   the child table containing the FK columns
         * @param fromColumns the FK columns in the child table
         * @param toTable     the parent/referenced table
         * @param toColumns   the referenced columns (usually the PK columns)
         */
        public Edge(String fromTable, String[] fromColumns, String toTable, String[] toColumns) {
            if (fromColumns.length != toColumns.length || fromColumns.length == 0)
                throw new IllegalArgumentException("fromColumns and toColumns must have equal non-zero length");
            this.fromTable = fromTable.toLowerCase();
            this.toTable = toTable.toLowerCase();
            List<String> fc = new ArrayList<>(fromColumns.length);
            List<String> tc = new ArrayList<>(toColumns.length);
            for (String c : fromColumns)
                fc.add(c.toLowerCase());
            for (String c : toColumns)
                tc.add(c.toLowerCase());
            this.fromColumns = Collections.unmodifiableList(fc);
            this.toColumns = Collections.unmodifiableList(tc);
        }

        /**
         * Internal constructor from pre-built lists.
         */
        private Edge(String fromTable, List<String> fromColumns, String toTable, List<String> toColumns) {
            this.fromTable = fromTable;
            this.fromColumns = Collections.unmodifiableList(fromColumns);
            this.toTable = toTable;
            this.toColumns = Collections.unmodifiableList(toColumns);
        }

        /**
         * Return the child table name containing the foreign key column(s).
         *
         * @return the child table name
         */
        public String getFromTable() {
            return fromTable;
        }

        /**
         * Get the FK column name.  For composite keys, returns the first column.
         *
         * @return the first (or only) FK column name
         */
        public String getFromColumn() {
            return fromColumns.get(0);
        }

        /**
         * Get all FK column names.  For single-column keys, returns a list of one.
         *
         * @return unmodifiable list of FK column names
         */
        public List<String> getFromColumns() {
            return fromColumns;
        }

        /**
         * Return the parent/referenced table name.
         *
         * @return the parent table name
         */
        public String getToTable() {
            return toTable;
        }

        /**
         * Get the referenced column name.  For composite keys, returns the first column.
         *
         * @return the first (or only) referenced column name
         */
        public String getToColumn() {
            return toColumns.get(0);
        }

        /**
         * Get all referenced column names.  For single-column keys, returns a list of one.
         *
         * @return unmodifiable list of referenced column names
         */
        public List<String> getToColumns() {
            return toColumns;
        }

        /**
         * Return whether this is a composite (multi-column) foreign key.
         *
         * @return true if this edge has more than one column pair
         */
        public boolean isComposite() {
            return fromColumns.size() > 1;
        }

        /**
         * Returns the table on the other side of this edge.
         */
        String otherTable(String table) {
            if (table.equals(fromTable))
                return toTable;
            if (table.equals(toTable))
                return fromTable;
            throw new IllegalArgumentException("Table '" + table + "' is not part of this edge");
        }

        /**
         * Build the ON condition string for this edge, optionally using aliases.
         *
         * @param fromAlias alias for the fromTable, or null to use the table name
         * @param toAlias   alias for the toTable, or null to use the table name
         * @return the ON condition (e.g. "employee.dept_id = department.dept_id"
         *         or "a.col1 = b.col1 AND a.col2 = b.col2" for composite keys)
         */
        public String buildOnCondition(String fromAlias, String toAlias) {
            String fa = fromAlias != null ? fromAlias : fromTable;
            String ta = toAlias != null ? toAlias : toTable;
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < fromColumns.size(); i++) {
                if (i > 0)
                    sb.append(" AND ");
                sb.append(fa).append('.').append(fromColumns.get(i))
                        .append(" = ")
                        .append(ta).append('.').append(toColumns.get(i));
            }
            return sb.toString();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Edge)) return false;
            Edge e = (Edge) o;
            return fromTable.equals(e.fromTable) && fromColumns.equals(e.fromColumns)
                    && toTable.equals(e.toTable) && toColumns.equals(e.toColumns);
        }

        @Override
        public int hashCode() {
            return Objects.hash(fromTable, fromColumns, toTable, toColumns);
        }

        @Override
        public String toString() {
            if (fromColumns.size() == 1)
                return fromTable + "." + fromColumns.get(0) + " -> " + toTable + "." + toColumns.get(0);
            return fromTable + "." + fromColumns + " -> " + toTable + "." + toColumns;
        }
    }

    // table name (lowercase) → list of edges incident on that table
    private final Map<String, List<Edge>> adjacency = new HashMap<>();

    // Cache of schema name → SchemaGraph for the two-argument fromDatabase method
    private static final ConcurrentHashMap<String, SchemaGraph> schemaCache = new ConcurrentHashMap<>();

    /**
     * Create an empty schema graph.  Use {@link #addForeignKey(String, String, String, String)}
     * or {@link #addForeignKey(String, String[], String, String[])} to populate it.
     */
    public SchemaGraph() {
    }

    /**
     * Build a schema graph by reading foreign key metadata from a live
     * JDBC connection.  This queries DatabaseMetaData for all tables and
     * their imported foreign keys, properly grouping composite (multi-column)
     * foreign keys by constraint name.
     * <br><br>
     * Called once at application startup.  The resulting graph is thread-safe
     * for read operations (path finding).
     *
     * @param conn a Kiss database connection
     * @return a populated SchemaGraph
     * @throws SQLException if database metadata cannot be read
     */
    public static SchemaGraph fromDatabase(Connection conn) throws SQLException {
        SchemaGraph graph = new SchemaGraph();
        DatabaseMetaData meta = conn.getSQLConnection().getMetaData();
        String schema = conn.getSQLConnection().getSchema();

        // Get all tables
        try (ResultSet tables = meta.getTables(null, schema, "%", new String[]{"TABLE"})) {
            while (tables.next()) {
                String tableName = tables.getString("TABLE_NAME").toLowerCase();
                graph.addTable(tableName);

                // Get imported (incoming) foreign keys for this table.
                // Composite FKs return multiple rows sharing the same FK_NAME,
                // ordered by KEY_SEQ.
                try (ResultSet fks = meta.getImportedKeys(null, schema, tables.getString("TABLE_NAME"))) {
                    // Group columns by FK constraint name
                    Map<String, List<String[]>> fkGroups = new LinkedHashMap<>();
                    while (fks.next()) {
                        String fkName = fks.getString("FK_NAME");
                        String fkTable = fks.getString("FKTABLE_NAME").toLowerCase();
                        String fkColumn = fks.getString("FKCOLUMN_NAME").toLowerCase();
                        String pkTable = fks.getString("PKTABLE_NAME").toLowerCase();
                        String pkColumn = fks.getString("PKCOLUMN_NAME").toLowerCase();
                        // Use a synthetic key if FK_NAME is null
                        if (fkName == null)
                            fkName = fkTable + "." + fkColumn + "->" + pkTable + "." + pkColumn;
                        fkGroups.computeIfAbsent(fkName, k -> new ArrayList<>())
                                .add(new String[]{fkTable, fkColumn, pkTable, pkColumn});
                    }

                    // Build edges, handling both single and composite FKs
                    for (List<String[]> group : fkGroups.values()) {
                        if (group.size() == 1) {
                            String[] row = group.get(0);
                            graph.addForeignKey(row[0], row[1], row[2], row[3]);
                        } else {
                            String[] first = group.get(0);
                            String fkTable = first[0];
                            String pkTable = first[2];
                            String[] fkCols = new String[group.size()];
                            String[] pkCols = new String[group.size()];
                            for (int i = 0; i < group.size(); i++) {
                                fkCols[i] = group.get(i)[1];
                                pkCols[i] = group.get(i)[3];
                            }
                            graph.addForeignKey(fkTable, fkCols, pkTable, pkCols);
                        }
                    }
                }
            }
        }

        return graph;
    }

    /**
     * Build or retrieve a cached schema graph for the given schema name.
     * On the first call for a given schema, this delegates to
     * {@link #fromDatabase(Connection)} and caches the result.
     * Subsequent calls with the same schema name return the cached instance
     * without reading the database again.
     * <br><br>
     * This method is safe for use by multiple threads.  If two threads
     * call this concurrently with the same schema name, the graph will be
     * built once and the same instance returned to both callers.
     *
     * @param conn   a Kiss database connection
     * @param schema a schema identifier used as the cache key (case-sensitive)
     * @return a populated {@code SchemaGraph}, either newly built or from the cache
     * @throws SQLException if the schema graph needs to be built and database metadata cannot be read
     */
    public static SchemaGraph fromDatabase(Connection conn, String schema) throws SQLException {
        SchemaGraph cached = schemaCache.get(schema);
        if (cached != null)
            return cached;
        synchronized (schemaCache) {
            cached = schemaCache.get(schema);
            if (cached != null)
                return cached;
            SchemaGraph graph = fromDatabase(conn);
            schemaCache.put(schema, graph);
            return graph;
        }
    }

    /**
     * Clear the internal schema cache used by {@link #fromDatabase(Connection, String)}.
     * After calling this method, the next call to {@code fromDatabase(conn, schema)}
     * will re-read the database metadata.
     */
    public static void clearSchemaCache() {
        schemaCache.clear();
    }

    /**
     * Remove a single entry from the internal schema cache.
     *
     * @param schema the schema identifier to remove
     */
    public static void clearSchemaCache(String schema) {
        schemaCache.remove(schema);
    }

    /**
     * Declare a table in the graph.  This is optional — tables are
     * auto-created when referenced by addForeignKey.
     *
     * @param tableName the table name
     */
    public void addTable(String tableName) {
        tableName = tableName.toLowerCase();
        adjacency.computeIfAbsent(tableName, k -> new ArrayList<>());
    }

    /**
     * Declare a single-column foreign key relationship between two tables.
     * Both tables are automatically added to the graph if not already present.
     * Duplicate edges (same tables and columns) are ignored.
     *
     * @param fromTable  the child table containing the FK column
     * @param fromColumn the FK column in the child table
     * @param toTable    the parent/referenced table
     * @param toColumn   the referenced column (usually the PK)
     */
    public void addForeignKey(String fromTable, String fromColumn, String toTable, String toColumn) {
        addEdge(new Edge(fromTable, fromColumn, toTable, toColumn));
    }

    /**
     * Declare a composite (multi-column) foreign key relationship between two tables.
     * Both tables are automatically added to the graph if not already present.
     * Duplicate edges are ignored.
     *
     * @param fromTable   the child table containing the FK columns
     * @param fromColumns the FK columns in the child table
     * @param toTable     the parent/referenced table
     * @param toColumns   the referenced columns (usually the PK columns)
     */
    public void addForeignKey(String fromTable, String[] fromColumns, String toTable, String[] toColumns) {
        addEdge(new Edge(fromTable, fromColumns, toTable, toColumns));
    }

    private void addEdge(Edge edge) {
        List<Edge> fromEdges = adjacency.computeIfAbsent(edge.fromTable, k -> new ArrayList<>());
        if (!fromEdges.contains(edge))
            fromEdges.add(edge);

        // Add the same edge to the other side (undirected graph)
        if (!edge.fromTable.equals(edge.toTable)) {
            List<Edge> toEdges = adjacency.computeIfAbsent(edge.toTable, k -> new ArrayList<>());
            if (!toEdges.contains(edge))
                toEdges.add(edge);
        }
    }

    /**
     * Check whether a table exists in the graph.
     *
     * @param tableName the table name
     * @return true if the table has been added to the graph
     */
    public boolean hasTable(String tableName) {
        return adjacency.containsKey(tableName.toLowerCase());
    }

    /**
     * Return all table names in the graph.
     *
     * @return an unmodifiable set of table names
     */
    public Set<String> getTables() {
        return Collections.unmodifiableSet(adjacency.keySet());
    }

    /**
     * Return all edges incident on a given table.
     *
     * @param tableName the table name
     * @return the list of edges, or an empty list if the table is not in the graph
     */
    public List<Edge> getEdges(String tableName) {
        List<Edge> edges = adjacency.get(tableName.toLowerCase());
        return edges != null ? Collections.unmodifiableList(edges) : Collections.emptyList();
    }

    /**
     * Find the shortest join path connecting all the given tables using BFS.
     * <br><br>
     * The algorithm picks the first table as the root, then greedily adds
     * the shortest BFS path from the current tree to each remaining table.
     * This is a standard approximation for the Steiner tree problem.
     *
     * @param tables    the set of tables that must be connected
     * @param rootTable the preferred root table (used as FROM), or null to use the first table in the set
     * @return an ordered list of edges representing the joins needed
     * @throws SQLException if no path exists between some tables
     */
    public List<Edge> findJoinPath(Set<String> tables, String rootTable) throws SQLException {
        if (tables.isEmpty())
            return Collections.emptyList();

        Set<String> normalizedTables = new LinkedHashSet<>();
        for (String t : tables)
            normalizedTables.add(t.toLowerCase());

        if (normalizedTables.size() == 1)
            return Collections.emptyList();

        // Determine root
        String root;
        if (rootTable != null && normalizedTables.contains(rootTable.toLowerCase()))
            root = rootTable.toLowerCase();
        else
            root = normalizedTables.iterator().next();

        // Tables already connected by the join tree
        Set<String> connected = new LinkedHashSet<>();
        connected.add(root);

        // Edges forming the join tree (ordered)
        List<Edge> joinEdges = new ArrayList<>();

        // Remaining tables to connect
        Set<String> remaining = new LinkedHashSet<>(normalizedTables);
        remaining.remove(root);

        while (!remaining.isEmpty()) {
            // Find the shortest path from any connected table to any remaining table
            PathResult best = null;
            for (String target : remaining) {
                PathResult path = bfsShortestPath(connected, target);
                if (path != null && (best == null || path.edges.size() < best.edges.size()))
                    best = path;
            }

            if (best == null) {
                throw new SQLException("No foreign key path exists connecting tables: "
                        + remaining + " to tables: " + connected);
            }

            // Add all edges and intermediate tables from this path
            for (Edge e : best.edges) {
                if (!joinEdges.contains(e))
                    joinEdges.add(e);
                connected.add(e.fromTable);
                connected.add(e.toTable);
            }

            // Remove newly connected tables from remaining
            remaining.removeAll(connected);
        }

        return joinEdges;
    }

    // ---- Schema Caching --------------------------------------------------------

    private static final String CACHE_HEADER = "# SchemaGraph cache v1";

    /**
     * Save this schema graph to a text file.  The file can later be loaded
     * with {@link #loadFromFile(String)} to avoid re-reading database metadata.
     * <br><br>
     * The file format is a simple line-based text format:
     * <ul>
     *   <li>{@code TABLE tablename} — declares a table</li>
     *   <li>{@code FK fromTable fromCol1,fromCol2 toTable toCol1,toCol2} — declares a foreign key</li>
     * </ul>
     *
     * @param filePath the path to the cache file
     * @throws IOException if the file cannot be written
     */
    public void saveToFile(String filePath) throws IOException {
        // Collect unique edges to avoid writing duplicates
        // (each edge appears in two adjacency lists)
        Set<Edge> writtenEdges = new LinkedHashSet<>();

        try (PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(filePath)))) {
            pw.println(CACHE_HEADER);

            // Write tables (including those with no edges)
            List<String> sortedTables = new ArrayList<>(adjacency.keySet());
            Collections.sort(sortedTables);
            for (String table : sortedTables)
                pw.println("TABLE " + table);

            // Write edges
            for (String table : sortedTables) {
                List<Edge> edges = adjacency.get(table);
                if (edges == null)
                    continue;
                for (Edge e : edges) {
                    if (writtenEdges.add(e)) {
                        pw.print("FK ");
                        pw.print(e.fromTable);
                        pw.print(" ");
                        pw.print(String.join(",", e.fromColumns));
                        pw.print(" ");
                        pw.print(e.toTable);
                        pw.print(" ");
                        pw.println(String.join(",", e.toColumns));
                    }
                }
            }
        }
    }

    /**
     * Load a schema graph from a cache file previously saved with
     * {@link #saveToFile(String)}.
     *
     * @param filePath the path to the cache file
     * @return a populated SchemaGraph
     * @throws IOException if the file cannot be read or has an invalid format
     */
    public static SchemaGraph loadFromFile(String filePath) throws IOException {
        SchemaGraph graph = new SchemaGraph();

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line = br.readLine();
            if (line == null || !line.equals(CACHE_HEADER))
                throw new IOException("Invalid schema cache file: missing header");

            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#"))
                    continue;

                if (line.startsWith("TABLE ")) {
                    String tableName = line.substring(6).trim();
                    graph.addTable(tableName);
                } else if (line.startsWith("FK ")) {
                    String[] parts = line.substring(3).trim().split("\\s+");
                    if (parts.length != 4)
                        throw new IOException("Invalid FK line: " + line);
                    String fromTable = parts[0];
                    String[] fromCols = parts[1].split(",");
                    String toTable = parts[2];
                    String[] toCols = parts[3].split(",");
                    if (fromCols.length == 1 && toCols.length == 1)
                        graph.addForeignKey(fromTable, fromCols[0], toTable, toCols[0]);
                    else
                        graph.addForeignKey(fromTable, fromCols, toTable, toCols);
                } else {
                    throw new IOException("Unrecognized line in schema cache: " + line);
                }
            }
        }

        return graph;
    }

    // ---- Internal helpers -------------------------------------------------------

    /**
     * BFS from any table in the 'sources' set to the given target table.
     * Returns the shortest path (list of edges) or null if unreachable.
     */
    private PathResult bfsShortestPath(Set<String> sources, String target) {
        if (sources.contains(target))
            return new PathResult(Collections.emptyList());

        // BFS state: table → the edge used to reach it
        Map<String, Edge> parentEdge = new HashMap<>();
        // table → the table we came from
        Map<String, String> parentTable = new HashMap<>();
        Queue<String> queue = new LinkedList<>();

        for (String source : sources) {
            queue.add(source);
            parentTable.put(source, null);
        }

        while (!queue.isEmpty()) {
            String current = queue.poll();
            List<Edge> edges = adjacency.get(current);
            if (edges == null)
                continue;

            for (Edge edge : edges) {
                String neighbor = edge.otherTable(current);
                if (!parentTable.containsKey(neighbor)) {
                    parentTable.put(neighbor, current);
                    parentEdge.put(neighbor, edge);
                    if (neighbor.equals(target)) {
                        // Reconstruct path
                        List<Edge> path = new ArrayList<>();
                        String node = target;
                        while (parentEdge.containsKey(node)) {
                            path.add(parentEdge.get(node));
                            node = parentTable.get(node);
                        }
                        Collections.reverse(path);
                        return new PathResult(path);
                    }
                    queue.add(neighbor);
                }
            }
        }

        return null;  // unreachable
    }

    private static class PathResult {
        final List<Edge> edges;

        PathResult(List<Edge> edges) {
            this.edges = edges;
        }
    }
}
