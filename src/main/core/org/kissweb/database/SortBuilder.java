package org.kissweb.database;

import org.kissweb.database.Connection.ConnectionType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Maps client-sent sort keys to fixed SQL <code>ORDER BY</code> expressions so that client text never
 * reaches SQL.
 * <p>
 * An application registers, once (typically in a <code>static</code> initializer), every sort key it
 * accepts together with the SQL expression(s) that produce the displayed value, and optionally the join
 * clause a key needs.  For each request it then calls {@link #build} with the keys the client sent.
 * Unknown or blank keys are ignored (never an exception), and the unknown ones are reported back in
 * {@link Result#rejectedKeys()} so the caller can log them.
 * <p>
 * Rules applied to every registered expression:
 * <ul>
 * <li>every expression gets the requested direction;</li>
 * <li>blank values (SQL <code>NULL</code>) sort <b>last in both directions</b>;</li>
 * <li>a caller-supplied default sort is used when no usable key was sent;</li>
 * <li>caller-supplied tiebreak items are always appended last, for a deterministic order.</li>
 * </ul>
 * <p>
 * <b>Database portability.</b>  Expressions are stored independent of the database and rendered at
 * {@link #build} time for the given {@link ConnectionType}:
 * <table border="1">
 * <caption>Per-database SQL</caption>
 * <tr><th>Database</th><th>{@link #text} blank rule</th><th>nulls last</th></tr>
 * <tr><td>PostgreSQL</td><td><code>lower(nullif(btrim(x), ''))</code></td><td><code>x asc nulls last</code></td></tr>
 * <tr><td>SQLite</td><td><code>lower(nullif(trim(x), ''))</code></td><td><code>x asc nulls last</code></td></tr>
 * <tr><td>Oracle</td><td><code>lower(trim(x))</code> (Oracle treats '' as NULL)</td><td><code>x asc nulls last</code></td></tr>
 * <tr><td>MySQL</td><td><code>lower(nullif(trim(x), ''))</code></td><td><code>case when x is null then 1 else 0 end, x asc</code></td></tr>
 * <tr><td>SQL Server</td><td><code>lower(nullif(ltrim(rtrim(x)), ''))</code></td><td><code>case when x is null then 1 else 0 end, x asc</code></td></tr>
 * </table>
 * Expressions given to {@link #raw} are emitted verbatim; use an {@link Expr} lambda (which receives the
 * database type) when a raw expression must vary by database.  Join clauses and tiebreak items are
 * also emitted verbatim.
 * <p>
 * An instance is meant to be configured once and then only read; {@link #build} is thread-safe after
 * configuration is complete.
 * <p>
 * Example (client sends sort1/asc1, sort2/asc2):
 * <pre>
 * private static final SortBuilder SORT = new SortBuilder()
 *     .key("name",     SortBuilder.text("og.group_name"))
 *     .key("salesRep", SortBuilder.text("p.lname"), SortBuilder.text("p.fname"))
 *     .key("lastDate", SortBuilder.zeroBlank("pr.last_contact_date"))
 *     .key("status",   SortBuilder.raw("st.seqno"))
 *     .key("lastMail", SortBuilder.raw("le.date_sent"))
 *     .join(LAST_MAIL_JOIN, "lastMail");
 *
 * SortBuilder.Result r = SORT.build(db.getDBType(),
 *     Arrays.asList(SortBuilder.Sort.of(sort1, asc1), SortBuilder.Sort.of(sort2, asc2)),
 *     SortBuilder.Sort.of("name", true),
 *     "lower(og.group_name) asc", "pr.org_group_id asc");
 * String sql = "select ... from ... " + r.joins() + " where ... " + r.orderBy();
 * for (String bad : r.rejectedKeys()) logger.warn("Rejected sort key: " + bad.replaceAll("[\\r\\n]", " "));
 * </pre>
 * The client side of this is {@code AGGrid.setServerSort} / {@code setSortIndicators} (see the AGGrid
 * "server-side sorting" section of the Kiss documentation).
 */
public class SortBuilder {

    /**
     * An SQL sort expression that is rendered for a particular database.
     */
    @FunctionalInterface
    public interface Expr {
        /**
         * @param type the database the SQL will run on
         * @return the SQL expression; it must yield NULL for a blank value
         */
        String sql(ConnectionType type);
    }

    /**
     * One requested sort: a client-sent key and a direction.
     */
    public static final class Sort {
        private final String key;
        private final boolean asc;

        private Sort(String key, boolean asc) {
            this.key = key;
            this.asc = asc;
        }

        /**
         * @param key the client-sent key (may be null or blank, in which case it is ignored)
         * @param asc true for ascending, false for descending
         * @return the sort request
         */
        public static Sort of(String key, boolean asc) {
            return new Sort(key, asc);
        }

        /** @return the key as supplied */
        public String getKey() {
            return key;
        }

        /** @return true if ascending */
        public boolean isAsc() {
            return asc;
        }
    }

    /**
     * The result of {@link #build}.
     */
    public static final class Result {
        private final String orderBy;
        private final List<String> joins;
        private final List<String> rejected;

        private Result(String orderBy, List<String> joins, List<String> rejected) {
            this.orderBy = orderBy;
            this.joins = Collections.unmodifiableList(joins);
            this.rejected = Collections.unmodifiableList(rejected);
        }

        /**
         * @return the complete clause, e.g. <code>order by a asc nulls last, b asc</code>, with no
         *         leading or trailing space; empty if there is nothing to sort on (no usable key, no
         *         default, no tiebreak)
         */
        public String orderBy() {
            return orderBy;
        }

        /**
         * @return the join clauses needed by the chosen keys, each once, in first-use order, separated by
         *         a single space and followed by a trailing space (empty string if none)
         */
        public String joins() {
            final StringBuilder sb = new StringBuilder();
            for (String j : joins)
                sb.append(j).append(' ');
            return sb.toString();
        }

        /** @return the join clauses needed, each once, in first-use order */
        public List<String> joinList() {
            return joins;
        }

        /**
         * The non-blank keys that were not registered (trimmed, each once, in request order).  This is
         * client text: sanitize it (e.g. strip line breaks) before logging.
         *
         * @return the rejected keys; empty if none
         */
        public List<String> rejectedKeys() {
            return rejected;
        }
    }

    private final Map<String, List<Expr>> keys = new LinkedHashMap<>();
    private final Map<String, List<String>> keyJoins = new LinkedHashMap<>();

    /**
     * Text sort value: case-insensitive; empty or whitespace-only counts as blank (NULL).
     *
     * @param column the column or SQL expression
     * @return the expression
     */
    public static Expr text(final String column) {
        return type -> textSql(type, column);
    }

    /**
     * Number or int-date sort value: 0 counts as blank (NULL).
     *
     * @param column the column or SQL expression
     * @return the expression
     */
    public static Expr zeroBlank(final String column) {
        return type -> "nullif(" + column + ", 0)";
    }

    /**
     * A raw expression, emitted verbatim.  It must yield NULL for a blank value.
     *
     * @param sql the SQL expression (a fixed program constant, never client text)
     * @return the expression
     */
    public static Expr raw(final String sql) {
        return type -> sql;
    }

    /**
     * The SQL for the {@link #text} blank rule on the given database.  Useful inside an {@link Expr}
     * lambda that composes a larger expression.
     *
     * @param type   the database
     * @param column the column or SQL expression
     * @return the SQL
     */
    public static String textSql(final ConnectionType type, final String column) {
        switch (type) {
            case PostgreSQL:
                return "lower(nullif(btrim(" + column + "), ''))";
            case MicrosoftServer:
                return "lower(nullif(ltrim(rtrim(" + column + ")), ''))";
            case Oracle:
                // Oracle treats '' as NULL, and trim of a whitespace-only string yields NULL
                return "lower(trim(" + column + "))";
            default:  // MySQL, SQLite
                return "lower(nullif(trim(" + column + "), ''))";
        }
    }

    /**
     * Registers a sort key.  Re-registering a key replaces it.
     *
     * @param key   the key the client sends (non-blank)
     * @param exprs one or more expressions (a multi-part sort, e.g. last name then first name); each
     *              gets the requested direction
     * @return this
     */
    public SortBuilder key(final String key, final Expr... exprs) {
        if (key == null || key.trim().isEmpty() || exprs == null || exprs.length == 0)
            throw new IllegalArgumentException("key and at least one expression are required");
        final List<Expr> list = new ArrayList<>();
        for (Expr e : exprs) {
            if (e == null)
                throw new IllegalArgumentException("null expression for key " + key);
            list.add(e);
        }
        keys.put(key, list);
        return this;
    }

    /**
     * Declares a join clause (verbatim SQL, e.g. a lateral join) that the given keys need.  It is
     * emitted, once, only when at least one of those keys is used.  Keys must already be registered.
     *
     * @param joinSql the join clause
     * @param forKeys the keys that need it
     * @return this
     */
    public SortBuilder join(final String joinSql, final String... forKeys) {
        for (String k : forKeys) {
            if (!keys.containsKey(k))
                throw new IllegalArgumentException("join references unregistered key " + k);
            keyJoins.computeIfAbsent(k, x -> new ArrayList<>()).add(joinSql);
        }
        return this;
    }

    /**
     * Builds the ORDER BY clause and the joins it needs.
     *
     * @param type        the database the SQL will run on
     * @param sorts       the requested sorts in priority order; blank keys are ignored silently, unknown
     *                    keys are ignored and reported in {@link Result#rejectedKeys()}; may be null
     * @param defaultSort used when no usable key was requested (must be a registered key); may be null
     * @param tiebreak    complete ORDER BY items, emitted verbatim after the keys (e.g.
     *                    <code>"pr.org_group_id asc"</code>)
     * @return the result
     * @throws IllegalArgumentException if defaultSort names an unregistered key (a programming error)
     */
    public Result build(final ConnectionType type, final List<Sort> sorts, final Sort defaultSort, final String... tiebreak) {
        final List<String> items = new ArrayList<>();
        final Set<String> joins = new LinkedHashSet<>();
        final List<String> rejected = new ArrayList<>();
        boolean used = false;
        if (sorts != null)
            for (Sort s : sorts) {
                if (s == null || s.key == null || s.key.trim().isEmpty())
                    continue;
                final String k = s.key.trim();
                if (!keys.containsKey(k)) {
                    if (!rejected.contains(k))
                        rejected.add(k);
                    continue;
                }
                add(items, joins, type, k, s.asc);
                used = true;
            }
        if (!used && defaultSort != null) {
            final String k = defaultSort.key == null ? "" : defaultSort.key.trim();
            if (!keys.containsKey(k))
                throw new IllegalArgumentException("default sort key is not registered: " + defaultSort.key);
            add(items, joins, type, k, defaultSort.asc);
        }
        if (tiebreak != null)
            for (String t : tiebreak)
                if (t != null && !t.trim().isEmpty())
                    items.add(t);
        final String orderBy = items.isEmpty() ? "" : "order by " + String.join(", ", items);
        return new Result(orderBy, new ArrayList<>(joins), rejected);
    }

    private void add(final List<String> items, final Set<String> joins, final ConnectionType type, final String key, final boolean asc) {
        final String dir = asc ? " asc" : " desc";
        final boolean nativeNullsLast = type == ConnectionType.PostgreSQL || type == ConnectionType.Oracle
                || type == ConnectionType.SQLite;
        for (Expr e : keys.get(key)) {
            final String x = e.sql(type);
            if (nativeNullsLast)
                items.add(x + dir + " nulls last");
            else {
                items.add("case when " + x + " is null then 1 else 0 end");
                items.add(x + dir);
            }
        }
        final List<String> j = keyJoins.get(key);
        if (j != null)
            joins.addAll(j);
    }
}
