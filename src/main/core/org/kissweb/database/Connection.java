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
import org.kissweb.json.JSONObject;
import org.kissweb.DateUtils;

import java.io.IOException;
import java.sql.*;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

/**
 *  This class represents a connection to an SQL database.
 * <br><br>
 *  Typically, one connection would be used for each thread in an application.  Operations on a connection
 *  are separate or isolated from all the other connections.
 *
 * @author Blake McBride
 *
 * @see Command
 * @see Cursor
 * @see Record
 *
 */
public class Connection implements AutoCloseable {

    /** Enumeration of supported database types */
    public enum ConnectionType {
        /** PostgreSQL database */
        PostgreSQL, 
        /** Microsoft SQL Server database */
        MicrosoftServer, 
        /** MySQL database */
        MySQL, 
        /** Oracle database */
        Oracle, 
        /** SQLite database */
        SQLite
    }

    /** Cache of table names to auto-increment primary key column names */
    private final ConcurrentHashMap<String, String> primaryColName = new ConcurrentHashMap<>();
    /** Cache of table names to primary key column names */
    private final ConcurrentHashMap<String, List<String>> primaryColumns = new ConcurrentHashMap<>();
    /** Cache of table existence status */
    private final HashMap<String, Boolean> TableExistenceCache = new HashMap<>();
    /** Flag indicating if this connection was created externally */
    private boolean externalConnection = false;

    /** A function that gets called whenever a record is deleted */
    BiConsumer<String,Object> deleteCallback = null;

    /** The underlying JDBC connection */
    java.sql.Connection conn;
    /** Database metadata from the connection */
    DatabaseMetaData dmd;
    /** The type of database connection */
    private ConnectionType ctype;
    /** Cache of table and column information */
    private final HashMap<String,HashMap<String,ColumnInfo>> columnInfo = new HashMap<>();

    /**
     * Create a Connection out of a pre-opened JDBC connection.
     * <br><br>
     * If a new instance of this class is created with this method, the JDBC connection passed in will not be closed
     * when this instance is closed.  Thus, if the connection was externally formed, it must be externally released.
     *
     * @param db the pre-opened JDBC connection to wrap
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public Connection(java.sql.Connection db) {
        this.conn = db;
        externalConnection = true;
        try {
            dmd = conn.getMetaData();
            switch (dmd.getDatabaseProductName()) {
                case "PostgreSQL":
                    ctype = ConnectionType.PostgreSQL;
                    break;
                case "Microsoft SQL Server":
                    ctype = ConnectionType.MicrosoftServer;
                    break;
                case "Oracle":
                    ctype = ConnectionType.Oracle;
                    break;
                case "MySQL":
                    ctype = ConnectionType.MySQL;
                    break;
                case "SQLite":
                    ctype = ConnectionType.SQLite;
                    break;
            }
        } catch (SQLException ignored) {
        }
    }

    /**
     * Create a connection string appropriate for the indicated database type.  This method is only used in special situations.
     *
     * @param type the database type
     * @param host (can use null for localhost)
     * @param port (use null for default)
     * @param dbname the database name
     * @param user (use null for integrated security)
     * @param pw the password
     * @return the formatted connection string
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public static String makeConnectionString(ConnectionType type, String host, Integer port, String dbname, String user, String pw) {
        String connectionString;

        if (host == null)
            host = "localhost";
        if (type == ConnectionType.PostgreSQL) {
            if (port == null)
                port = 5432;
            connectionString = "jdbc:postgresql://" + host + ":" + port + "/" + dbname + "?user=" + user + "&password=" + pw;
        } else if (type == ConnectionType.MicrosoftServer) {
            if (port == null)
                port = 1433;
            connectionString = "jdbc:sqlserver://" + host + ":" + port + ";databaseName=" + dbname + ";";
            if (user != null && !user.isEmpty())
                connectionString += "user=" + user + ";password=" + pw + ";";
            else
                connectionString += "integratedSecurity=true;";
        } else if (type == ConnectionType.MySQL) {
            if (port == null)
                port = 3306;
            connectionString = "jdbc:mysql://" + host + ":" + port + "/" + dbname + "?user=" + user + "&password=" + pw;
        } else if (type == ConnectionType.SQLite) {
            connectionString = "jdbc:sqlite:" + dbname;
            if (pw != null && !pw.isEmpty())
                connectionString += ";Password=" + pw + ";";
        } else if (type == ConnectionType.Oracle) {
            if (port == null)
                port = 1521;
            connectionString = "jdbc:oracle:thin:" + user + "/" + pw + "@//" + host + ":" + port + "/" + dbname;
        } else
            throw new UnsupportedOperationException();
        return connectionString;
    }

    /**
     * Return the name of the driver used for the specified database type.  This method is only used in special circumstances.
     *
     * @param type the database type
     * @return the JDBC driver class name for the specified database type
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public static String getDriverName(ConnectionType type) {
        String driver;
        if (type == ConnectionType.PostgreSQL)
            driver = "org.postgresql.Driver";
        else if (type == ConnectionType.MicrosoftServer)
            driver = "com.microsoft.sqlserver.jdbc.SQLServerDriver";
        else if (type == ConnectionType.MySQL)
            driver = "com.mysql.jdbc.Driver";
        else if (type == ConnectionType.SQLite)
            driver = "org.sqlite.JDBC";
        else if (type == ConnectionType.Oracle)
            driver = "oracle.jdbc.driver.OracleDriver";
        else
            throw new UnsupportedOperationException();
        return driver;
    }

    /**
     * Set the connection to be read-only.  This may be useful to prevent operations
     * from being committed to the database, but it is not a substitute for proper
     * database permissions.
     *
     * @throws SQLException if a database error occurs
     */
    public void setReadOnly() throws SQLException {
        conn.setReadOnly(true);
    }

    /**
     * Form a new connection to an SQL database.  This method is only used in special situations.
     * <br><br>
     * Auto commits are turned off thus always requiring commit() to complete a transaction.
     * However, KISS explicitly calls commit at the end of each web service (if it succeeds, and a rollback otherwise).
     *
     * @param type the database type
     * @param connectionString the JDBC connection string
     * @throws ClassNotFoundException if the JDBC driver class cannot be found
     * @throws SQLException if a database access error occurs
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public Connection(ConnectionType type, String connectionString) throws ClassNotFoundException, SQLException {
        String driver = getDriverName(type);
        Class.forName(driver);
        ctype = type;
        conn = DriverManager.getConnection(connectionString);
        conn.setAutoCommit(false);  // commits are always necessary and we can batch our reads
        dmd = conn.getMetaData();
    }

    /**
     * This is the main method of forming a new database connection.
     *
     * @param type the database type
     * @param host (null for localhost)
     * @param port (null for default)
     * @param dbname the database name
     * @param user the username
     * @param pw the password
     * @throws SQLException if a database access error occurs
     * @throws ClassNotFoundException if the JDBC driver class cannot be found
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public Connection(ConnectionType type, String host, Integer port, String dbname, String user, String pw) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, port, dbname, user, pw));
        ctype = type;
        dmd = conn.getMetaData();
    }

    /**
     * This is the main method of forming a new database connection when Windows authentication is used.
     *
     * @param type the database type
     * @param host (null for localhost)
     * @param port (null for default)
     * @param dbname the database name
     * @throws SQLException if a database access error occurs
     * @throws ClassNotFoundException if the JDBC driver class cannot be found
     *
     * @see Connection(ConnectionType, String, Integer, String, String, String)
     */
    public Connection(ConnectionType type, String host, Integer port, String dbname) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, port, dbname, null, null));
        ctype = type;
        dmd = conn.getMetaData();
    }

    /**
     * This method closes a connection.  It rarely needs to be called explicitly because it gets called
     * automatically when using the Java try-with-resource statement.
     *
     * @throws SQLException if a database access error occurs
     */
    @Override
    public void close() throws SQLException {
        if (conn != null) {
            if (!externalConnection)
                conn.close();
            conn = null;
        }
    }

    /**
     * Test to see if the database is open.
     *
     * @return true if connection is open and false otherwise
     */
    public boolean isOpen() {
        try {
            return conn != null  &&  !conn.isClosed();
        } catch (SQLException e) {
            return false;
        }
    }

    /**
     * Commit all the operations to the database since the last commit().
     * <br><br>
     * Updates to a database do not take effect until they are committed.  This method performs the commit.
     * When a commit occurs, all database changes done since the last commit are effectively written to the database.
     * If a commit does not occur, the updates will not occur.
     * <br><br>
     * Note that the KISS system does a commit at the end of each web service if the service completes.  However, if the
     * service fails (throws an exception) KISS does a rollback instead.
     *
     * @throws SQLException if a database access error occurs
     *
     * @see #rollback
     */
    public void commit() throws SQLException {
        conn.commit();
    }

    /**
     * Rollback, erase, or forget all the operations since the last commit.
     *
     * @throws SQLException if a database access error occurs
     *
     * @see #commit
     */
    public void rollback() throws SQLException {
        conn.rollback();
    }

    /**
     * Execute an SQL statement provided in a string.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This is a convenience method and mainly useful in isolated situations where there aren't other SQL operations
     * within the same connection occurring.  Remember, each REST service has its own connection.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param sql the SQL statement to execute
     * @param args the parameter values for the SQL statement
     * @throws SQLException if a database access error occurs
     *
     * @see Command#execute(String, Object...)
     */
    public void execute(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            cmd.execute(sql, args);
        }
    }

    /**
     * Executes the given SQL statement immediately (outside any transaction).
     *
     * @param  sql the SQL statement to be executed
     * @throws SQLException if a database access error occurs
     */
    public void executeImmediate(String sql) throws SQLException {
        try (Statement stmt = conn.createStatement()) {
            conn.setAutoCommit(true);
            stmt.executeUpdate(sql);
            conn.setAutoCommit(false);
        }
    }

    /**
     * This is the main way of creating a new Command instance.
     *
     * @return a new Command instance for this connection
     */
    public Command newCommand() {
        try {
            return new Command(this);
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Read in the first record and then close it.
     * The record can be updated or deleted if it was a single-table select and
     * the primary key was selected.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This is a convenience method and mainly useful in isolated situations where there aren't other SQL operations
     * within the same connection occurring.  Remember, each REST service has its own connection.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>Array</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return the Record or null if none
     * @throws Exception if a database access error occurs
     *
     * @see Command#fetchOne(String, Object...)
     */
    public Record fetchOne(String sql, Object... args) throws Exception {
        try (Command cmd = newCommand()) {
            return cmd.fetchOne(sql, args);
        }
    }

    /**
     * Same as <code>fetchOne</code> except returns a JSON object.
     *
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return the JSON object or <code>null</code> if none
     * @throws Exception if a database access error occurs
     *
     * @see #fetchOne(String, Object...)
     */
    public JSONObject fetchOneJSON(String sql, Object... args) throws Exception {
        Record r = fetchOne(sql, args);
        return r != null ? r.toJSON() : null;
    }

    /**
     * Same as <code>fetchOne</code> except adds the columns to an existing JSON object passed in.
     *
     * @param obj the JSON object to add columns to
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return the JSON object passed in
     * @throws Exception if a database access error occurs
     *
     * @see #fetchOne(String, Object...)
     */
    public JSONObject fetchOneJSON(JSONObject obj, String sql, Object... args) throws Exception {
        Record r = fetchOne(sql, args);
        return r != null ? r.addToJSON(obj) : obj;
    }

    /**
     * Fetch all the records and close it.
     * The records can be updated or deleted if there was a single-table select and
     * the primary key was selected.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This is a convenience method and mainly useful in isolated situations where there aren't other SQL operations
     * within the same connection occurring.  Remember, each REST service has its own connection.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>Array</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * If no records are found, an empty list is returned.
     * <br><br>
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a list of records matching the query
     * @throws Exception if a database access error occurs
     *
     * @see Command#fetchAll(String, Object...)
     * @see #fetchAll(int, String, Object...)
     * @see #fetchAll(int, int, String, Object...)
     */
    public List<Record> fetchAll(String sql, Object... args) throws Exception {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(sql, args);
        }
    }

    /**
     * Same as <code>fetchAll</code> except returns a JSON array.
     *
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a JSON array of records
     * @throws Exception if a database access error occurs
     *
     * @see #fetchAll(String, Object...)
     */
    public JSONArray fetchAllJSON(String sql, Object... args) throws Exception {
        return Record.toJSONArray(fetchAll(sql, args));
    }

    /**
     * Fetch all (but no more than max) of the records and close it.
     * The records can be updated or deleted if there was a single-table select and
     * the primary key was selected.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This is a convenience method and mainly useful in isolated situations where there aren't other SQL operations
     * within the same connection occurring.  Remember, each REST service has its own connection.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>Array</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * If no records are found, an empty list is returned.
     * <br><br>
     * @param max the maximum number of records to return
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a list of records matching the query
     * @throws Exception if a database access error occurs
     *
     * @see Command#fetchAll(String, Object...)
     * @see #fetchAll(int, int, String, Object...)
     * @see #fetchAll(String, Object...)
     */
    public List<Record> fetchAll(int max, String sql, Object... args) throws Exception {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(max, sql, args);
        }
    }

    /**
     * Same as <code>fetchAll</code> except returns a JSON array.
     *
     * @param max the maximum number of records to return
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a JSON array of records
     * @throws Exception if a database access error occurs
     *
     * @see #fetchAll(int, String, Object...)
     */
    public JSONArray fetchAllJSON(int max, String sql, Object... args) throws Exception {
        return Record.toJSONArray(fetchAll(max, sql, args));
    }

    /**
     * Group the result set into groups of records the size of <code>max</code> records in each group.
     * Retrieve the group indicated by <code>page</code> (starting at zero) and then close it.
     * The records can be updated or deleted if there was a single-table select and
     * the primary key was selected.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This is a convenience method and mainly useful in isolated situations where there aren't other SQL operations
     * within the same connection occurring.  Remember, each REST service has its own connection.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>Array</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * If no records are found, an empty list is returned.
     * <br><br>
     * @param page starts at zero
     * @param max the maximum number of records per page
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a list of records for the specified page
     * @throws Exception if a database access error occurs
     *
     * @see Command#fetchAll(int, int, String, Object...)
     * @see #fetchAll(int, String, Object...)
     * @see #fetchCount(String, Object...)
     */
    public List<Record> fetchAll(int page, int max, String sql, Object... args) throws Exception {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(page, max, sql, args);
        }
    }

    /**
     * Same as <code>fetchAll</code> except returns a JSON array.
     *
     * @param page starts at zero
     * @param max the maximum number of records per page
     * @param sql the SQL query to execute
     * @param args the parameter values for the SQL statement
     * @return a JSON array of records for the specified page
     * @throws Exception if a database access error occurs
     *
     * @see #fetchAll(int, int, String, Object...)
     */
    public JSONArray fetchAllJSON(int page, int max, String sql, Object... args) throws Exception {
        return Record.toJSONArray(fetchAll(page, max, sql, args));
    }

    /**
     * Returns <code>true</code> if there are any records matching the given SQL statement and <code>false</code> otherwise.
     *
     * @param sql the SQL query to test for existence
     * @param args the parameter values for the SQL statement
     * @return true if any records exist, false otherwise
     * @throws Exception if a database access error occurs
     */
    public boolean exists(String sql, Object... args) throws Exception {
        Record r = fetchOne("select exists (" + sql + ")", args);
        return (Boolean) r.get("exists");
    }

    /**
     * This method returns the total number of records that would be returned with a given select
     * in an efficient manner.  It is very useful when using the paging facility.
     * <br><br>
     * On the other hand, this method executes a costly SQL query so should be used only when necessary.
     * This would mainly be in conjunction with paging.
     * 
     * @param sql the SQL query to count
     * @param args the parameter values for the SQL statement
     * @return the total number of records that would be returned
     * @throws Exception if a database access error occurs
     * @see #fetchAll(int, int, String, Object...) 
     */
    public long fetchCount(String sql, Object ... args) throws Exception {
        try (Command cmd = newCommand()) {
            sql = "select count(*) from (" + sql + ") as tmp123";
            /*
            String lsql = sql.toLowerCase();
            int b = lsql.indexOf(" from ");
            int e = lsql.lastIndexOf(" order by ");
            sql = "select count(*)" + sql.substring(b, e);
             */
            Record rec = cmd.fetchOne(sql, args);
            return rec.getLong("count");
        }
    }

    /**
     * Execute a select statement returning a Kiss Cursor (not a database cursor) that may be used to
     * obtain each subsequent row.  This is useful when a large number of records
     * is possible and fetching all into memory at one time is unneeded.  This can
     * save a significant amount of memory since only one record is in memory at a time.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param sql the sql statement with ? parameters
     * @param args the parameter values
     * @return a Cursor for iterating through the results
     * @throws SQLException if a database access error occurs
     * @throws IOException if an I/O error occurs
     *
     * @see Cursor
     * @see #fetchAll(String, Object...)
     * @see #fetchOne(String, Object...)
     * @see #query(int, String, Object...)
     */
    public Cursor query(String sql, Object ... args) throws SQLException, IOException {
        try (Command cmd = newCommand()) {
            return cmd.query(false, 0, 0, sql, args);
        }
    }

    /**
     * Execute a select statement returning a Kiss Cursor (not a database cursor) that may be used to
     * obtain each subsequent row.
     * <br><br>
     * The maximum number of records returned is given by <code>max</code>.
     * This is useful when a large number of records
     * is possible and fetching all into memory at one time is unneeded.  This can
     * save a significant amount of memory since only one record is in memory at a time.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param max the maximum number of records to return
     * @param sql the sql statement with ? parameters
     * @param args the parameter values
     * @return a Cursor for iterating through the results
     * @throws SQLException if a database access error occurs
     * @throws IOException if an I/O error occurs
     *
     * @see Cursor
     * @see #fetchAll(String, Object...)
     * @see #fetchOne(String, Object...)
     * @see #query(String, Object...)
     */
    public Cursor query(int max, String sql, Object ... args) throws SQLException, IOException {
        try (Command cmd = newCommand()) {
            return cmd.query(false, 0, max, sql, args);
        }
    }

    /**
     * Execute a select statement returning a Kiss Cursor (not a database cursor) that may be used to
     * obtain each subsequent row.  This version is used for paging results.
     * The total result set is broken down into <code>max</code> sized pages (starting at zero)
     * You can then choose which group of <code>max</code> records wanted.
     * <br><br>
     * The <code>page</code> parameter selects the desired page of results (starting at zero).
     * <br><br>
     * The maximum number of records returned is given by <code>max</code>.
     * This is useful when a large number of records
     * is possible and fetching all into memory at one time is unneeded.  This can
     * save a significant amount of memory since only one record is in memory at a time.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     * <br><br>
     * This method normally takes a variable argument list representing the consecutive parameters.
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param page starting at zero
     * @param max the maximum number of records per page
     * @param sql the sql statement with ? parameters
     * @param args the parameter values
     * @return a Cursor for iterating through the results
     * @throws SQLException if a database access error occurs
     * @throws IOException if an I/O error occurs
     *
     * @see Cursor
     * @see #fetchAll(String, Object...)
     * @see #fetchOne(String, Object...)
     * @see #query(String, Object...)
     */
    public Cursor query(int page, int max, String sql, Object ... args) throws SQLException, IOException {
        try (Command cmd = newCommand()) {
            return cmd.query(false, page, max, sql, args);
        }
    }

    /**
     * Return the name of the column that is the table's primary key.  Throws an exception of
     * the table has a composite primary key.
     *
     * @param table the table name
     * @return the primary key column name
     * @throws SQLException if a database access error occurs or table has a composite primary key
     *
     * @see #getPrimaryColumns(String table)
     */
    public String getPrimaryColumnName(String table) throws SQLException {
        String colname = primaryColName.get(table);
        if (colname == null) {
            try (ResultSet r = dmd.getPrimaryKeys(null, null, table)) {
                if (!r.next())
                    throw new SQLException("No primary column");
                colname = r.getString(4);
                if (r.next())
                    throw new SQLException("Primary column is composit");
            }
            primaryColName.put(table, colname);
        }
        return colname;
    }

    /**
     * Returns a list of column names that make up the primary key.
     *
     * @param table the table name
     * @return a list of primary key column names
     * @throws SQLException if a database access error occurs
     *
     * @see #getPrimaryColumnName(String)
     */
    public List<String> getPrimaryColumns(String table) throws SQLException {
        List<String> colnames = primaryColumns.get(table);
        if (colnames == null) {
            try (ResultSet r = dmd.getPrimaryKeys(null, null, table)) {
                if (!r.next())
                    throw new SQLException("No primary column");
                colnames = new ArrayList<String>();
                colnames.add(r.getString(4));
                while (r.next())
                    colnames.add(r.getString(4));
            }
            primaryColumns.put(table, colnames);
        }
        return colnames;
    }

    /**
     * This is the primary method of creating a new row in a table.  First, the new row would be created with this method.
     * Then the columns would be filled with the methods in the Record class.  Finally, the addRecord() method would be called
     * to perform the operation.
     *
     * @param table the table name
     * @return a new Record instance for the specified table
     *
     * @see Record#set(String, Object)
     * @see Record#addRecord()
     * @see Record#addRecordAutoInc()
     */
    public Record newRecord(String table) {
        return new Record(this, table);
    }

    /**
     * Tests to see if a specified table exists.
     * Returns true if it does and false otherwise.
     *
     * @param table the table name to test
     * @return true if the table exists, false otherwise
     */
    public boolean tableExists(String table) {
        String schema = null;

        table = table.replaceAll("\\[", "");
        table = table.replaceAll("]", "");
        if (table.indexOf('.') >= 0) {
            String[] parts = table.split(".");
            schema = parts[parts.length - 2];
            table = parts[parts.length - 1];
        }

        if (TableExistenceCache.containsKey(table))
            return TableExistenceCache.get(table);
        boolean res = false;
        try (Command cmd = newCommand()) {
            if (schema == null)
                schema = "%";
            try (Cursor cursor = cmd.query("select count(*) from information_schema.tables where table_schema like ? and table_name = ?", schema, table)) {
                if (cursor.isNext()) {
                    Record rec = cursor.getRecord();
                    res = rec.getLong("count(*)") != 0;
                }
            } catch (Exception e) {
                return false;
            }
        }
        TableExistenceCache.put(table, res);
        return res;
    }

    /**
     * Manly useful in CHAR or VARCHAR columns, this method returns the maximum size of the column.
     *
     * @param table the table name
     * @param cname the column name
     * @return the maximum size of the column
     * @throws SQLException if a database access error occurs
     */
    public int getColumnSize(String table, String cname) throws SQLException {
        int size;
        try (ResultSet columns = dmd.getColumns(null, null, table, cname)) {
            if (columns.next()) {
                String s = columns.getString("COLUMN_SIZE");
                size = Integer.parseInt(s);
            } else
                size = -1;
        }
        return size;
    }

    /**
     * Return the underlying java.sql.Connection associated with this Connection.
     *
     * @return the underlying JDBC Connection object
     */
    public java.sql.Connection getSQLConnection() {
        return conn;
    }

    /**
     * Modifies an SQL statement to limit the number of rows returned.
     * This is needed since different databases do it differently.
     *
     * A max less than 1 means all records (no max).
     *
     * This method assumes that the SQL statement is not ending in any closing character (like ";").
     *
     * @param max the limit of number of records to return
     * @param sql the SQL statement to be modified
     * @return the modified SQL statement
     */
    String limit(int max, String sql) {
        if (max < 1)  //  all
            return sql;
        switch (ctype) {
            case PostgreSQL:
            case MySQL:
            case SQLite:
                return sql + " limit " + max;
            case MicrosoftServer:
                return "select top " + max + sql.trim().substring(6);
            case Oracle:
                return sql + " offset 0 rows fetch next " + max + " rows only";
            default:
                return sql;
        }
    }

    /**
     * Read a single page worth of records.  Useful for screens that page.
     *
     * @param pageNumber starting at zero
     * @param maxRecords number of records in each page
     * @param sql the SQL statement to modify
     * @return the modified SQL statement with paging
     */
    String page(int pageNumber, int maxRecords, String sql) {
        if (pageNumber == 0)
            return limit(maxRecords, sql);
        if (maxRecords < 1)  //  all
            return sql;
        switch (ctype) {
            case PostgreSQL:
            case MySQL:
            case SQLite:
                return sql + " limit " + maxRecords + " offset " + (pageNumber * maxRecords);
            case MicrosoftServer:
            case Oracle:
                return sql + " offset " + (pageNumber * maxRecords) + " rows fetch next " + maxRecords + " rows only";
            default:
                return sql;
        }
    }

    /**
     * Returns the database type.
     *
     * @return the ConnectionType enum value for this connection
     */
    public ConnectionType getDBType() {
        return ctype;
    }

    /**
     * Utility method to convert a Java Date into an SQL Timestamp.
     *
     * @param dt the Java Date to convert
     * @return the SQL Timestamp or null if input is null
     */
    public static java.sql.Timestamp toTimestamp(Date dt) {
        return dt == null ? null : new java.sql.Timestamp(dt.getTime());
    }

    /**
     * Utility method to convert an integer date into an SQL Timestamp.
     *
     * @param dt YYYYMMDD
     * @return the SQL Timestamp or null if input is 0
     */
    public static java.sql.Timestamp toTimestamp(int dt) {
        return dt == 0 ? null : new java.sql.Timestamp(DateUtils.toDate(dt).getTime());
    }

    /**
     * Utility method to convert a Java Date into an SQL Date.
     *
     * @param dt the Java Date to convert
     * @return the SQL Date or null if input is null
     */
    public static java.sql.Date toDate(Date dt) {
        return dt == null ? null : new java.sql.Date(dt.getTime());
    }

    /**
     * Utility method to convert an integer date into an SQL Date.
     *
     * @param dt YYYYMMDD
     * @return the SQL Date or null if input is 0
     */
    public static java.sql.Date toDate(int dt) {
        return dt == 0 ? null : new java.sql.Date(DateUtils.toDate(dt).getTime());
    }

    /**
     * Sets the connection-wide delete callback method.  This callback
     * gets called whenever <code>Record.delete()</code> is called.
     *
     * @param  deleteCallback  the callback function to be set
     * @return                 the previous delete callback
     */
    public BiConsumer<String,Object> setDeleteCallback(BiConsumer<String,Object> deleteCallback) {
        BiConsumer<String,Object> oldCallback = this.deleteCallback;
        this.deleteCallback = deleteCallback;
        return oldCallback;
    }

    /**
     * Set the default schema for the connection.  This is the schema that
     * will be used if no schema is specified in a query.
     *
     * @param  schema  the schema to set
     * @return         the previous schema
     * @throws SQLException if an error occurs setting the schema
     */
    public String setSchema(String schema) throws SQLException {
        String oldSchema = conn.getSchema();
        conn.setSchema(schema);
        return oldSchema;
    }

    /**
     * Retrieves the current schema set for this connection.
     *
     * @return the name of the schema currently in use
     * @throws SQLException if a database access error occurs
     */
    public String getSchema() throws SQLException {
        return conn.getSchema();
    }

    /**
     * Local method used to assure Dates of any type are of the SQL type.
     * If the object is not a date type, it is simply returned.
     *
     * @param dt the object to convert
     * @return the SQL-compatible date object or the original object if not a date
     */
    static Object fixDate(Object dt) {
        if (dt != null) {
            Class<?> cls = dt.getClass();
            if (cls == java.util.Date.class)
                dt = new java.sql.Timestamp(((java.util.Date) dt).getTime());
            else if (dt instanceof java.util.Calendar)
                dt = new java.sql.Timestamp(((java.util.Calendar) dt).getTime().getTime());
            else if (cls == java.time.LocalDateTime.class) {
                java.time.LocalDateTime ldt = (java.time.LocalDateTime) dt;
                dt = Date.from(ldt.atZone(ZoneId.systemDefault()).toInstant());
                dt = new java.sql.Timestamp(((java.util.Date) dt).getTime());
            } else if (cls == java.time.ZonedDateTime.class) {
                java.time.ZonedDateTime zdt = (java.time.ZonedDateTime) dt;
                dt = Date.from(zdt.toInstant());
                dt = new java.sql.Timestamp(((java.util.Date) dt).getTime());
            } else if (cls == java.time.LocalDate.class) {
                java.time.LocalDate ld = (java.time.LocalDate) dt;
                java.time.ZonedDateTime zonedDateTime = ld.atStartOfDay(ZoneId.systemDefault());
                dt = Date.from(zonedDateTime.toInstant());
                dt = new java.sql.Timestamp(((java.util.Date) dt).getTime());
            }
        }
        return dt;
    }

    /**
     * Gets column information for the specified table.
     *
     * @param table the table name
     * @return a map of column names to ColumnInfo objects, or null if table is null or empty
     * @throws SQLException if a database access error occurs
     */
    HashMap<String, ColumnInfo> getColumnInfo(String table) throws SQLException {
        if (table == null || table.isEmpty())
            return null;
        HashMap<String, ColumnInfo> colInfo = columnInfo.get(table);
        if (colInfo != null)
            return colInfo;
        DatabaseMetaData meta = conn.getMetaData();
        ResultSet res = meta.getColumns(null, null, table, null);
        colInfo = new HashMap<>();
        while (res.next()) {
            String colName = res.getString("COLUMN_NAME");
            colInfo.put(colName, new ColumnInfo(colName,
                    res.getInt("DATA_TYPE"),
                    res.getInt("COLUMN_SIZE"),
                    res.getInt("NULLABLE")));
        }
        res.close();
        columnInfo.put(table, colInfo);
        return colInfo;
    }

}