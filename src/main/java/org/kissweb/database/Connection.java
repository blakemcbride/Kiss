/*
*  Copyright (c) 2015 Blake McBride (blake@mcbride.name)
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

import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;


/**
 *  This class represents a connection to an SQL database.
 * <br><br>
 *  Typically, one connection would be used for each thread in an application.  Operations on a connection
 *  is separate or isolated from all of the other connections.
 *
 * @author Blake McBride
 *
 * @see Command
 * @see Cursor
 * @see Record
 *
 */
public class Connection implements AutoCloseable {

    public enum ConnectionType {PostgreSQL, MicrosoftServer, MySQL, Oracle, SQLite}

    private ConcurrentHashMap<String, String> primaryColName = new ConcurrentHashMap<>();  // table name, auto-inc primary key column name
    private ConcurrentHashMap<String, List<String>> primaryColumns = new ConcurrentHashMap<>();  // table name, primary key column names
    private HashMap<String, Boolean> TableExistanceCache = new HashMap<>();
    private boolean externalConnection = false;

    java.sql.Connection conn;
    DatabaseMetaData dmd;
    private ConnectionType ctype;

    /**
     * Create a Connection out of a pre-opened JDBC connection.
     * <br><br>
     * If a new instance of this class is created with this method, the JDBC connection passed in will not be closed
     * when this instance is closed.  Thus, if the connection was externally formed, it must be externally released.
     *
     * @param db
     *
     * @see Connection(ConnectionType, String, String, String, String)
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
        } catch (SQLException e) {
        }
    }

    /**
     * Create a connection string appropriate for the indicated database type.  This method is only used in special situations.
     *
     * @param type
     * @param host
     * @param dbname
     * @param user
     * @param pw
     * @return
     *
     * @see Connection(ConnectionType, String, String, String, String)
     */
    public static String makeConnectionString(ConnectionType type, String host, String dbname, String user, String pw) {
        String connectionString;

        if (type == ConnectionType.PostgreSQL) {
            connectionString = "jdbc:postgresql://" + host + "/" + dbname + "?user=" + user + "&password=" + pw;
        } else if (type == ConnectionType.MicrosoftServer) {
            connectionString = "jdbc:sqlserver://" + host + ";databaseName=" + dbname + ";";
            if (user != null && !"".equals(user))
                connectionString += "user=" + user + ";password=" + pw + ";";
            else
                connectionString += "integratedSecurity=true;";
        } else if (type == ConnectionType.MySQL) {
            connectionString = "jdbc:mysql://" + host + "/" + dbname + "?user=" + user + "&password=" + pw;
        } else if (type == ConnectionType.SQLite) {
            connectionString = "jdbc:sqlite:" + dbname;
            if (pw != null && !pw.isEmpty())
                connectionString += ";Password=" + pw + ";";
        } else if (type == ConnectionType.Oracle) {
            connectionString = "jdbc:oracle:thin:" + user + "/" + pw + "@" + dbname;
        } else
            throw new UnsupportedOperationException();
        return connectionString;
    }

    /**
     * Return the name of the driver used for the specified database type.  This method is only used in special circumstances.
     *
     * @param type
     * @return
     *
     * @see Connection(ConnectionType, String, String, String, String)
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
     * Form a new connection to an SQL database.  This method is only used in special situations.
     *
     * @param type
     * @param connectionString
     * @throws ClassNotFoundException
     * @throws SQLException
     *
     * @see Connection(ConnectionType, String, String, String, String)
     */
    public Connection(ConnectionType type, String connectionString) throws ClassNotFoundException, SQLException {
        String driver = getDriverName(type);
        Class.forName(driver);
        ctype = type;
        conn = DriverManager.getConnection(connectionString);
////    conn.setAutoCommit(false);  // so we can use cursors and not retrieve the entire result set at once
        dmd = conn.getMetaData();
    }

    /**
     * This is the main method of forming a new database connection.
     *
     * @param type
     * @param host
     * @param dbname
     * @param user
     * @param pw
     * @throws SQLException
     * @throws ClassNotFoundException
     *
     * @see Connection(ConnectionType, String, String)
     */
    public Connection(ConnectionType type, String host, String dbname, String user, String pw) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, dbname, user, pw));
        ctype = type;
        dmd = conn.getMetaData();
    }

    /**
     * This is the main method of forming a new database connection when Windows authentication is used.
     *
     * @param type
     * @param host
     * @param dbname
     * @throws SQLException
     * @throws ClassNotFoundException
     *
     * @see Connection(ConnectionType, String, String, String, String)
     */
    public Connection(ConnectionType type, String host, String dbname) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, dbname, null, null));
        ctype = type;
        dmd = conn.getMetaData();
    }

    /**
     * This method closes a connection.  It rarely needs to be called explicitly because it gets called
     * automatically when using the Java try-with-resource statement.
     *
     * @throws SQLException
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
     * Begin a transaction.  All following operations are part of the transaction up untill a commit() or rollback().
     * @throws SQLException
     *
     * @see #commit
     * @see #rollback
     */
    public void beginTransaction() throws SQLException {
        conn.setAutoCommit(false);
    }

    /**
     * Commit all the operations to the database since the last beginTransaction().  Also ends the transaction.
     *
     * @throws SQLException
     *
     * @see #beginTransaction
     * @see #rollback
     */
    public void commit() throws SQLException {
        conn.commit();
        conn.setAutoCommit(true);
    }

    /**
     * Rollback, erase, or forget all the operations since the last beginTransaction.  Also ends the transaction.
     *
     * @throws SQLException
     *
     * @see #beginTransaction
     * @see #commit
     */
    public void rollback() throws SQLException {
        conn.rollback();
        conn.setAutoCommit(true);
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
     * @param args
     * @throws SQLException
     *
     * @see Command#execute(String, Object...)
     */
    public void execute(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            cmd.execute(sql, args);
        }
    }

    /**
     * This is the main way of creating a new Command instance.
     *
     * @return
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
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * @param sql
     * @param args
     * @return the Record or null if none
     * @throws SQLException
     *
     * @see Command#fetchOne(String, Object...)
     */
    public Record fetchOne(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            return cmd.fetchOne(sql, args);
        }
    }

    /**
     * Fetch all of the records and close it.
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
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * If no records are found, an empty list is returned.
     * <br><br>
     * @param sql
     * @param args
     * @return
     * @throws SQLException
     *
     * @see Command#fetchAll(String, Object...)
     * @see #fetchAll(int, String, Object...)
     */
    public List<Record> fetchAll(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(sql, args);
        }
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
     * However, this method also accepts a single argument (which must be an <code>ArrayList</code>) that
     * represents the parameters rather than an in-line list of parameters.
     * <br><br>
     * If no records are found, an empty list is returned.
     * <br><br>
     * @param max
     * @param sql
     * @param args
     * @return
     * @throws SQLException
     *
     * @see Command#fetchAll(String, Object...)
     * @see #fetchAll(String, Object...)
     */
    public List<Record> fetchAll(int max, String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(max, sql, args);
        }
    }

    /**
     * Return the name of the column that is the table's primary key.  Throws an exception of
     * the table has a composite primary key.
     *
     * @param table
     * @return
     * @throws SQLException
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
     * @param table
     * @return
     * @throws SQLException
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
     * @param table
     * @return
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
     * @param table
     * @return
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

        if (TableExistanceCache.containsKey(table))
            return TableExistanceCache.get(table);
        boolean res = false;
        try (Command cmd = newCommand()) {
            if (schema == null)
                schema = "%";
            try (Cursor cursor = cmd.query("select count(*) from information_schema.tables where table_schema like ? and table_name = ?", schema, table)) {
                if (cursor.isNext()) {
                    Record rec = cursor.getRecord();
                    res = rec.getLong("count(*)") != 0;
                }
            } catch (SQLException e) {
                return false;
            }
        }
        TableExistanceCache.put(table, res);
        return res;
    }

    /**
     * Manly useful in CHAR or VARCHAR columns, this method returns the maximum size of the column.
     *
     * @param table
     * @param cname
     * @return
     * @throws SQLException
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
     * @return
     */
    public java.sql.Connection getSQLConnection() {
        return conn;
    }

    /**
     * Modifies an SQL statement to limit the number of rows returned.
     * This is needed since different databases do it differently.
     *
     * This method assumes that the SQL statement is not ending in any closing character (like ";").
     *
     * @param max the limit of number of records to return
     * @param sql the SQL statement to be modified
     * @return the modified SQL statement
     */
    public String limit(int max, String sql) {
        switch (ctype) {
            case PostgreSQL:
            case MySQL:
            case SQLite:
                return sql + " limit " + max;
            case MicrosoftServer:
                return "select top " + max + sql.trim().substring(6);
            case Oracle:
                return "select * from (" + sql + ") qv2279 where rownum <= " + max;
            default:
                return sql;
        }
    }

    /**
     * Returns the database type.
     *
     * @return
     */
    public ConnectionType getDBType() {
        return ctype;
    }
}