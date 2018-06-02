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
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;


/**
 *
 * @author Blake McBride
 */
public class Connection implements AutoCloseable {

    public enum ConnectionType {PostgreSQL, MicrosoftServer, MySQL, Oracle, SQLite}

    private ConcurrentHashMap<String, String> primaryColName = new ConcurrentHashMap<>();  // table name, auto-inc primary key column name
    private ConcurrentHashMap<String, List<String>> primaryColumns = new ConcurrentHashMap<>();  // table name, primary key column names
    private Hashtable<String, Boolean> TableExistanceCache = new Hashtable<>();
    private boolean externalConnection = false;

    java.sql.Connection conn;
    DatabaseMetaData dmd;
    ConnectionType ctype;

    public Connection(java.sql.Connection db) {
        this.conn = db;
        externalConnection = true;
    }

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

    public Connection(ConnectionType type, String connectionString) throws ClassNotFoundException, SQLException {
        String driver = getDriverName(type);
        Class.forName(driver);
        ctype = type;
        conn = DriverManager.getConnection(connectionString);
////    conn.setAutoCommit(false);  // so we can use cursors and not retrieve the entire result set at once
        dmd = conn.getMetaData();
    }

    public Connection(ConnectionType type, String host, String dbname, String user, String pw) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, dbname, user, pw));
        ctype = type;
    }

    public Connection(ConnectionType type, String host, String dbname) throws SQLException, ClassNotFoundException {
        this(type, makeConnectionString(type, host, dbname, null, null));
        ctype = type;
    }

    @Override
    public void close() throws SQLException {
        if (conn != null) {
            if (!externalConnection)
                conn.close();
            conn = null;
        }
    }

    public void beginTransaction() throws SQLException {
        conn.setAutoCommit(false);
    }

    public void commit() throws SQLException {
        conn.commit();
        conn.setAutoCommit(true);
    }

    public void rollback() throws SQLException {
        conn.rollback();
        conn.setAutoCommit(true);
    }

    public void execute(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            cmd.execute(sql, args);
        }
    }

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
     * The record cannot be updated or deleted.
     *
     * @param sql
     * @param args
     * @return
     * @throws SQLException
     */
    public Record fetchOne(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            return cmd.fetchOne(sql, args);
        }
    }

    /**
     * Fetch all of the records and close it.
     * No records can be updated or deleted.
     *
     * @param sql
     * @param args
     * @return
     * @throws SQLException
     */
    public List<Record> fetchAll(String sql, Object... args) throws SQLException {
        try (Command cmd = newCommand()) {
            return cmd.fetchAll(sql, args);
        }
    }

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

    public Record newRecord(String table) {
        return new Record(this, table);
    }

    public boolean tableExists(String table) {
        String schema = null;

        table = table.replaceAll("[", "");
        table = table.replaceAll("]", "");
        if (table.indexOf('.') >= 0) {
            String[] parts = table.split(".");
            schema = parts[parts.length - 2];
            table = parts[parts.length - 1];
        }

        if (TableExistanceCache.contains(table))
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

}
