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

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;


/**
 * This class represents a single statement or command against a database.  Of course, each Connection may have
 * many Command instances in play at a time.  New Command instances may by obtained via the Connection class as follows:
 *<br><br>
 *   &nbsp;&nbsp;&nbsp;&nbsp;   <code>Command cmd = db.newCommand();</code>
 *<br><br>
 *  where <code>db</code> is a Connection instance.
 *
 * @see Connection#newCommand()
 *
 * @author Blake McBride
 */
public class Command implements AutoCloseable {

    Connection conn;
    PreparedStatement pstat;
    boolean isSelect;
    private String lastSQL;
    private List<String> pcols;


    Command(Connection c) throws SQLException {
        conn = c;
//        stmt = conn.conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY, ResultSet.HOLD_CURSORS_OVER_COMMIT);
    }

    /**
     * Execute non-select statement.  This is useful, for example, for UPDATE, INSERT, and DELETE SQL statements.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     *
     * @param sql the sql statement with ? parameters
     * @param args the parameter values
     * @return false is a normal result
     * @throws SQLException
     *
     * @see #fetchOne(String, Object...)
     * @see #fetchAll(String, Object...)
     */
    public boolean execute(String sql, Object ... args) throws SQLException {
        if (lastSQL == null || lastSQL != sql && !lastSQL.equals(sql)) {
            if (pstat != null)
                pstat.close();
            pstat = conn.conn.prepareStatement(sql);
            lastSQL = null;
            if (pcols != null)
                pcols.clear();
        } else
            pstat.clearParameters();
        for (int i = 0; i < args.length; i++)
            pstat.setObject(i + 1, args[i]);
        if (lastSQL == null)
            lastSQL = sql;
        isSelect = false;
        return pstat.execute();
    }

    /**
     * Execute a select statement returning a Cursor that may be used to
     * obtain each subsequent row.  This is useful when a large number of records
     * is possible and fetching all into memory at one time is unneeded.  This can
     * save a significant amount of memory since only one record is in memory at a time.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     *
     * @param sql the sql statement with ? parameters
     * @param args the parameter values
     * @return
     * @throws SQLException
     *
     * @see #fetchAll(String, Object...)
     * @see #fetchOne(String, Object...)
     */
    public Cursor query(String sql, Object ... args) throws SQLException {
        if (lastSQL == null || lastSQL != sql && !lastSQL.equals(sql)) {
            if (pstat != null)
                pstat.close();
            pstat = conn.conn.prepareStatement(sql);
            lastSQL = null;
            if (pcols != null)
                pcols.clear();
        } else
            pstat.clearParameters();
        for (int i = 0; i < args.length; i++)
            pstat.setObject(i + 1, args[i]);
        Cursor c = new Cursor(this);
        if (lastSQL == null)
            lastSQL = sql;
        return c;
    }

    /**
     * Read in the first record and then close the cursor.
     * The record cannot be updated or deleted.
     * <br><br>
     * Adding code to the SQL statement telling the database to limit its result set to
     * one record doesn't affect the result but it can make the query significantly faster.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     *
     * @param sql SQL statement with ? parameters
     * @param args the parameter values
     * @return
     * @throws SQLException
     *
     * @see #fetchAll(String, Object...)
     * @see #execute(String, Object...)
     */
    public Record fetchOne(String sql, Object ... args) throws SQLException {
        try (Cursor c = query(sql, args)) {
            return c.fetchOne();
        }
    }

    /**
     * Fetch all of the records and close the cursor.
     * No records can be updated or deleted.
     * <br><br>
     * The SQL string may contain parameters indicated by the '?' character.
     * A variable number of arguments to this method are used to fill those parameters.
     * Each argument gets applied to each '?' parameter in the same order as they appear
     * in the SQL statement. An SQL prepared statement is used.
     *
     * @param sql SQL statement with ? parameters
     * @param args the parameter values
     * @return
     * @throws SQLException
     *
     * @see #fetchOne(String, Object...)
     * @see #execute(String, Object...)
     */
    public List<Record> fetchAll(String sql, Object ... args) throws SQLException {
        return query(sql, args).fetchAll();
    }

    List<String> getPriColumns(Cursor c) {
        if (pcols == null  ||  pcols.isEmpty()) {
            if (pcols == null)
                pcols = new ArrayList<String>();
            try (ResultSet r = conn.dmd.getPrimaryKeys(null, null, c.getTableName())) {
                while (r.next())
                    pcols.add(r.getString(4));
            } catch (SQLException e) {
                return null;
            }
        }
        return pcols;
    }

    /**
     * This closes the Command instance.  This need not be done manually since this class implements the AutoCloseable interface.
     */
    @Override
    public void close() {
        if (pstat != null) {
            try {
                pstat.close();
            } catch (SQLException e) {
               // e.printStackTrace();
            }
            pstat = null;
            lastSQL = null;
        }
    }
}
