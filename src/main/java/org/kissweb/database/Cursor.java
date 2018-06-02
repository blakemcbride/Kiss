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
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;


/**
 *
 * @author Blake McBride
 */
public class Cursor implements AutoCloseable {
    Command cmd;
    String tname;
    PreparedStatement ustmt;
    private ResultSet rset;
    private ResultSetMetaData mdata;
    StringBuilder prevsql;
    private Record lastRec;

    Cursor(Command cmd) throws SQLException {
        this.cmd = cmd;
        rset = cmd.pstat.executeQuery();
        mdata = rset.getMetaData();
        cmd.isSelect = true;
    }

    public String getTableName() {
        if (tname == null) {
            try {
                tname = mdata.getTableName(1).toLowerCase();
            } catch (SQLException e) {
                return null;
            }
        }
        return tname;
    }

    public Record getRecord() {
        return lastRec;
    }

    public boolean isNext() throws SQLException {
        return null != next();
    }

    public Record next() throws SQLException {
        if (rset.next()) {
            HashMap<String,Object> ocols = new HashMap<>();
            LinkedHashMap<String,Object> cols = new LinkedHashMap<>();
            int ncols = mdata.getColumnCount();
            for (int i=1 ; i <= ncols ; i++) {
                Object val = rset.getObject(i);
                String name = mdata.getColumnName(i).toLowerCase();
                cols.put(name, val);
                ocols.put(name, val);
            }
            return lastRec = new Record(cmd.conn, this, ocols, cols);
        } else
            close();
        return lastRec = null;
    }

    /**
     * Read in a single record and close the read cursor (no more records can be read).
     * The record read can be updated or deleted.
     *
     * @return
     * @throws SQLException
     */
    public Record fetchOne() throws SQLException {
        lastRec = next();
        partialClose();
        return lastRec;
    }

    /**
     * Read in all of the records and close the cursor.
     * Records cannot be updated or deleted.
     *
     * @return
     * @throws SQLException
     */
    public List<Record> fetchAll() throws SQLException {
        List<Record> r = new ArrayList<Record>();
        Record rec;
        while (null != (rec=next()))
            r.add(rec);
        close();
        lastRec = null;
        return r;
    }

    /**
     * After this, you can edit and delete records.  You just can't read anymore.
     *
     * @throws SQLException
     */
    void partialClose() throws SQLException {
        if (rset != null) {
            rset.close();
            rset = null;
        }
    }

    /**
     * Close the entire cursor.  No more read, edit, or deletes.
     *
     * @throws SQLException
     */
    @Override
    public void close() throws SQLException {
        partialClose();
        if (ustmt != null) {
            ustmt.close();
            ustmt = null;
        }
        prevsql = null;
        lastRec = null;
    }

    public Object set(String name, Object val) {
        lastRec.cols.put(name.toLowerCase(), val);
        return val;
    }

    public Object get(String fname) throws SQLException {
        fname = fname.toLowerCase();
        if (lastRec.cols.containsKey(fname))
            return lastRec.cols.get(fname);
        else
            throw new SQLException("Column " + fname + " not found.");
    }

    public Integer getShort(String fname) throws SQLException {
        return (Integer) get(fname);
    }

    public Integer getInt(String fname) throws SQLException {
        return (Integer) get(fname);
    }

    public Long getLong(String fname) throws SQLException {
        return (Long) get(fname);
    }

    public Float getFloat(String fname) throws SQLException {
        return (Float) get(fname);
    }

    public Double getDouble(String fname) throws SQLException {
        return (Double) get(fname);
    }

    public Date getDate(String fname) throws SQLException {
        return (Date) get(fname);
    }

    public String getString(String fname) throws SQLException {
        return (String) get(fname);
    }

    public Character getChar(String fname) throws SQLException {
        String s = (String) get(fname);
        if (s == null)
            return null;
        if (s.length() != 1)
            throw new SQLException("Column \"" + fname + "\" not a single character");
        return s.charAt(0);
    }

}
