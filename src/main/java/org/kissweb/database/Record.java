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
import java.util.*;


/**
 * Instances of this class represent a single row in a table or a row in a result set.
 * <br><br>
 * There are two ways of getting instances of this class.  The first way is by doing a
 * select against the database.  In that case, a <code>Record</code> or list of <code>Record's</code>
 * is returned.  The other way is to create a new row or record for inseration into the database.
 * In that case, one should create the new <code>Record</code> instance as follows:
 * <br><br>
 *  &nbsp;&nbsp;&nbsp;&nbsp;   <code>Record rec = db.newRecord('myTable');</code>
 *<br><br>
 *     where <code>db</code> is a <code>Connection</code> instance.
 *
 * @see Connection#newRecord(String)
 * @see Connection#fetchAll(String, Object...)
 * @see Connection#fetchOne(String, Object...)
 * @see Command#fetchAll(String, Object...)
 * @see Command#fetchOne(String, Object...)
 * @author Blake McBride
 */
public class Record implements AutoCloseable {
    LinkedHashMap<String,Object> cols;
    private HashMap<String,Object> ocols;
    private final Connection conn;
    private Cursor cursor;
    private final String table;
    private PreparedStatement pstmt;

    Record(Connection c, String tbl) {
        conn = c;
        table = tbl.toLowerCase();
        cols = new LinkedHashMap<>();
    }

    Record(Connection c, Cursor cursor, HashMap<String,Object> ocols, LinkedHashMap<String,Object> cols) {
        conn = c;
        this.cursor = cursor;
        table = cursor.getTableName();
        this.ocols = ocols;
        this.cols = cols;
    }

    /**
     * Set the value of a column in the record.
     * This method should not be used with dates or times.
     *
     * @param name the column name
     * @param val the value to set.  Can be any type.
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setDateTime(String, java.util.Date)
     */
    public Object set(String name, Object val) {
        cols.put(name.toLowerCase(), val);
        return val;
    }

    /**
     * Set the date portion of the Date only (no time info) value of a column in the record.
     *
     * @param name the column name
     * @param val the value to set.
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     * @see #setDateOnly(String, int)
     * @see #setTime(String, long)
     */
    public Object setDateOnly(String name, java.util.Date val) {
        cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
        return val;
    }

    /**
     * Set the date portion of the Date only (no time info) value of a column in the record.
     *
     * @param name the column name
     * @param dat the value to set. Format is YYYYMMDD
     * @return
     *
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     */
    public int setDateOnly(String name, int dat) {
        if (dat == 0) {
            cols.put(name.toLowerCase(), null);
            return 0;
        }
        int y = dat / 10000;
        int m = (dat % 10000) / 100;
        int d = dat % 100;
        java.util.Date val = new GregorianCalendar(y, m-1, d).getTime();
        cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
        return dat;
    }

    /**
     * Set the time value in milliseconds
     *
     * @param name the column name
     * @param dat the value to set in milliseconds
     * @return
     *
     * @see #setDateOnly(String, java.util.Date)
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     */
    public long setTime(String name, long dat) {
        if (dat == 0) {
            cols.put(name.toLowerCase(), null);
            return 0;
        }
        java.util.Date val = new java.util.Date(dat);
        cols.put(name.toLowerCase(), new java.sql.Time(val.getTime()));
        return dat;
    }

    /**
     * Set the date and time value of a column in the record.
     *
     * @param name the column name
     * @param val the value to set.
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see #set(String, Object)
     */
    public Object setDateTime(String name, java.util.Date val) {
        cols.put(name.toLowerCase(), new java.sql.Timestamp(val.getTime()));
        return val;
    }

    /**
     * Get the value of a column as an <code>Object</code>.  Other methods that get
     * expected types are typically used over this method.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#get(String)
     * @see #getShort(String)
     * @see #getLong(String)
     * @see #getString(String)
     * etc.
     */
    public Object get(String cname) throws SQLException {
        cname = cname.toLowerCase();
        if (cols.containsKey(cname))
            return cols.get(cname);
        else
            throw new SQLException("Column " + cname + " not found.");
    }

    /**
     * Return the <code>Short</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getShort(String)
     */
    public Short getShort(String cname) throws SQLException {
        Integer r = (Integer) get(cname);
        return r == null ? null : (Short)(short)(int) r;
    }

    /**
     * Return the <code>Integer</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getInt(String)
     */
    public Integer getInt(String cname) throws SQLException {
        return (Integer) get(cname);
    }

    /**
     * Return the <code>Long</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getLong(String)
     */
    public Long getLong(String cname) throws SQLException {
        return (Long) get(cname);
    }

    /**
     * Return the <code>Float</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getFloat(String)
     */
    public Float getFloat(String cname) throws SQLException {
        return (Float) get(cname);
    }

    /**
     * Return the <code>Double</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getDouble(String)
     */
    public Double getDouble(String cname) throws SQLException {
        return (Double) get(cname);
    }

    /**
     * Return the <code>java.util.Date</code> value of the named column.
     * Just a date and no time.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateAsInt(String)
     * @see Cursor#getDateOnly(String)
     * @see #getDateTime(String)
     */
    public java.util.Date getDateOnly(String cname) throws SQLException {
        java.sql.Date dt = (java.sql.Date) get(cname);
        if (dt == null)
            return null;
        return new java.util.Date(dt.getTime());
    }

    /**
     * Return the date in an int formatted as YYYYMMDD for the named column.
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return date format YYYYMMDD
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     * @see Cursor#getDateOnly(String)
     * @see #getDateTime(String)
     */
    public int getDateAsInt(String cname) throws SQLException {
        java.sql.Date dt = (java.sql.Date) get(cname);
        if (dt == null)
            return 0;
        Calendar cal = Calendar.getInstance();
        cal.setTime(dt);
        return cal.get(Calendar.DAY_OF_MONTH) + ((cal.get(Calendar.MONTH) + 1) * 100) + ((cal.get(Calendar.YEAR)) * 10000);
    }

    /**
     * Return the <code>java.util.Date</code> value of the named column.
     * Date and time information.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getDateOnly(String)
     * @see #getTime(String)
     */
    public java.util.Date getDateTime(String cname) throws SQLException {
        Timestamp ts = (Timestamp) get(cname);
        if (ts == null)
            return null;
        return new java.util.Date(ts.getTime());
    }

    /**
     * Return the <code>long</code> value of the named column.
     * Only time information is returned.
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return time in milliseconds
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     */
    public long getTime(String cname) throws SQLException {
        java.sql.Time ts = (java.sql.Time) get(cname);
        if (ts == null)
            return 0;
        return ts.getTime();
    }

    /**
     * Return the <code>String</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getString(String)
     */
    public String getString(String cname) throws SQLException {
        return (String) get(cname);
    }

    /**
     * Return the <code>Character</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#getChar(String)
     */
    public Character getChar(String cname) throws SQLException {
        String s = (String) get(cname);
        if (s == null)
            return null;
        if (s.length() != 1)
            throw new SQLException("Column \"" + cname + "\" not a single character");
        return s.charAt(0);
    }

    /**
     * Return the <code>byte[]</code> value of the named column.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see Cursor#get(String)
     */
    public byte [] getByteArray(String cname) throws SQLException {
        return (byte []) get(cname);
    }

    /**
     * Erases all the column information associated with a <code>Record</code> instance.
     *
     * @return this
     */
    public Record clear() {
        if (cols != null)
            cols.clear();
        if (ocols != null)
            ocols.clear();
        return this;
    }

    /**
     * Performs an SQL update on the record.  This is done by creating an actual update statement and
     * executing it against the database.  It does not affect any cursors.
     *
     * @throws SQLException
     */
    public void update() throws SQLException {
        if (cursor == null  ||  !cursor.cmd.isSelect)
            throw new RuntimeException("Can't update record; not in select");
        if (table == null)
            throw new RuntimeException("Can't update record; no table name");
        LinkedList<AbstractMap.SimpleEntry<String,Object>> cf = new LinkedList<>();
        cols.forEach((key, val) -> {
            if (!ocols.containsKey(key) || ocols.get(key) != val)
                cf.addFirst(new AbstractMap.SimpleEntry<>(key, val));
        });
        if (!cf.isEmpty()) {
            StringBuilder sql = new StringBuilder("update " + table + " set ");
            boolean needComma = false;
            for (AbstractMap.SimpleEntry<String,Object> fld : cf) {
                if (needComma)
                    sql.append(", ");
                else
                    needComma = true;
                sql.append(fld.getKey()).append("=?");
            }
            if (cursor.ustmt == null  ||  !sql.equals(cursor.prevsql)) {
                cursor.prevsql = new StringBuilder(sql);
                sql.append(" where ");
                needComma = false;
                List<String> pc = conn.getPrimaryColumns(table);
                if (pc == null)
                    throw new RuntimeException("Can't update table " + table + ": no primary key");
                for (String pcol : pc) {
                    if (needComma)
                        sql.append(", ");
                    else
                        needComma = true;
                    sql.append(pcol).append("=?");
                }
                if (cursor.ustmt != null)
                    cursor.ustmt.close();
                cursor.ustmt = cursor.cmd.conn.conn.prepareStatement(sql.toString());
            } else
                cursor.ustmt.clearParameters();
            int i = 1;
            for (AbstractMap.SimpleEntry<String,Object> fld : cf)
                cursor.ustmt.setObject(i++, fld.getValue());
            for (String pcol : cursor.cmd.getPriColumns(cursor))
                cursor.ustmt.setObject(i++, ocols.get(pcol));
            cursor.ustmt.execute();
        }
    }

    /**
     * Performs an SQL delete on the record.  This is done by creating an actual update statement and
     * executing it against the database.  It does not affect any cursors.
     *
     * @throws SQLException
     */
    public void delete() throws SQLException {
        /*
        if (cursor == null || !cursor.cmd.isSelect)
            throw new RuntimeException("Can't delete record; not in select");
            */
        if (table == null)
            throw new RuntimeException("Can't delete record; no table name");
        StringBuilder sql = new StringBuilder("delete from " + table + " where ");
        if (cursor == null || !cursor.cmd.isSelect) {
            boolean needAnd = false;
            List<String> pc = conn.getPrimaryColumns(table);
            if (pc == null)
                throw new RuntimeException("Can't delete from table " + table + ": no primary key");
            for (String col : pc) {
                if (needAnd)
                    sql.append(" and ");
                else
                    needAnd = true;
                sql.append(col).append("=?");
            }
            try (PreparedStatement ustmt = conn.conn.prepareStatement(sql.toString())) {
                int i = 1;
                for (String pcol : conn.getPrimaryColumns(table))
                    ustmt.setObject(i++, cols.get(pcol));
                ustmt.execute();
            }
            return;
        } else if (cursor.ustmt == null || !sql.equals(cursor.prevsql)) {
            cursor.prevsql = new StringBuilder(sql);
            boolean needAnd = false;
            List<String> pc = conn.getPrimaryColumns(table);
            if (pc == null)
                throw new RuntimeException("Can't delete from table " + table + ": no primary key");
            for (String col : pc) {
                if (needAnd)
                    sql.append(" and ");
                else
                    needAnd = true;
                sql.append(col).append("=?");
            }
            if (cursor.ustmt != null)
                cursor.ustmt.close();
            cursor.ustmt = cursor.cmd.conn.conn.prepareStatement(sql.toString());
        } else
            cursor.ustmt.clearParameters();
        int i = 1;
        for (String pcol : cursor.cmd.getPriColumns(cursor))
            cursor.ustmt.setObject(i++, ocols.get(pcol));
        cursor.ustmt.execute();
    }

    /**
     * Works like addRecord() except that it returns the value of the serial/primary key column
     * that the database used for the new record.
     *
     * @return an Object - should be cast to (short), (int), or (long) depending on the serial type
     * @throws SQLException
     *
     * @see #addRecord()
     */
    public Object addRecordAutoInc() throws SQLException {
        if (pstmt == null) {
            StringBuilder sql = new StringBuilder("insert into " + table + " (");
            boolean needComma = false;
            for (String fld : cols.keySet()) {
                if (needComma)
                    sql.append(", ");
                else
                    needComma = true;
                sql.append(fld);
            }
            sql.append(") values (");
            needComma = false;

            for (int i = 0; i < cols.size(); i++)
                if (needComma)
                    sql.append(", ?");
                else {
                    needComma = true;
                    sql.append("?");
                }
            sql.append(")");
//            if (conn.ctype == Connection.ConnectionType.PostgreSQL)
//                sql.append(" returning " + colname);
//            else if (conn.ctype == Connection.ConnectionType.MicrosoftServer)
//                sql.append("; SELECT SCOPE_IDENTITY();");
//            else
//                throw new SQLException("serial column support not added to this database type yet.");
            pstmt = conn.conn.prepareStatement(sql.toString(), Statement.RETURN_GENERATED_KEYS);
        } else
            pstmt.clearParameters();
        int i = 1;
        for (Object val : cols.values())
            pstmt.setObject(i++, val);
//        ResultSet rset = pstmt.executeQuery();
        pstmt.executeUpdate();
        Object nextId = null;
        try (ResultSet rset = pstmt.getGeneratedKeys()) {
            if (rset.next())
                nextId = rset.getObject(1);
            else
                throw new SQLException("Failure to get next serial");
        }
        return nextId;
    }

    /**
     * Inserts a new row into the database by creating an executing an SQL statement.
     * It does not affect any cursors.
     *
     * @return
     * @throws SQLException
     *
     * @see #addRecordAutoInc()
     */
    public boolean addRecord() throws SQLException {
        if (pstmt == null) {
            StringBuilder sql = new StringBuilder("insert into " + table + " (");
            boolean needComma = false;
            for (String fld : cols.keySet()) {
                if (needComma)
                    sql.append(", ");
                else
                    needComma = true;
                sql.append(fld);
            }
            sql.append(") values (");
            needComma = false;
            for (int i = 0; i < cols.size(); i++)
                if (needComma)
                    sql.append(", ?");
                else {
                    needComma = true;
                    sql.append("?");
                }
            sql.append(")");
            pstmt = conn.conn.prepareStatement(sql.toString());
        } else
            pstmt.clearParameters();
        int i = 1;
        for (Object val : cols.values())
            pstmt.setObject(i++, val);
        return pstmt.execute();
    }

    /**
     * Copy all columns from rec to 'this' that have the same column names and types
     *
     * @param rec the record to be copied from
     */
    public void copyCorresponding(Record rec) {
        for (Map.Entry<String,Object> entry : rec.cols.entrySet()) {
            String key = entry.getKey();
            if (!cols.containsKey(key))
                continue;
            Object src = entry.getValue();
            Object dest = cols.get(key);
            if (dest == null  ||  src == null  ||  dest.getClass() == src.getClass())
                cols.replace(key, src);
        }
    }

    /**
     * Copy all columns from rec to 'this'
     *
     * @param rec the record to be copied from
     */
    public void copy(Record rec) {
        for (Map.Entry<String,Object> entry : rec.cols.entrySet())
            cols.put(entry.getKey(), entry.getValue());
    }

    /**
     * Closes any open prepared statements against this record.  It is not normally needed since
     * this class implements the AutoCloseable interface.
     */
    @Override
    public void close() {
        if (pstmt != null) {
            try {
                pstmt.close();
            } catch (SQLException e) {
            }
            pstmt = null;
        }
    }

    /**
     * Returns the <code>Connection</code> instance associated to this <code>Record</code> instance.
     *
     * @return
     */
    public Connection getConnection() {
        return conn;
    }

    /**
     * Returns the name of the table associated to this <code>Record</code> instance.
     *
     * @return
     */
    public String getTableName() {
        return table;
    }
}
