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

import org.json.JSONArray;
import org.json.JSONObject;

import java.sql.*;
import java.util.*;


/**
 * Instances of this class represent a single row in a table or a row in a result set.
 * <br><br>
 * There are two ways of getting instances of this class.  The first way is by doing a
 * select against the database.  In that case, a <code>Record</code> or list of <code>Record's</code>
 * is returned.  The other way is to create a new row or record for insertion into the database.
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
    LinkedHashMap<String,Object> cols;       // current column values
    private HashMap<String,Object> ocols;    // original column values
    private final Connection conn;
    private Cursor cursor;
    private final String table;
    private PreparedStatement pstmt;

    // Intended to be used internally only.
    Record(Connection c, String tbl) {
        conn = c;
        table = tbl.toLowerCase();
        cols = new LinkedHashMap<>();
    }

    // Intended to be used internally only.
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
    public Record set(String name, Object val) {
        cols.put(name.toLowerCase(), val);
        return this;
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
    public Record setDateOnly(String name, java.util.Date val) {
        cols.put(name.toLowerCase(), val == null ? null : new java.sql.Date(val.getTime()));
        return this;
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
    public Record setDateOnly(String name, int dat) {
        if (dat == 0) {
            cols.put(name.toLowerCase(), null);
            return this;
        }
        int y = dat / 10000;
        int m = (dat % 10000) / 100;
        int d = dat % 100;
        java.util.Date val = new GregorianCalendar(y, m-1, d).getTime();
        cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
        return this;
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
    public Record setTime(String name, long dat) {
        if (dat == 0) {
            cols.put(name.toLowerCase(), null);
            return this;
        }
        java.util.Date val = new java.util.Date(dat);
        cols.put(name.toLowerCase(), new java.sql.Time(val.getTime()));
        return this;
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
    public Record setDateTime(String name, java.util.Date val) {
        cols.put(name.toLowerCase(), val == null ? null : new java.sql.Timestamp(val.getTime()));
        return this;
    }

    /**
     * Set the date and time value of a column in the record.
     *
     * @param name the column name
     * @param val number ov milliseconds since 1970 UTC
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see #set(String, Object)
     */
    public Record setDateTime(String name, long val) {
        cols.put(name.toLowerCase(), val == 0L ? null : new java.sql.Timestamp(val));
        return this;
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
     * Tests if a given column exists.
     * Returns <code>true</code> of the column exists, and <code>false</code> if it does not.
     *
     * @param cname
     * @return
     * @throws SQLException
     */
    public boolean columnExists(String cname) throws SQLException {
        return cols.containsKey(cname.toLowerCase());
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
        // some databases return an Integer even though the field is a smallint
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj.getClass() == Short.class)
            return (Short) obj;
        else if (obj.getClass() == Integer.class) {
            int i = (Integer) obj;
            return (short) i;
        }
        throw new SQLException("column " + cname + " is not a short or integer");
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
        Object obj = get(cname);
        if (obj == null)
            return null;
        if (obj instanceof java.sql.Date) {
            java.sql.Date dt = (java.sql.Date) obj;
            return new java.util.Date(dt.getTime());
        } else {
            // assume Timestamp
            java.sql.Timestamp ts = (java.sql.Timestamp) obj;
            Calendar cal = Calendar.getInstance();
            cal.setTime(ts);
            cal.set(Calendar.HOUR, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            return cal.getTime();
        }
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
        Calendar cal;
        Object obj = get(cname);
        if (obj == null)
            return 0;
        if (obj instanceof java.sql.Date) {
            java.sql.Date dt = (java.sql.Date) obj;
            cal = Calendar.getInstance();
            cal.setTime(dt);
        } else {
            // assume Timestamp
            java.sql.Timestamp ts = (java.sql.Timestamp) obj;
            cal = Calendar.getInstance();
            cal.setTime(ts);
        }
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
     * @see #getDateTimeMS(String)
     */
    public java.util.Date getDateTime(String cname) throws SQLException {
        Timestamp ts = (Timestamp) get(cname);
        if (ts == null)
            return null;
        return new java.util.Date(ts.getTime());
    }

    /**
     * Return the date/time value as the number of milliseconds since 1970 UTC.
     *
     * A <code>0</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateOnly(String)
     * @see #getTime(String)
     * @see #getDateTime(String)
     */
    public long getDateTimeMS(String cname) throws SQLException {
        Timestamp ts = (Timestamp) get(cname);
        if (ts == null)
            return 0L;
        return ts.getTime();
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
     * <br><br>
     * This method is smart.  It only updates the fields that have changed, and if none have, it does nothing.
     *
     * @throws SQLException
     * @see Connection#commit()
     */
    public void update() throws SQLException {
        if (cursor != null  &&  !cursor.cmd.isSelect)
            throw new RuntimeException("Can't update record; not in select");
        if (table == null)
            throw new RuntimeException("Can't update record; no table name");
        final ArrayList<AbstractMap.SimpleEntry<String,Object>> cf = new ArrayList<>();
        cols.forEach((key, val) -> {
            if (!ocols.containsKey(key) || ocols.get(key) != val)
                cf.add(new AbstractMap.SimpleEntry<>(key, val));
        });
        if (!cf.isEmpty()) {
            final StringBuilder sql = new StringBuilder("update " + table + " set ");
            boolean needComma = false;
            for (AbstractMap.SimpleEntry<String,Object> fld : cf) {
                if (needComma)
                    sql.append(", ");
                else
                    needComma = true;
                sql.append(fld.getKey()).append("=?");
            }
            PreparedStatement pstmt;
            if (cursor == null) {
                // Update a new record
                sql.append(" where ");
                boolean needAnd = false;
                final List<String> pc = conn.getPrimaryColumns(table);
                if (pc == null)
                    throw new RuntimeException("Can't update table " + table + ": no primary key");
                for (String pcol : pc) {
                    if (needAnd)
                        sql.append(" and ");
                    else
                        needAnd = true;
                    sql.append(pcol).append("=?");
                }
                pstmt = conn.conn.prepareStatement(sql.toString());
            } else if (cursor.ustmt == null  ||  !sql.equals(cursor.prevsql)) {
                //  Update a record read in + new statement
                cursor.prevsql = new StringBuilder(sql);
                sql.append(" where ");
                boolean needAnd = false;
                final List<String> pc = conn.getPrimaryColumns(table);
                if (pc == null)
                    throw new RuntimeException("Can't update table " + table + ": no primary key");
                for (String pcol : pc) {
                    if (needAnd)
                        sql.append(" and ");
                    else
                        needAnd = true;
                    sql.append(pcol).append("=?");
                }
                if (cursor.ustmt != null)
                    cursor.ustmt.close();
                cursor.ustmt = cursor.cmd.conn.conn.prepareStatement(sql.toString());
                pstmt = cursor.ustmt;
            } else {
                // Update a record read in - re-use same prepared statement
                cursor.ustmt.clearParameters();
                pstmt = cursor.ustmt;
            }
            int i = 1;
            for (AbstractMap.SimpleEntry<String,Object> fld : cf) {
                Object val = fld.getValue();
                Array a = makeSQLArray(conn, val);
                if (a == null)
                    pstmt.setObject(i++, Connection.fixDate(val));
                else
                    pstmt.setArray(i++, a);
            }
            List<String> pcols;
            if (cursor == null)
                pcols = conn.getPrimaryColumns(table);
            else
                pcols = cursor.cmd.getPriColumns(cursor);
            for (String pcol : pcols)
                pstmt.setObject(i++, Connection.fixDate(ocols.get(pcol)));
            pstmt.execute();
            // now update our memory of the original values
            ocols.clear();
            ocols.putAll(cols);
            if (cursor == null)
                pstmt.close();
        }
    }

    /**
     * Performs an SQL delete on the record.  This is done by creating an actual update statement and
     * executing it against the database.  It does not affect any cursors.
     *
     * @throws SQLException
     * @see Connection#commit()
     */
    public void delete() throws SQLException {
        /*
        if (cursor == null || !cursor.cmd.isSelect)
            throw new RuntimeException("Can't delete record; not in select");
            */
        if (table == null)
            throw new RuntimeException("Can't delete record; no table name");
        final StringBuilder sql = new StringBuilder("delete from " + table + " where ");
        if (cursor == null || !cursor.cmd.isSelect) {
            boolean needAnd = false;
            final List<String> pc = conn.getPrimaryColumns(table);
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
                    ustmt.setObject(i++, Connection.fixDate(cols.get(pcol)));
                ustmt.execute();
            }
            return;
        } else if (cursor.ustmt == null || !sql.equals(cursor.prevsql)) {
            cursor.prevsql = new StringBuilder(sql);
            boolean needAnd = false;
            final List<String> pc = conn.getPrimaryColumns(table);
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
            cursor.ustmt.setObject(i++, Connection.fixDate(ocols.get(pcol)));
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
     * @see Connection#commit()
     */
    public Object addRecordAutoInc() throws SQLException {
        if (pstmt == null) {
            final StringBuilder sql = new StringBuilder("insert into " + table + " (");
            boolean needComma = false;
            for (String fld : cols.keySet())
                if (cols.get(fld) != null) {
                    if (needComma)
                        sql.append(", ");
                    else
                        needComma = true;
                    sql.append(fld);
                }
            sql.append(") values (");
            needComma = false;
            for (Object v : cols.values())
                if (v != null)
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
        for (Object val : cols.values()) {
            if (val != null) {
                Array a = makeSQLArray(conn, val);
                if (a == null)
                    pstmt.setObject(i++, Connection.fixDate(val));
                else
                    pstmt.setArray(i++, a);
            }
        }
//        ResultSet rset = pstmt.executeQuery();
        pstmt.executeUpdate();
        Object nextId;
        try (ResultSet rset = pstmt.getGeneratedKeys()) {
            if (rset.next())
                nextId = rset.getObject(1);
            else
                throw new SQLException("Failure to get next serial");
        }

        //  update the column value
        final List<String> pcols = conn.getPrimaryColumns(table);
        cols.put(pcols.get(0), nextId);

        if (ocols == null)
            ocols = new HashMap<>();
        else
            ocols.clear();
        ocols.putAll(cols);
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
     * @see Connection#commit()
     */
    public boolean addRecord() throws SQLException {
        if (pstmt == null) {
            final StringBuilder sql = new StringBuilder("insert into " + table + " (");
            boolean needComma = false;
            for (String fld : cols.keySet())
                if (cols.get(fld) != null) {
                    if (needComma)
                        sql.append(", ");
                    else
                        needComma = true;
                    sql.append(fld);
                }
            sql.append(") values (");
            needComma = false;
            for (Object v : cols.values())
                if (v != null)
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
        for (Object val : cols.values()) {
            if (val != null) {
                Array a = makeSQLArray(conn, val);
                if (a == null)
                    pstmt.setObject(i++, Connection.fixDate(val));
                else
                    pstmt.setArray(i++, a);
            }
        }
        boolean ret = pstmt.execute();
        if (ocols == null)
            ocols = new HashMap<>();
        else
            ocols.clear();
        for (String key : cols.keySet())
            ocols.put(key, cols.get(key));
        return ret;
    }

    /**
     * If the object passed in is a collection, make it suitable for setArray.
     * Otherwise, return null if it is not a collection type.
     * <br><br>
     * At least in PostgreSQL, your query should look like:<br>
     *     <code>where col = ANY(?)</code><br>
     *  rather than:<br>
     *     <code>where col in (?)</code><br>
     *  Also, if using
     *      <code>where col &lt;&gt; ANY(?)</code><br>
     *  the list must have at least one element.
     *
     * @param a
     * @return
     * @throws SQLException
     * @see ArrayListInteger
     * @see ArrayListLong
     * @see ArrayListShort
     * @see ArrayListString
     */
    @SuppressWarnings("unchecked")
    static Array makeSQLArray(Connection conn, Object a) throws SQLException {
        if (a instanceof int[]) {
            int [] a1 = (int[]) a;
            Integer [] a2 = new Integer[a1.length];
            for (int j=0 ; j < a1.length ; j++)
                a2[j] = a1[j];
            return conn.conn.createArrayOf("integer", a2);
        } else if (a instanceof Integer[]) {
            return conn.conn.createArrayOf("integer", (Object[]) a);
        } else if (a instanceof long[]) {
            long [] a1 = (long[]) a;
            Long [] a2 = new Long[a1.length];
            for (int j=0 ; j < a1.length ; j++)
                a2[j] = a1[j];
            return conn.conn.createArrayOf("bigint", a2);
        } else if (a instanceof Long[]) {
            return conn.conn.createArrayOf("bigint", (Object[]) a);
        } else if (a instanceof float[]) {
            float [] a1 = (float[]) a;
            Float [] a2 = new Float[a1.length];
            for (int j=0 ; j < a1.length ; j++)
                a2[j] = a1[j];
            return conn.conn.createArrayOf("real", a2);
        } else if (a instanceof Float[]) {
            return conn.conn.createArrayOf("real", (Object[]) a);
        } else if (a instanceof double[]) {
            double [] a1 = (double[]) a;
            Double [] a2 = new Double[a1.length];
            for (int j=0 ; j < a1.length ; j++)
                a2[j] = a1[j];
            return conn.conn.createArrayOf("double precision", a2);
        } else if (a instanceof Double[]) {
            return conn.conn.createArrayOf("double precision", (Object[]) a);
        } else if (a instanceof short[]) {
            short [] a1 = (short[]) a;
            Short [] a2 = new Short[a1.length];
            for (int j=0 ; j < a1.length ; j++)
                a2[j] = a1[j];
            return conn.conn.createArrayOf("smallint", a2);
        } else if (a instanceof Short[]) {
            return conn.conn.createArrayOf("smallint", (Object[]) a);
        } else if (a instanceof String[]) {
            return conn.conn.createArrayOf("varchar", (Object[]) a);
        } else if (a instanceof ArrayListInteger) {
            ArrayList<Integer> lst = (ArrayList<Integer>) a;
            int sz = lst.size();
            Integer [] a2 = new Integer[sz];
            for (int j=0 ; j < sz ; j++)
                a2[j] = lst.get(j);
            return conn.conn.createArrayOf("integer", a2);
        } else if (a instanceof ArrayListLong) {
            ArrayList<Long> lst = (ArrayList<Long>) a;
            int sz = lst.size();
            Long [] a2 = new Long[sz];
            for (int j=0 ; j < sz ; j++)
                a2[j] = lst.get(j);
            return conn.conn.createArrayOf("bigint", a2);
        } else if (a instanceof ArrayListShort) {
            ArrayList<Short> lst = (ArrayList<Short>) a;
            int sz = lst.size();
            Short [] a2 = new Short[sz];
            for (int j=0 ; j < sz ; j++)
                a2[j] = lst.get(j);
            return conn.conn.createArrayOf("smallint", a2);
        } else if (a instanceof ArrayListString) {
            ArrayList<String> lst = (ArrayList<String>) a;
            int sz = lst.size();
            String [] a2 = new String[sz];
            for (int j=0 ; j < sz ; j++)
                a2[j] = lst.get(j);
            return conn.conn.createArrayOf("varchar", a2);
        } else if (a instanceof Collection)
            throw new SQLException("Use one of ArrayListString, ArrayListInteger, etc.");
        return null;
    }

    /**
     * Copy all columns from <code>fromRec</code> to <code>this</code> that have the same column names and types
     *
     * @param fromRec the record to be copied from
     */
    public void copyCorresponding(Record fromRec) throws SQLException {
        final Record toRec = this;
        final String fromTable = fromRec.table;
        if (fromTable == null)
            throw new SQLException("Missing from table name");
        final String toTable = table;
        if (toTable == null)
            throw new SQLException("Missing to table name");
        final HashMap<String, ColumnInfo> fromCols = conn.getColumnInfo(fromTable);
        if (fromCols == null)
            throw new SQLException("Error acquiring from table column information");
        final HashMap<String, ColumnInfo> toCols = conn.getColumnInfo(toTable);
        if (toCols == null)
            throw new SQLException("Error acquiring to table column information");
        for (Map.Entry<String, Object> e : fromRec.cols.entrySet()) {
            String fromFieldName = e.getKey();
            ColumnInfo toColInfo = toCols.get(fromFieldName);
            if (toColInfo == null)
                continue;
            ColumnInfo fromCol = fromCols.get(fromFieldName);
            if (fromCol.getDataType() != toColInfo.getDataType())
                continue;
            toRec.cols.put(fromFieldName, e.getValue());
        }
    }

    /**
     * Copy all columns from rec to 'this'
     *
     * @param rec the record to be copied from
     */
    public void copy(Record rec) {
        cols.putAll(rec.cols);
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
            } catch (SQLException ignored) {
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

    /**
     * Returns a map of all the columns in the record.
     *
     * @return
     */
    public AbstractMap<String,Object> getAllColumns() {
        return cols;
    }

    /**
     * Convert a record into a JSON object.
     *
     * @return
     */
    public JSONObject toJSON() {
        JSONObject obj = new JSONObject();
        cols.forEach((id, val) -> {
            if (val instanceof java.util.Date)
                obj.put(id, (java.util.Date) val);
            else
                obj.put(id, val);
        });
        return obj;
    }

    /**
     * Add columns of a Record to an existing JSON object.
     *
     * @return
     */
    public JSONObject addToJSON(JSONObject obj) {
        cols.forEach(obj::put);
        return obj;
    }

    /**
     * Utility method to convert a list of Record's into a JSON array of JSON objects.
     *
     * @param recs
     * @return
     */
    public static JSONArray toJSONArray(List<Record> recs) {
        JSONArray ary = new JSONArray();
        recs.forEach(rec -> ary.put(rec.toJSON()));
        return ary;
    }
}
