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
 * Instances of this class represent a row into a result set.  Cursors are forward-only and read-only from the database's
 * perspective so are very efficient and shouldn't create an actual cursor on the database side.  If a single table is
 * selected from, rows may be updated or deleted via these facilities without the use of a database cursor.
 * <br><br>
 * A new cursor is created with the following code:
 * <br><br>
 *  &nbsp;&nbsp;&nbsp;&nbsp;   <code>Cursor cursor = cmd.query("select .....</code>
 * <br><br>
 *     where <code>cmd</code> is an instance of Command.
 * @see Command#query
 *
 * @author Blake McBride
 */
public class Cursor implements AutoCloseable {
    Command cmd;
    String tname;
    PreparedStatement ustmt;
    private ResultSet rset;
    private final ResultSetMetaData mdata;
    StringBuilder prevsql;
    private Record lastRec;

    Cursor(Command cmd) throws SQLException {
        this.cmd = cmd;
        rset = cmd.pstat.executeQuery();
        mdata = rset.getMetaData();
        cmd.isSelect = true;
    }

    /**
     * Returns the name of the table associated with the select associated with this cursor.
     *
     * @return
     */
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

    /**
     * Returns the <code>Record</code> instance representing the current row.  This method does not advance the row pointer.
     *
     * @return
     *
     * @see #next()
     * @see #isNext()
     */
    public Record getRecord() {
        return lastRec;
    }

    /**
     * This method advances the row pointer and returns <code>true</code> if there is a next record.
     * @return
     * @throws SQLException
     *
     * @see #getRecord()
     * @see #next()
     */
    public boolean isNext() throws SQLException {
        return null != next();
    }

    /**
     * This method is used to advance the row pointer and return the next <code>Record</code>
     * instance representing the next row.  If there are no more records, <code>null</code>
     * is returned and the cursor is closed.
     *
     *
     * @return
     * @throws SQLException
     *
     *  @see #getRecord()
     * @see #isNext()
     */
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
        }
        close();
        return lastRec = null;
    }

    /**
     * Read in a single record and close the read cursor (no more records can be read).
     * The record read can be updated or deleted.  <code>null</code> is returned if
     * there is no record.
     *
     * @return the Record or null if none
     * @throws SQLException
     */
    public Record fetchOne() throws SQLException {
        lastRec = next();
        partialClose();
        return lastRec;
    }

    /**
     * This method works exactly like <code>fetchOne</code> except that it return a JSON object representing the record.
     * <code>null</code> is returned if there is no record.
     *
     * @return the JSON object or <code>null</code> if none
     * @throws SQLException
     *
     * @see #fetchOne()
     */
    public JSONObject fetchOneJSON() throws SQLException {
        Record rec = fetchOne();
        return rec != null ? rec.toJSON() : null;
    }

    /**
     * This method works like <code>fetchOne</code> except that it adds the record columns to an existin JSON object
     * passed in.
     *
     * @param obj
     * @return the JSON object passed in
     * @throws SQLException
     */
    public JSONObject fetchOneJSON(JSONObject obj) throws SQLException {
        Record rec = fetchOne();
        if (rec != null)
            rec.addToJSON(obj);
        return obj;
    }

    /**
     * Read in all of the records and close the cursor.
     * Records can be updated or deleted.
     *
     * If no records are found, an empty list is returned.
     *
     * @return
     * @throws SQLException
     */
    public List<Record> fetchAll() throws SQLException {
        List<Record> r = new ArrayList<>();
        Record rec;
        while (null != (rec=next()))
            r.add(rec);
        close();
        lastRec = null;
        return r;
    }

    /**
     * This method does the same thing as <code>fetchAll</code> except that it return a
     * JSON array representing all of the records.
     *
     * @return
     * @throws SQLException
     *
     * @see #fetchAll()
     */
    public JSONArray fetchAllJSON() throws SQLException {
        return Record.toJSONArray(fetchAll());
    }


    /**
     * After this, you can edit and delete records.  You just can't read any more.
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
     * Since this class implement AutoCloseable, this method is rarely needed.
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

    /**
     * Set the value of a column in the current record.
     * This method should not be used with dates or times.
     *
     * @param name the column name
     * @param val the value to set.  Can be any type.
     * @return
     *
     * @see Record#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setDateTime(String, java.util.Date)
     */
    public Cursor set(String name, Object val) {
        lastRec.cols.put(name.toLowerCase(), val);
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
    public Cursor setDateOnly(String name, java.util.Date val) {
        lastRec.cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
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
    public Cursor setTime(String name, long dat) {
        if (dat == 0) {
            lastRec.cols.put(name.toLowerCase(), null);
            return this;
        }
        java.util.Date val = new java.util.Date(dat);
        lastRec.cols.put(name.toLowerCase(), new java.sql.Time(val.getTime()));
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
     * @see Cursor#set(String, Object)
     * @see #set(String, Object)
     * @see #setDateTime(String, java.util.Date)
     * @see #setTime(String, long)
     */
    public Cursor setDateOnly(String name, int dat) {
        if (dat == 0) {
            lastRec.cols.put(name.toLowerCase(), null);
            return this;
        }
        int y = dat / 10000;
        int m = (dat % 10000) / 100;
        int d = dat % 100;
        java.util.Date val = new GregorianCalendar(y, m-1, d).getTime();
        lastRec.cols.put(name.toLowerCase(), new java.sql.Date(val.getTime()));
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
    public Cursor setDateTime(String name, java.util.Date val) {
        lastRec.cols.put(name.toLowerCase(), val == null ? null : new java.sql.Timestamp(val.getTime()));
        return this;
    }

    /**
     * Set the date and time value of a column in the record.
     *
     * @param name the column name
     * @param val the number of milliseconds since 1970 UTC
     * @return
     *
     * @see Cursor#set(String, Object)
     * @see #setDateOnly(String, java.util.Date)
     * @see #setTime(String, long)
     * @see #set(String, Object)
     */
    public Cursor setDateTime(String name, long val) {
        lastRec.cols.put(name.toLowerCase(), val == 0 ? null : new java.sql.Timestamp(val));
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
     * @see Record#get(String)
     * @see #getShort(String)
     * @see #getLong(String)
     * @see #getString(String)
     * etc.
     */
    public Object get(String cname) throws SQLException {
        cname = cname.toLowerCase();
        if (lastRec.cols.containsKey(cname))
            return lastRec.cols.get(cname);
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
     * @see Record#getShort(String)
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
     * @see Record#getInt(String)
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
     * @see Record#getLong(String)
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
     * @see Record#getFloat(String)
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
     * @see Record#getDouble(String)
     */
    public Double getDouble(String cname) throws SQLException {
        return (Double) get(cname);
    }

    /**
     * Return the <code>java.util.Date</code> value of the named column.
     * That is a date without a time.
     * A <code>null</code> is returned on <code>null</code> valued columns.
     *
     * @param cname
     * @return
     * @throws SQLException
     *
     * @see #getDateAsInt(String)
     * @see Record#getDateOnly(String)
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
     * @see #getDateOnly(String)
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
     * @see Record#getString(String)
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
     * @see Record#getChar(String)
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
     * @see Record#get(String)
     */
    public byte [] getByteArray(String cname) throws SQLException {
        return (byte []) get(cname);
    }

}