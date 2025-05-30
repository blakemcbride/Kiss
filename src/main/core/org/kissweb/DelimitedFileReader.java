/*
*  Copyright (c) 2006-2015 Blake McBride (blake@mcbridemail.com)
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

package org.kissweb;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Read and parse a CSV file.
 * This class implements the AutoCloseable interface.
 * <br><br>
 * This code attempts to conform to RFC 4180.
 * <br><br>
 * Fields can be accessed sequentially, by index, or by field name.
 * Field names are obtained at the first record of the CSV file.
 *
 */
@SuppressWarnings("unchecked")
public class DelimitedFileReader implements AutoCloseable {

    private char delimiter;
    private String delimiterString;
    private char quote;
    private final ArrayList<String> lineValues = new ArrayList();
    private int fieldPos = 0;
    private int fieldCountCheck = -1;
    private BufferedReader fr;
    private File fyle;
    private String originalRow;
    private HashMap<String,Integer> nameMap;

    /**
     * Open an existing CSV file with the specified delimiter and quote character.
     *
     * @param f the file to open
     * @param delimiter the delimiter character to use for separating fields
     * @param quote the quote character to use for quoted fields
     * @throws FileNotFoundException if the file does not exist
     */
    public DelimitedFileReader(File f, char delimiter, char quote) throws FileNotFoundException {
        fyle = f;
        fr = new BufferedReader(new FileReader(f));
        this.delimiter = delimiter;
        delimiterString = Character.toString(delimiter);
        this.quote = quote;
    }

    /**
     * Open an existing CSV file with the default delimiter (,) and quote (") characters,
     *
     * @param f the file to open
     * @throws FileNotFoundException if the file does not exist
     */
    public DelimitedFileReader(File f) throws FileNotFoundException {
        this(f, ',', '"');
    }

    /**
     * Open an existing CSV file with the default delimiter (,) and quote (") characters,
     *
     * @param name the name/path of the file to open
     * @throws FileNotFoundException if the file does not exist
     */
    public DelimitedFileReader(String name) throws FileNotFoundException {
        this(new File(name), ',', '"');
    }

    /**
     * Open an existing CSV file with the specified delimiter and quote character.
     *
     * @param name the name/path of the file to open
     * @param delimiter the delimiter character to use for separating fields
     * @param quote the quote character to use for quoted fields
     * @throws FileNotFoundException if the file does not exist
     */
    public DelimitedFileReader(String name, char delimiter, char quote) throws FileNotFoundException {
        this(new File(name), delimiter, quote);
    }

    /**
     * Close the CSV file.
     * <br><br>
     * Note that this method will be called automatically if the try-with-resource Java facility is utilized.
     */
    @Override
    public void close() {
        try {
            if (fr != null)
                fr.close();
        } catch (IOException ignored) {
        } finally {
            fr = null;
        }
    }

    /**
     * Gets the double value of field number i.
     *
     * @param i field number, index origin 0
     * @return the double value of the field, or 0.0 if parsing fails
     */
    public double getDouble(int i) {
        return Double.parseDouble(getString(i));
    }

    /**
     * Get the double value of the field indicated by fld.
     *
     * @param fld the field name to retrieve
     * @return the double value of the field, or 0.0 if field not found or parsing fails
     */
    public double getDouble(String fld) {
        if (nameMap == null)
            return 0.0;
        Integer i = nameMap.get(fld.toLowerCase());
        if (i == null)
            return 0.0;
        try {
            return Double.parseDouble(getString(i));
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }

    /**
     * Move the record pointer back to the beginning of the CSV file.  If the first record are the field names it will need to be re-read.
     *
     */
    public void moveToStart() {
        try {
            lineValues.clear();
            fr.close();
            fr = new BufferedReader(new FileReader(fyle));
        } catch (IOException ex) {//it was just there! so log it only
            Logger.getLogger(DelimitedFileReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * Skip the next line in the CSV file.
     *
     * @throws IOException if an I/O error occurs
     * @throws Exception if an error occurs during line processing
     */
    public void skipLine() throws IOException, Exception {
        fr.readLine();
        originalRow = null;
    }

    /**
     * Read the first row and map column title names to indexes.
     * Column titles are case-insensitive.
     *
     */
    public void readHeader() {
        originalRow = null;
        lineValues.clear();
        nameMap = new HashMap<String, Integer>();
        try {
            nextLine();
            for (int i = 0; i < lineValues.size(); i++) {
                String name = getString(i);
                if (name != null && !name.isEmpty())
                    nameMap.put(name.toLowerCase(), i);
            }
        } catch (Exception ignored) {

        }
    }

    /**
     * Checks if the given column header exists in the delimited file.
     * Note that this only works after <code>readHeader()</code> has been called.
     *
     * @param  header   the header to check for
     * @return          true if the header exists, false otherwise
     */
    public boolean hasHeader(String header) {
        return nameMap.containsKey(header.toLowerCase());
    }

    /**
     * Checks if the given column titles exist in the delimited file.
     * Note that this only works after <code>readHeader()</code> has been called.
     *
     * @param  colNames   an array of column names to check
     * @return             a list of column names that do not exist in the nameMap, or null if all column names exist
     */
    public ArrayList<String> checkHeaders(String [] colNames) {
        ArrayList<String> res = null;
        for (String colName : colNames) {
            if (!nameMap.containsKey(colName.toLowerCase())) {
                if (res == null)
                    res = new ArrayList<>();
                res.add(colName);
            }
        }
        return res;
    }

    /**
     * Return the original row / line last read.
     *
     * @return the original unparsed line as a string
     */
    public String getRow() {
        return originalRow;
    }

    private enum State {
        NORMAL,
        QUOTE,
        BEFORE_DELIMITER,
        AFTER_DELIMITER
    }

    /**
     * Read and parse the next row in the CSV file.
     *
     * @return true if another line was found, false if end-of-file
     * @throws IOException if an I/O error occurs
     * @throws Exception if field count validation fails or other parsing errors occur
     */
    public boolean nextLine() throws IOException, Exception {
        State state = State.NORMAL;
        lineValues.clear();
        final boolean ignoreSpaces = false;  // ignore spaces around the delimiter

        originalRow = null;
        String line = fr.readLine();

//		System.out.println(line);
        while (true) {
            if (line == null)
                return false;
            if (!line.trim().isEmpty()  &&  !line.replaceAll(delimiterString, "").trim().isEmpty())
                break;
            line = fr.readLine();
        }

        originalRow = line;
        StringBuilder sb = new StringBuilder();
        do {
            if (state == State.QUOTE) {
                // continuing a multi-line quoted section, re-append newline
                sb.append("\n");
                line = fr.readLine();
                if (line == null)
                    break;
                originalRow += "\n" + line;
            }
            int sz = line.length();
            for (int i=0 ; i < sz ; i++) {
                char c = line.charAt(i);
                switch (state) {
                    case QUOTE:
                        if (c != quote)
                            sb.append(c);
                        else if (sz > (i + 1) && line.charAt(i + 1) == quote) {
                            sb.append(c);
                            i++;  //  skip second quote
                        } else {
                            state = State.BEFORE_DELIMITER;
                            lineValues.add(sb.toString());
                            sb.setLength(0); // start work on next token
                        }
                        break;
                    case BEFORE_DELIMITER:
                        if (c == delimiter)
                            state = State.AFTER_DELIMITER;
                        break;
                    case AFTER_DELIMITER:
                        if (c == quote)
                            state = State.QUOTE;
                        else if (c == delimiter) {
                            lineValues.add("");
                            state = State.AFTER_DELIMITER;
                        } else if (!ignoreSpaces || c != ' ') {
                            sb.append(c);
                            state = State.NORMAL;
                        }
                        break;
                    case NORMAL:
                        if (c == delimiter) {
                            if (ignoreSpaces)
                                lineValues.add(sb.toString().trim());
                            else
                                lineValues.add(sb.toString());
                            sb.setLength(0); // start work on next token
                            state = State.AFTER_DELIMITER;
                        } else if (c == quote) {
                            if (sb.length() == 0 || sb.toString().trim().isEmpty()) {
                                state = State.QUOTE;
                                sb.setLength(0);
                            } else
                                sb.append(c);
                        } else
                            sb.append(c);
                        break;
                }
            }
        } while (state == State.QUOTE);
        if (state != State.BEFORE_DELIMITER)
            if (ignoreSpaces)
                lineValues.add(sb.toString().trim());
            else
                lineValues.add(sb.toString());

        if (fieldCountCheck > 0 && fieldCountCheck != lineValues.size())
            throw new Exception("Bad number of records read.  Expected " + fieldCountCheck + " got " + lineValues.size());

        fieldPos = 0;

        return true;
    }

    /**
     * Returns the number of columns / fields in the current row / record.
     *
     * @return the number of fields in the current record
     */
    public int size() {
        return lineValues.size();
    }

    /**
     * Returns the next String field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return the next string field value
     */
    public String nextString() {
        return getString(fieldPos++);
    }

    /**
     * Return the String at the indicated field number.
     *
     * @param item field number, index origin 0
     * @return the string value at the specified field index, or empty string if index is out of bounds
     */
    public String getString(int item) {
        if (item >= lineValues.size())
            return "";
        String s = lineValues.get(item);
        if (s != null)
            s = s.trim();
        return s;
    }

    /**
     * Return the String at the field with the indicated label.
     *
     * @param fld the field name to retrieve
     * @return the string value at the specified field name, or empty string if field not found
     *
     * @see #readHeader()
     */
    public String getString(String fld) {
        if (nameMap == null)
            return "";
        final Integer item = nameMap.get(fld.toLowerCase());
        if (item == null || item >= lineValues.size())
            return "";
        String s = lineValues.get(item);
        if (s != null)
            s = s.trim();
        return s;
    }

    /**
     * Returns the next int field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return the next integer field value, or 0 if parsing fails
     */
    public int nextInt() {
        try {
            return Integer.parseInt(nextString());
        } catch (NumberFormatException numberFormatException) {
            return 0;
        }
    }

    /**
     * Returns the next double field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return the next double field value, or 0.0 if parsing fails
     */
    public double nextDouble() {
        try {
            return Double.parseDouble(nextString());
        } catch (NumberFormatException numberFormatException) {
            return 0;
        }
    }

    /**
     * Return the int at the indicated field number.
     *
     * @param item field number, index origin 0
     * @return the integer value at the specified field index, or 0 if parsing fails
     */
    public int getInt(int item) {
        try {
            return Integer.parseInt(getString(item));
        } catch (NumberFormatException numberFormatException) {
            return 0;
        }
    }

    /**
     * Return the int at the field with the indicated label.
     *
     * @param fld the field name to retrieve
     * @return the integer value at the specified field name, or 0 if field not found or parsing fails
     *
     * @see #readHeader()
     */
    public int getInt(String fld) {
        if (nameMap == null)
            return 0;
        Integer item = nameMap.get(fld.toLowerCase());
        if (item == null)
            return 0;
        try {
            return Integer.parseInt(getString(item));
        } catch (NumberFormatException numberFormatException) {
            return 0;
        }
    }

    /**
     * Returns the int date at the next sequential field.
     *
     * @return date as int YYYYMMDD
     */
    public int nextDate() {
        return DateUtils.parse(nextString());
    }

    /**
     * Return the date as an int at the indicated field number.
     *
     * @param item field number, index origin 0
     * @return date as int YYYYMMDD
     */
    public int getDate(int item) {
        return DateUtils.parse(getString(item));
    }

    /**
     * Return the date as an int at the indicated field label.
     *
     * @param fld the field name to retrieve
     * @return date as int YYYYMMDD
     *
     * @see #readHeader()
     */
    public int getDate(String fld) {
        return DateUtils.parse(getString(fld));
    }

    /**
     * Optional method used to set the expected number of fields in each row.
     * If this is set and the number of columns is not what is expected, nextLine() will throw an exception.
     *
     * @param check the expected number of fields per row
     */
    public void setFieldCountCheck(int check) {
        fieldCountCheck = check;
    }

    /**
     * Test method that reads a CSV file and outputs it in quoted format.
     *
     * @param args command line arguments
     * @throws Exception if file cannot be read
     */
    public static void main(String [] args) throws Exception {
        DelimitedFileReader dfr = new DelimitedFileReader("test.csv");
        while (dfr.nextLine()) {
            int sz = dfr.size();
            for (int i=0 ; i < sz ; i++) {
                if (i != 0)
                    System.out.print(",");
                System.out.print("\"" + dfr.getString(i) + "\"");
            }
            System.out.println();
        }
        dfr.close();
    }
}
