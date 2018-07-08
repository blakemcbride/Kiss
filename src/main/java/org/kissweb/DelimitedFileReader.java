/*
*  Copyright (c) 2006-2015 Blake McBride (blake@mcbride.name)
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
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Read and parse a CSV file.
 * This class implements the AutoCloseable interface.
 *
 * Fields can be accessed sequentially, by index, or by field name.  Field names are obtained at the first record of the CSV file.
 *
 */
@SuppressWarnings("unchecked")
public class DelimitedFileReader implements AutoCloseable {

    private char delimiter;
    private String delimeterString;
    private char quote;
    private ArrayList<String> lineValues = new ArrayList();
    private int fieldPos = 0;
    private int fieldCountCheck = -1;
    private BufferedReader fr;
    private File fyle;
    private String originalRow;
    private Hashtable<String,Integer> nameMap;

    /**
     * Open an existing CSV file with the specified delimiter and quote character.
     *
     * @param f
     * @param delimiter
     * @param quote
     * @throws FileNotFoundException
     */
    public DelimitedFileReader(File f, char delimiter, char quote) throws FileNotFoundException {
        fyle = f;
        fr = new BufferedReader(new FileReader(f));
        this.delimiter = delimiter;
        delimeterString = Character.toString(delimiter);
        this.quote = quote;
    }

    /**
     * Open an existing CSV file with the default delimiter (,) and quote (") characters,
     *
     * @param f
     * @throws FileNotFoundException
     */
    public DelimitedFileReader(File f) throws FileNotFoundException {
        this(f, ',', '"');
    }

    /**
     * Open an existing CSV file with the default delimiter (,) and quote (") characters,
     *
     * @param name
     * @throws FileNotFoundException
     */
    public DelimitedFileReader(String name) throws FileNotFoundException {
        this(new File(name), ',', '"');
    }

    /**
     * Open an existing CSV file with the specified delimiter and quote character.
     *
     * @param name
     * @param delimiter
     * @param quote
     * @throws FileNotFoundException
     */
    public DelimitedFileReader(String name, char delimiter, char quote) throws FileNotFoundException {
        this(new File(name), delimiter, quote);
    }

    /**
     * Close the CSV file.
     *
     * Note that this method will be called automatically if the try-with-resource Java facility is utilized.
     */
    public void close() {
        try {
            if (fr != null)
                fr.close();
        } catch (IOException e) {
        } finally {
            fr = null;
        }
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            close();        // close open files
        } finally {
            super.finalize();
        }
    }

    /**
     * Gets the double value of field number i.
     *
     *
     * @param i field number, index origin 0
     * @return
     */
    public double getDouble(int i) {
        try {
            return Double.parseDouble(getString(i));
        } catch (NumberFormatException e) {
            System.out.println(lineValues);
            throw e;
        }
    }

    /**
     * Get the double value of the field indicated by fld.
     *
     * @param fld
     * @return
     */
    public double getDouble(String fld) {
        if (nameMap == null)
            return 0.0;
        int i = nameMap.get(fld);
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
     * @throws IOException
     * @throws Exception
     */
    public void skipLine() throws IOException, Exception {
        fr.readLine();
        originalRow = null;
    }

    /**
     * Read the first row and map column title names to indexes.
     *
     * @throws IOException
     * @throws Exception
     */
    public void readHeader() {
        originalRow = null;
        lineValues.clear();
        nameMap = new Hashtable<String, Integer>();
        try {
            nextLine();
            for (int i = 0; i < lineValues.size(); i++) {
                String name = getString(i);
                if (name != null && name.length() > 0)
                    nameMap.put(name, i);
            }
        } catch (Exception e) {

        }
    }

    /**
     * Return the original row / line last read.
     *
     * @return
     */
    public String getRow() {
        return originalRow;
    }

    /**
     * Read and parse the next row in the CSV file.
     *
     * @return true if another line was found, false of end-of-file
     * @throws IOException
     * @throws Exception
     */
    public boolean nextLine() throws IOException, Exception {
        lineValues.clear();

        originalRow = null;
        String line = fr.readLine();

//		System.out.println(line);
        while (true) {
            if (line == null)
                return false;
            if (!line.trim().isEmpty()  &&  !line.replaceAll(delimeterString, "").trim().isEmpty())
                break;
            line = fr.readLine();
        }

        originalRow = line;
        StringBuilder sb = new StringBuilder();
        boolean inQuotes = false;
        do {
            if (inQuotes) {
                // continuing a quoted section, reappend newline
                sb.append("\n");
                line = fr.readLine();
                if (line == null)
                    break;
                originalRow += "\n" + line;
            }
            for (int i = 0; i < line.length(); i++) {
                char c = line.charAt(i);
                if (c == quote)
                    // this gets complex... the quote may end a quoted block, or escape another quote.
                    // do a 1-char lookahead:
                    if (inQuotes // we are in quotes, therefore there can be escaped quotes in here.
                            && line.length() > (i + 1) // there is indeed another character to check.
                            && line.charAt(i + 1) == quote) { // ..and that char. is a quote also.
                        // we have two quote chars in a row == one quote char, so consume them both and
                        // put one on the token. we do *not* exit the quoted text.
                        sb.append(line.charAt(i + 1));
                        i++;
                    } else {
                        inQuotes = !inQuotes;
                        // the tricky case of an embedded quote in the middle: a,bc"d"ef,g
                        if (i > 2 //not on the beginning of the line
                                && line.charAt(i - 1) != delimiter //not at the beginning of an escape sequence
                                && line.length() > (i + 1)
                                && line.charAt(i + 1) != this.delimiter //not at the end of an escape sequence
                                )
                            sb.append(c);
                    }
                else if (c == delimiter && !inQuotes) {
                    lineValues.add(sb.toString());
                    sb.setLength(0); // start work on next token
                } else
                    sb.append(c);
            }
        } while (inQuotes);
        lineValues.add(sb.toString());

        if (fieldCountCheck > 0 && fieldCountCheck != lineValues.size())
            throw new Exception("Bad number of records read.  Expected " + fieldCountCheck + " got " + lineValues.size());

        fieldPos = 0;

        return true;
    }

    /**
     * Returns the number of columns / fields in the current row / record.
     *
     * @return
     */
    public int size() {
        return lineValues.size();
    }

    /**
     * Returns the next String field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return
     */
    public String nextString() {
        return getString(fieldPos++);
    }

    /**
     * Return the String at the indicated field number.
     *
     * @param item field number, index origin 0
     * @return
     */
    public String getString(int item) {
        if (item >= lineValues.size())
            return "";
        return lineValues.get(item);
    }

    /**
     * Return the String at the field with the indicated label.
     *
     * @param fld
     * @return
     *
     * @see #readHeader()
     */
    public String getString(String fld) {
        if (nameMap == null)
            return "";
        int item = nameMap.get(fld);
        if (item >= lineValues.size())
            return "";
        return lineValues.get(item);
    }

    /**
     * Returns the next int field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return
     */
    public int nextInt() {
        try {
            return Integer.parseInt(nextString());
        } catch (NumberFormatException numberFormatException) {
            return 0;
        }
    }

    /**
     * Returns the next String field.
     * This method is useful when reading the fields in sequence rather than by index.
     *
     * @return
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
     * @return
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
     * @param fld
     * @return
     *
     * @see #readHeader()
     */
    public int getInt(String fld) {
        if (nameMap == null)
            return 0;
        int item = nameMap.get(fld);
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
     * @param fld
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
     * @param check
     */
    public void setFieldCountCheck(int check) {
        fieldCountCheck = check;
    }
}
