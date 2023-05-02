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

import java.io.FileWriter;
import java.io.IOException;

/**
 * Class dealing with the creation of standard comma delimited or CSV files.
 * This class implements the AutoCloseable interface.
 *
 */
public class DelimitedFileWriter implements AutoCloseable {

	private FileWriter fw;
	private int fcount = 0;
	private String quote;
	private String delimiter;
	private int itemsPerRow;
	private String dateFmt = "MM/dd/yyyy";

    /**
     * Create a new CSV file us ing the specified delimiter character and the default quote character (").
     *
     * @param name
     * @param delim
     * @throws IOException
     */
	public DelimitedFileWriter(String name, char delim) throws IOException {
		this(name, false, -1, delim, '"');
	}

    /**
     * Create a new or append to an existing CSV file.
     *
     * @param name
     * @param append
     * @param itemsPerRow create a standard number of columns-per-row or specify -1 to indicate that you will explicitly output what is needed
     * @param delimiter
     * @param quote
     * @throws IOException
     */
	public DelimitedFileWriter(String name, boolean append, int itemsPerRow, char delimiter, char quote) throws IOException {
		fw = new FileWriter(name, append);
		this.delimiter = delimiter + "";
		this.quote = quote + "";
		this.itemsPerRow = itemsPerRow;
	}

    /**
     * Create or append to a standard CSV file using the standard delimiter (,) and quote (") characters.
     *
     * @param name
     * @param append
     * @param itemsPerRow create a standard number of columns-per-row or specify -1 to indicate that you will explicitly output what is needed
     * @throws IOException
     */
	public DelimitedFileWriter(String name, boolean append, int itemsPerRow) throws IOException {
		this(name, append, itemsPerRow, ',', '"');
	}

    /**
     * Create or append to a standard CSV file using the standard delimiter (,) and quote (") characters.
     *
     * @param name
     * @param append
     * @throws IOException
     */
	public DelimitedFileWriter(String name, boolean append) throws IOException {
		this(name, append, -1, ',', '"');
	}

    /**
     * Create a standard CSV file using the standard delimiter (,) and quote (") characters.
     *
     * @param name
     * @throws IOException
     */
	public DelimitedFileWriter(String name) throws IOException {
		this(name, false, -1, ',', '"');
	}

    /**
     * Set the format of date output.  Default is mm/dd/yyyy
     *
     * @param fmt
     *
     * @see DateUtils#format(String, int)
     */
	public void setDateFormat(String fmt) {
		dateFmt = fmt;
	}

    /**
     * Close the CSV file being created.
     *
     * Note that this method will be called automatically if the try-with-resource Java facility is utilized.
     */
        @Override
	public void close() {
	    try {
                if (fw != null) {
                    fw.flush();
                    fw.close();
                }
            } catch (IOException e) {
            } finally {
	        fw = null;
            }
	}

	private void writeWithQuote(String x) throws IOException {
		if (x == null)
			x = "";
		if (fcount++ > 0)
			fw.write(delimiter);
		fw.write(quote + x.replaceAll(quote, quote + quote) + quote);
	}

	private void writeWithQuoteUpperCase(String x) throws IOException {
		if (x == null)
			x = "";
		if (fcount++ > 0)
			fw.write(delimiter);
		fw.write(quote + x.toUpperCase().replaceAll(quote, quote + quote) + quote);
	}

	private void write(String x) throws IOException {
		if (x == null)
			x = "";
		if (fcount++ > 0)
			fw.write(delimiter);
		if (x.contains(delimiter)  ||  x.contains(quote))
			fw.write(quote + x.replaceAll(quote, quote + quote) + quote);
		else
			fw.write(x.replaceAll(quote, quote + quote));
	}

    /**
     * Write a String field correctly dealing with embedded quotes and delimiters
     *
     * @param x
     * @throws Exception
     */
	public void writeField(String x) throws Exception {
		write(x);
	}

    /**
     * Write a String field limiting the size of the String.
     * Deals correctly with embedded quotes and delimiters.
     *
     * @param charLimit max size of the string
     * @param x the string
     * @throws Exception
     */
	public void writeField(int charLimit, String x) throws Exception {
		if (x == null)
			x = "";
		if (x.length() > charLimit)
			x = StringUtils.rightStrip(x.substring(0, charLimit));
		write(x);
	}

    /**
     * Write a String field forcing the string to uppercase and a quoted output.
     *
     * @param x
     * @throws Exception
     */
	public void writeFieldUpperCase(String x) throws Exception {
		writeWithQuoteUpperCase(x);
	}

    /**
     * Output a floating point number with a maximum of two decimal places.
     *
     * @param x
     * @throws Exception
     */
	public void writeField(double x) throws Exception {
		x *= 100;
		x = Math.round(x);
		x /= 100;
		write(x + "");
	}

    /**
     * Output an integer column.
     *
     * @param x
     * @throws Exception
     */
	public void writeField(int x) throws Exception {
		write(x + "");
	}

    /**
     * Output an integer field using an empty field if the number is zero.
     *
     * @param x
     * @throws Exception
     */
	public void writeNoZero(int x) throws Exception {
		write(x == 0 ? "" : x + "");
	}

    /**
     * Output a single character field.  Quotes and delimiters are correctly handled.
     *
     * @param x
     * @throws Exception
     */
	public void writeField(char x) throws Exception {
		write(x + "");
	}

    /**
     * Output a numeric date in the default format.
     *
     * @param x the date as an integer YYYYMMDD
     * @throws Exception
     *
     * @see #dateFmt
     */
	public void writeDate(int x) throws Exception {
		write(DateUtils.format(dateFmt, x));
	}

    /**
     * End the current row.  Throws an exception if all of the columns weren't specified.
     *
     * @throws IOException
     * @throws Exception
     */
	public void endRecord() throws IOException, Exception {
		fw.write("\r\n");
		if (itemsPerRow > 0 && itemsPerRow != fcount)
			throw new Exception("Incorrect field count.  Not all items were written to the file.  Operation cancelled.");
		fcount = 0;
	}

    /**
     * Returns the number of columns output already on the current row.
     *
     * @return
     */
	public int getFieldCount() {
		return fcount;
	}
}
