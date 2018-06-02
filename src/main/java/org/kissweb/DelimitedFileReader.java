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
import java.util.logging.Level;
import java.util.logging.Logger;


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

	public DelimitedFileReader(File f, char delimiter, char quote) throws FileNotFoundException {
		fyle = f;
		fr = new BufferedReader(new FileReader(f));
		this.delimiter = delimiter;
		delimeterString = Character.toString(delimiter);
		this.quote = quote;
	}

	public DelimitedFileReader(File f) throws FileNotFoundException {
		this(f, ',', '"');
	}

	public DelimitedFileReader(String name) throws FileNotFoundException {
		this(new File(name), ',', '"');
	}

	public DelimitedFileReader(String name, char delimiter, char quote) throws FileNotFoundException {
		this(new File(name), delimiter, quote);
	}
	
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
	public double getDouble(int i) {
		try {
			return Double.parseDouble(getString(i));
		} catch (NumberFormatException e) {
			System.out.println(lineValues);
			throw e;
		}
	}

	public void moveToStart() {
		try {
			lineValues.clear();
			fr.close();
			fr = new BufferedReader(new FileReader(fyle));
		} catch (IOException ex) {//it was just there! so log it only
			Logger.getLogger(DelimitedFileReader.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	public void skipLine() throws IOException, Exception {
		fr.readLine();
		originalRow = null;
	}
	
	public String getRow() {
		return originalRow;
	}

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

	public int size() {
		return lineValues.size();
	}

	public String nextString() {
		return getString(fieldPos++);
	}

	public String getString(int item) {
		if (item >= lineValues.size())
			return "";
		return lineValues.get(item);
	}

	public int nextInt() {
		try {
			return Integer.parseInt(nextString());
		} catch (NumberFormatException numberFormatException) {
			return 0;
		}
	}

	public double nextDouble() {
		try {
			return Double.parseDouble(nextString());
		} catch (NumberFormatException numberFormatException) {
			return 0;
		}
	}

	public int getInt(int item) {
		try {
			return Integer.parseInt(getString(item));
		} catch (NumberFormatException numberFormatException) {
			return 0;
		}
	}

	public void setFieldCountCheck(int check) {
		fieldCountCheck = check;
	}
}
