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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.text.Normalizer;

/**
 * This class contains many methods used to manipulate <code>String</code>s.
 *
 * @author Blake McBride
 */
public class StringUtils {
    
    /**
     * This method provides the ability to get a substring of a string by 
     * providing a starting location and a length.
     * 
     * @param str the String to be substring'ed
     * @param from index of the starting position (starting at 0)
     * @param n number of characters to take
     * @return the sub-string
     */
    public static String substring(String str, int from, int n) {
        return str.substring(from, n + from);
    }

    /**
     * An APL-like TAKE function. Assures that the string is <code>n</code> characters long.
     * <br><br>
     * If the string is shorter than <code>n</code> it is padded by spaces.
     * <br><br>
     * If the string is longer than <code>n</code> it is cut short.
     * <br><br>
     * If <code>n</code> is negative, the same thing occurs but it starts at the back of the string.
     * Thus padding or cutting of the string occur at the start of the string.
     *
     * @param s the string to take characters from
     * @param n the number of characters to take
     * @return the resulting string
     *
     * @see #drop(String, int)
     */
    public static String take(String s, int n) {
		if (s == null)
			return null;
        int len = s.length();
        if (len == n)
            return s;
        if (n >= 0) {
            if (n < len)
                return s.substring(0, n);
            StringBuilder sb = new StringBuilder(s);
            for (n -= len; n-- > 0;)
                sb.append(' ');
            return sb.toString();
        } else {
            n = -n;
            if (n < len)
                return drop(s, len - n);
            StringBuilder sb = new StringBuilder();
            for (n -= len; n-- > 0;)
                sb.append(' ');
            sb.append(s);
            return sb.toString();
        }
    }
	
	/**
	 * Take n characters from a char array.
	 *
	 * @param s the char array to take characters from
	 * @param n the number of characters to take
	 * @return the resulting char array
	 */
	public static char [] take(char [] s, int n) {
		int i, si=0, l;
		boolean flg = false;
		
		if (n >= 0) {
			for (i=0 ; i < n ; i++)
				if (s[i] == '\0' || flg) {
					s[i] = ' ';
					flg = true;
				}
			s[n] = '\0';
			return s;
		}
		l = strLength(s);
		n = -n;
		if (l == n)
			return s;
		if (l < n) {
			movmem(s, 0, s, n-l, l+1);
			for (i=0 ; i < n - l ; i++)
				s[i] = ' ';
			return s;
		}
		movmem(s, l-n, s, 0, n + 1);
		return s;
	}
	
	/**
	 * Calculate the length of a null-terminated char array.
	 *
	 * @param v the char array to measure
	 * @return the length of the array up to the null terminator
	 */
	public static int strLength(char [] v) {
		int i=0;
		while (i < v.length  &&  v[i] != '\0')
			i++;
		return i;
	}
	
	/**
	 * Moves characters from one array to another.  The two arrays may be the same array.  Correctly handles overlapping regions.
	 * 
	 * @param f  the from or source array
	 * @param fi the index into f to start at
	 * @param t the to or destination array
	 * @param ti the starting index into t
	 * @param n  the number of characters to move
	 * @return  the destination array
	 */
	public static char [] movmem(char [] f, int fi, char [] t, int ti, int n) {
		if (t == f  &&  fi == ti  ||  n <= 0)
			return t;
		if (f != t)
			while (n-- > 0)
				t[ti++] = f[fi++];
		else if (fi > ti)
			while (n-- > 0)
				t[ti++] = f[fi++];
		else {
			ti += n - 1;
			fi += n - 1;
			while (n-- > 0)
				t[ti--] = f[fi--];
		}
		return t;
	}

    /**
     * An APL-like DROP function. Drops <code>n</code> characters from the string.
     * <br><br>
     * If <code>n</code> is positive, <code>n</code> characters get dropped from the beginning of the string.
     * <br><br>
     * If <code>n</code> is negative, <code>n</code> characters get dropped from the end of the string.
     * <br><br>
     * If <code>n</code> is greater than the length of the string, "" is returned.
     *
     * @param s the string to drop characters from
     * @param n the number of characters to drop
     * @return the resulting string
     *
     * @see #take(String, int)
     */
    public static String drop(String s, int n) {
        if (n == 0 || s == null)
            return s;
        int len = s.length();
        if (n >= len || -n >= len)
            return "";
        if (n > 0)
            return s.substring(n);
        return s.substring(0, len + n);
    }

    /**
     * Strip the spaces of the end of a string.
     *
     * @param s the string to strip
     * @return the string with trailing spaces removed
     */
    public static String rightStrip(String s) {
		if (s == null)
			return null;
        return s.replaceAll("\\s+$", "");
    }

	/**
	 * Strip the spaces at the beginning of a string.
	 *
	 * @param s the string to strip
	 * @return the string with leading spaces removed
	 */
	public static String leftStrip(String s) {
		if (s == null)
			return null;
        return s.replaceAll("^\\s+", "");
    }

	/**
	 * Strip the spaces on both ends of a string.
	 *
	 * @param s the string to strip
	 * @return the string with leading and trailing spaces removed
	 */
	public static String centerStrip(String s) {
		if (s == null)
			return null;
        return s.trim();
    }

	/**
	 * C-like sprintf method.  Format a string according to the format inserting the arguments
     * as placed in the format string.
     *
	 * @param fmt format string
	 * @param arg1 consecutive values used by the format string
	 * @return the formatted string
     *
     * @see Formatter
	 */
    public static String sprintf(String fmt, Object ... arg1) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        pw.printf(fmt, arg1);
        pw.flush();
        sw.flush();
        return sw.toString();
    }

    /**
     * Join rows or a String array into a String separated by a delimiter.
     *
     * @param strings the array of strings to join
     * @param separator the separator to use between strings
     * @return the joined string
     */
    public static String join(String[] strings, String separator) {
        StringBuilder sb = new StringBuilder();
        for (String str : strings) {
            if (sb.length() > 0)
                sb.append(separator);
            sb.append(str);
        }
        return sb.toString();
    }

    /**
     * String null or empty test.  Returns true if string is null or empty.
     *
     * @param str the string to test
     * @return true if the string is null or empty
     */
	public static boolean isEmpty(final String str) {
		return str == null || str.isEmpty();
	}

	/**
	 * Convert <code>String</code> to <code>Character</code> correctly handling nulls and zero length strings.
	 *
	 * @param s the string to convert
	 * @return the first character of the string, or null if the string is null or empty
	 */
	public static Character stringToCharacter(String s) {
		return s == null || s.isEmpty() ? null : s.charAt(0);
	}

	/**
	 * Convert <code>Character</code> to <code>String</code> correctly handling nulls.
	 *
	 * @param c the character to convert
	 * @return the string representation of the character, or null if the character is null
	 */
	public static String characterToString(Character c) {
		return c == null ? null : c.toString();
	}

	/**
	 * Splits a string into an array of substrings based on a specified delimiter character.
	 * Unlike the Java version, this one correctly handles empty strings.
	 *
	 * @param  s  the string to be split
	 * @param  r  the delimiter regex
	 * @return    an array of substrings
	 */
	public static String [] split(String s, String r) {
		if (s == null || s.isEmpty())
			return new String[0];
		return s.split(r);
	}

	/**
	 * Converts HTML text to plain text.
	 * Also corrects some Unicode characters.
	 *
	 * @param  html  the HTML text to be converted
	 * @return       the converted plain text
	 */
	public static String htmlToText(String html) {
		if (html == null || html.isEmpty())
			return "";
		return html
				.replaceAll("<br *[^>]*>", "\n")

				// iPhone uses Unicode!  Convert to ASCII.
				.replaceAll("\u2018", "'")
				.replaceAll("\u2019", "'")
				.replaceAll("\u201B", "'")
				.replaceAll("\u201C", "\"")
				.replaceAll("\u201F", "\"")
				.replaceAll("\u201D", "\"")
				.replaceAll("\u275D", "\"")
				.replaceAll("\u275E", "\"")
				.replaceAll("\u301D", "\"")
				.replaceAll("\u301E", "\"")
				.replaceAll("\u275B", "'")
				.replaceAll("\u275C", "'")

				.replaceAll("<[^>]*>", " ")

				// these need to be last
				.replaceAll("&sp;", " ")
				.replaceAll("&nbsp;", " ")
				.replaceAll("&amp;", "&")
				.replaceAll("&lt;", "<")
				.replaceAll("&gt;", ">")
				.replaceAll("&quot;", "\"")
				.replaceAll("&apos;", "'")
				.replaceAll(" {2}", "  ")
				;
	}

	/**
	 * Finds the string from the given vector of strings that most closely matches the input string.
	 * <br><br>
	 * This method normalizes the input and the vector strings by removing spaces and converting to lowercase
     * before comparing them using the Jaro-Winkler distance algorithm.
     *
     * @param strings a vector of strings to compare against the input string
     * @param input the input string that may contain typos, case, or space differences
     * @return the string from the vector that most closely matches the input string
	 */
    public static String findClosestMatch(Vector<String> strings, String input) {
        String closestMatch = null;
        double maxSimilarity = -1;

        // Preprocess the input string
        String normalizedInput = normalizeString(input);

        for (String str : strings) {
            // Preprocess the current string from the vector
            String normalizedStr = normalizeString(str);
            double similarity = jaroWinklerDistance(normalizedInput, normalizedStr);
            if (similarity > maxSimilarity) {
                maxSimilarity = similarity;
                closestMatch = str;
            }
        }
        return closestMatch;
    }

    // Normalize the string by removing spaces and converting to lower case
    private static String normalizeString(String str) {
        return str.replaceAll("\\s", "").toLowerCase();
    }

    // Jaro-Winkler distance algorithm implementation
    private static double jaroWinklerDistance(String s1, String s2) {
        if (s1.equals(s2))
            return 1.0;

        int[] mtp = matches(s1, s2);
        float m = (float) mtp[0];
        if (m == 0)
            return 0.0;
        float j = ((m / s1.length() + m / s2.length() + (m - mtp[1]) / m)) / 3;
        float jw = j < 0.7 ? j : j + Math.min(0.1f, 1.0f / mtp[3]) * mtp[2] * (1 - j);
        return jw;
    }

    private static int[] matches(String s1, String s2) {
        String max, min;
        if (s1.length() > s2.length()) {
            max = s1;
            min = s2;
        } else {
            max = s2;
            min = s1;
        }

        int range = Math.max(max.length() / 2 - 1, 0);
        boolean[] matchIndexes = new boolean[max.length()];
        boolean[] matchFlags = new boolean[min.length()];

        int matches = 0;
        for (int i = 0 ; i < min.length() ; i++) {
            int start = Math.max(i - range, 0);
            int end = Math.min(i + range + 1, max.length());
            for (int j = start; j < end; j++)
                if (!matchFlags[i] && max.charAt(j) == min.charAt(i) && !matchIndexes[j]) {
                    matchIndexes[j] = true;
                    matchFlags[i] = true;
                    matches++;
                    break;
                }
        }

        char[] ms1 = new char[matches];
        char[] ms2 = new char[matches];
        int si = 0;
        for (int i = 0 ; i < min.length() ; i++)
            if (matchFlags[i]) {
                ms1[si] = min.charAt(i);
                si++;
            }
        si = 0;
        for (int i = 0 ; i < max.length() ; i++)
            if (matchIndexes[i]) {
                ms2[si] = max.charAt(i);
                si++;
            }

        int transpositions = 0;
        for (int i = 0 ; i < ms1.length ; i++)
            if (ms1[i] != ms2[i])
                transpositions++;

        int prefix = 0;
        for (int i = 0 ; i < min.length() ; i++)
            if (s1.charAt(i) == s2.charAt(i))
                prefix++;
            else
                break;

        return new int[] { matches, transpositions / 2, prefix, max.length() };
    }

	private static final Map<String, String> nonAsciiReplacements = new HashMap<>();

    static {
        // Populate the map with common non-ASCII characters and their ASCII approximations
        nonAsciiReplacements.put("\u2019", "'");
        nonAsciiReplacements.put("\u2018", "'");
        nonAsciiReplacements.put("\u201B", "'");
        nonAsciiReplacements.put("\u201A", "'");
        nonAsciiReplacements.put("\u201C", "\"");
        nonAsciiReplacements.put("\u201D", "\"");
        nonAsciiReplacements.put("\u201E", "\"");
        nonAsciiReplacements.put("\u00AB", "<<");
        nonAsciiReplacements.put("\u00BB", ">>");
        nonAsciiReplacements.put("\u2014", "-");
        nonAsciiReplacements.put("\u2013", "-");
        nonAsciiReplacements.put("\u2212", "-");
        nonAsciiReplacements.put("\u00E7", "c");
        nonAsciiReplacements.put("\u00DF", "ss");
        nonAsciiReplacements.put("\u00F1", "n");
        nonAsciiReplacements.put("\u00F8", "o");
        nonAsciiReplacements.put("\u00E6", "ae");
        nonAsciiReplacements.put("\u0153", "oe");
        nonAsciiReplacements.put("\u00A9", "(c)");
        nonAsciiReplacements.put("\u00AE", "(r)");
        nonAsciiReplacements.put("\u20AC", "EUR");
        nonAsciiReplacements.put("\u00A3", "GBP");
        nonAsciiReplacements.put("\u00A5", "JPY");
        nonAsciiReplacements.put("\u00E1", "a");
        nonAsciiReplacements.put("\u00E0", "a");
        nonAsciiReplacements.put("\u00E4", "a");
        nonAsciiReplacements.put("\u00E2", "a");
        nonAsciiReplacements.put("\u00E3", "a");
        nonAsciiReplacements.put("\u00E5", "a");
        nonAsciiReplacements.put("\u00E9", "e");
        nonAsciiReplacements.put("\u00E8", "e");
        nonAsciiReplacements.put("\u00EB", "e");
        nonAsciiReplacements.put("\u00EA", "e");
        nonAsciiReplacements.put("\u00ED", "i");
        nonAsciiReplacements.put("\u00EC", "i");
        nonAsciiReplacements.put("\u00EF", "i");
        nonAsciiReplacements.put("\u00EE", "i");
        nonAsciiReplacements.put("\u00F3", "o");
        nonAsciiReplacements.put("\u00F2", "o");
        nonAsciiReplacements.put("\u00F6", "o");
        nonAsciiReplacements.put("\u00F4", "o");
        nonAsciiReplacements.put("\u00F5", "o");
        nonAsciiReplacements.put("\u00FA", "u");
        nonAsciiReplacements.put("\u00F9", "u");
        nonAsciiReplacements.put("\u00FC", "u");
        nonAsciiReplacements.put("\u00FB", "u");
        nonAsciiReplacements.put("\u00FD", "y");
        nonAsciiReplacements.put("\u00FF", "y");
        nonAsciiReplacements.put("\u00C6", "AE");
        nonAsciiReplacements.put("\u0152", "OE");
        nonAsciiReplacements.put("\u2122", "TM");
        nonAsciiReplacements.put("\u00B6", "P");
        nonAsciiReplacements.put("\u00A7", "S");
        nonAsciiReplacements.put("\u00B0", "o");
        nonAsciiReplacements.put("\u00FE", "th");
        nonAsciiReplacements.put("\u00DE", "TH");
        nonAsciiReplacements.put("\u00F0", "d");
        nonAsciiReplacements.put("\u00D0", "D");
        // Add more replacements as needed
    }

    /**
     * Converts a string to ASCII by replacing non-ASCII characters with their ASCII approximations
     * and removing other non-ASCII characters.
     *
     * @param input the original string
     * @return the ASCII string
     */
    public static String toAscii(String input) {
        // Replace common non-ASCII characters with ASCII equivalents
        for (Map.Entry<String, String> entry : nonAsciiReplacements.entrySet())
            input = input.replace(entry.getKey(), entry.getValue());

        // Normalize the string to decompose characters
        String normalizedString = Normalizer.normalize(input, Normalizer.Form.NFD);

        // Remove diacritical marks
        String asciiString = normalizedString.replaceAll("\\p{M}", "");

        // Remove any remaining non-ASCII characters
        asciiString = asciiString.replaceAll("[^\\x00-\\x7F]", "");

        return asciiString;
    }
}
