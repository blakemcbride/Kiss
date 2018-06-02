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

import java.io.PrintWriter;
import java.io.StringWriter;

/**
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

    public static String take(String s, int n) {
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

    public static String drop(String s, int n) {
        if (n == 0)
            return s;
        int len = s.length();
        if (n >= len || -n >= len)
            return "";
        if (n > 0)
            return s.substring(n);
        return s.substring(0, len + n);
    }

    public static String rightStrip(String s) {
        int i;
        for (i = s.length() - 1; i >= 0 && Character.isSpaceChar(s.charAt(i));)
            i--;
        if (i == -1)
            return s;
        return s.substring(0, i + 1);
    }

    public static String leftStrip(String s) {
        int i, m = s.length();
        for (i = 0; i < m && Character.isSpaceChar(s.charAt(i));)
            i++;
        if (i == 0)
            return s;
        return s.substring(i);
    }

    public static String centerStrip(String s) {
        return s.trim();
    }

//    public static String sprintf(String fmt) {
//        StringWriter sw = new StringWriter();
//        PrintWriter pw = new PrintWriter(sw);
//        pw.printf(fmt);
//        pw.flush();
//        sw.flush();
//        return sw.toString();
//    }

    public static String sprintf(String fmt, Object ... arg1) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        pw.printf(fmt, arg1);
        pw.flush();
        sw.flush();
        return sw.toString();
    }

    public static String join(String[] strings, String separator) {
        StringBuilder sb = new StringBuilder();
        for (String str : strings) {
            if (sb.length() > 0)
                sb.append(separator);
            sb.append(str);
        }
        return sb.toString();
    }

	public static boolean isEmpty(final String str) {
		return str == null || str.equals("");
	}
	
	public static String charArrayToString(char [] v) {
		int n=0;
		while (v[n] != '\0')
			n++;
		return new String(v, 0, n);
	}

}
