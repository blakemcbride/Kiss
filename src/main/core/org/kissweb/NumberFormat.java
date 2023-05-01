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

/**
 * @author Blake McBride
 *
 */
public class NumberFormat {

    private static final char[] alpha = "0123456789abcdefghijklmnopqrstuvwxyz".toCharArray();

    /**
     *  Numeric	formatter.  Takes a double and converts it to a nicely formatted String in a specified number base.
     *
     *	@param num		number to be formatted
     *	@param base		numeric base (like base 2 = binary, 16=hex...)
     *	@param msk		format mask - any combination of the following:<br>
     *  <ul>
     *     <li>B = blank if zero</li>
     *     <li>C = add commas</li>
     *     <li>L = left justify number</li>
     *     <li>P = put parentheses around negative numbers</li>
     *     <li>Z = zero fill</li>
     *     <li>D = floating dollar sign</li>
     *     <li>U = uppercase letters in conversion</li>
     *     <li>R = add a percent sign to the end of the number</li>
     *  </ul>
     *	@param  wth	total field width (0 means auto)
     *	@param  dp		number of decimal places (-1 means auto)
     *  @return the formatted String
     *<p>
     *	example:<br><br>
     *
     *		String r = Formatb(-12345.348, 10, "CP", 12, 2);<br><br>
     *
     *		result in r:&nbsp;&nbsp;&nbsp;"(12,345.35)"
     * </p>
     *
     * @see #Format(double, String, int, int)
     */
    public static String Formatb(double num, int base, String msk, int wth, int dp) {
        int si, i;
        int sign, blnk, comma, left, paren, zfill, nd, dol, tw, dl, ez, ucase, cf, percent;
        double dbase;

        if (base < 2 || base > alpha.length)
            base = 10;
        dbase = base;

        if (num < 0.0) {
            num = -num;
            sign = 1;
        } else
            sign = 0;

        /*  round number  */

        if (dp >= 0) {
            double r = Math.pow(dbase, (double) dp);
//		n = Math.floor(base/20.0 + n * r) / r;
            num = Math.floor(.5 + num * r) / r;
        }
        switch (base) {
            case 10:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + (int) Math.log10(num);	/* # of	digits left of .  */
                break;
            case 2:
                cf = 4;
                dl = num < 1.0 ? 0 : 1 + (int) (Math.log(num) / .6931471806);  /* # of digits left	of .  */
                break;
            case 8:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + (int) (Math.log(num) / 2.079441542);  /* # of digits left	of .  */
                break;
            case 16:
                cf = 4;
                dl = num < 1.0 ? 0 : 1 + (int) (Math.log(num) / 2.772588722);  /* # of digits left	of .  */
                break;
            default:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + (int) (Math.log(num) / Math.log(dbase));  /* # of digits left of .  */
                break;
        }

        if (dp < 0) {   /* calculate the number of digits right of decimal point */
            double n = num < 0.0 ? -num : num;
            dp = 0;
            while (dp < 20) {
                n -= Math.floor(n);
                if (1E-5 >= n)
                    break;
                dp++;
                /*  round n to 5 places	 */
                double r = Math.pow(10.0, 5.0);
                r = Math.floor(.5 + Math.abs(n * base * r)) / r;
                n = n < 0.0 ? -r : r;
            }
        }
        blnk = comma = left = paren = zfill = dol = ucase = percent = 0;
        if (msk != null) {
            char [] t2 = msk.toCharArray();
            for (char c : t2)
                switch (c) {
                    case 'B':  // blank if zero
                        blnk = 1;
                        break;
                    case 'C':  //  add commas
                        comma = (dl - 1) / cf;
                        break;
                    case 'L':  //  left justify
                        left = 1;
                        break;
                    case 'P':  //  parens around negative numbers
                        paren = 1;
                        break;
                    case 'Z':  //  zero fill
                        zfill = 1;
                        break;
                    case 'D':  //  dollar sign
                        dol = 1;
                        break;
                    case 'U':  //  upper case letters
                        ucase = 1;
                        break;
                    case 'R':  //  add percent
                        percent = 1;
                        break;
                }
        }
        /*  calculate what the number should take up	*/

        ez = num < 1.0 ? 1 : 0;
        tw = dol + paren + comma + sign + dl + dp + (dp == 0 ? 0 : 1) + ez + percent;
        if (wth < 1)
            wth = tw;
        else if (tw > wth) {
            if (ez != 0)
                tw -= ez--;
            if ((i = dol) != 0 && tw > wth)
                tw -= dol--;
            if (tw > wth && comma != 0) {
                tw -= comma;
                comma = 0;
            }
            if (tw < wth && i != 0) {
                tw++;
                dol = 1;
            }
            if (tw > wth && paren != 0)
                tw -= paren--;
            if (tw > wth && percent != 0)
                tw -= percent--;
            if (tw > wth) {
                final char[] tbuf = new char[wth];
                for (i = 0; i < wth;)
                    tbuf[i++] = '*';
                return new String(tbuf);
            }
        }
        final char[] buf = new char[wth];
        num = Math.floor(.5 + num * Math.floor(.5 + Math.pow(dbase, (double) dp)));
        if (blnk != 0 && num == 0.0) {
            for (i = 0; i < wth;)
                buf[i++] = ' ';
            return new String(buf);
        }
        si = wth;

        if (left != 0 && wth > tw) {
            i = wth - tw;
            while (i-- != 0)
                buf[--si] = ' ';
        }
        if (paren != 0)
            buf[--si] = (sign != 0 ? ')' : ' ');
        if (percent != 0)
            buf[--si] = '%';
        for (nd = 0; nd < dp && si != 0; nd++) {
            num /= dbase;
            i = (int) Math.floor(dbase * (num - Math.floor(num)) + .5);
            num = Math.floor(num);
            buf[--si] = ucase != 0 && i > 9 ? Character.toUpperCase(alpha[i]) : alpha[i];
        }
        if (dp != 0)
            if (si != 0)
                buf[--si] = '.';
            else
                num = 1.0;
        if (ez != 0 && si > sign + dol)
            buf[--si] = '0';
        nd = 0;
        while (num > 0.0 && si != 0)
            if (comma != 0 && nd == cf) {
                buf[--si] = ',';
                nd = 0;
            } else {
                num /= dbase;
                i = (int) Math.floor(dbase * (num - Math.floor(num)) + .5);
                num = Math.floor(num);
                if (ucase != 0 && i > 9)
                    buf[--si] = Character.toUpperCase(alpha[i]);
                else
                    buf[--si] = alpha[i];
                nd++;
            }
        if (zfill != 0) {
            i = sign + dol;
            while (si > i)
                buf[--si] = '0';
        }
        if (dol != 0 && si != 0)
            buf[--si] = '$';
        if (sign != 0)
            if (si != 0)
                buf[--si] = (paren != 0 ? '(' : '-');
            else
                num = 1.0;	/*  signal error condition	*/

        while (si != 0)
            buf[--si] = ' ';

        if (num != 0.0)         /*  should never happen. but just in case	*/
            for (i = 0; i < wth;)
                buf[i++] = '*';

        return new String(buf);
    }

    /**
     *  Numeric	formatter.  Takes a double and converts it to a nicely formatted String (for number in base 10).
     *
     *	@param num		number to be formatted
     *	@param msk		format mask - any combination of the following:<br>
     *  <ul>
     *     <li>B = blank if zero</li>
     *     <li>C = add commas</li>
     *     <li>L = left justify number</li>
     *     <li>P = put parentheses around negative numbers</li>
     *     <li>Z = zero fill</li>
     *     <li>D = floating dollar sign</li>
     *     <li>U = uppercase letters in conversion</li>
     *     <li>R = add a percent sign to the end of the number</li>
     *  </ul>
     *	@param  wth	total field width (0 means auto)
     *	@param  dp		number of decimal places (-1 means auto)
     *  @return the formatted String
     *<p>
     *	example:<br><br>
     *
     *		String r = Format(-12345.348, "CP", 12, 2);<br><br>
     *
     *		result in r:&nbsp;&nbsp;&nbsp;"(12,345.35)"
     * </p>
     *
     * @see #Formatb(double, int, String, int, int)
     */
    public static String Format(double num, String msk, int wth, int dp) {
        return Formatb(num, 10, msk, wth, dp);
    }

    private static void print(double num, String msk, int wth, int dp) {
        System.out.println("\"" + Format(num, msk, wth, dp) + "\"");
    }

    /**
     * Test function.
     *
     * @param argv
     */
    public static void main(String[] argv) {
        print(12345.146, "CD", 10, 2);
        print(12345.146, "CD", 10, 2);
        print(125.146, "CDL", 10, 2);
        print(1236354545.146, "CD", 0, -1);
        print(345.146, "CD", 10, 2);
        print(5.146, "CD", 10, 2);
        print(1345.146, "CD", 10, 2);
        print(5.146, "CP", 10, 2);
        print(-5.146, "ZCP", 10, 2);
        print(0, "C", -1, -1);
    }
}
