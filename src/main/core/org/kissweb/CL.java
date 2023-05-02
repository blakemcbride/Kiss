package org.kissweb;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.function.Supplier;

/**
 * Command Line IO.
 * <br><br>
 * In the old days, programs worked by asking one question at a time.  Although this is very primitive in today's
 * terms, it did work.  You could build any application with no GUI and just this interface.  The advantage is
 * that it is very fast to put together a program that works sufficiently well.  This may make sense for internal
 * use situations.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 1/4/22
 */
public class CL {

    private static BufferedReader reader;
    private static String commandString = "";
    private static String delimiter = ":";
    private static String padding = "  ";

    /**
     * End, help, or nothing.
     *
     * @param s
     * @return
     */
    public static int EHN(String s) {
        if (s == null)
            return 3;  // nothing
        s = s.trim();
        if (s.isEmpty())
            return 3;  // nothing
        if ("end".equals(s))
            return 1;  // end
        if ("help".equals(s))
            return 2;  // help
        return 0;  //  valid input
    }

    /**
     * Prompt user for a character input.
     *
     * @param choices
     * @param prompt
     * @return
     */
    public static String inputCharacter(String choices, String prompt) {
        while (true) {
            String s = input(prompt+" [" + choices + "]?");
            int i = EHN(s);
            if (i != 0)
                return s;
            int len = s.length();
            if (len > 1 || !choices.contains(s)) {
                error("Error: Invalid entry; valid responses are [" + choices + "].");
                continue;
            }
            return s;
        }
    }

    /**
     * Prompt user for a string input.
     *
     * @param prompt
     * @param min
     * @param max
     * @return
     */
    public static String inputString(int min, int max, String prompt) {
        while (true) {
            String s = input(prompt+"?");
            int i = EHN(s);
            if (i != 0)
                return s;
            int len = s.length();
            if (len >= min && len <= max)
                return s;
            error("Error: Please re-enter from " + min + " to " + max + " characters.");
        }
    }

    /**
     * Prompt user for a numeric input.
     *
     * @param prompt
     * @param min
     * @param max
     * @param decimalPlaces
     * @return
     */
    public static String inputNumber(double min, double max, int decimalPlaces, String prompt) {
        while (true) {
            String s = input(prompt+"?");
            int i = EHN(s);
            if (i != 0)
                return s;
            if (!NumberUtils.isValidNumber(s)) {
                if (decimalPlaces == 0)
                    error("Error: Please re-enter a number from " + NumberFormat.Format(min, "C", 0, decimalPlaces) + " to " + NumberFormat.Format(max, "C", 0, decimalPlaces) + " with " + decimalPlaces + " decimal places.");
                else
                    error("Error: Please re-enter a number from " + NumberFormat.Format(min, "C", 0, decimalPlaces) + " to " + NumberFormat.Format(max, "C", 0, decimalPlaces) + ".");
                continue;
            }
            if (decimalPlaces == 0 && s.contains(".")) {
                error("Error: Please re-enter a number from " + NumberFormat.Format(min, "C", 0, decimalPlaces) + " to " + NumberFormat.Format(max, "C", 0, decimalPlaces) + " without a decimal point.");
                continue;
            }
            double n = NumberUtils.parseDouble(s);
            if (n < min || n > max) {
                error("Error: Please re-enter a number from " + NumberFormat.Format(min, "C", 0, decimalPlaces) + " to " + NumberFormat.Format(max, "C", 0, decimalPlaces) + " with " + decimalPlaces + " decimal places.");
                continue;
            }
            if (!s.contains("."))
                return s;
            if (s.replaceAll("[0-9,]*\\.", "").length() > decimalPlaces) {
                error("Error: Please re-enter a number from " + NumberFormat.Format(min, "C", 0, decimalPlaces) + " to " + NumberFormat.Format(max, "C", 0, decimalPlaces) + " with " + decimalPlaces + " decimal places.");
                continue;
            }
            return s;
        }
    }

    /**
     * Prompt user for a selection amongst a fixed set of choices.
     *
     * @param options series of strings options separated by commas
     * @param prompt
     * @return the full option chosen
     */
    public static String inputList(String options, String prompt) {
        final String [] m = options.split(",");
        while (true) {
            final String s = input(prompt+"?");
            int i = EHN(s);
            if (i == 2) {
                println("\nValid responses are:");
                for (String value : m)
                    println("    " + value);
                println("");
                continue;
            }
            if (i != 0)
                return s;

            int nMatches = 0;
            int iMatch = -1;
            for (i=0 ; i < m.length ; ++i)
                if (m[i].startsWith(s)) {
                    if (++nMatches > 1)
                        break;
                    iMatch = i;
                }
            if (nMatches != 1) {
                error("Error: you have not entered a valid and unique response.");
                continue;
            }
            return m[iMatch];
        }
    }

    /**
     * Prompt user for a date input.
     * Also accepted are "today", "yesterday", and "tomorrow".
     *
     * @param min YYYYMMDD
     * @param max YYYYMMDD
     * @param prompt
     * @return EHN or date as a string in the YYYYMMDD format
     */
    public static String inputDate(int min, int max, String prompt) {
        while (true) {
            final String s = input(prompt+"?");
            final int i = EHN(s);
            if (i != 0)
                return s;
            if ("today".equals(s))
                return DateUtils.today() + "";
            if ("yesterday".equals(s))
                return DateUtils.addDays(DateUtils.today(), -1) + "";
            if ("tomorrow".equals(s))
                return DateUtils.addDays(DateUtils.today(), 1) + "";
            final int dt = DateUtils.parse(s);
            if (dt == 0 || DateUtils.calendar(DateUtils.julian(dt)) != dt) {
                error(s + " is not a valid date; please re-enter.");
                continue;
            }
            if (dt < min || dt > max) {
                error(s + " is out of range.  Please re-enter between " + DateUtils.format4(min) + " and " + DateUtils.format4(max) + ".");
                continue;
            }
            return dt + "";
        }
    }

    /**
     * Display an error message.
     *
     * @param msg
     */
    public static void error(String msg) {
        System.out.println("\n***  " + msg + "  ***\n");
        if (commandString.isEmpty())
            return;
        commandString = "";
        error("Error in command string; returning to manual input.");
    }

    private static String input(String prompt) {
        if (commandString.isEmpty()) {
            System.out.print(prompt + padding);
            if (reader == null)
                reader = new BufferedReader(new InputStreamReader(System.in));
            try {
                String line = reader.readLine();
                int i = line.indexOf(delimiter);
                if (i == -1)
                    return line;
                commandString = line.substring(i+1);
                return line.substring(0, i);
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            int i = commandString.indexOf(delimiter);
            if (i == -1) {
                String r = commandString;
                commandString = "";
                return r;
            } else {
                String r = commandString.substring(0, i-1);
                commandString = commandString.substring(i);
                return r;
            }
        }
        return "";
    }

    /**
     * Print a line of output terminating with a new line.
     *
     * @param s
     */
    public static void println(String s) {
        System.out.println(s);
    }

    public static class Entry {
        int label;
        Supplier<Integer> fun;

        public Entry(int label, Supplier<Integer> fun) {
            this.label = label;
            this.fun = fun;
        }
    }

    public static class Questions {
        private final ArrayList<Entry> questions = new ArrayList<>();
        private final int start;

        public Questions(int start) {
            this.start = start;
        }

        /**
         * Add a question.
         *
         * @param label
         * @param fun
         */
        public void add(int label, Supplier<Integer> fun) {
            questions.add(new Entry(label, fun));
        }

        /**
         * Run through the list of questions.
         *
         * @return
         */
        public boolean run() {
            int len = questions.size();
            int i = 0;
            while (i < len && i >= 0) {
                Entry e = questions.get(i);
                int n = e.fun.get();
                if (n < start)
                    i += n;
                else {
                    int j = 0;
                    for ( ; j < len ; j++) {
                        Entry e2 = questions.get(j);
                        if (e2.label == n)
                            break;
                    }
                    i = j;
                }
            }
            return i >= len;
        }
    }

}
