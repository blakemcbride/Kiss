package org.kissweb;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.kissweb.restServer.GroovyClass;

import java.io.*;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * System to auto-run Groovy files on a defined period as Unix cron does.
 * The file that controls what gets run and when is in backend/CronTasks/crontab
 * The groovy files it runs are located in the backend/CronTasks directory.
 * See the file backend/crontab
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 12/22/21
 */
public class Cron {

    private final static Logger logger = Logger.getLogger(Cron.class);

    private final Timer timer;

    /**
     * Start the cron process.
     *
     * @param crontabFileName the path to the crontab file
     * @param getParameter method that returns the parameter passed to the Groovy cron job
     * @param success method that performs the cleanup action after a successful run of the cron job
     * @param failure method that performs the cleanup action after a failed run of the cron job
     * @throws IOException if there is an error reading the crontab file
     */
    public Cron(String crontabFileName, Supplier<Object> getParameter, Consumer<Object> success, Consumer<Object> failure) throws IOException {
        //logger.setLevel(Level.INFO);
        CronFile cronFile = new CronFile(crontabFileName, getParameter, success, failure);
        TheTimerTask ttt = new TheTimerTask(cronFile);
        timer = new Timer();
        timer.scheduleAtFixedRate(ttt, 0L, 60000L);
    }

    /**
     * Stop further evocation of all cron jobs.
     */
    public void cancel() {
        timer.cancel();
    }

    private static class TheTimerTask extends TimerTask {

        private final CronFile cronFile;

        TheTimerTask(CronFile cronFile) {
            this.cronFile = cronFile;
        }

        // runs once per minute
        @Override
        public void run() {
            try {
                cronFile.process();
            } catch (IOException e) {
                logger.error("cron error", e);
            }
        }
    }

    private static class CronFile {
        private FileTime lastModifiedTime;
        private final String cronFileName;
        private List<CronLine> lines;
        private final Supplier<Object> getParameter;
        private final Consumer<Object> success, failure;

        CronFile(String cronFileName, Supplier<Object> getParameter, Consumer<Object> success, Consumer<Object> failure) {
            this.cronFileName = cronFileName;
            this.getParameter = getParameter;
            this.success = success;
            this.failure = failure;
        }

        void process() throws IOException {
            Calendar now = Calendar.getInstance();
            String path = new File(cronFileName).getParent() + "/";
            update();
            for (CronLine cl : lines)
                cl.process(now, path);
        }

        private void update() throws IOException {
            File f = new File(cronFileName);
            if (!f.exists())
                throw new FileNotFoundException("File " + cronFileName + " not found.");
            BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
            FileTime lastModifiedTime = attr.lastModifiedTime();
            if (this.lastModifiedTime != null && lastModifiedTime.compareTo(this.lastModifiedTime) == 0)
                return;  // already up to date
            this.lastModifiedTime = lastModifiedTime;
            lines = new ArrayList<>();
            BufferedReader br = new BufferedReader(new FileReader(f));
            String line;
            while (true) {
                line = br.readLine();
                if (line == null)
                    break;
                CronLine cl = CronLine.newCronLine(line, getParameter, success, failure);
                if (cl != null)
                    lines.add(cl);
            }
            br.close();
        }
    }

    private static class CronLine {
        private String line;
        private int pos, len;

        private String minutes;
        private String hours;
        private String daysOfMonth;
        private String months;
        private String daysOfWeek;
        private String command;
        private Supplier<Object> getParameter;
        private Consumer<Object> success, failure;

        private volatile GroovyClass cachedGroovy;
        private volatile long cachedTimestamp = -1;

        private static final Object COMPILE_LOCK = new Object();  

        static CronLine newCronLine(String line, Supplier<Object> getParameter, Consumer<Object> success, Consumer<Object> failure) {
            if (line == null)
                return null;
            line = line.trim();
            if (line.length() < 11)
                return null;
            if (line.charAt(0) == '#')
                return null;  // comment
            CronLine cl = new CronLine();

            cl.getParameter = getParameter;
            cl.success = success;
            cl.failure = failure;

            cl.line = line;
            cl.len = line.length();
            cl.pos = 0;

            if ((cl.minutes = cl.nextPartSpace()) == null)
                return null;
            if ((cl.hours = cl.nextPartSpace()) == null)
                return null;
            if ((cl.daysOfMonth = cl.nextPartSpace()) == null)
                return null;
            if ((cl.months = cl.nextPartSpace()) == null)
                return null;
            if ((cl.daysOfWeek = cl.nextPartSpace()) == null)
                return null;
            if ((cl.command = cl.getCommand()) == null)
                return null;
            return cl;
        }

        void process(Calendar now, String path) {
            if (notMinuteToExecute(now))
                return;
            if (notHourToExecute(now))
                return;
            if (notDayOfMonthToExecute(now))
                return;
            if (notMonthToExecute(now))
                return;
            if (notDayOfWeekToExecute(now))
                return;
            new Thread(() -> {
                runLine(path + command);
            }).start();
        }

        private void runLine(String sourceFile) {
            logger.info("Running " + sourceFile);
            try {
                GroovyClass gc = getOrCompile(sourceFile);
                Method methp   = gc.getMethod("start", Object.class);
                Object param   = getParameter.get();
        
                boolean ok = false;
                try {
                    methp.invoke(null, param); // run job
                    ok = true;
                } finally {
                    if (ok)
                        success.accept(param);
                    else
                        failure.accept(param);
                }
            } catch (Throwable t) {
                logger.error("cron error", t);
            }
        }

        private GroovyClass getOrCompile(String fname) throws IOException, Exception {
            File file = new File(fname);
            long ts   = file.lastModified();
        
            GroovyClass gc = cachedGroovy;                 // fast path
            if (gc != null && ts == cachedTimestamp)
                return gc;
        
            /* slow path: recompile only when timestamp changed */
            synchronized (COMPILE_LOCK) {
                if (cachedGroovy == null || ts != cachedTimestamp) {
                    cachedGroovy = new GroovyClass(false, fname);
                    cachedTimestamp = ts;
                }
                return cachedGroovy;
            }
        }

        private boolean notMinuteToExecute(Calendar cal) {
            try {
                int min = cal.get(Calendar.MINUTE);
                line = minutes;
                len = line.length();
                pos = 0;
                while (pos < len) {
                    String part = nextPartComma();
                    if (!part.isEmpty()) {
                        if (part.charAt(0) == '*')
                            return false;  // now
                        if (part.contains("-")) {
                            // range
                            int b = Integer.parseInt(part.substring(0, part.indexOf("-")));
                            int e = Integer.parseInt(part.substring(part.indexOf("-")+1));
                            if (min >= b && min <= e)
                                return false;  // now
                        }
                        if (min == Integer.parseInt(part))
                            return false;  // now
                    }
                }
            } catch (Exception ignore) {

            }
            return true; // not now
        }

        private boolean notHourToExecute(Calendar cal) {
            try {
                int hour = cal.get(Calendar.HOUR_OF_DAY);
                line = hours;
                len = line.length();
                pos = 0;
                while (pos < len) {
                    String part = nextPartComma();
                    if (!part.isEmpty()) {
                        if (part.charAt(0) == '*')
                            return false;  // now
                        if (part.contains("-")) {
                            // range
                            int b = Integer.parseInt(part.substring(0, part.indexOf("-")));
                            int e = Integer.parseInt(part.substring(part.indexOf("-")+1));
                            if (hour >= b && hour <= e)
                                return false;  // now
                        }
                        if (hour == Integer.parseInt(part))
                            return false;  // now
                    }
                }
            } catch (Exception ignore) {

            }
            return true; // not now
        }

        private boolean notDayOfMonthToExecute(Calendar cal) {
            try {
                int day = cal.get(Calendar.DAY_OF_MONTH);
                line = daysOfMonth;
                len = line.length();
                pos = 0;
                while (pos < len) {
                    String part = nextPartComma();
                    if (!part.isEmpty()) {
                        if (part.charAt(0) == '*')
                            return false;  // now
                        if (part.contains("-")) {
                            // range
                            int b = Integer.parseInt(part.substring(0, part.indexOf("-")));
                            int e = Integer.parseInt(part.substring(part.indexOf("-")+1));
                            if (day >= b && day <= e)
                                return false;  // now
                        }
                        if (day == Integer.parseInt(part))
                            return false;  // now
                    }
                }
            } catch (Exception ignore) {

            }
            return true; // not now
        }

        private boolean notMonthToExecute(Calendar cal) {
            try {
                int month = cal.get(Calendar.MONTH) + 1;
                line = months;
                len = line.length();
                pos = 0;
                while (pos < len) {
                    String part = nextPartComma();
                    if (!part.isEmpty()) {
                        if (part.charAt(0) == '*')
                            return false;  // now
                        if (part.contains("-")) {
                            // range
                            int b = Integer.parseInt(part.substring(0, part.indexOf("-")));
                            int e = Integer.parseInt(part.substring(part.indexOf("-")+1));
                            if (month >= b && month <= e)
                                return false;  // now
                        }
                        if (month == Integer.parseInt(part))
                            return false;  // now
                    }
                }
            } catch (Exception ignore) {

            }
            return true; // not now
        }

        private boolean notDayOfWeekToExecute(Calendar cal) {
            try {
                int dow = -1;
                switch (cal.get(Calendar.DAY_OF_WEEK)) {
                    case Calendar.SUNDAY:    dow = 0; break;
                    case Calendar.MONDAY:    dow = 1; break;
                    case Calendar.TUESDAY:   dow = 2; break;
                    case Calendar.WEDNESDAY: dow = 3; break;
                    case Calendar.THURSDAY:  dow = 4; break;
                    case Calendar.FRIDAY:    dow = 5; break;
                    case Calendar.SATURDAY:  dow = 6; break;
                }
                line = daysOfWeek;
                len = line.length();
                pos = 0;
                while (pos < len) {
                    String part = nextPartComma();
                    if (!part.isEmpty()) {
                        if (part.charAt(0) == '*')
                            return false;  // now
                        if (part.contains("-")) {
                            // range
                            int b = Integer.parseInt(part.substring(0, part.indexOf("-")));
                            if (b == 7)
                                b = 0;
                            int e = Integer.parseInt(part.substring(part.indexOf("-")+1));
                            if (e == 0)
                                e = 7;
                            if (dow >= b && dow <= e)
                                return false;  // now
                        }
                        int t = Integer.parseInt(part);
                        if (t == 7)
                            t = 0;
                        if (dow == t)
                            return false;  // now
                    }
                }
            } catch (Exception ignore) {

            }
            return true; // not now
        }

        private String nextPartComma() {
            final StringBuilder sb = new StringBuilder();
            while (pos < len) {
                final char c = line.charAt(pos++);
                if (c == ',')
                    break;
                sb.append(c);
            }
            return sb.toString();
        }

        private String nextPartSpace() {
            final StringBuilder sb = new StringBuilder();
            boolean atBeginning = true;
            while (pos < len) {
                final char c = line.charAt(pos++);
                if (c == ' ' || c == '\t') {
                    if (!atBeginning)
                        break;
                } else {
                    atBeginning = false;
                    sb.append(c);
                }
            }
            return sb.isEmpty() ? null : sb.toString();
        }

        private String getCommand() {
            final StringBuilder sb = new StringBuilder();
            boolean atBeginning = true;
            while (pos < len) {
                final char c = line.charAt(pos++);
                if (atBeginning && (c == ' ' || c == '\t'))
                    continue;
                atBeginning = false;
                sb.append(c);
            }
            return sb.isEmpty() ? null : sb.toString();
        }

    }

}
