package org.kissweb;

import java.io.*;

/**
 * Class to interface with the groff typesetting system.  This is an easy way to generate nicely formatted reports.
 * Of course, the underlying system must have groff/tbl/mm installed.
 *
 * See: https://www.gnu.org/software/groff
 *
 * @author Blake McBride
 *
 */
public class Groff {

    private static Boolean isMac;
    private static Boolean isWindows;

    private final PrintWriter pw;
    private final String pdfname;
    private final String mmfname;
    private final boolean landscape;
    private String title;
    private boolean atTop = true;
    private boolean autoPageHeader = true;
    private boolean deleteGroffFile = true;

    /**
     * Initialize a new report.  The files it uses are put in temporary files
     * that are auto-cleaned and in a place that can be served to the front-end.
     * On the front-end side, see JavaScript Utils.showReport()
     *
     * @param fnamePrefix file name prefix
     * @param title     report title
     * @param landscape true if landscape format
     * @throws IOException
     */
    public Groff(String fnamePrefix, String title, boolean landscape) throws IOException {
        if (isMac == null) {
            String os = System.getProperty("os.name").toLowerCase();
            isMac = os.startsWith("mac ");
            isWindows = os.startsWith("windows");
        }
        File fyle = FileUtils.createReportFile(fnamePrefix, ".pdf");
        this.landscape = landscape;
        this.pdfname = fyle.getAbsolutePath();
        mmfname = pdfname.replaceAll("\\.pdf$", ".mm");
        pw = new PrintWriter(new BufferedWriter(new FileWriter(mmfname)));
        this.title = title;
    }

    /**
     * Do not auto-generate the page title, or if already printed, stop.
     */
    public void noAutoPageHeader() {
        if (autoPageHeader)
            autoPageHeader = false;
        else
            pw.println(".rm TP");
    }

    private void setDefaults1() {
        pw.println(".PH \"''''\"");
        pw.println(".PF \"''Page \\\\\\\\nP''\"");
        pw.println(".S 11");;
    }

    private void setDefaults2() {
        pw.println(".nf");
    }

    private void writePageHeader(String title) {
        pw.println(".de TP");
        pw.println("'SP .5i");
        pw.println("'tl '''Run date: " + DateTime.currentDateTimeFormatted());
        pw.println("'tl ''\\s(14" + title + "\\s0''");
        pw.println("'SP");
        pw.println("..");
    }

    /**
     * Prevent the deletion of the intermediate Groff file for debugging purposes.
     */
    public void DontDeleteTroffFile() {
        deleteGroffFile = false;
    }

    /**
     * Write a line to the groff input file.
     *
     * @param str
     */
    public void out(String str) {
        boolean needDefaults2 = atTop;
        if (atTop) {
            setDefaults1();
            atTop = false;
        }
        if (autoPageHeader) {
            writePageHeader(title);
            autoPageHeader = false;
        }
        if (needDefaults2)
            setDefaults2();
        if (str != null)
            pw.println(str);
    }

    /**
     * Process the groff/tbl/mm input, produce the PDF output file, and return the path to the PDF file.
     *
     * @param sideMargin size of the margin on the left side of the page in inches
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    public String process(float sideMargin) throws IOException, InterruptedException {
        ProcessBuilder builder;
        String psfname = null;
        out(null);
        pw.flush();
        pw.close();
        if (isMac) 
            if (landscape)
                builder = new ProcessBuilder("/bin/sh", "-c", "groff -mm -t -P-pletter -rL=8.5i -P-l -rO="+sideMargin+"i -rW=" + (11-2*sideMargin) + "i " +  mmfname + " |pstopdf -i -o " + pdfname);
            else
                builder = new ProcessBuilder("/bin/sh", "-c", "groff -mm -t -P-pletter -rL=11i -rO="+sideMargin+"i -rW=" + (8.5-2*sideMargin) + "i " +  mmfname + " |pstopdf -i -o " + pdfname);
        else if (isWindows) {
            psfname = pdfname.replaceAll("\\.pdf$", ".ps");
            if (landscape)
                builder = new ProcessBuilder("cmd", "/c", "groff", "-mm", "-t", "-P-pletter", "-rL=8.5i", "-P-l", "-rO=" + sideMargin + "i", "-rW=" + (11 - 2 * sideMargin) + "i", mmfname);
            else
                builder = new ProcessBuilder("cmd", "/c", "groff", "-mm", "-t", "-P-pletter", "-rL=11i", "-rO=" + sideMargin + "i", "-rW=" + (8.5 - 2 * sideMargin) + "i", mmfname);
            builder.redirectOutput(new File(psfname));
        } else {
            if (landscape)
                builder = new ProcessBuilder("groff", "-mm", "-t", "-Tpdf", "-P-pletter", "-rL=8.5i", "-P-l", "-rO="+sideMargin+"i", "-rW=" + (11-2*sideMargin) + "i", mmfname);
            else
                builder = new ProcessBuilder("groff", "-mm", "-t", "-Tpdf", "-P-pletter", "-rL=11i", "-rO="+sideMargin+"i", "-rW=" + (8.5-2*sideMargin) + "i", mmfname);
            builder.redirectOutput(new File(pdfname));
        }
        Process p = builder.start();
        p.waitFor();
        if (deleteGroffFile)
            (new File(mmfname)).delete();
        if (isWindows) {
            builder = new ProcessBuilder("cmd", "/c", "ps2pdf.bat", psfname, pdfname);
            p = builder.start();
            p.waitFor();
            (new File(psfname)).delete();
        }
        return FileUtils.getHTTPPath(pdfname);
    }

    /**
     * Process the groff/tbl/mm input, produce the PDF output file, and return the path to the PDF file.
     * Defaults to a 1 inch side margin.
     *
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    public String process() throws IOException, InterruptedException {
        return process(1f);
    }
}
