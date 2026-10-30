package org.kissweb;

import org.junit.jupiter.api.Test;

import java.io.File;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntSupplier;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link DelimitedFileImport}.
 */
class DelimitedFileImportTest {

    private static String tmp(String content) throws Exception {
        File f = File.createTempFile("dfi", ".csv");
        f.deleteOnExit();
        Files.write(f.toPath(), content.getBytes(StandardCharsets.UTF_8));
        return f.getPath();
    }

    static class Imp extends DelimitedFileImport {
        final List<String> parsed = new ArrayList<>();
        final List<String> titles = new ArrayList<>();
        int commits, finishes, deactivations, headerCalls, rowChecks, fieldCount;
        String rejectTitle;
        Result finished;

        Imp(String file, int mode) throws Exception {
            super("person", file, mode);
        }

        @Override protected void initColumns() {
            addColumn("Last Name", true, "LNAME", "Surname");
            addColumn("Phone", false);
            if (fieldCount > 0)
                df.setFieldCountCheck(fieldCount);
        }
        @Override protected void checkHeader() {
            headerCalls++;
            titles.clear();
            for (int i = 0; i < df.size(); i++)
                titles.add(df.getString(i));
            if (rejectTitle != null && titles.contains(rejectTitle))
                addError("The column title '" + rejectTitle + "' is not allowed");
        }
        @Override protected void checkRow(int n) {
            rowChecks++;
            if (getString("Last Name").isEmpty())
                addError("Last Name is missing");
            if (getString("Phone").startsWith("x"))
                addError("Phone must be numeric");
        }
        @Override protected void parseRow(int n) {
            parsed.add(getString("Last Name") + "|" + columnHasData("Phone"));
            recordsAdded++;
        }
        @Override protected void commitBatch() { commits++; }
        @Override protected void deactivateMissing() { deactivations++; recordsDeactivated += 2; }
        @Override protected void onFinish(Result r) { finishes++; finished = r; }
    }

    @Test
    void goodFileTwoPasses() throws Exception {
        Imp i = new Imp(tmp("lname, Phone\nSmith,555\nJones,\n"), DelimitedFileImport.FULL_MODE);
        i.setBatchSize(1);
        DelimitedFileImport.Result r = i.run();
        assertTrue(r.isSuccess());
        assertEquals(2, r.getRowsRead());
        assertEquals(2, r.getRecordsAdded());
        assertEquals(2, r.getRecordsDeactivated());
        assertEquals(List.of("Smith|true", "Jones|false"), i.parsed);
        assertEquals(3, i.commits);   // every row plus the final one
        assertEquals(1, i.finishes);
        assertTrue(r.getSummary().contains("2 persons added"));
    }

    @Test
    void checkErrorsStopParse() throws Exception {
        Imp i = new Imp(tmp("Phone,Surname\n1,Smith\n2,\n3,Jones\n"), DelimitedFileImport.FULL_MODE);
        DelimitedFileImport.Result r = i.run();
        assertFalse(r.isSuccess());
        assertEquals(1, r.getErrors().size());
        assertTrue(r.getErrors().get(0).startsWith("Row 2:"));
        assertTrue(i.parsed.isEmpty());
        assertEquals(0, i.deactivations);
        assertEquals(1, i.finishes);
    }

    @Test
    void headerProblems() throws Exception {
        DelimitedFileImport.Result r = new Imp(tmp("Phone\n1\n"), DelimitedFileImport.APPEND_MODE).run();
        assertTrue(r.getErrors().get(0).contains("missing required column Last Name"));
        r = new Imp(tmp("Surname,LNAME\nA,B\n"), DelimitedFileImport.APPEND_MODE).run();
        assertTrue(r.getErrors().get(0).contains("duplicate"));
        r = new Imp(tmp("x,y\n1,2\n"), DelimitedFileImport.APPEND_MODE).run();
        assertFalse(r.isSuccess());
    }

    @Test
    void appendModeSkipsDeactivationAndWritesLog() throws Exception {
        Imp i = new Imp(tmp("Last Name\nA\n"), DelimitedFileImport.APPEND_MODE);
        File log = File.createTempFile("dfilog", ".txt");
        log.deleteOnExit();
        i.setLogPath(log.getPath());
        i.run();
        assertEquals(0, i.deactivations);
        assertTrue(new String(Files.readAllBytes(log.toPath()), StandardCharsets.UTF_8).contains("1 import records read"));
    }

    // ---------------------------------------------------------------- check(): the whole file, nothing imported

    @Test
    void checkCleanFileImportsNothing() throws Exception {
        Imp i = new Imp(tmp("lname, Phone\nSmith,555\nJones,\n"), DelimitedFileImport.FULL_MODE);
        assertEquals(List.of(), i.check());
        assertEquals(2, i.rowChecks);
        assertEquals(1, i.headerCalls);
        assertEquals(List.of("lname", "Phone"), i.titles);
        assertTrue(i.parsed.isEmpty());
        assertEquals(0, i.commits);
        assertEquals(0, i.deactivations);
        assertEquals(0, i.finishes);
    }

    @Test
    void checkReportsEveryRowProblemNumberedByDataRow() throws Exception {
        //The blank line is skipped and the title line is not counted: Smith is Row 1, the empty name Row 2.
        Imp i = new Imp(tmp("Last Name,Phone\nSmith,1\n\n,x9\nJones,2\n,3\nBrown,x\n"), DelimitedFileImport.UPDATE_MODE);
        assertEquals(List.of(
                "Row 2: Last Name is missing",
                "Row 2: Phone must be numeric",
                "Row 4: Last Name is missing",
                "Row 5: Phone must be numeric"), i.check());
        assertEquals(5, i.rowChecks);
        assertTrue(i.parsed.isEmpty());
    }

    @Test
    void checkEmptyFileIsMissingItsTitleLine() throws Exception {
        Imp i = new Imp(tmp(""), DelimitedFileImport.APPEND_MODE);
        assertEquals(List.of("The file is missing its first line of column titles."), i.check());
        assertEquals(0, i.headerCalls);
        assertEquals(0, i.rowChecks);
    }

    @Test
    void checkMissingRequiredColumnIsReportedAlone() throws Exception {
        Imp i = new Imp(tmp("Phone\n,x\n,y\n"), DelimitedFileImport.APPEND_MODE);
        assertEquals(List.of("The file is missing the required column \"Last Name\"."), i.check());
        assertEquals(0, i.headerCalls);
        assertEquals(0, i.rowChecks);
    }

    @Test
    void checkNoRecognizableColumnsAlsoReportsEveryMissingRequiredColumn() throws Exception {
        Imp i = new Imp(tmp("x,y\n1,2\n"), DelimitedFileImport.APPEND_MODE);
        assertEquals(List.of(
                "The file is missing the required column \"Last Name\".",
                "The file has no recognizable columns."), i.check());
        assertEquals(0, i.rowChecks);
    }

    @Test
    void checkDuplicateTitleIsReportedAndRowsAreStillChecked() throws Exception {
        //Surname and LNAME are two titles of the same column.
        Imp i = new Imp(tmp("Surname,LNAME\nA,B\nC,\n"), DelimitedFileImport.APPEND_MODE);
        assertEquals(List.of(
                "Duplicate column titles (Surname, LNAME); each column may appear only once, even under a different accepted name.",
                "Row 2: Last Name is missing"), i.check());
        assertEquals(1, i.headerCalls);
        assertEquals(2, i.rowChecks);
    }

    @Test
    void checkHeaderHookProblemHasNoRowNumberAndRowsAreStillChecked() throws Exception {
        Imp i = new Imp(tmp("Last Name,Secret\n,1\n"), DelimitedFileImport.APPEND_MODE);
        i.rejectTitle = "Secret";
        assertEquals(List.of("The column title 'Secret' is not allowed", "Row 1: Last Name is missing"), i.check());
    }

    @Test
    void checkUnreadableLineIsReportedAndCheckingContinues() throws Exception {
        Imp i = new Imp(tmp("Last Name,Phone\nA,1\nB,2,3\nC,x\n"), DelimitedFileImport.APPEND_MODE);
        i.fieldCount = 2;
        assertEquals(List.of(
                "Row 2 could not be read: Bad number of records read.  Expected 2 got 3",
                "Row 3: Phone must be numeric"), i.check());
        assertEquals(2, i.rowChecks);   //the unreadable row is not handed to checkRow
    }

    @Test
    void checkUsesTheSameRulesAsRun() throws Exception {
        String content = "Phone,Surname\n1,Smith\n2,\n3,Jones\n";
        List<String> viaCheck = new Imp(tmp(content), DelimitedFileImport.APPEND_MODE).check();
        List<String> viaRun = new Imp(tmp(content), DelimitedFileImport.APPEND_MODE).run().getErrors();
        assertEquals(viaRun, viaCheck);
        assertEquals(List.of("Row 2: Last Name is missing"), viaCheck);
    }

    @Test
    void checkPropagatesAnExceptionFromTheSubclass() throws Exception {
        Imp i = new Imp(tmp("Last Name\nA\n"), DelimitedFileImport.APPEND_MODE) {
            @Override protected void checkRow(int n) { throw new IllegalArgumentException("lookup failed"); }
        };
        assertThrows(IllegalArgumentException.class, i::check);
    }

    @Test
    void anInstanceIsSingleUse() throws Exception {
        Imp i = new Imp(tmp("Last Name\nA\n"), DelimitedFileImport.APPEND_MODE);
        assertEquals(List.of(), i.check());
        assertThrows(IllegalStateException.class, i::check);
        DelimitedFileImport.Result r = i.run();   //never throws
        assertFalse(r.isSuccess());
        assertTrue(i.parsed.isEmpty());
    }

    // ---------------------------------------------------------------- runChecked(): the same check, then import if clean

    @Test
    void runCheckedImportsACleanFile() throws Exception {
        Imp i = new Imp(tmp("lname, Phone\nSmith,555\nJones,\n"), DelimitedFileImport.FULL_MODE);
        i.setBatchSize(1);
        DelimitedFileImport.Result r = i.runChecked();
        assertTrue(r.isSuccess());
        assertEquals(2, r.getRowsRead());
        assertEquals(2, r.getRecordsAdded());
        assertEquals(2, r.getRecordsDeactivated());
        assertEquals(List.of("Smith|true", "Jones|false"), i.parsed);
        assertEquals(1, i.finishes);
        assertEquals(1, i.headerCalls);
        assertEquals(2, i.rowChecks);   //checkRow runs once per row, in the check pass only
    }

    @Test
    void runCheckedReportsHeaderAndRowProblemsTogetherAndImportsNothing() throws Exception {
        Imp i = new Imp(tmp("Surname,LNAME\nA,B\nC,\n"), DelimitedFileImport.FULL_MODE);
        DelimitedFileImport.Result r = i.runChecked();
        assertFalse(r.isSuccess());
        assertEquals(List.of(
                "Duplicate column titles (Surname, LNAME); each column may appear only once, even under a different accepted name.",
                "Row 2: Last Name is missing"), r.getErrors());
        assertTrue(i.parsed.isEmpty());
        assertEquals(0, i.deactivations);
        assertEquals(0, i.commits);
        assertEquals(1, i.finishes);
    }

    @Test
    void runCheckedHeaderProblemWording() throws Exception {
        assertEquals(List.of("The file is missing its first line of column titles."),
                new Imp(tmp(""), DelimitedFileImport.APPEND_MODE).runChecked().getErrors());
        assertEquals(List.of("The file is missing the required column \"Last Name\"."),
                new Imp(tmp("Phone\n1\n"), DelimitedFileImport.APPEND_MODE).runChecked().getErrors());
    }

    // ---------------------------------------------------------------- run() is unchanged, plus the header hook

    @Test
    void runKeepsItsOriginalHeaderWording() throws Exception {
        assertEquals(List.of("import file is missing the expected (first) column title line"),
                new Imp(tmp(""), DelimitedFileImport.APPEND_MODE).run().getErrors());
        assertEquals(List.of("import file is missing required column Last Name"),
                new Imp(tmp("Phone\n1\n"), DelimitedFileImport.APPEND_MODE).run().getErrors());
        assertEquals(List.of("import file is missing required column Last Name", "import file has no usable / recognizable columns"),
                new Imp(tmp("x,y\n1,2\n"), DelimitedFileImport.APPEND_MODE).run().getErrors());
    }

    @Test
    void runStopsAtADuplicateTitleWithoutCheckingRows() throws Exception {
        Imp i = new Imp(tmp("Surname,LNAME\nA,B\nC,\n"), DelimitedFileImport.APPEND_MODE);
        DelimitedFileImport.Result r = i.run();
        assertEquals(1, r.getErrors().size());
        assertTrue(r.getErrors().get(0).contains("duplicate"));
        assertEquals(0, i.rowChecks);
        assertEquals(0, i.headerCalls);
    }

    // ---------------------------------------------------------------- the file's own titles and the unrecognized columns

    /**
     * Records what the title accessors return in checkHeader, in every checkRow and in every parseRow, and the
     * value of each unrecognized column in each row.
     */
    static class Cols extends DelimitedFileImport {
        List<String> headerTitles, headerUnknown, rowTitles, parseTitles;
        List<Integer> headerUnknownIdx;
        String headerLastNameTitle, headerPhoneTitle;
        final List<String> checkValues = new ArrayList<>();
        final List<String> parseValues = new ArrayList<>();

        Cols(String file) throws Exception {
            super("person", file, APPEND_MODE);
        }

        @Override protected void initColumns() {
            addColumn("Last Name", true, "LNAME", "Surname");
            addColumn("Phone", false, "Telephone");
        }
        @Override protected void checkHeader() {
            headerTitles = new ArrayList<>(getFileColumnTitles());
            headerUnknown = new ArrayList<>(getUnknownColumns());
            headerUnknownIdx = new ArrayList<>(getUnknownColumnIndexes());
            headerLastNameTitle = getFileColumnTitle("Last Name");
            headerPhoneTitle = getFileColumnTitle("Phone");
        }
        private String unknownValues() {
            java.util.StringJoiner sj = new java.util.StringJoiner("|");
            for (int i : getUnknownColumnIndexes())
                sj.add(df.getString(i));
            return sj.toString();
        }
        @Override protected void checkRow(int n) {
            rowTitles = new ArrayList<>(getFileColumnTitles());
            checkValues.add(unknownValues());
        }
        @Override protected void parseRow(int n) {
            parseTitles = new ArrayList<>(getFileColumnTitles());
            parseValues.add(unknownValues());
            recordsAdded++;
        }
    }

    private static final String COLS_FILE = "Telephone, SUR-name ,Fax No,,NoTeS\n1,Smith,2,3,hello\n4,Jones,5,6,\n";

    private static void assertColsHeader(Cols c) {
        //Every column of the title line, by index, trimmed and otherwise exactly as in the file (an empty title is "").
        assertEquals(List.of("Telephone", "SUR-name", "Fax No", "", "NoTeS"), c.headerTitles);
        //SUR-name is the alias Surname (case and hyphen ignored) and Telephone is the alias of Phone, so only these two
        //are unrecognized; the empty title is left out; the original text and the file order are kept.
        assertEquals(List.of("Fax No", "NoTeS"), c.headerUnknown);
        assertEquals(List.of(2, 4), c.headerUnknownIdx);
        //The file's own title for a declared column, which may be the alias; "" when the file lacks the column.
        assertEquals("SUR-name", c.headerLastNameTitle);
        assertEquals("Telephone", c.headerPhoneTitle);
    }

    @Test
    void titlesAndUnknownColumnsAreAvailableInCheckHeaderRowsAndParseUnderCheck() throws Exception {
        Cols c = new Cols(tmp(COLS_FILE));
        assertEquals(List.of(), c.check());
        assertColsHeader(c);
        assertEquals(List.of("Telephone", "SUR-name", "Fax No", "", "NoTeS"), c.rowTitles);
        assertEquals(List.of("2|hello", "5|"), c.checkValues);
        assertNull(c.parseTitles);   //check() never parses
        //The lists are still there after the run, unchanged.
        assertEquals(List.of("Telephone", "SUR-name", "Fax No", "", "NoTeS"), c.getFileColumnTitles());
        assertEquals(List.of("Fax No", "NoTeS"), c.getUnknownColumns());
    }

    @Test
    void titlesAndUnknownColumnsAreAvailableUnderRun() throws Exception {
        Cols c = new Cols(tmp(COLS_FILE));
        assertTrue(c.run().isSuccess());
        assertColsHeader(c);
        assertEquals(List.of("2|hello", "5|"), c.checkValues);
        //In the parse pass the reader has been repositioned and re-read the title line; nothing has changed.
        assertEquals(List.of("Telephone", "SUR-name", "Fax No", "", "NoTeS"), c.parseTitles);
        assertEquals(List.of("2|hello", "5|"), c.parseValues);
        assertEquals(List.of("Fax No", "NoTeS"), c.getUnknownColumns());
        assertEquals(List.of(2, 4), c.getUnknownColumnIndexes());
    }

    @Test
    void titlesAndUnknownColumnsAreAvailableUnderRunChecked() throws Exception {
        Cols c = new Cols(tmp(COLS_FILE));
        assertTrue(c.runChecked().isSuccess());
        assertColsHeader(c);
        assertEquals(List.of("2|hello", "5|"), c.checkValues);
        assertEquals(List.of("2|hello", "5|"), c.parseValues);
        assertEquals(List.of("Telephone", "SUR-name", "Fax No", "", "NoTeS"), c.parseTitles);
    }

    @Test
    void aFileWithOnlyRecognizedColumnsHasNoUnknownColumns() throws Exception {
        Cols c = new Cols(tmp("Surname,Phone\nA,1\n"));
        assertEquals(List.of(), c.check());
        assertEquals(List.of("Surname", "Phone"), c.headerTitles);
        assertEquals(List.of(), c.headerUnknown);
        assertEquals(List.of(), c.headerUnknownIdx);
        assertEquals(List.of(""), c.checkValues);
        assertEquals("Surname", c.headerLastNameTitle);
        assertEquals("Phone", c.headerPhoneTitle);
    }

    @Test
    void anAbsentDeclaredColumnHasAnEmptyFileTitle() throws Exception {
        Cols c = new Cols(tmp("Last Name,Extra\nA,1\n"));
        assertEquals(List.of(), c.check());
        assertEquals("Last Name", c.headerLastNameTitle);
        assertEquals("", c.headerPhoneTitle);
        assertEquals(List.of("Extra"), c.headerUnknown);
        assertEquals(List.of(1), c.headerUnknownIdx);
        assertEquals("", c.getFileColumnTitle("No Such Column"));
    }

    @Test
    void unknownColumnMatchingUsesTheSameNormalizationAsDeclaredColumns() throws Exception {
        //Case, white space, hyphens and underscores are ignored when a title is matched to a declared column, so
        //"LAST - name" and "PHONE_" are recognized; "Phone #" is a different title, so it is unrecognized.
        Cols c = new Cols(tmp("LAST - name,Phone #,PHONE_\nA,1,2\n"));
        assertEquals(List.of(), c.check());
        assertEquals(List.of("LAST - name", "Phone #", "PHONE_"), c.headerTitles);   //the titles themselves are not cleaned
        assertEquals(List.of("Phone #"), c.headerUnknown);
        assertEquals(List.of(1), c.headerUnknownIdx);
        assertEquals("LAST - name", c.headerLastNameTitle);
    }

    @Test
    void theSecondOfTwoColumnsForOneDeclaredColumnIsListedAsUnknownToo() throws Exception {
        //Surname and LNAME are two titles of Last Name: a duplicate-title problem.  A lookup by name reads LNAME, so
        //Surname is the column no declared column reads.
        Cols c = new Cols(tmp("Surname,LNAME\nA,B\n"));
        List<String> problems = c.check();
        assertEquals(1, problems.size());
        assertTrue(problems.get(0).startsWith("Duplicate column titles (Surname, LNAME)"));
        assertEquals(List.of("Surname", "LNAME"), c.headerTitles);
        assertEquals(List.of("Surname"), c.headerUnknown);
        assertEquals(List.of(0), c.headerUnknownIdx);
    }

    @Test
    void titlesArePopulatedEvenWhenTheHeaderIsRejectedAndEmptyWithoutATitleLine() throws Exception {
        //A missing required column: the hook is not called, but the titles can be read afterwards (for example in onFinish).
        Cols c = new Cols(tmp("Phone,Extra\n1,2\n"));
        assertEquals(1, c.check().size());
        assertNull(c.headerTitles);
        assertEquals(List.of("Phone", "Extra"), c.getFileColumnTitles());
        assertEquals(List.of("Extra"), c.getUnknownColumns());

        Cols empty = new Cols(tmp(""));
        assertEquals(1, empty.check().size());
        assertEquals(List.of(), empty.getFileColumnTitles());
        assertEquals(List.of(), empty.getUnknownColumns());
        assertEquals(List.of(), empty.getUnknownColumnIndexes());
        assertEquals("", empty.getFileColumnTitle("Last Name"));
    }

    @Test
    void titleListsAreEmptyBeforeTheFileIsReadAndUnmodifiable() throws Exception {
        Cols c = new Cols(tmp("Last Name,Extra\nA,1\n"));
        assertEquals(List.of(), c.getFileColumnTitles());
        assertEquals(List.of(), c.getUnknownColumns());
        assertEquals(List.of(), c.getUnknownColumnIndexes());
        assertEquals(List.of(), c.check());
        assertThrows(UnsupportedOperationException.class, () -> c.getFileColumnTitles().add("x"));
        assertThrows(UnsupportedOperationException.class, () -> c.getUnknownColumns().add("x"));
        assertThrows(UnsupportedOperationException.class, () -> c.getUnknownColumnIndexes().add(9));
    }

    @Test
    void anUnknownColumnValueOfAShortRowIsEmpty() throws Exception {
        //Reading by index past the end of a row gives "", as for any column.
        Cols c = new Cols(tmp("Last Name,Extra,More\nA,1,2\nB\n"));
        assertEquals(List.of(), c.check());
        assertEquals(List.of("1|2", "|"), c.checkValues);
    }

    @Test
    void runCallsTheHeaderHookAndStopsOnItsError() throws Exception {
        Imp ok = new Imp(tmp("Last Name\nA\n"), DelimitedFileImport.APPEND_MODE);
        assertTrue(ok.run().isSuccess());
        assertEquals(1, ok.headerCalls);
        assertEquals(List.of("A|false"), ok.parsed);

        Imp bad = new Imp(tmp("Last Name,Secret\nA,1\n"), DelimitedFileImport.APPEND_MODE);
        bad.rejectTitle = "Secret";
        DelimitedFileImport.Result r = bad.run();
        assertEquals(List.of("The column title 'Secret' is not allowed"), r.getErrors());
        assertEquals(0, bad.rowChecks);
        assertTrue(bad.parsed.isEmpty());
    }

    // ---------------------------------------------------------------- declared column rules

    /**
     * An import whose columns are declared by the test, and which records, for every row of the check pass and of the
     * parse pass, whatever the {@code probe} reads from the import.
     */
    static class Rules extends DelimitedFileImport {
        final Consumer<Rules> declare;
        Function<Rules, String> probe = r -> "";
        final List<String> checkSeen = new ArrayList<>();
        final List<String> parseSeen = new ArrayList<>();
        int checkRows, parseRows;
        String extraCheckError;      //reported by checkRow, to show the order of the messages

        Rules(String file, Consumer<Rules> declare) throws Exception {
            super("thing", file, APPEND_MODE);
            this.declare = declare;
        }

        @Override protected void initColumns() { declare.accept(this); }
        @Override protected void checkRow(int n) {
            checkRows++;
            checkSeen.add(probe.apply(this));
            if (extraCheckError != null)
                addError(extraCheckError);
        }
        @Override protected void parseRow(int n) {
            parseRows++;
            parseSeen.add(probe.apply(this));
            recordsAdded++;
        }
    }

    private static Rules rules(String csv, Consumer<Rules> declare) throws Exception {
        return new Rules(tmp(csv), declare);
    }

    // ---- string length: reject or truncate

    @Test
    void maxLengthRejectsATooLongValueAndNamesTheFilesOwnTitle() throws Exception {
        //The column is declared as "Job" and the file calls it "Job Title": the message uses the file's own title.
        Rules r = rules("Name,Job Title\nA,abcdefghij\nB,abcdefghijk\nC,\nD,abc\n",
                x -> { x.addColumn("Name"); x.addColumn("Job", false, "Job Title").maxLength(10); });
        assertEquals(List.of("Row 2: \"Job Title\" is 11 characters long; at most 10 are allowed."), r.check());
    }

    @Test
    void maxLengthAcceptsAValueExactlyAtTheLimitAndAnUnlimitedColumn() throws Exception {
        assertEquals(List.of(), rules("Job\nabcdefghij\n", x -> x.addColumn("Job").maxLength(10)).check());
        assertEquals(List.of(), rules("Job\nabcdefghij\n", x -> x.addColumn("Job").maxLength(Integer.MAX_VALUE)).check());
        assertEquals(List.of(), rules("Job\nabcdefghij\n", x -> x.addColumn("Job").maxLength(() -> Integer.MAX_VALUE)).check());
    }

    @Test
    void maxLengthSingularWording() throws Exception {
        assertEquals(List.of("Row 1: \"Job\" is 2 characters long; at most 1 is allowed."),
                rules("Job\nab\n", x -> x.addColumn("Job").maxLength(1)).check());
    }

    @Test
    void maxLengthCountsCharactersNotUtf16Units() throws Exception {
        //Three characters, six UTF-16 units: the emoji each count as one, as a database column counts them.
        String three = new String(Character.toChars(0x1F600)).repeat(3);
        assertEquals(List.of(), rules("Job\n" + three + "\n", x -> x.addColumn("Job").maxLength(3)).check());
        assertEquals(1, rules("Job\n" + three + "\n", x -> x.addColumn("Job").maxLength(2)).check().size());
    }

    @Test
    void truncateShortensTheValueInOnePlaceSoCheckRowAndParseRowSeeTheShortValue() throws Exception {
        Rules r = rules("Name,Title\nA,abcdefghijkl\nB,abc\n",
                x -> { x.addColumn("Name"); x.addColumn("Title").maxLength(5).truncate(); });
        r.probe = x -> x.getString("Title") + "/" + x.df.getString("Title") + "/" + x.columnHasData("Title");
        DelimitedFileImport.Result res = r.run();
        assertTrue(res.isSuccess());
        //getString is the shortened value; the reader still has the file's own text.
        assertEquals(List.of("abcde/abcdefghijkl/true", "abc/abc/true"), r.checkSeen);
        assertEquals(List.of("abcde/abcdefghijkl/true", "abc/abc/true"), r.parseSeen);
    }

    @Test
    void truncationIsAWarningRecordedOnceAndNeverAnError() throws Exception {
        Rules r = rules("Title\nabcdefghijkl\nabc\nabcdefg\n", x -> x.addColumn("Title").maxLength(5).truncate());
        DelimitedFileImport.Result res = r.run();   //the check pass and the parse pass both shorten the values
        assertTrue(res.isSuccess());
        assertEquals(3, r.parseRows);
        assertEquals(List.of(
                "Row 1: \"Title\" was 12 characters long and was shortened to 5 characters.",
                "Row 3: \"Title\" was 7 characters long and was shortened to 5 characters."), res.getWarnings());
        assertTrue(res.getSummary().contains("WARNING: Row 1: \"Title\" was 12 characters long and was shortened to 5 characters."));
        assertTrue(res.getSummary().contains("2 warnings"));
        assertEquals(res.getWarnings(), r.getWarnings());
    }

    @Test
    void checkReportsNoProblemForATruncatedValueAndTheWarningsAreReadWithGetWarnings() throws Exception {
        Rules r = rules("Title\nabcdefghijkl\n", x -> x.addColumn("Title").maxLength(5).truncate());
        assertEquals(List.of(), r.check());
        assertEquals(List.of("Row 1: \"Title\" was 12 characters long and was shortened to 5 characters."), r.getWarnings());
        assertThrows(UnsupportedOperationException.class, () -> r.getWarnings().add("x"));
        assertEquals(0, r.parseRows);
    }

    @Test
    void runCheckedRecordsEachTruncationOnce() throws Exception {
        Rules r = rules("Title\nabcdefghijkl\n", x -> x.addColumn("Title").maxLength(5).truncate());
        r.probe = x -> x.getString("Title");
        DelimitedFileImport.Result res = r.runChecked();
        assertTrue(res.isSuccess());
        assertEquals(1, res.getWarnings().size());
        assertEquals(List.of("abcde"), r.parseSeen);
    }

    @Test
    void truncateNeverSplitsASurrogatePairAndKeepsTheTrailingBlank() throws Exception {
        String smile = new String(Character.toChars(0x1F600));
        Rules r = rules("Title\nab" + smile + "cd\nabc def\n", x -> x.addColumn("Title").maxLength(3).truncate());
        r.probe = x -> x.getString("Title");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("ab" + smile, "abc"), r.checkSeen);   //3 characters; the emoji is one of them
        Rules s = rules("Title\nabc def\n", x -> x.addColumn("Title").maxLength(4).truncate());
        s.probe = x -> "[" + x.getString("Title") + "]";
        s.check();
        assertEquals(List.of("[abc ]"), s.checkSeen);   //exactly 4 characters
    }

    @Test
    void truncateWithoutAMaxLengthDoesNothing() throws Exception {
        Rules r = rules("Title\nabcdefghijkl\n", x -> x.addColumn("Title").truncate());
        r.probe = x -> x.getString("Title");
        assertEquals(List.of(), r.check());
        assertEquals(List.of(), r.getWarnings());
        assertEquals(List.of("abcdefghijkl"), r.checkSeen);
    }

    @Test
    void truncateAndRejectCanBeMixedAcrossColumns() throws Exception {
        Rules r = rules("A,B\nabcdef,abcdef\n", x -> {
            x.addColumn("A").maxLength(3).truncate();
            x.addColumn("B").maxLength(3);
        });
        assertEquals(List.of("Row 1: \"B\" is 6 characters long; at most 3 are allowed."), r.check());
        assertEquals(List.of("Row 1: \"A\" was 6 characters long and was shortened to 3 characters."), r.getWarnings());
    }

    // ---- a limit supplied at run time

    @Test
    void aRunTimeLimitIsReadOnceAfterTheTitleLineAndUsedByBothPasses() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        Rules r = rules("Title\nabcdefgh\nabc\n", x -> x.addColumn("Title").maxLength(() -> {
            calls.incrementAndGet();
            return 4;
        }).truncate());
        r.probe = x -> x.getString("Title");
        assertEquals(0, calls.get());   //nothing is read when the column is declared
        assertTrue(r.run().isSuccess());
        assertEquals(1, calls.get());   //not once per row, not once per pass
        assertEquals(List.of("abcd", "abc"), r.parseSeen);
    }

    @Test
    void aRunTimeLimitIsNotReadForAColumnTheFileLacks() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        Rules r = rules("Name\nA\n", x -> {
            x.addColumn("Name");
            x.addColumn("Title").maxLength(() -> {
                calls.incrementAndGet();
                return 4;
            });
        });
        assertEquals(List.of(), r.check());
        assertEquals(0, calls.get());
    }

    @Test
    void aRunTimeLimitCanBeDeclaredToBeUnlimitedAndANegativeOneIsAProgramError() throws Exception {
        assertEquals(List.of(), rules("T\nabcdefgh\n", x -> x.addColumn("T").maxLength(() -> Integer.MAX_VALUE)).check());
        assertThrows(IllegalStateException.class, () -> rules("T\nabc\n", x -> x.addColumn("T").maxLength(() -> -1)).check());
        DelimitedFileImport.Result res = rules("T\nabc\n", x -> x.addColumn("T").maxLength(() -> -1)).run();   //run() never throws
        assertFalse(res.isSuccess());
        assertTrue(res.getErrors().get(0).startsWith("Unexpected error: java.lang.IllegalStateException"));
    }

    @Test
    void aFixedNegativeLimitIsRefusedWhenDeclared() throws Exception {
        assertThrows(IllegalArgumentException.class, () -> rules("T\nabc\n", x -> x.addColumn("T").maxLength(-1)).check());
        assertThrows(NullPointerException.class, () -> rules("T\nabc\n", x -> x.addColumn("T").maxLength((IntSupplier) null)).check());
    }

    // ---- notBlank

    @Test
    void notBlankRejectsEmptyWhiteSpaceAndMissingCells() throws Exception {
        //Row 3 is too short to reach the column; the blank line is skipped.
        Rules r = rules("Name,Phone\nA,1\nB,\n\nC\nD,   \nE,2\n", x -> {
            x.addColumn("Name");
            x.addColumn("Phone").notBlank();
        });
        assertEquals(List.of(
                "Row 2: \"Phone\" must have a value.",
                "Row 3: \"Phone\" must have a value.",
                "Row 4: \"Phone\" must have a value."), r.check());
    }

    @Test
    void notBlankDoesNotApplyToAColumnTheFileLacks() throws Exception {
        assertEquals(List.of(), rules("Name\nA\n", x -> {
            x.addColumn("Name");
            x.addColumn("Phone").notBlank();
        }).check());
    }

    @Test
    void requiredAndNotBlankBothApply() throws Exception {
        Consumer<Rules> decl = x -> {
            x.addColumn("Name");
            x.addColumn("Phone").required().notBlank();
        };
        //Required: the file must have the column.  Not blank: no row may leave it empty.
        assertEquals(List.of("The file is missing the required column \"Phone\"."), rules("Name\nA\n", decl).check());
        assertEquals(List.of("Row 1: \"Phone\" must have a value."), rules("Phone,Name\n,A\n", decl).check());
        assertEquals(List.of(), rules("Phone\n5\n", decl).check());
    }

    @Test
    void aBlankCellOfAColumnThatIsNotNotBlankIsValidAndSkipsTheTypeChecks() throws Exception {
        Rules r = rules("Name,N,D\nA,,\nB,5,03/25/2024\n", x -> {
            x.addColumn("Name");
            x.addColumn("N").integer().range(1, 9);
            x.addColumn("D").date("MM/dd/yyyy").min(LocalDate.of(2020, 1, 1));
        });
        r.probe = x -> x.getInt("N") + "|" + x.getLocalDate("D") + "|" + x.columnHasData("N");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("0|null|false", "5|2024-03-25|true"), r.checkSeen);
    }

    @Test
    void aBlankCellOfANotBlankTypedColumnIsOneErrorOnly() throws Exception {
        Rules r = rules("Name,N,D\nA,,\n", x -> {
            x.addColumn("Name");
            x.addColumn("N").notBlank().integer();
            x.addColumn("D").notBlank().date();
        });
        assertEquals(List.of("Row 1: \"N\" must have a value.", "Row 1: \"D\" must have a value."), r.check());
    }

    // ---- whole numbers

    @Test
    void integerRangeMessageNamesTheColumnTheRangeAndTheValue() throws Exception {
        Rules r = rules("Name,# Employees\nA,50\nB,50-100\nC,1000000\nD,1000001\nE,0\nF,-1\n", x -> {
            x.addColumn("Name");
            x.addColumn("# Employees").integer().range(0, 1_000_000);
        });
        assertEquals(List.of(
                "Row 2: \"# Employees\" must be a whole number from 0 to 1,000,000 (found \"50-100\").",
                "Row 4: \"# Employees\" must be a whole number from 0 to 1,000,000 (found \"1000001\").",
                "Row 6: \"# Employees\" must be a whole number from 0 to 1,000,000 (found \"-1\")."), r.check());
    }

    @Test
    void integerReadsDollarSignsCommasSpacesAndSignsAndRoundsADecimalHalfUp() throws Exception {
        Rules r = rules("N\n\"$1,500,000\"\n 1 500 \n1500.5\n2.4\n.5\n5.\n+7\n-5\n0.49\n-2.5\n", x -> x.addColumn("N").integer());
        r.probe = x -> String.valueOf(x.getInt("N"));
        assertEquals(List.of(), r.check());
        //A tie rounds away from zero (BigDecimal HALF_UP): 1500.5 is 1501, -2.5 is -3.
        assertEquals(List.of("1500000", "1500", "1501", "2", "1", "5", "7", "-5", "0", "-3"), r.checkSeen);
    }

    @Test
    void integerRangeIsCheckedAfterRounding() throws Exception {
        Rules r = rules("N\n10.4\n10.5\n-0.4\n", x -> x.addColumn("N").integer().range(0, 10));
        assertEquals(List.of("Row 2: \"N\" must be a whole number from 0 to 10 (found \"10.5\")."), r.check());
    }

    @Test
    void oneSidedRangesSayAtLeastAndAtMost() throws Exception {
        //Only the declared bound is named; the type's own limit is named too when it is the one a value breaks.
        Rules low = rules("N\n-1\nabc\n0\n3000000000\n", x -> x.addColumn("N").integer().min(0));
        assertEquals(List.of(
                "Row 1: \"N\" must be a whole number of at least 0 (found \"-1\").",
                "Row 2: \"N\" must be a whole number of at least 0 (found \"abc\").",
                "Row 4: \"N\" must be a whole number from 0 to 2,147,483,647 (found \"3000000000\")."), low.check());
        Rules high = rules("N\n101\n100\n-3000000000\n", x -> x.addColumn("N").integer().max(100));
        assertEquals(List.of(
                "Row 1: \"N\" must be a whole number of at most 100 (found \"101\").",
                "Row 3: \"N\" must be a whole number from -2,147,483,648 to 100 (found \"-3000000000\")."), high.check());
    }

    @Test
    void integerWithoutARangeFitsAnIntAndLongIntegerFitsALong() throws Exception {
        Rules r = rules("N\nabc\n3000000000\n2147483647\n-2147483648\n", x -> x.addColumn("N").integer());
        assertEquals(List.of(
                "Row 1: \"N\" must be a whole number (found \"abc\").",
                "Row 2: \"N\" must be a whole number from -2,147,483,648 to 2,147,483,647 (found \"3000000000\")."), r.check());

        Rules l = rules("N\n3000000000\n-9223372036854775808\n99999999999999999999\n", x -> x.addColumn("N").longInteger());
        l.probe = x -> String.valueOf(x.getLong("N"));
        assertEquals(List.of("Row 3: \"N\" must be a whole number from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807"
                + " (found \"99999999999999999999\")."), l.check());
        assertEquals(List.of("3000000000", "-9223372036854775808", "0"), l.checkSeen);
    }

    @Test
    void integerRefusesTextExponentsAndPercentSigns() throws Exception {
        Rules r = rules("N\n1e3\n5%\n12abc\n--5\n1.2.3\n$\n-\n1.5 million\n", x -> x.addColumn("N").integer());
        assertEquals(List.of(
                "Row 1: \"N\" must be a whole number (found \"1e3\").",
                "Row 2: \"N\" must be a whole number (found \"5%\").",
                "Row 3: \"N\" must be a whole number (found \"12abc\").",
                "Row 4: \"N\" must be a whole number (found \"--5\").",
                "Row 5: \"N\" must be a whole number (found \"1.2.3\").",
                "Row 6: \"N\" must be a whole number (found \"$\").",
                "Row 7: \"N\" must be a whole number (found \"-\").",
                "Row 8: \"N\" must be a whole number (found \"1.5 million\")."), r.check());
    }

    @Test
    void aVeryLongBadValueIsShortenedInTheMessage() throws Exception {
        String junk = "x".repeat(100);
        Rules r = rules("N\n" + junk + "\n", x -> x.addColumn("N").integer());
        assertEquals(List.of("Row 1: \"N\" must be a whole number (found \"" + "x".repeat(57) + "...\")."), r.check());
    }

    // ---- decimals

    @Test
    void decimalKeepsTheExactValueAndChecksTheRange() throws Exception {
        Rules r = rules("P\n\"$1,234.50\"\n12.5\n100.01\n-0.5\nabc\n", x -> x.addColumn("P").decimal().range(0, 100000));
        r.probe = x -> String.valueOf(x.getBigDecimal("P")) + "|" + x.getDouble("P");
        assertEquals(List.of(
                "Row 4: \"P\" must be a number from 0 to 100,000 (found \"-0.5\").",
                "Row 5: \"P\" must be a number from 0 to 100,000 (found \"abc\")."), r.check());
        assertEquals("1234.50|1234.5", r.checkSeen.get(0));   //scale as written
        assertEquals("12.5|12.5", r.checkSeen.get(1));
        assertEquals("null|0.0", r.checkSeen.get(3));          //invalid: no value
    }

    @Test
    void decimalBoundsKeepTheirDecimalsInTheMessage() throws Exception {
        Rules r = rules("P\n100\n", x -> x.addColumn("P").decimal().range(0.5, 99.99));
        assertEquals(List.of("Row 1: \"P\" must be a number from 0.5 to 99.99 (found \"100\")."), r.check());
        Rules exact = rules("P\n0.0999\n", x -> x.addColumn("P").decimal().min(new BigDecimal("0.10")));
        assertEquals(List.of("Row 1: \"P\" must be a number of at least 0.1 (found \"0.0999\")."), exact.check());
        Rules open = rules("P\n-1000000.25\n", x -> x.addColumn("P").decimal());
        assertEquals(List.of(), open.check());
    }

    @Test
    void decimalBoundsAreInclusive() throws Exception {
        Rules r = rules("P\n0\n10\n10.000001\n", x -> x.addColumn("P").decimal().range(0, 10));
        assertEquals(List.of("Row 3: \"P\" must be a number from 0 to 10 (found \"10.000001\")."), r.check());
    }

    // ---- typed getters

    @Test
    void typedGettersAreAvailableInCheckRowAndParseRowAndGiveZeroForAnAbsentColumn() throws Exception {
        Rules r = rules("Name,I,L,D\nA,7,8000000000,1.25\nB,,,\n", x -> {
            x.addColumn("Name");
            x.addColumn("I").integer();
            x.addColumn("L").longInteger();
            x.addColumn("D").decimal();
            x.addColumn("Absent").integer();
            x.addColumn("AbsentDate").date();
        });
        r.probe = x -> x.getInt("I") + "|" + x.getLong("I") + "|" + x.getLong("L") + "|" + x.getBigDecimal("D") + "|" + x.getBigDecimal("I")
                + "|" + x.getInt("Absent") + "|" + x.getLocalDate("AbsentDate") + "|" + x.getIntDate("AbsentDate");
        assertTrue(r.run().isSuccess());
        List<String> expected = List.of("7|7|8000000000|1.25|7|0|null|0", "0|0|0|null|null|0|null|0");
        assertEquals(expected, r.checkSeen);
        assertEquals(expected, r.parseSeen);
    }

    @Test
    void typedGettersFindAColumnByAliasOrDifferentCase() throws Exception {
        //Found the way getString finds it: by the declared name (any case, spacing) or by the file's own title.
        Rules r = rules("Staff\n12\n", x -> x.addColumn("# Employees").aliases("Employees", "Staff").integer());
        r.probe = x -> x.getInt("# Employees") + "|" + x.getInt("# EMPLOYEES") + "|" + x.getInt("STAFF");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("12|12|12"), r.checkSeen);
    }

    @Test
    void typedGettersAreMisuseErrorsNotSilentZeros() throws Exception {
        Rules unknown = rules("N\n1\n", x -> x.addColumn("N").integer());
        unknown.probe = x -> String.valueOf(x.getInt("Nope"));
        assertThrows(IllegalArgumentException.class, unknown::check);

        Rules text = rules("N\n1\n", x -> x.addColumn("N"));
        text.probe = x -> String.valueOf(x.getInt("N"));
        IllegalStateException e = assertThrows(IllegalStateException.class, text::check);
        assertTrue(e.getMessage().contains("integer()"), e.getMessage());

        Rules wide = rules("N\n1\n", x -> x.addColumn("N").longInteger());
        wide.probe = x -> String.valueOf(x.getInt("N"));
        assertThrows(IllegalStateException.class, wide::check);

        Rules dec = rules("N\n1\n", x -> x.addColumn("N").decimal());
        dec.probe = x -> String.valueOf(x.getLong("N"));
        assertThrows(IllegalStateException.class, dec::check);

        Rules date = rules("N\n1\n", x -> x.addColumn("N").integer());
        date.probe = x -> String.valueOf(x.getLocalDate("N"));
        assertThrows(IllegalStateException.class, date::check);

        Rules dateOnly = rules("D\n03/25/2024\n", x -> x.addColumn("D").date("MM/dd/yyyy"));
        dateOnly.probe = x -> String.valueOf(x.getIntTime("D"));
        assertThrows(IllegalStateException.class, dateOnly::check);
    }

    // ---- dates

    @Test
    void dateAcceptsItsFormatReadsStrictlyAndExplainsWhatIsExpected() throws Exception {
        Rules r = rules("Name,Start Date\nA,03/25/2024\nB,3/25/2024\nC,13/45/2024\nD,02/30/2024\nE,03/25/24\nF,03/25/2024 extra\nG,2024-03-25\n", x -> {
            x.addColumn("Name");
            x.addColumn("Start Date").date("MM/dd/yyyy");
        });
        r.probe = x -> x.getLocalDate("Start Date") + "|" + x.getIntDate("Start Date");
        assertEquals(List.of(
                "Row 2: \"Start Date\" must be a date such as 03/25/2024 (found \"3/25/2024\").",
                "Row 3: \"Start Date\" must be a date such as 03/25/2024 (found \"13/45/2024\").",
                "Row 4: \"Start Date\" must be a date such as 03/25/2024 (found \"02/30/2024\").",
                "Row 5: \"Start Date\" must be a date such as 03/25/2024 (found \"03/25/24\").",
                "Row 6: \"Start Date\" must be a date such as 03/25/2024 (found \"03/25/2024 extra\").",
                "Row 7: \"Start Date\" must be a date such as 03/25/2024 (found \"2024-03-25\")."), r.check());
        assertEquals("2024-03-25|20240325", r.checkSeen.get(0));
        assertEquals("null|0", r.checkSeen.get(2));
    }

    @Test
    void singleLetterPatternsAcceptOneOrTwoDigits() throws Exception {
        Rules r = rules("D\n3/5/2024\n03/05/2024\n", x -> x.addColumn("D").date("M/d/yyyy"));
        r.probe = x -> String.valueOf(x.getIntDate("D"));
        assertEquals(List.of(), r.check());
        assertEquals(List.of("20240305", "20240305"), r.checkSeen);
    }

    @Test
    void dateRangeIsInclusiveAndSaysFromToOrOnOrAfterOrOnOrBefore() throws Exception {
        LocalDate lo = LocalDate.of(2020, 1, 1);
        LocalDate hi = LocalDate.of(2030, 12, 31);
        String file = "D\n01/01/2020\n12/31/2030\n12/31/2019\n01/01/2031\n";
        assertEquals(List.of(
                "Row 3: \"D\" must be a date from 01/01/2020 to 12/31/2030 (found \"12/31/2019\").",
                "Row 4: \"D\" must be a date from 01/01/2020 to 12/31/2030 (found \"01/01/2031\")."),
                rules(file, x -> x.addColumn("D").date("MM/dd/yyyy").range(lo, hi)).check());
        assertEquals(List.of(
                "Row 3: \"D\" must be a date on or after 01/01/2020 (found \"12/31/2019\")."),
                rules(file, x -> x.addColumn("D").date("MM/dd/yyyy").min(lo)).check());
        assertEquals(List.of(
                "Row 4: \"D\" must be a date on or before 12/31/2030 (found \"01/01/2031\")."),
                rules(file, x -> x.addColumn("D").date("MM/dd/yyyy").max(hi)).check());
    }

    @Test
    void severalFormatsAreTriedInOrderAndAllAreShownAsExamples() throws Exception {
        Rules r = rules("D\n03/25/2024\n2024-03-25\n25 mar 2024\nMarch 2024\n", x -> x.addColumn("D").date("MM/dd/yyyy", "yyyy-MM-dd", "d MMM yyyy"));
        r.probe = x -> String.valueOf(x.getIntDate("D"));
        assertEquals(List.of("Row 4: \"D\" must be a date such as 03/25/2024 or 2024-03-25 or 25 Mar 2024 (found \"March 2024\")."), r.check());
        assertEquals(List.of("20240325", "20240325", "20240325", "0"), r.checkSeen);
    }

    @Test
    void theDefaultDateFormatsAreThoseOfDateUtilsParseButStrict() throws Exception {
        Rules r = rules("D\n3/5/2024\n3/5/24\n3.5.2024\n3.5.24\n2024-3-5\n2024-03-05\n1/2/99\n1/2/50\n12/31/36\n12/31/37\n2/30/2024\n13/1/2024\n", x -> x.addColumn("D").date());
        r.probe = x -> String.valueOf(x.getIntDate("D"));
        //DateUtils.parse accepts 2/30/2024 (it rolls over to March); this refuses it.
        assertEquals(List.of(
                "Row 11: \"D\" must be a date such as 3/25/2024 or 3/25/24 or 3.25.2024 or 3.25.24 or 2024-3-25 (found \"2/30/2024\").",
                "Row 12: \"D\" must be a date such as 3/25/2024 or 3/25/24 or 3.25.2024 or 3.25.24 or 2024-3-25 (found \"13/1/2024\")."), r.check());
        //Every text DateUtils.parse reads, this reads the same way (the two-digit year window included).
        for (int i = 0; i < 10; i++) {
            String text = new String[]{"3/5/2024", "3/5/24", "3.5.2024", "3.5.24", "2024-3-5", "2024-03-05", "1/2/99", "1/2/50", "12/31/36", "12/31/37"}[i];
            assertEquals(String.valueOf(DateUtils.parse(text)), r.checkSeen.get(i), text);
        }
    }

    @Test
    void dateTimeReadsTheAmPmFormatAndRequiresAFourDigitYear() throws Exception {
        Rules r = rules("Date and Time\n03-15-2024 02:30 PM\n03-15-2024 02:30 pm\n03-15-2024 12:15 AM\n03-15-24 02:30 PM\n03-15-2024 13:00 PM\n03-15-2024\n", x ->
                x.addColumn("Date and Time").dateTime("MM-dd-yyyy hh:mm a"));
        r.probe = x -> x.getLocalDateTime("Date and Time") + "|" + x.getIntDate("Date and Time") + "|" + x.getIntTime("Date and Time");
        assertEquals(List.of(
                "Row 4: \"Date and Time\" must be a date and time such as 03-25-2024 02:30 PM (found \"03-15-24 02:30 PM\").",
                "Row 5: \"Date and Time\" must be a date and time such as 03-25-2024 02:30 PM (found \"03-15-2024 13:00 PM\").",
                "Row 6: \"Date and Time\" must be a date and time such as 03-25-2024 02:30 PM (found \"03-15-2024\")."), r.check());
        assertEquals("2024-03-15T14:30|20240315|1430", r.checkSeen.get(0));
        assertEquals("2024-03-15T14:30|20240315|1430", r.checkSeen.get(1));
        assertEquals("2024-03-15T00:15|20240315|15", r.checkSeen.get(2));
        assertEquals("null|0|0", r.checkSeen.get(3));
    }

    @Test
    void dateTimeRangeIsInclusive() throws Exception {
        LocalDateTime lo = LocalDateTime.of(2024, 3, 1, 8, 0);
        LocalDateTime hi = LocalDateTime.of(2024, 3, 31, 17, 30);
        String file = "T\n03/01/2024 08:00\n03/31/2024 17:30\n03/01/2024 07:59\n03/31/2024 17:31\n";
        assertEquals(List.of(
                "Row 3: \"T\" must be a date and time from 03/01/2024 08:00 to 03/31/2024 17:30 (found \"03/01/2024 07:59\").",
                "Row 4: \"T\" must be a date and time from 03/01/2024 08:00 to 03/31/2024 17:30 (found \"03/31/2024 17:31\")."),
                rules(file, x -> x.addColumn("T").dateTime("MM/dd/yyyy HH:mm").range(lo, hi)).check());
        assertEquals(List.of("Row 3: \"T\" must be a date and time at or after 03/01/2024 08:00 (found \"03/01/2024 07:59\")."),
                rules(file, x -> x.addColumn("T").dateTime("MM/dd/yyyy HH:mm").min(lo)).check());
        assertEquals(List.of("Row 4: \"T\" must be a date and time at or before 03/31/2024 17:30 (found \"03/31/2024 17:31\")."),
                rules(file, x -> x.addColumn("T").dateTime("MM/dd/yyyy HH:mm").max(hi)).check());
    }

    @Test
    void aDateTimeColumnCanAlsoBeReadAsADate() throws Exception {
        Rules r = rules("T\n03/25/2024 08:00\n", x -> x.addColumn("T").dateTime("MM/dd/yyyy HH:mm"));
        r.probe = x -> x.getLocalDate("T") + "|" + x.getLocalDateTime("T");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("2024-03-25|2024-03-25T08:00"), r.checkSeen);
    }

    @Test
    void aBadDateFormatIsRefusedWhenDeclared() throws Exception {
        for (String bad : new String[]{"MM/dd/yyyy z", "MM/yyyy", "hh:mm", "MM/dd/yyyy bb", ""})
            assertThrows(IllegalArgumentException.class, () -> rules("D\n1\n", x -> x.addColumn("D").date(bad)).check(), bad);
        //A date-time needs a time of day, and a 12-hour clock needs AM/PM.
        assertThrows(IllegalArgumentException.class, () -> rules("D\n1\n", x -> x.addColumn("D").dateTime("MM/dd/yyyy")).check());
        assertThrows(IllegalArgumentException.class, () -> rules("D\n1\n", x -> x.addColumn("D").dateTime("MM/dd/yyyy hh:mm")).check());
        assertEquals(List.of(), rules("D\n03/25/2024 08:00\n", x -> x.addColumn("D").dateTime("MM/dd/yyyy HH:mm")).check());
    }

    // ---- declaration mistakes

    @Test
    void mistakesInTheDeclarationAreProgramErrorsAtOnce() throws Exception {
        //A range needs its type first.
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").range(0, 5)).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").min(0)).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").max(LocalDate.of(2024, 1, 1))).check());
        //The wrong kind of bound for the type.
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().min(LocalDate.of(2024, 1, 1))).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").date().min(5)).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").date().range(LocalDateTime.MIN, LocalDateTime.MAX)).check());
        //One type; and truncate is for text.
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().date()).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().integer()).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").maxLength(3).truncate().integer()).check());
        assertThrows(IllegalStateException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().truncate()).check());
        //A range cannot be backwards, and a bound must be a number.
        assertThrows(IllegalArgumentException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().range(5, 1)).check());
        assertThrows(IllegalArgumentException.class, () -> rules("N\n1\n", x -> x.addColumn("N").integer().min(5).max(1)).check());
        assertThrows(IllegalArgumentException.class, () -> rules("N\n1\n", x -> x.addColumn("N").decimal().min(Double.NaN)).check());
        assertThrows(IllegalArgumentException.class, () -> rules("N\n1\n", x -> x.addColumn("N").date().range(LocalDate.of(2024, 2, 1), LocalDate.of(2024, 1, 1))).check());
        //Under run() a program error is reported in the result, as for any exception, and never thrown.
        assertFalse(rules("N\n1\n", x -> x.addColumn("N").range(0, 5)).run().isSuccess());
    }

    // ---- how the rules sit in the lifecycle

    @Test
    void theRulesRunBeforeCheckRowAndEveryProblemIsCollectedInFileOrder() throws Exception {
        Rules r = rules("Name,Size,Hired\n,abc,13/13/2024\nB,5,03/25/2024\n", x -> {
            x.addColumn("Hired").date("MM/dd/yyyy");     //declared first, but the file has it last
            x.addColumn("Name").notBlank();
            x.addColumn("Size").integer().min(1);
        });
        r.extraCheckError = "checked";
        assertEquals(List.of(
                "Row 1: \"Name\" must have a value.",
                "Row 1: \"Size\" must be a whole number of at least 1 (found \"abc\").",
                "Row 1: \"Hired\" must be a date such as 03/25/2024 (found \"13/13/2024\").",
                "Row 1: checked",
                "Row 2: checked"), r.check());
        assertEquals(2, r.checkRows);   //checkRow is still called for a row that has rule errors
    }

    @Test
    void checkRunAndRunCheckedAgreeAndRunDoesNotImportAFileWithRuleErrors() throws Exception {
        String csv = "Name,Size\nA,1\n,x\nC,3\n";
        Consumer<Rules> decl = x -> {
            x.addColumn("Name").notBlank();
            x.addColumn("Size").integer();
        };
        List<String> viaCheck = rules(csv, decl).check();
        Rules run = rules(csv, decl);
        List<String> viaRun = run.run().getErrors();
        List<String> viaChecked = rules(csv, decl).runChecked().getErrors();
        assertEquals(List.of("Row 2: \"Name\" must have a value.", "Row 2: \"Size\" must be a whole number (found \"x\")."), viaCheck);
        assertEquals(viaCheck, viaRun);
        assertEquals(viaCheck, viaChecked);
        assertEquals(0, run.parseRows);
    }

    @Test
    void nothingIsCappedEveryBadRowIsReported() throws Exception {
        StringBuilder sb = new StringBuilder("N\n");
        for (int i = 0; i < 1200; i++)
            sb.append("bad").append(i).append('\n');
        assertEquals(1200, rules(sb.toString(), x -> x.addColumn("N").integer()).check().size());
        assertEquals(1200, rules(sb.toString(), x -> x.addColumn("N").integer()).run().getErrors().size());
    }

    @Test
    void ruleWarningsSurviveARejectedFileAndTheRowsAreStillNumbered() throws Exception {
        Rules r = rules("Title,N\nabcdefgh,1\nabc,x\n", x -> {
            x.addColumn("Title").maxLength(4).truncate();
            x.addColumn("N").integer();
        });
        DelimitedFileImport.Result res = r.runChecked();
        assertEquals(List.of("Row 2: \"N\" must be a whole number (found \"x\")."), res.getErrors());
        assertEquals(List.of("Row 1: \"Title\" was 8 characters long and was shortened to 4 characters."), res.getWarnings());
        assertEquals(0, r.parseRows);
    }

    @Test
    void theMessagesUseTheTitleTheFileUsesForAnAliasedColumn() throws Exception {
        //"SUR-name" is the alias Surname (case and hyphen ignored): the message shows the title as the file spells it.
        Rules r = rules("SUR-name,x\n,1\n", x -> x.addColumn("Last Name", true, "LNAME", "Surname").notBlank());
        assertEquals(List.of("Row 1: \"SUR-name\" must have a value."), r.check());
    }

    @Test
    void theFluentFormDeclaresRequiredAndAliasedColumns() throws Exception {
        Consumer<Rules> decl = x -> {
            x.addColumn("Name");
            x.addColumn("Phone").required().aliases("Tel", "Telephone").notBlank();
        };
        assertEquals(List.of("The file is missing the required column \"Phone\"."), rules("Name\nA\n", decl).check());
        assertEquals(List.of("Row 1: \"Tel\" must have a value."), rules("Tel,Name\n,A\n", decl).check());
        assertEquals(List.of(), rules("Telephone\n555\n", decl).check());
    }

    @Test
    void theOriginalDeclarationStillWorksAndCanNowBeChained() throws Exception {
        //Ignoring the return value is what every existing import does.
        Rules plain = rules("Last Name,Phone\n,x\n", x -> {
            x.addColumn("Last Name", true, "LNAME", "Surname");
            x.addColumn("Phone", false);
        });
        assertEquals(List.of(), plain.check());
        Rules chained = rules("Surname,Phone\n,x\n", x -> {
            x.addColumn("Last Name", true, "LNAME", "Surname").notBlank();
            x.addColumn("Phone", false).maxLength(0);
        });
        assertEquals(List.of("Row 1: \"Surname\" must have a value.", "Row 1: \"Phone\" is 1 character long; at most 0 are allowed."), chained.check());
    }

    @Test
    void aColumnWithNoRulesIsReadAsBefore() throws Exception {
        Rules r = rules("Name\n  padded  \n\"quoted, text\"\n", x -> x.addColumn("Name"));
        r.probe = x -> "[" + x.getString("Name") + "]" + x.columnHasData("Name");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("[padded]true", "[quoted, text]true"), r.checkSeen);
    }

    // ---- rules for the unrecognized columns

    @Test
    void withNoUnknownColumnRulesTheUnknownColumnsAreReadAsBefore() throws Exception {
        //Nothing is declared: no rule, no warning, the trimmed text by index or by title ("" for a title the file lacks).
        String longText = "x".repeat(5000);
        Rules r = rules("Name,Extra,Note\nA,  padded  ," + longText + "\n", x -> x.addColumn("Name"));
        r.probe = x -> x.getUnknownString(1) + "|" + x.getUnknownString("Note").length() + "|" + x.getUnknownString("Nope") + "|" + x.getUnknownString("extra");
        assertEquals(List.of(), r.check());
        assertEquals(List.of("padded|5000||"), r.checkSeen);
        assertEquals(List.of(), r.getWarnings());
        //Asking for the rules and giving them none changes nothing either.
        Rules none = rules("Name,Extra\nA," + longText + "\n", x -> { x.addColumn("Name"); x.unknownColumnRules(); });
        none.probe = x -> String.valueOf(x.getUnknownString(1).length());
        assertTrue(none.run().isSuccess());
        assertEquals(List.of("5000"), none.parseSeen);
    }

    @Test
    void unknownColumnRulesRejectATooLongValueAndNameTheFilesOwnTitleOfEachColumn() throws Exception {
        Rules r = rules("Name,Fax No,Notes\nA,abcdef,abc\nB,abc,abcdefg\nC,abcde,abcde\n",
                x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(5); });
        assertEquals(List.of("Row 1: \"Fax No\" is 6 characters long; at most 5 are allowed.",
                "Row 2: \"Notes\" is 7 characters long; at most 5 are allowed."), r.check());
        assertEquals(0, r.parseRows);
    }

    @Test
    void unknownColumnRuleErrorsAreInFileOrderWithTheDeclaredColumnsAndBeforeCheckRow() throws Exception {
        Rules r = rules("Extra,Name,More\nabcdef,abcd,abcdefg\n", x -> {
            x.addColumn("Name").maxLength(3);
            x.unknownColumnRules().maxLength(5);
        });
        r.extraCheckError = "from checkRow";
        assertEquals(List.of("Row 1: \"Extra\" is 6 characters long; at most 5 are allowed.",
                "Row 1: \"Name\" is 4 characters long; at most 3 are allowed.",
                "Row 1: \"More\" is 7 characters long; at most 5 are allowed.",
                "Row 1: from checkRow"), r.check());
    }

    @Test
    void unknownColumnRulesDoNotTouchADeclaredColumn() throws Exception {
        Rules r = rules("Name,Extra\nabcdefghij,abc\n", x -> {
            x.addColumn("Name");
            x.unknownColumnRules().maxLength(5).notBlank();
        });
        assertEquals(List.of(), r.check());
    }

    @Test
    void truncateOnTheUnknownColumnsShortensInOnePlaceAndWarnsOnce() throws Exception {
        Rules r = rules("Name,Fax No\nA,abcdefgh\nB,abc\nC,abcdefg\n",
                x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(5).truncate(); });
        r.probe = x -> x.getUnknownString(1) + "/" + x.getUnknownString("Fax No") + "/" + x.df.getString(1);
        DelimitedFileImport.Result res = r.run();
        assertTrue(res.isSuccess());
        //The shortened value in checkRow and parseRow alike; the reader still has the file's own text.
        assertEquals(List.of("abcde/abcde/abcdefgh", "abc/abc/abc", "abcde/abcde/abcdefg"), r.checkSeen);
        assertEquals(r.checkSeen, r.parseSeen);
        //One warning per cut, listed once although the file was read twice.
        List<String> warnings = List.of("Row 1: \"Fax No\" was 8 characters long and was shortened to 5 characters.",
                "Row 3: \"Fax No\" was 7 characters long and was shortened to 5 characters.");
        assertEquals(warnings, res.getWarnings());
        assertEquals(warnings, r.getWarnings());
    }

    @Test
    void unknownColumnWarningsAreReadableAfterCheckAndSurviveARejectedFile() throws Exception {
        Consumer<Rules> decl = x -> {
            x.addColumn("Name").notBlank();
            x.unknownColumnRules().maxLength(3).truncate();
        };
        Rules checked = rules("Name,Tag\nA,abcdef\n", decl);
        assertEquals(List.of(), checked.check());
        assertEquals(List.of("Row 1: \"Tag\" was 6 characters long and was shortened to 3 characters."), checked.getWarnings());

        Rules rejected = rules("Name,Tag\n,abcdef\n", decl);
        DelimitedFileImport.Result res = rejected.runChecked();
        assertEquals(List.of("Row 1: \"Name\" must have a value."), res.getErrors());
        assertEquals(List.of("Row 1: \"Tag\" was 6 characters long and was shortened to 3 characters."), res.getWarnings());
        assertEquals(0, rejected.parseRows);

        Rules imported = rules("Name,Tag\nA,abcdef\n", decl);
        assertEquals(1, imported.runChecked().getWarnings().size());
        assertEquals(1, imported.parseRows);
    }

    @Test
    void unknownColumnTruncationCountsCharactersAndNeverSplitsASurrogatePair() throws Exception {
        String smile = new String(Character.toChars(0x1F600));
        //Four characters, eight UTF-16 units: three are kept, whole.
        Rules r = rules("Name,Tag\nA," + smile.repeat(4) + "\n",
                x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(3).truncate(); });
        r.probe = x -> x.getUnknownString(1);
        DelimitedFileImport.Result res = r.run();
        assertEquals(List.of(smile.repeat(3)), r.parseSeen);
        assertEquals(List.of("Row 1: \"Tag\" was 4 characters long and was shortened to 3 characters."), res.getWarnings());
        //Exactly at the limit, counted in characters: no cut.
        assertEquals(List.of(), rules("Name,Tag\nA," + smile.repeat(3) + "\n",
                x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(3); }).check());
    }

    @Test
    void notBlankOnTheUnknownColumnsNeedsAValueInEveryRowIncludingAShortRow() throws Exception {
        Rules r = rules("Name,Extra,More\nA,,x\nB,y,z\nC\n", x -> { x.addColumn("Name"); x.unknownColumnRules().notBlank(); });
        assertEquals(List.of("Row 1: \"Extra\" must have a value.",
                "Row 3: \"Extra\" must have a value.", "Row 3: \"More\" must have a value."), r.check());
        //A blank cell is valid when notBlank() is not declared, and skips the length rule.
        assertEquals(List.of(), rules("Name,Extra\nA,\n", x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(1); }).check());
    }

    @Test
    void aTypeDeclaredOnTheUnknownColumnsIsEnforcedInEveryRow() throws Exception {
        Rules r = rules("Name,Qty,Seen\nA,5,12/25/2024\nB,x,13/45/2024\n", x -> {
            x.addColumn("Name");
            x.unknownColumnRules().date();
        });
        assertEquals(List.of("Row 1: \"Qty\" must be a date such as 3/25/2024 or 3/25/24 or 3.25.2024 or 3.25.24 or 2024-3-25 (found \"5\").",
                "Row 2: \"Qty\" must be a date such as 3/25/2024 or 3/25/24 or 3.25.2024 or 3.25.24 or 2024-3-25 (found \"x\").",
                "Row 2: \"Seen\" must be a date such as 3/25/2024 or 3/25/24 or 3.25.2024 or 3.25.24 or 2024-3-25 (found \"13/45/2024\")."),
                r.check());
        //The text is still readable.
        Rules ok = rules("Name,Qty\nA,1500.5\n", x -> { x.addColumn("Name"); x.unknownColumnRules().integer().range(0, 2000); });
        ok.probe = x -> x.getUnknownString(1);
        assertTrue(ok.run().isSuccess());
        assertEquals(List.of("1500.5"), ok.parseSeen);
        assertEquals(1, rules("Name,Qty\nA,3000\n", x -> { x.addColumn("Name"); x.unknownColumnRules().integer().range(0, 2000); }).check().size());
    }

    @Test
    void aLongUnknownTitleIsShownShortenedInTheMessage() throws Exception {
        String title = "T".repeat(70);
        Rules r = rules("Name," + title + "\nA,abcdef\n", x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(3); });
        assertEquals(List.of("Row 1: \"" + "T".repeat(57) + "...\" is 6 characters long; at most 3 are allowed."), r.check());
    }

    @Test
    void theUnknownColumnLimitIsReadOnceAndOnlyIfTheFileHasAnUnknownColumn() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        Consumer<Rules> decl = x -> {
            x.addColumn("Name");
            x.unknownColumnRules().maxLength(() -> {
                calls.incrementAndGet();
                return 4;
            }).truncate();
        };
        assertTrue(rules("Name\nA\n", decl).run().isSuccess());
        assertEquals(0, calls.get());               //no unrecognized column: not read
        Rules r = rules("Name,A,B,C\n1,abcdefg,abcdefg,abcdefg\n2,abc,abc,abc\n", decl);
        assertTrue(r.run().isSuccess());
        assertEquals(1, calls.get());               //not per column, per row or per pass
        assertEquals(3, r.getWarnings().size());
    }

    @Test
    void aNegativeUnknownColumnLimitIsAProgramError() throws Exception {
        Consumer<Rules> decl = x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(() -> -1); };
        assertThrows(IllegalStateException.class, () -> rules("Name,Extra\nA,b\n", decl).check());
        DelimitedFileImport.Result res = rules("Name,Extra\nA,b\n", decl).run();   //run() never throws
        assertTrue(res.getErrors().get(0).startsWith("Unexpected error: java.lang.IllegalStateException"));
        assertThrows(IllegalArgumentException.class, () -> rules("Name,Extra\nA,b\n", x -> x.unknownColumnRules().maxLength(-1)).check());
    }

    @Test
    void unknownColumnRulesAreNotAppliedToARejectedHeader() throws Exception {
        AtomicInteger calls = new AtomicInteger();
        Rules r = rules("Other,Extra\n1,abcdefgh\n", x -> {
            x.addColumn("Name", true);
            x.addColumn("Other");
            x.unknownColumnRules().maxLength(() -> {
                calls.incrementAndGet();
                return 3;
            });
        });
        assertEquals(List.of("The file is missing the required column \"Name\"."), r.check());
        assertEquals(0, calls.get());
    }

    @Test
    void unknownColumnRulesAccumulateOnOneDeclaration() throws Exception {
        Rules r = rules("Name,Extra\nA,abcdef\n", x -> {
            x.addColumn("Name");
            x.unknownColumnRules().maxLength(3);
            x.unknownColumnRules().truncate();
            assertSame(x.unknownColumnRules(), x.unknownColumnRules());
        });
        assertEquals(List.of(), r.check());
        assertEquals(1, r.getWarnings().size());
    }

    @Test
    void theUnknownColumnRulesHaveNoRequiredOrAliases() throws Exception {
        assertThrows(IllegalStateException.class, () -> rules("A\n1\n", x -> { x.addColumn("A"); x.unknownColumnRules().required(); }).check());
        assertThrows(IllegalStateException.class, () -> rules("A\n1\n", x -> { x.addColumn("A"); x.unknownColumnRules().required(false); }).check());
        assertThrows(IllegalStateException.class, () -> rules("A\n1\n", x -> { x.addColumn("A"); x.unknownColumnRules().aliases("B"); }).check());
        //Declaring a truncated number is refused as for a declared column.
        assertThrows(IllegalStateException.class, () -> rules("A\n1\n", x -> { x.addColumn("A"); x.unknownColumnRules().truncate().integer(); }).check());
    }

    @Test
    void theSecondColumnForOneDeclaredColumnIsAnUnknownColumnAndGetsTheRulesToo() throws Exception {
        //Surname and LNAME are the same declared column (a duplicate-title problem); the one a lookup by name reads is
        //recognized, the other is listed as unknown, so the unknown rules apply to it.
        Rules r = rules("Surname,LNAME\nabcdef,ghijkl\n", x -> { x.addColumn("Last Name", false, "LNAME", "Surname"); x.unknownColumnRules().maxLength(3); });
        List<String> problems = r.check();
        assertEquals(2, problems.size());
        assertTrue(problems.get(0).startsWith("Duplicate column titles"));
        assertEquals("Row 1: \"Surname\" is 6 characters long; at most 3 are allowed.", problems.get(1));
    }

    @Test
    void getUnknownStringByTitleIsExactAndTheFirstOfEqualTitlesWins() throws Exception {
        //Two columns with the same title are a header problem, but check() still reads the rows.
        Rules r = rules("Name,Tag,Other,Tag\nA,first,o,second\n", x -> x.addColumn("Name"));
        r.probe = x -> x.getUnknownString("Tag") + "|" + x.getUnknownString("TAG") + "|" + x.getUnknownString(3) + "|" + x.getUnknownString("Other");
        List<String> problems = r.check();
        assertEquals(1, problems.size());
        assertTrue(problems.get(0).startsWith("Duplicate column titles"));
        assertEquals(List.of("first||second|o"), r.checkSeen);
    }

    @Test
    void getUnknownStringByIndexRefusesAColumnThatIsNotUnrecognized() throws Exception {
        //Column 0 is declared, column 2 has no title, column 9 does not exist: a mistake in the program, so loud.
        Rules r = rules("Name,Extra,,More\nA,1,2,3\n", x -> x.addColumn("Name"));
        r.probe = x -> x.getUnknownString(1) + x.getUnknownString(3);
        assertEquals(List.of(), r.check());
        assertEquals(List.of("13"), r.checkSeen);
        assertThrows(IllegalArgumentException.class, () -> r.getUnknownString(0));
        assertThrows(IllegalArgumentException.class, () -> r.getUnknownString(2));
        assertThrows(IllegalArgumentException.class, () -> r.getUnknownString(9));
        assertThrows(IllegalArgumentException.class, () -> r.getUnknownString(-1));
    }

    @Test
    void unknownColumnRulesBehaveTheSameUnderRunRunCheckedAndCheck() throws Exception {
        Consumer<Rules> decl = x -> { x.addColumn("Name"); x.unknownColumnRules().maxLength(2).truncate(); };
        String csv = "Name,Tag\nA,abcd\nB,xy\n";
        String expected = "Row 1: \"Tag\" was 4 characters long and was shortened to 2 characters.";
        Rules viaCheck = rules(csv, decl);
        assertEquals(List.of(), viaCheck.check());
        assertEquals(List.of(expected), viaCheck.getWarnings());
        assertEquals(List.of(expected), rules(csv, decl).run().getWarnings());
        assertEquals(List.of(expected), rules(csv, decl).runChecked().getWarnings());
    }

    // ---- truncate(): shortening a value derived from a column

    private static final String SMILE = new String(Character.toChars(0x1F600));

    @Test
    void truncateShortensADerivedValueAndRecordsTheKissWarningOnce() throws Exception {
        //The probe runs in checkRow and in parseRow; the warning is recorded in the check pass only.
        Rules r = rules("Addr\nabcdefgh\nabc\nabcdef\n", x -> x.addColumn("Addr"));
        r.probe = x -> x.truncate("Street (from Addr)", x.getString("Addr"), 5);
        DelimitedFileImport.Result res = r.run();
        assertTrue(res.isSuccess());
        assertEquals(List.of("abcde", "abc", "abcde"), r.checkSeen);
        assertEquals(r.checkSeen, r.parseSeen);
        List<String> expected = List.of("Row 1: \"Street (from Addr)\" was 8 characters long and was shortened to 5 characters.",
                "Row 3: \"Street (from Addr)\" was 6 characters long and was shortened to 5 characters.");
        assertEquals(expected, res.getWarnings());
        assertEquals(expected, r.getWarnings());
    }

    @Test
    void truncateWordsTheWarningLikeADeclaredTruncateColumn() throws Exception {
        //The same cell, once cut by a declared column and once by truncate(): the same text.
        Rules declared = rules("Job\nabcdefgh\n", x -> x.addColumn("Job").maxLength(5).truncate());
        assertEquals(List.of(), declared.check());
        Rules derived = rules("Job\nabcdefgh\n", x -> x.addColumn("Job"));
        derived.probe = x -> x.truncate("Job", x.getString("Job"), 5);
        assertEquals(List.of(), derived.check());
        assertEquals(declared.getWarnings(), derived.getWarnings());
        //Singular wording, as for a declared column.
        Rules one = rules("Job\nab\n", x -> x.addColumn("Job"));
        one.probe = x -> x.truncate("Job", x.getString("Job"), 1);
        one.check();
        assertEquals(List.of("Row 1: \"Job\" was 2 characters long and was shortened to 1 character."), one.getWarnings());
    }

    @Test
    void truncateIsRecordedOnceAcrossTheModes() throws Exception {
        Consumer<Rules> decl = x -> x.addColumn("Addr");
        String csv = "Addr\nabcdefgh\n";
        String w = "Row 1: \"Part\" was 8 characters long and was shortened to 4 characters.";
        for (int mode = 0; mode < 3; mode++) {
            Rules r = rules(csv, decl);
            r.probe = x -> x.truncate("Part", x.getString("Addr"), 4);
            List<String> got;
            if (mode == 0) {
                r.check();
                got = r.getWarnings();
            } else if (mode == 1)
                got = r.run().getWarnings();
            else
                got = r.runChecked().getWarnings();
            assertEquals(List.of(w), got, "mode " + mode);
        }
    }

    @Test
    void truncateReturnsAValueThatFitsUnchangedWithoutAWarning() throws Exception {
        Rules r = rules("Addr\nabcde\n", x -> x.addColumn("Addr"));
        r.probe = x -> {
            String v = x.getString("Addr");
            return x.truncate("Part", v, 5) == v && x.truncate("Part", v, Integer.MAX_VALUE) == v ? "same" : "different";
        };
        assertTrue(r.run().isSuccess());
        assertEquals(List.of("same"), r.parseSeen);
        assertEquals(List.of(), r.getWarnings());
    }

    @Test
    void truncateCountsCharactersAndNeverSplitsASurrogatePair() throws Exception {
        Rules r = rules("Addr\n" + SMILE.repeat(4) + "\n" + SMILE.repeat(3) + "\n", x -> x.addColumn("Addr"));
        r.probe = x -> x.truncate("Part", x.getString("Addr"), 3);
        r.run();
        assertEquals(List.of(SMILE.repeat(3), SMILE.repeat(3)), r.parseSeen);
        //Four characters (eight UTF-16 units) cut to three; three characters (six units) are not cut.
        assertEquals(List.of("Row 1: \"Part\" was 4 characters long and was shortened to 3 characters."), r.getWarnings());
    }

    @Test
    void truncateKeepsTheTrailingBlanksTheCutLeavesLikeADeclaredColumn() throws Exception {
        Rules declared = rules("Job\nab   cd\n", x -> x.addColumn("Job").maxLength(4).truncate());
        declared.probe = x -> "[" + x.getString("Job") + "]";
        declared.check();
        Rules derived = rules("Job\nab   cd\n", x -> x.addColumn("Job"));
        derived.probe = x -> "[" + x.truncate("Job", x.getString("Job"), 4) + "]";
        derived.check();
        assertEquals(List.of("[ab  ]"), declared.checkSeen);
        assertEquals(declared.checkSeen, derived.checkSeen);
    }

    @Test
    void truncateRecordsTheSameWarningOncePerRowButEachRowAndEachLabel() throws Exception {
        //checkRow reaching the same cut value twice (two helpers) is one warning; another row, or another label, is another.
        Rules r = rules("Addr\nabcdefgh\nabcdefgh\n", x -> x.addColumn("Addr"));
        r.probe = x -> x.truncate("Street", x.getString("Addr"), 4) + x.truncate("Street", x.getString("Addr"), 4)
                + x.truncate("City", x.getString("Addr"), 4);
        assertTrue(r.run().isSuccess());
        assertEquals(List.of("Row 1: \"Street\" was 8 characters long and was shortened to 4 characters.",
                "Row 1: \"City\" was 8 characters long and was shortened to 4 characters.",
                "Row 2: \"Street\" was 8 characters long and was shortened to 4 characters.",
                "Row 2: \"City\" was 8 characters long and was shortened to 4 characters."), r.getWarnings());
    }

    @Test
    void truncateCalledOnlyInParseRowIsNeverReported() throws Exception {
        //Documented: the parse pass is silent, so the check pass must shorten the value too.
        Rules r = rules("Addr\nabcdefgh\n", x -> x.addColumn("Addr"));
        r.probe = x -> x.parseRows > 0 ? x.truncate("Part", x.getString("Addr"), 4) : "";
        assertTrue(r.run().isSuccess());
        assertEquals(List.of("abcd"), r.parseSeen);
        assertEquals(List.of(), r.getWarnings());
    }

    @Test
    void truncateCalledInCheckRowOnlyIsReportedOnceAndNotInTheParsePass() throws Exception {
        Rules r = rules("Addr\nabcdefgh\n", x -> x.addColumn("Addr"));
        r.probe = x -> x.parseRows > 0 ? "" : x.truncate("Part", x.getString("Addr"), 4);
        assertTrue(r.run().isSuccess());
        assertEquals(1, r.getWarnings().size());
    }

    @Test
    void truncateFromTheHeaderHookRecordsAWarningWithoutARowNumber() throws Exception {
        Rules r = new Rules(tmp("Addr\nabcdefgh\n"), x -> x.addColumn("Addr")) {
            @Override protected void checkHeader() {
                truncate("Title line", "abcdef", 3);
            }
        };
        assertEquals(List.of(), r.check());
        assertEquals(List.of("\"Title line\" was 6 characters long and was shortened to 3 characters."), r.getWarnings());
    }

    @Test
    void truncateEdgeCases() throws Exception {
        Rules r = rules("Addr\nx\n", x -> x.addColumn("Addr"));
        assertNull(r.truncate("Part", null, 3));
        assertEquals("", r.truncate("Part", "", 3));
        assertEquals("", r.truncate("Part", "abc", 0));
        assertThrows(IllegalArgumentException.class, () -> r.truncate("Part", "abc", -1));
        assertThrows(NullPointerException.class, () -> r.truncate(null, "abc", 1));
        assertEquals(List.of("\"Part\" was 3 characters long and was shortened to 0 characters."), r.getWarnings());
    }

    @Test
    void anUnknownColumnValueAndADerivedValueAreCutTheSameWayInOneRun() throws Exception {
        //The two features together, as an import that keeps the extra columns and splits a declared cell uses them.
        Rules r = rules("Address,Note\n12345 Main Street,abcdefghij\n", x -> {
            x.addColumn("Address");
            x.unknownColumnRules().maxLength(4).truncate();
        });
        r.probe = x -> x.truncate("Street (from Address)", x.getString("Address"), 8) + "|" + x.getUnknownString(1);
        DelimitedFileImport.Result res = r.runChecked();
        assertTrue(res.isSuccess());
        assertEquals(List.of("12345 Ma|abcd"), r.parseSeen);
        assertEquals(List.of("Row 1: \"Note\" was 10 characters long and was shortened to 4 characters.",
                "Row 1: \"Street (from Address)\" was 17 characters long and was shortened to 8 characters."),
                res.getWarnings());
    }
}
