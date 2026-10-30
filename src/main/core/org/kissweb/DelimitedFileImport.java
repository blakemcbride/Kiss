package org.kissweb;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.ResolverStyle;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.IntSupplier;
import java.util.regex.Pattern;

/**
 * Persistence-agnostic base class for importing a delimited (CSV) file.
 * <br><br>
 * This class knows nothing about databases, ORMs, logging frameworks, or web paths.  Everything of that sort is
 * done by a subclass or through the hook methods ({@link #commitBatch()}, {@link #onFinish(Result)},
 * {@link #onFailure(Throwable)}, {@link #log(int, String)}, {@link #deactivateMissing()}), which have no-op defaults.
 * <br><br>
 * <b>Lifecycle</b> (run by {@link #run()}):
 * <ol>
 *   <li>The header is read and validated: missing required columns, duplicate columns, and "no recognized
 *       column" are reported as errors.  If it is acceptable, {@link #checkHeader()} is called.</li>
 *   <li><i>Check pass</i>: every data row is read, the declared {@link Column column rules} are applied to it
 *       (see below), and it is passed to {@link #checkRow(int)}, which reports problems with
 *       {@link #addError(String)}.  Nothing should be written in this pass.</li>
 *   <li><i>Parse pass</i>: only if there were no errors, the file is read again and every row, with the same
 *       column rules applied (so the same shortened and parsed values), is passed to
 *       {@link #parseRow(int)}, which does the actual work and increments {@link #recordsAdded},
 *       {@link #recordsUpdated}, and/or {@link #recordsDeactivated}.  {@link #commitBatch()} is called every
 *       {@link #setBatchSize(int) batch size} rows and at the end.</li>
 *   <li>In {@link #FULL_MODE}, and only if there were still no errors, {@link #deactivateMissing()} is called.</li>
 *   <li>{@link #onFinish(Result)} (or {@link #onFailure(Throwable)} on an unexpected exception) is called and
 *       the {@link Result} is returned.</li>
 * </ol>
 * <br>
 * <b>Checking a file without importing it</b>: {@link #check()} runs the same header validation and the same
 * {@link #checkRow(int)} rules over the whole file and returns every problem found, writing nothing.  It is for the
 * moment a file must be accepted or refused <i>before</i> any import is started (for example an upload handler that
 * reports a bad file straight back to the user).  {@link #runChecked()} is the all-or-nothing import that applies the
 * identical check phase and imports only if it found nothing.  The optional {@link #checkHeader()} hook is called
 * once, after the title line has been accepted and before the first row is checked, for per-file preparation and
 * for problems with the titles themselves.  See {@link #check()} for the exact semantics.
 * <br><br>
 * <b>The file's own titles.</b>  Besides the declared columns, a subclass can see every column title of the file:
 * {@link #getFileColumnTitles()} (all of them, as the file has them, by column index),
 * {@link #getFileColumnTitle(String)} (the title the file uses for a declared column), and
 * {@link #getUnknownColumns()} / {@link #getUnknownColumnIndexes()} (the columns that match no declared column or
 * alias, such as extra columns a subclass wants to keep).  They are available from the moment the title line has been
 * read, so in {@link #checkHeader()}, {@link #checkRow(int)} and {@link #parseRow(int)} of {@link #run()},
 * {@link #runChecked()} and {@link #check()} alike.  The value of a column in the current row is {@code df.getString(index)}.
 * <br><br>
 * <b>Rules for the unrecognized columns.</b>  The unrecognized columns can have rules too, one set for all of them,
 * declared in {@link #initColumns()} with {@link #unknownColumnRules()}, which returns the same {@link Column}
 * declaration {@link #addColumn(String)} does: {@code unknownColumnRules().maxLength(2000).truncate();}.  They run
 * exactly like the rules of a declared column (same messages, same warnings, naming the column by the title the file
 * has), and {@link #getUnknownString(int)} / {@link #getUnknownString(String)} return an unrecognized column's value
 * with them applied (a truncated value shortened).  With no rules declared nothing changes.
 * <br><br>
 * <b>Derived values.</b>  A value the column rules cannot reach, such as one part of a declared value that the
 * subclass splits, is shortened with {@link #truncate(String, String, int)}, which words and records the warning the
 * way a declared {@link Column#truncate()} column does, once, in the check pass only.
 * <br><br>
 * Columns are declared by plain logical name with {@link #addColumn(String, boolean, String...)}; the other
 * titles a file may use for the column are given as aliases.  Matching ignores case, white space, hyphens,
 * underscores and non-printable characters (see {@link DelimitedFileReader#setNormalizeHeaders(boolean)}).
 * <br><br>
 * <b>Declarative column rules.</b>  {@link #addColumn(String, boolean, String...)} and {@link #addColumn(String)}
 * return the column's {@link Column declaration}, to which rules are chained: {@link Column#notBlank()} (every row
 * must have a value), {@link Column#maxLength(int)} with {@link Column#truncate()} (reject, or shorten and warn),
 * the numbers {@link Column#integer()}, {@link Column#longInteger()} and {@link Column#decimal()} with
 * {@code min}/{@code max}/{@code range}, and the dates {@link Column#date()}, {@link Column#date(String, String...)}
 * and {@link Column#dateTime(String, String...)} with {@code range}.  The rules run automatically, before
 * {@link #checkRow(int)}, in {@link #run()}, {@link #runChecked()} and {@link #check()} alike; each unmet rule is a
 * row error in plain English that names the column by the title the file uses, and every one is collected.  The
 * typed getters ({@link #getInt(String)}, {@link #getLong(String)}, {@link #getBigDecimal(String)},
 * {@link #getDouble(String)}, {@link #getLocalDate(String)}, {@link #getLocalDateTime(String)},
 * {@link #getIntDate(String)}, {@link #getIntTime(String)}) return the parsed values and {@link #getString(String)}
 * returns a truncated value shortened.  A value that was shortened is reported as a warning, not an error, and
 * the warnings are in {@link Result#getWarnings()} and {@link #getWarnings()}.  The rules need no database; a
 * limit known only at run time (a database column width) is passed as an {@code IntSupplier}.
 * <pre>
 *   protected void initColumns() {
 *       addColumn("Last Name", true, "LNAME", "Surname").notBlank().maxLength(30);
 *       addColumn("Job Title").maxLength(() -&gt; widthOf("person", "job_title")).truncate();
 *       addColumn("# Employees").integer().range(0, 1_000_000);
 *       addColumn("Hire Date").date("MM/dd/yyyy").min(LocalDate.of(1990, 1, 1));
 *   }
 *   // Row 5: "# Employees" must be a whole number from 0 to 1,000,000 (found "50-100").
 *   // Row 7: "Job Title" was 45 characters long and was shortened to 30 characters.   (a warning)
 *   protected void parseRow(int rowNum) {
 *       int size = getInt("# Employees");
 *       String title = getString("Job Title");        // already shortened
 *       LocalDate hired = getLocalDate("Hire Date");
 *   }
 * </pre>
 * <pre>
 *   class PersonImport extends DelimitedFileImport {
 *       PersonImport(String file, int mode) throws FileNotFoundException {
 *           super("person", file, mode);
 *       }
 *       protected void initColumns() {
 *           addColumn("Last Name", true, "LNAME", "Surname");
 *           addColumn("Phone", false);
 *       }
 *       protected void checkRow(int rowNum) {
 *           if (getString("Last Name").isEmpty())
 *               addError("Last Name is missing");
 *       }
 *       protected void parseRow(int rowNum) {
 *           // ... write getString("Last Name") using your persistence layer ...
 *           recordsAdded++;
 *       }
 *       protected void commitBatch() { /* flush / commit your persistence layer *&#47; }
 *   }
 *   Result r = new PersonImport(path, DelimitedFileImport.UPDATE_MODE).run();
 * </pre>
 */
public abstract class DelimitedFileImport {

    /** Add new records; existing records are ignored and not updated. */
    public static final int APPEND_MODE = 1;
    /** Add new records and update existing ones. */
    public static final int UPDATE_MODE = 2;
    /** Add new records, update existing records, and deactivate records not in the import file. */
    public static final int FULL_MODE = 3;

    /** Log level for {@link #log(int, String)}. */
    public static final int LOG_INFO = 0;
    /** Log level for {@link #log(int, String)}. */
    public static final int LOG_WARNING = 1;
    /** Log level for {@link #log(int, String)}. */
    public static final int LOG_ERROR = 2;

    /** The reader, positioned on the current data row during {@link #checkRow(int)} and {@link #parseRow(int)}. */
    protected final DelimitedFileReader df;
    /** One of {@link #APPEND_MODE}, {@link #UPDATE_MODE}, {@link #FULL_MODE}. */
    protected final int mode;
    /** Rows read.  Maintained by this class (counted during the parse pass; during the check pass it is the check count). */
    protected int rowsRead;
    /** Incremented by the subclass. */
    protected int recordsAdded;
    /** Incremented by the subclass. */
    protected int recordsUpdated;
    /** Incremented by the subclass. */
    protected int recordsDeactivated;

    private final String typeOfImport;
    private final String importFileName;
    private final Map<String, Column> columns = new LinkedHashMap<>();   // logical name -> declaration
    private final Map<Integer, Column> columnsByIndex = new HashMap<>();   // file column index -> declared column present in the file
    private final List<Column> ruleColumns = new ArrayList<>();            // present declared columns that have rules, in file order
    private final List<String> fileColumns = new ArrayList<>();
    private final List<String> fileColumnTitles = new ArrayList<>();    // every title of the title line, by file column index
    private final List<Integer> unknownColumnIndexes = new ArrayList<>();  // file columns that no declared column reads
    private final Set<Integer> unknownIndexSet = new HashSet<>();           // the same indexes, for membership tests
    private final Map<Integer, Column> unknownRuleColumns = new HashMap<>();   // unrecognized file column index -> the rules bound to it (when rules were declared)
    private Column unknownRule;     // the rules declared for every unrecognized column; null if none were declared
    private boolean parsing;        // true while the rows of the parse pass are processed
    private final Set<String> shortenedInRow = new HashSet<>();     // the warnings truncate() has recorded for the current row
    private final List<String> errors = new ArrayList<>();
    private final List<String> warnings = new ArrayList<>();
    private int batchSize = 20;
    private String logPath;
    private int currentRow;
    private boolean used;           // run(), runChecked() or check() was called; an instance is single-use
    private boolean checkWording;   // true during check() / runChecked(): user-facing wording for file problems

    /**
     * @param typeOfImport a word for what is being imported (e.g. "employee"), used in messages
     * @param filename     the file to import
     * @param mode         {@link #APPEND_MODE}, {@link #UPDATE_MODE}, or {@link #FULL_MODE}
     * @throws FileNotFoundException if the file cannot be opened
     */
    protected DelimitedFileImport(String typeOfImport, String filename, int mode) throws FileNotFoundException {
        this(typeOfImport, filename, mode, ',', '"');
    }

    /**
     * @param typeOfImport a word for what is being imported, used in messages
     * @param filename     the file to import
     * @param mode         {@link #APPEND_MODE}, {@link #UPDATE_MODE}, or {@link #FULL_MODE}
     * @param delimiter    the field delimiter
     * @param quote        the quote character
     * @throws FileNotFoundException if the file cannot be opened
     */
    protected DelimitedFileImport(String typeOfImport, String filename, int mode, char delimiter, char quote) throws FileNotFoundException {
        this.typeOfImport = typeOfImport;
        this.importFileName = filename;
        this.mode = mode;
        df = new DelimitedFileReader(filename, delimiter, quote);
        df.setNormalizeHeaders(true);
    }

    /** Declare the columns by calling {@link #addColumn(String, boolean, String...)}.  Called once by {@link #run()}. */
    protected abstract void initColumns();

    /**
     * Check the current row (available through {@code df} and the {@code get*} helpers).  Report problems with
     * {@link #addError(String)}.  Do not write data.
     *
     * @param rowNum the 1-based data row number (the header is not counted)
     */
    protected abstract void checkRow(int rowNum);

    /**
     * Process the current row.  Only called if the check pass found no errors.  Increment
     * {@link #recordsAdded}, {@link #recordsUpdated}, {@link #recordsDeactivated} as appropriate.
     *
     * @param rowNum the 1-based data row number (the header is not counted)
     */
    protected abstract void parseRow(int rowNum);

    /**
     * Optional per-file hook.  Called once per run, after the title line has been read and accepted (it has a
     * title line and every required column, and at least one declared column is present) and before the first
     * data row is checked, by {@link #run()}, {@link #runChecked()} and {@link #check()} alike.  Use it to
     * prepare per-file state that {@link #checkRow(int)} needs (such as resolving column positions, or finding
     * the file's extra columns) and to report problems with the titles themselves with {@link #addError(String)}
     * (such an error has no "Row n: " prefix).
     * <br><br>
     * The reader is still positioned on the title line, so {@code df.getString(i)} is the title of file column
     * {@code i} and {@code df.size()} is the number of titles; {@link #getFileColumns()},
     * {@link #colExists(String)}, {@link #getFileColumnTitles()}, {@link #getFileColumnTitle(String)},
     * {@link #getUnknownColumns()} and {@link #getUnknownColumnIndexes()} are valid (they stay valid in every later
     * hook and in the row passes, where the reader is no longer on the title line).  An instance reads one file, so per-file state held in fields is
     * normally just assigned here; a helper object shared between instances should be reset here instead.
     * Default does nothing.
     */
    protected void checkHeader() {
    }

    /** Hook for {@link #FULL_MODE}: deactivate records not seen in the file.  Default does nothing. */
    protected void deactivateMissing() {
    }

    /** Hook called every {@link #setBatchSize(int)} rows during the parse pass and once at its end.  Default does nothing. */
    protected void commitBatch() {
    }

    /** Hook called at the end of every run, after the result is built.  Default does nothing. */
    protected void onFinish(Result result) {
    }

    /** Hook called if an unexpected exception aborts the run (the exception is also added as an error). Default does nothing. */
    protected void onFailure(Throwable t) {
    }

    /**
     * Hook for logging.  Default does nothing.
     *
     * @param level {@link #LOG_INFO}, {@link #LOG_WARNING}, or {@link #LOG_ERROR}
     * @param msg   the message
     */
    protected void log(int level, String msg) {
    }

    /**
     * Declare a column, and return its declaration so that rules can be chained onto it (see {@link Column}).  A
     * caller that wants no rules simply ignores the return value.
     * <pre>
     *   addColumn("Last Name", true, "LNAME", "Surname").notBlank().maxLength(30);
     *   addColumn("Phone", false);                                  // no rules
     * </pre>
     *
     * @param name     the logical name (used in messages and by the {@code get*} helpers)
     * @param required true if the file must contain the column
     * @param aliases  other titles the file may use for this column
     * @return the declaration of the column
     */
    protected Column addColumn(String name, boolean required, String... aliases) {
        final Column c = new Column(name);
        c.required = required;
        columns.put(name, c);
        if (aliases.length > 0)
            df.addAlias(name, aliases);
        return c;
    }

    /**
     * Declare a column that the file does not have to contain, and return its declaration so that rules can be
     * chained onto it.  This is the starting point of the fluent form; {@link Column#required()} and
     * {@link Column#aliases(String...)} give what the longer {@link #addColumn(String, boolean, String...)} gives.
     * <pre>
     *   addColumn("Job Title").maxLength(() -&gt; widthOfJobTitle()).truncate();
     *   addColumn("# Employees").aliases("Employees", "Staff").integer().range(0, 1_000_000);
     *   addColumn("Start Date").required().notBlank().date("MM/dd/yyyy").range(minDate, maxDate);
     * </pre>
     *
     * @param name the logical name (used in messages and by the {@code get*} helpers)
     * @return the declaration of the column
     */
    protected Column addColumn(String name) {
        return addColumn(name, false);
    }

    /** @return true if the file has the column (valid after the header has been read) */
    protected boolean colExists(String name) {
        return df.getColumnIndex(name) > -1;
    }

    /** @return the declared columns that are present, in file order */
    protected List<String> getFileColumns() {
        return Collections.unmodifiableList(fileColumns);
    }

    /**
     * The column titles of the file's title line, in file order: element {@code i} is the title of file column
     * {@code i}, the same index {@code df.getString(i)} uses on a data row.  This is <i>every</i> column of the title
     * line (unlike {@link #getFileColumns()}, which names only the declared columns that are present), so a column
     * the subclass did not declare, a column with an empty title (which is {@code ""}, keeping the indexes aligned),
     * and a column that duplicates another are all in it.
     * <br><br>
     * A title is the text the file has, as the reader returns it: leading and trailing white space is trimmed, and
     * nothing else is changed (letter case, inner spaces, hyphens, underscores and any byte-order mark are kept).  To
     * compare titles the way columns are matched to declared names, ignoring those differences, use
     * {@link #colExists(String)} / {@code df.getColumnIndex(name)} rather than comparing this text.
     * <br><br>
     * Available as soon as the title line has been read, in {@link #checkHeader()} and everything after it, in
     * {@link #run()}, {@link #runChecked()} and {@link #check()}.  It is empty before then, and for a file with no
     * title line.  The list is unmodifiable and does not change while the rows are read.
     *
     * @return the titles by file column index
     */
    protected List<String> getFileColumnTitles() {
        return Collections.unmodifiableList(fileColumnTitles);
    }

    /**
     * The title the file uses for a declared column, as {@link #getFileColumnTitles()} has it.  The column is found
     * the same way {@link #getString(String)} and {@link #colExists(String)} find it, by its logical name or any of its
     * aliases; the title returned is the file's own text, which may be the alias.
     *
     * @param name the logical name the column was declared with
     * @return the file's title for the column, or {@code ""} if the file does not have the column (or the name is not
     * declared)
     */
    protected String getFileColumnTitle(String name) {
        int i = df.getColumnIndex(name);
        return i < 0 || i >= fileColumnTitles.size() ? "" : fileColumnTitles.get(i);
    }

    /**
     * The titles of the file's <i>unrecognized</i> columns: the columns that no declared column reads, because their
     * title matches neither a declared column's logical name nor any of its aliases.  Use it to keep the extra columns
     * a file carries (for instance as free-form attributes) or to warn about them.
     * <br><br>
     * Recognition is not a second implementation of the matching: it asks the same reader lookup that
     * {@link #getString(String)}, {@link #colExists(String)} and the header validation use, so letter case, white
     * space, hyphens, underscores and non-printable characters are ignored exactly as there, and aliases count.  The
     * titles are in file order and have their original text, as {@link #getFileColumnTitles()} has them, so the
     * result is a sub-list of that list.  A column whose title is empty is not included (it has no name to report).
     * <br><br>
     * If the file has two columns for the same declared column (a duplicate-title problem, reported by the header
     * validation), only the column a lookup by name reads is recognized; the other one is listed here too.
     * <br><br>
     * The element at position {@code k} belongs to the file column {@code getUnknownColumnIndexes().get(k)}.  Available
     * wherever {@link #getFileColumnTitles()} is.  The list is unmodifiable.
     *
     * @return the unrecognized columns' titles, in file order
     */
    protected List<String> getUnknownColumns() {
        List<String> out = new ArrayList<>(unknownColumnIndexes.size());
        for (int i : unknownColumnIndexes)
            out.add(fileColumnTitles.get(i));
        return Collections.unmodifiableList(out);
    }

    /**
     * The file column indexes of the unrecognized columns, parallel to {@link #getUnknownColumns()} (same size, same
     * order, which is file order): the value of unrecognized column {@code k} in the current row is
     * {@code df.getString(getUnknownColumnIndexes().get(k))}, trimmed, and {@code ""} for a row that has fewer
     * fields.  See {@link #getUnknownColumns()} for what is unrecognized.  The list is unmodifiable.
     *
     * @return the zero-based file column indexes of the unrecognized columns, in file order
     */
    protected List<Integer> getUnknownColumnIndexes() {
        return Collections.unmodifiableList(unknownColumnIndexes);
    }

    /**
     * The rules for <i>every</i> unrecognized column (see {@link #getUnknownColumns()}), declared in
     * {@link #initColumns()} like those of a declared column: the same {@link Column} declaration is returned, so
     * {@link Column#notBlank()}, {@link Column#maxLength(int)} / {@link Column#maxLength(IntSupplier)},
     * {@link Column#truncate()}, and the number and date types with their bounds are all available.
     * <pre>
     *   protected void initColumns() {
     *       addColumn("Company", true).notBlank();
     *       unknownColumnRules().maxLength(() -&gt; answerWidth()).truncate();   // every extra column is kept, shortened
     *   }
     *   // Row 4: "Fax No" was 2300 characters long and was shortened to 2000 characters.   (a warning)
     *   protected void parseRow(int rowNum) {
     *       final List&lt;String&gt; titles = getFileColumnTitles();
     *       for (int i : getUnknownColumnIndexes())
     *           save(titles.get(i), getUnknownString(i));                       // already shortened
     *   }
     * </pre>
     * <b>The rules behave exactly as on a declared column</b>: they run once per data row, in {@link #check()}, the
     * check phase of {@link #runChecked()} and the check pass of {@link #run()}, before {@link #checkRow(int)}; each
     * unmet rule is a row error and every one is recorded, in file order together with those of the declared
     * columns; a value that {@code truncate()} shortened is a warning recorded once, in the check pass only, worded
     * {@code Row 4: "Fax No" was 45 characters long and was shortened to 30 characters.}; a blank cell is valid
     * unless {@code notBlank()}.  The column is named by the title the file has (a title longer than 60 characters is
     * shown shortened, with "...").  The limit of a run-time {@code maxLength(IntSupplier)} is read once per run, not
     * once per column, after the title line is accepted and before {@link #checkHeader()}, and only if the file has
     * at least one unrecognized column.
     * <br><br>
     * <b>Which columns.</b>  Those {@link #getUnknownColumnIndexes()} lists, which are the columns no declared column
     * reads and whose title is not empty.  The set is fixed when the title line has been read, so declare the rules
     * in {@link #initColumns()}; a declaration made later, in {@link #checkHeader()} or a row hook, has no effect on
     * the run in progress.  A subclass that must keep only some of the unrecognized columns (or none) applies its own
     * selection when it reads them; to apply no rules at all, do not call this method.
     * <br><br>
     * <b>Differences from a declared column.</b>  {@code required()} and {@code aliases(...)} are about matching a
     * title to a declared name, which an unrecognized column by definition has not got: calling either on this
     * declaration is an {@link IllegalStateException}.  The value is read as text with
     * {@link #getUnknownString(int)}; a number or date type declared here is enforced in every row, but there is no
     * typed getter for it.
     * <br><br>
     * Calling this method more than once returns the same declaration, to which rules accumulate.  With no rule on it
     * (or if it is never called) the unrecognized columns are read, and nothing is checked, as before.
     *
     * @return the rules of the unrecognized columns
     */
    protected Column unknownColumnRules() {
        if (unknownRule == null) {
            unknownRule = new Column(UNRECOGNIZED_COLUMNS);
            unknownRule.unrecognized = true;
        }
        return unknownRule;
    }

    /**
     * The trimmed value of an unrecognized column in the current row, with the rules declared by
     * {@link #unknownColumnRules()} applied: a value that {@link Column#truncate()} shortened is the shortened text,
     * in {@link #checkRow(int)} and {@link #parseRow(int)} alike (so the check and the import see the same text).
     * Without such rules this is just {@code df.getString(fileColumnIndex)}.  {@code ""} for a row that has fewer
     * fields.  {@code df.getString(index)} is always the file's own, untouched text.
     * <br><br>
     * Meant for the indexes {@link #getUnknownColumnIndexes()} returns; the value of a declared column is read
     * with {@link #getString(String)}.
     *
     * @param fileColumnIndex the zero-based file column index, as listed by {@link #getUnknownColumnIndexes()}
     * @return the value, shortened if a rule shortened it
     * @throws IllegalArgumentException if that file column is not an unrecognized column (it is a declared column, has
     *                                  an empty title, or does not exist): a mistake in the program, not in the file
     */
    protected String getUnknownString(int fileColumnIndex) {
        if (!unknownIndexSet.contains(fileColumnIndex))
            throw new IllegalArgumentException("getUnknownString(" + fileColumnIndex + "): file column " + fileColumnIndex
                    + " is not an unrecognized column; the unrecognized columns are listed by getUnknownColumnIndexes().");
        final Column c = unknownRuleColumns.get(fileColumnIndex);
        if (c != null && c.text != null)
            return c.text;
        final String s = df.getString(fileColumnIndex);
        return s == null ? "" : s.trim();
    }

    /**
     * As {@link #getUnknownString(int)}, for the unrecognized column whose title is {@code title}, written as
     * {@link #getUnknownColumns()} has it (the file's own text, so compared exactly, including letter case).  If
     * several unrecognized columns have that title the first, in file order, is read; use the index to tell them
     * apart.
     *
     * @param title the title of the unrecognized column
     * @return the value, shortened if a rule shortened it; {@code ""} if no unrecognized column has that title (the
     * file may simply not have it), as {@link #getString(String)} answers for an absent declared column
     */
    protected String getUnknownString(String title) {
        for (int i : unknownColumnIndexes)
            if (fileColumnTitles.get(i).equals(title))
                return getUnknownString(i);
        return "";
    }

    /** @return true if the column exists and the current row has non-blank data in it */
    protected boolean columnHasData(String name) {
        return !getString(name).isEmpty();
    }

    /**
     * The trimmed value of the column in the current row, or "" if the file does not have the column.  If the column
     * was declared with {@link Column#truncate()} and the value is longer than its limit, this is the shortened
     * value (see {@link Column#maxLength(IntSupplier)}); it is shortened in this one place only, so every caller
     * ({@link #checkRow(int)} and {@link #parseRow(int)} alike) sees the same text.  {@code df.getString(...)} is
     * always the file's own, untouched text.
     */
    protected String getString(String name) {
        int i = df.getColumnIndex(name);
        if (i < 0)
            return "";
        final Column c = columnsByIndex.get(i);
        if (c != null && c.text != null)
            return c.text;
        String s = df.getString(i);
        return s == null ? "" : s.trim();
    }

    /**
     * The value of a column declared with {@link Column#integer()}, in the current row.  A typed getter returns what
     * the declared rules accepted: blank cells, cells that failed the rules (which are already reported as row
     * errors), and a column the file does not have all give {@code 0} (use {@link #columnHasData(String)} to tell
     * blank from zero), so it may be called freely from {@link #checkRow(int)}.  Valid in {@link #checkRow(int)} and
     * {@link #parseRow(int)}; there is no current row in {@link #checkHeader()}.
     *
     * @param name the column's logical name
     * @return the value, rounded to a whole number as {@link Column#integer()} describes
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column was not declared with {@code integer()}
     */
    protected int getInt(String name) {
        final Object v = typedValue(name, "getInt", Kind.INTEGER);
        return v == null ? 0 : ((BigDecimal) v).intValue();
    }

    /**
     * As {@link #getInt(String)}, for a column declared with {@link Column#integer()} or
     * {@link Column#longInteger()}.
     *
     * @param name the column's logical name
     * @return the value, or 0
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a whole number
     */
    protected long getLong(String name) {
        final Object v = typedValue(name, "getLong", Kind.INTEGER, Kind.LONG);
        return v == null ? 0L : ((BigDecimal) v).longValue();
    }

    /**
     * As {@link #getInt(String)}, for a column declared with {@link Column#integer()},
     * {@link Column#longInteger()} or {@link Column#decimal()}.  The value is exact (a decimal keeps the scale it was
     * written with).  A blank, invalid or absent value is {@code null}, since a number object can say "no value".
     *
     * @param name the column's logical name
     * @return the value, or null
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a number
     */
    protected BigDecimal getBigDecimal(String name) {
        return (BigDecimal) typedValue(name, "getBigDecimal", Kind.INTEGER, Kind.LONG, Kind.DECIMAL);
    }

    /**
     * As {@link #getBigDecimal(String)} as a {@code double}; 0 for a blank, invalid or absent value.
     *
     * @param name the column's logical name
     * @return the value, or 0
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a number
     */
    protected double getDouble(String name) {
        final BigDecimal v = getBigDecimal(name);
        return v == null ? 0.0 : v.doubleValue();
    }

    /**
     * The value of a column declared with {@link Column#date(String, String...)} or
     * {@link Column#dateTime(String, String...)}, in the current row (for a date-time, its date).  Same rules as
     * {@link #getInt(String)}, except that a blank, invalid or absent value is {@code null}.
     *
     * @param name the column's logical name
     * @return the date, or null
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a date or date-time
     */
    protected LocalDate getLocalDate(String name) {
        final Object v = typedValue(name, "getLocalDate", Kind.DATE, Kind.DATETIME);
        if (v == null)
            return null;
        return v instanceof LocalDateTime ? ((LocalDateTime) v).toLocalDate() : (LocalDate) v;
    }

    /**
     * The value of a column declared with {@link Column#dateTime(String, String...)}, in the current row; {@code null}
     * for a blank, invalid or absent value.  See {@link #getInt(String)}.
     *
     * @param name the column's logical name
     * @return the date and time, or null
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a date-time
     */
    protected LocalDateTime getLocalDateTime(String name) {
        return (LocalDateTime) typedValue(name, "getLocalDateTime", Kind.DATETIME);
    }

    /**
     * The date of a {@link Column#date(String, String...)} or {@link Column#dateTime(String, String...)} column as a
     * Kiss {@code int} date, YYYYMMDD (see {@link DateUtils}); 0 for a blank, invalid or absent value.
     *
     * @param name the column's logical name
     * @return the date as YYYYMMDD, or 0
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a date or date-time
     */
    protected int getIntDate(String name) {
        final LocalDate d = getLocalDate(name);
        return d == null ? 0 : DateUtils.toInt(d);
    }

    /**
     * The time of day of a {@link Column#dateTime(String, String...)} column as a Kiss {@code int} time, HHMM (see
     * {@link TimeUtils}); 0 for a blank, invalid or absent value.
     *
     * @param name the column's logical name
     * @return the time as HHMM, or 0
     * @throws IllegalArgumentException if no column of that name was declared
     * @throws IllegalStateException    if the column is not declared as a date-time
     */
    protected int getIntTime(String name) {
        final LocalDateTime dt = getLocalDateTime(name);
        return dt == null ? 0 : dt.getHour() * 100 + dt.getMinute();
    }

    /**
     * The parsed value of a typed column in the current row, or null if there is none.
     */
    private Object typedValue(String name, String getter, Kind... accepted) {
        Column c = columns.get(name);
        if (c == null) {
            final int i = df.getColumnIndex(name);
            if (i >= 0)
                c = columnsByIndex.get(i);
        }
        if (c == null)
            throw new IllegalArgumentException(getter + "(\"" + name + "\"): no column of that name was declared in initColumns().");
        for (Kind k : accepted)
            if (c.kind == k)
                return c.value;
        throw new IllegalStateException(getter + "(\"" + name + "\"): the column is declared as " + c.kind.what
                + "; declare it with " + Kind.declaration(accepted) + " in initColumns().");
    }

    /** Remove dollar signs and commas from a number so it can be parsed. */
    protected String fixNum(String num) {
        return num.replace("$", "").replace(",", "");
    }

    /**
     * Record an error.  During the row passes the current row number is prefixed.
     */
    protected void addError(String err) {
        errors.add(currentRow > 0 ? "Row " + currentRow + ": " + err : err);
    }

    /** Record a warning (does not stop the import). */
    protected void addWarning(String warn) {
        warnings.add(currentRow > 0 ? "Row " + currentRow + ": " + warn : warn);
    }

    /**
     * Shorten a value that the {@link Column column rules} cannot reach, and report the cut as a column rule would.
     * It is for a <i>derived</i> value: a part of a declared column's value, such as the street that the subclass
     * takes out of a full address cell, that must fit a width of its own.  (A declared column is shortened by
     * {@link Column#truncate()}, an unrecognized one by {@link #unknownColumnRules()}.)
     * <pre>
     *   String street = truncate("Street (from Company Address)", parts.street, widthOf("address", "street"));
     *   // Row 7: "Street (from Company Address)" was 45 characters long and was shortened to 30 characters.
     * </pre>
     * The value is cut to at most {@code max} characters, counted as a database column counts them (Unicode code
     * points; a character made of two UTF-16 units is never split); the cut keeps trailing white space it leaves,
     * exactly as {@link Column#truncate()} does.  A value that is not longer than {@code max} is returned as it is.
     * When the value is cut the warning
     * {@code Row n: "label" was X characters long and was shortened to N characters.} is recorded with
     * {@link #addWarning(String)}: the label is put between double quotes and should name the value as the user knows
     * it; the row number is that of the current row.
     * <br><br>
     * <b>Call it in both passes; it reports once.</b>  The value that is checked must be the value that is saved, so
     * call this method wherever the value is computed, in {@link #checkRow(int)} and in {@link #parseRow(int)} alike
     * (for instance from one method both call); it returns the same text in both.  The warning is recorded only while
     * the rows are <i>not</i> being imported, that is in the check pass of {@link #run()}, in {@link #check()} and in
     * the check phase of {@link #runChecked()}: the parse pass shortens the same value silently, which is how a cut
     * is listed once, as it is for a declared column.  Within one row the same warning (same label, same value,
     * same limit) is also recorded only once, so the method may be called again for the same value by several helpers
     * of {@code checkRow}.  A value that is only shortened in {@code parseRow} is therefore never reported: it must
     * be shortened in the check pass too.  Called outside the rows (for instance from {@link #checkHeader()}) it
     * records a warning without a row number.
     *
     * @param label the name of the value as the user knows it, without quotes (not null)
     * @param value the text; null is returned as null
     * @param max   the most characters allowed; {@link Integer#MAX_VALUE} means no limit
     * @return the value, shortened to {@code max} characters if it was longer
     * @throws IllegalArgumentException if {@code max} is negative
     */
    protected String truncate(String label, String value, int max) {
        Objects.requireNonNull(label, "label");
        if (max < 0)
            throw new IllegalArgumentException("truncate(\"" + label + "\"): the maximum length " + max + " must not be negative.");
        if (value == null || max == Integer.MAX_VALUE)
            return value;
        final int n = value.codePointCount(0, value.length());
        if (n <= max)
            return value;
        if (!parsing) {
            final String warn = shortenedMessage("\"" + label + "\"", n, max);
            if (shortenedInRow.add(warn))
                addWarning(warn);
        }
        return cutTo(value, max);
    }

    /** The first {@code max} characters (code points) of a value that is longer than that. */
    private static String cutTo(String value, int max) {
        return value.substring(0, value.offsetByCodePoints(0, max));
    }

    /** The warning for a value that was cut: {@code "Job Title" was 45 characters long and was shortened to 30 characters.} */
    private static String shortenedMessage(String quotedName, int length, int max) {
        return quotedName + " was " + chars(length) + " long and was shortened to " + chars(max) + ".";
    }

    /** @return the number of errors so far */
    protected int errorCount() {
        return errors.size();
    }

    /**
     * The warnings recorded so far: a value shortened by a {@link Column#truncate()} rule, and anything the subclass
     * reported with {@link #addWarning(String)}.  A warning never makes a file unacceptable.  After {@link #run()} or
     * {@link #runChecked()} the same list is {@link Result#getWarnings()}; after {@link #check()}, which returns only
     * the problems, this is the way to read them.  The list is a snapshot, in the order they were recorded; row
     * warnings are {@code "Row n: ..."} like the problems.
     *
     * @return the warnings, possibly empty
     */
    public List<String> getWarnings() {
        return Collections.unmodifiableList(new ArrayList<>(warnings));
    }

    /** Set how many rows are processed between {@link #commitBatch()} calls (default 20). */
    public void setBatchSize(int n) {
        batchSize = Math.max(1, n);
    }

    /** Ask this class to write the summary / error log to the given file with plain java.io.  Not written if never set. */
    public void setLogPath(String path) {
        logPath = path;
    }

    /**
     * Check the entire file without importing anything.  Reads the title line, applies the header validation, makes
     * sure every line of the file can be read, and calls {@link #checkRow(int)} for every data row.  Nothing is
     * imported: {@link #parseRow(int)}, {@link #commitBatch()}, {@link #deactivateMissing()} and
     * {@link #onFinish(Result)} are not called.  The subclass's {@link #initColumns()} and its
     * {@link #checkHeader()} hook are.
     * <br><br>
     * <b>The problems returned</b> are all of them, with no limit, in file order, as plain strings meant for the
     * user.  A problem in a data row is {@code "Row n: problem"}, where {@code n} counts the data rows (blank lines
     * are skipped; the title line is not counted, so the first data row is Row 1).  The header problems are:
     * <ul>
     *   <li>{@code The file is missing its first line of column titles.}</li>
     *   <li>{@code Duplicate column titles (A, B); each column may appear only once, even under a different accepted name.}</li>
     *   <li>{@code The file is missing the required column "Name".} (one per missing required column)</li>
     *   <li>{@code The file has no recognizable columns.}</li>
     * </ul>
     * A header problem that makes the rows meaningless (no title line, a missing required column, no recognizable
     * column) is reported <i>alone</i>: {@link #checkHeader()} and {@link #checkRow(int)} are not called.  A repeated
     * title is reported and the rows are still checked (the last of the repeated columns is the one a lookup by name
     * reads, exactly as in the import).  A line that cannot be read is
     * {@code "Row n could not be read: message"} and checking continues; an I/O failure ends the check with
     * {@code "The file could not be read after row n: message"}.
     * <br><br>
     * The returned list is empty if, and only if, the file may be imported.  The header validation is the same
     * implementation {@link #run()} uses; only the wording of the file-level messages differs (run() keeps its
     * original technical wording).
     * <br><br>
     * An exception thrown by {@link #initColumns()}, {@link #checkHeader()} or {@link #checkRow(int)} is not
     * converted into a problem: it propagates to the caller (after the file is closed), because it is a failure of
     * the program, not a defect of the file.
     * <br><br>
     * Like {@link #run()}, this consumes the instance: the file is closed when it returns.  To import after a clean
     * check either use {@link #runChecked()} (check and import in one call) or create a new instance for the import.
     * Warnings (a value shortened by {@link Column#truncate()}, or anything added with {@link #addWarning(String)})
     * are not part of the returned list, which holds only problems; read them from {@link #getWarnings()} afterwards.
     * <br><br>
     * The returned problems include those of the declared {@link Column column rules}, which are applied to every
     * row before {@link #checkRow(int)}.  The limit of a {@link Column#maxLength(IntSupplier) run-time length} is
     * read once, after the title line is accepted.
     *
     * @return every problem found; empty if the file is acceptable
     * @throws IllegalStateException if this instance was already used by {@code run()}, {@code runChecked()} or {@code check()}
     */
    public List<String> check() {
        if (used)
            throw new IllegalStateException("A DelimitedFileImport instance can be used only once; create a new instance.");
        used = true;
        checkWording = true;
        try {
            initColumns();
            if (readHeader()) {
                bindColumns();
                checkHeader();
                readRows(false);
            }
        } catch (IOException e) {
            currentRow = 0;
            addError("The file could not be read: " + e.getMessage());
        } finally {
            currentRow = 0;
            df.close();
        }
        return new ArrayList<>(errors);
    }

    /**
     * Run the import.  Never throws; failures are reported in the result.
     * <br><br>
     * The header is validated, and if it has any problem nothing else is done.  Otherwise every row is checked and,
     * only if there was no error at all, every row is imported.  See {@link #runChecked()} for the variant that
     * reports all the problems of a file at once.
     *
     * @return the result
     */
    public Result run() {
        return execute(false);
    }

    /**
     * Check the whole file and import it only if the check found nothing wrong (all-or-nothing).  Never throws;
     * failures are reported in the result.
     * <br><br>
     * The check phase is exactly that of {@link #check()}: the same header validation, the same
     * {@link #checkHeader()} and {@link #checkRow(int)} calls, every problem collected with the same wording and
     * numbering, a repeated column title reported while the rows are still checked.  Where {@link #run()} stops at
     * a header problem, this reports the header problems and the row problems together.  If the problem list is
     * empty the parse pass then runs exactly as in {@link #run()} (including {@link #deactivateMissing()} in
     * {@link #FULL_MODE}); otherwise nothing is imported.  The result's errors are the problems.
     * <br><br>
     * A caller that must refuse a bad file before starting a background import calls {@link #check()} first on one
     * instance (for example in the upload request) and then {@code run()} or {@code runChecked()} on a new instance
     * in the background: either re-checks the file, which is the right thing to do if the data the rules read may
     * have changed in between.
     *
     * @return the result
     */
    public Result runChecked() {
        return execute(true);
    }

    private Result execute(boolean checked) {
        if (used) {
            List<String> err = new ArrayList<>();
            err.add("This import was already run or checked; create a new instance to run it again.");
            return new Result(typeOfImport, new File(importFileName).getName(), err, new ArrayList<>(), 0, 0, 0, 0);
        }
        used = true;
        checkWording = checked;
        try {
            initColumns();
            final boolean usable = readHeader();
            //run() stops at the first header problem; runChecked() stops only at one that makes the rows meaningless.
            if (usable && (checked || errors.isEmpty())) {
                bindColumns();
                checkHeader();
                if (checked || errors.isEmpty())
                    passes();
            }
        } catch (Throwable t) {
            currentRow = 0;
            addError("Unexpected error: " + t);
            log(LOG_ERROR, "Unexpected error: " + t);
            try {
                onFailure(t);
            } catch (Throwable ignored) {
            }
        } finally {
            df.close();
        }
        currentRow = 0;
        Result r = new Result(typeOfImport, new File(importFileName).getName(), new ArrayList<>(errors),
                new ArrayList<>(warnings), rowsRead, recordsAdded, recordsUpdated, recordsDeactivated);
        writeLog(r);
        try {
            onFinish(r);
        } catch (Throwable t) {
            log(LOG_ERROR, "onFinish failed: " + t);
        }
        return r;
    }

    /**
     * Read and validate the title line.  This is the single implementation of the header rules, used by
     * {@link #run()}, {@link #runChecked()} and {@link #check()}.  Problems are added to the error list.
     *
     * @return true if the rows can be meaningfully checked: there is a title line, every required column is present
     * and at least one declared column is present.  A repeated title is an error but does not make it false.
     */
    private boolean readHeader() {
        df.readHeader();
        captureTitles();
        if (df.size() == 0) {
            addError(checkWording ? "The file is missing its first line of column titles."
                    : "import file is missing the expected (first) column title line");
            return false;
        }
        boolean usable = true;
        for (String dup : df.getDuplicateHeaders())
            addError(checkWording
                    ? "Duplicate column titles (" + dup + "); each column may appear only once, even under a different accepted name."
                    : "import file has duplicate column titles (" + dup + ")");
        for (Column c : columns.values())
            if (c.required && !colExists(c.name)) {
                addError(checkWording ? "The file is missing the required column \"" + c.name + "\"."
                        : "import file is missing required column " + c.name);
                usable = false;
            }
        List<String> found = new ArrayList<>();
        for (String name : columns.keySet())
            if (colExists(name))
                found.add(name);
        if (found.isEmpty()) {
            addError(checkWording ? "The file has no recognizable columns."
                    : "import file has no usable / recognizable columns");
            return false;
        }
        found.sort((x, y) -> Integer.compare(df.getColumnIndex(x), df.getColumnIndex(y)));
        fileColumns.addAll(found);
        return usable;
    }

    /**
     * Record every title of the title line, and which of its columns no declared column reads.  Called once, right
     * after the reader has read the title line, so both lists are complete before any hook sees them.  The columns
     * that are read are those the reader resolves the declared names to ({@code df.getColumnIndex}), which is the
     * resolution the header validation and the {@code get*} helpers use.
     */
    private void captureTitles() {
        fileColumnTitles.clear();
        unknownColumnIndexes.clear();
        unknownIndexSet.clear();
        unknownRuleColumns.clear();
        for (int i = 0; i < df.size(); i++) {
            String t = df.getString(i);
            fileColumnTitles.add(t == null ? "" : t);
        }
        Set<Integer> read = new HashSet<>();
        for (String name : columns.keySet()) {
            int i = df.getColumnIndex(name);
            if (i >= 0)
                read.add(i);
        }
        for (int i = 0; i < fileColumnTitles.size(); i++)
            if (!fileColumnTitles.get(i).isEmpty() && !read.contains(i)) {
                unknownColumnIndexes.add(i);
                unknownIndexSet.add(i);
            }
    }

    private void passes() throws IOException {
        int n = readRows(false);
        if (!errors.isEmpty())
            return;
        df.moveToStart();
        df.readHeader();
        rowsRead = 0;
        recordsAdded = recordsUpdated = recordsDeactivated = 0;
        readRows(true);
        currentRow = 0;
        if (mode == FULL_MODE && errors.isEmpty())
            deactivateMissing();
        log(LOG_INFO, typeOfImport + " import processed " + n + " rows");
    }

    private int readRows(boolean parse) throws IOException {
        parsing = parse;
        try {
            return readRowsOf(parse);
        } finally {
            parsing = false;
            shortenedInRow.clear();
        }
    }

    private int readRowsOf(boolean parse) throws IOException {
        int n = 0;
        while (true) {
            try {
                if (!df.nextLine())
                    break;
            } catch (IOException ioe) {
                if (!checkWording)
                    throw ioe;
                currentRow = 0;
                addError("The file could not be read after row " + n + ": " + ioe.getMessage());
                break;
            } catch (Exception e) {
                currentRow = ++n;
                if (checkWording) {
                    currentRow = 0;
                    addError("Row " + n + " could not be read: " + e.getMessage());
                } else
                    addError(e.getMessage());
                continue;
            }
            currentRow = ++n;
            rowsRead++;
            shortenedInRow.clear();
            prepareRow(!parse);
            if (parse) {
                parseRow(n);
                if (n % batchSize == 0)
                    commitBatch();
            } else
                checkRow(n);
        }
        currentRow = 0;
        clearRowValues();
        if (parse)
            commitBatch();
        return n;
    }

    /**
     * Resolve the declared columns against the file's title line.  Called once, after the title line has been
     * accepted and before {@link #checkHeader()}.  For each declared column the file has: its file index, the title
     * the file uses for it (for messages), and the value of its run-time length limit.  A column the file does not
     * have is skipped, so its limit callback is not called.
     */
    private void bindColumns() {
        columnsByIndex.clear();
        ruleColumns.clear();
        for (Column c : columns.values()) {
            c.fileIndex = df.getColumnIndex(c.name);
            if (c.fileIndex < 0)
                continue;
            columnsByIndex.put(c.fileIndex, c);
            c.title = c.fileIndex < fileColumnTitles.size() ? fileColumnTitles.get(c.fileIndex) : "";
            if (c.title.isEmpty())
                c.title = c.name;
            c.limit = Integer.MAX_VALUE;
            if (c.maxLength != null) {
                c.limit = c.maxLength.getAsInt();
                if (c.limit < 0)
                    throw new IllegalStateException("The maximum length of column \"" + c.name + "\" is " + c.limit
                            + "; it must not be negative.");
            }
            if (c.hasRules())
                ruleColumns.add(c);
        }
        bindUnknownColumns();
        ruleColumns.sort((x, y) -> Integer.compare(x.fileIndex, y.fileIndex));
    }

    /**
     * Bind the rules of {@link #unknownColumnRules()} to each unrecognized column: a copy of the rules for every
     * column, so each has its own title for messages and its own value for the row.  The run-time length limit is read
     * once and shared, and only if the file has an unrecognized column.
     */
    private void bindUnknownColumns() {
        unknownRuleColumns.clear();
        if (unknownRule == null || !unknownRule.hasRules() || unknownColumnIndexes.isEmpty())
            return;
        int limit = Integer.MAX_VALUE;
        if (unknownRule.maxLength != null) {
            limit = unknownRule.maxLength.getAsInt();
            if (limit < 0)
                throw new IllegalStateException("The maximum length of column \"" + unknownRule.name + "\" is " + limit
                        + "; it must not be negative.");
        }
        for (int i : unknownColumnIndexes) {
            final Column c = new Column(unknownRule);
            c.fileIndex = i;
            c.title = shown(fileColumnTitles.get(i));
            c.limit = limit;
            unknownRuleColumns.put(i, c);
            ruleColumns.add(c);
        }
    }

    /**
     * Apply the declared column rules to the row the reader is on, before the subclass sees it.  This is the one
     * place a value is shortened or parsed: {@link #getString(String)} and the typed getters read what is computed
     * here.  In the check pass ({@code report} true) the problems and the truncation warnings are recorded as row
     * messages; the parse pass computes the same values and records nothing, so a warning is never listed twice.
     */
    private void prepareRow(boolean report) {
        for (Column c : ruleColumns) {
            final String raw = df.getString(c.fileIndex);
            c.evaluate(raw == null ? "" : raw.trim(), report);
        }
    }

    /** Forget the values of the last row, so nothing stale can be read once the rows are done. */
    private void clearRowValues() {
        for (Column c : ruleColumns) {
            c.text = null;
            c.value = null;
        }
    }

    private void writeLog(Result r) {
        if (logPath == null)
            return;
        try (PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(logPath)))) {
            pw.print(r.getSummary());
        } catch (Exception e) {
            log(LOG_ERROR, "Unable to write log file: " + e);
        }
    }

    // ======================================================================================================
    // Declared column rules
    // ======================================================================================================

    /** What a declared column holds.  TEXT is the default: no type, the value is just a string. */
    private enum Kind {
        TEXT("text", "no type"),
        INTEGER("a whole number", "integer()"),
        LONG("a 64-bit whole number", "longInteger()"),
        DECIMAL("a decimal number", "decimal()"),
        DATE("a date", "date()"),
        DATETIME("a date and time", "dateTime()");

        final String what;
        final String method;

        Kind(String what, String method) {
            this.what = what;
            this.method = method;
        }

        static String declaration(Kind... kinds) {
            final StringBuilder sb = new StringBuilder();
            for (Kind k : kinds) {
                if (sb.length() > 0)
                    sb.append(" or ");
                sb.append(k.method);
            }
            return sb.toString();
        }
    }

    /** The name of the rules of the unrecognized columns, as the messages about a mistaken declaration give it. */
    private static final String UNRECOGNIZED_COLUMNS = "(unrecognized columns)";

    /** The two-digit year window of {@link DateUtils}: a year no more than 10 years ahead is 20yy, otherwise 19yy. */
    private static int twoDigitYearBase() {
        return DateUtils.year(DateUtils.today()) - 89;
    }

    /** A date a pattern is tried on when a column is declared, and shown to the user as an example of the pattern. */
    private static final LocalDateTime SAMPLE = LocalDateTime.of(2024, 3, 25, 14, 30);

    /** The formats {@link Column#date()} accepts: the ones {@link DateUtils#parse(String)} accepts. */
    private static final String[] DEFAULT_DATE_FORMATS = {"M/d/yyyy", "M/d/yy", "M.d.yyyy", "M.d.yy", "yyyy-M-d"};

    private static final Pattern NUMBER = Pattern.compile("[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)");
    private static final BigDecimal INT_MIN = BigDecimal.valueOf(Integer.MIN_VALUE);
    private static final BigDecimal INT_MAX = BigDecimal.valueOf(Integer.MAX_VALUE);
    private static final BigDecimal LONG_MIN = BigDecimal.valueOf(Long.MIN_VALUE);
    private static final BigDecimal LONG_MAX = BigDecimal.valueOf(Long.MAX_VALUE);

    /**
     * Read a number the way people write them in a spreadsheet: dollar signs, commas and white space are ignored
     * ({@code "$1,500,000"}, {@code "1 500"}), a leading sign is allowed, and a decimal point may be used.  Nothing
     * else is allowed (no exponent, no percent sign, no text).
     *
     * @return the number, or null if the text is not one
     */
    private static BigDecimal parseNumber(String text) {
        final StringBuilder sb = new StringBuilder(text.length());
        for (int i = 0; i < text.length(); i++) {
            final char ch = text.charAt(i);
            if (ch == '$' || ch == ',' || Character.isWhitespace(ch) || ch == ' ' || ch == ' ')
                continue;
            sb.append(ch);
        }
        final String s = sb.toString();
        return NUMBER.matcher(s).matches() ? new BigDecimal(s) : null;
    }

    /**
     * Build the formatter for a date pattern, which is a {@link DateTimeFormatter} pattern with these differences: a
     * year ({@code y}) is the calendar year; {@code yyyy} accepts exactly a four-digit year and {@code yy} a
     * two-digit year read with the window of {@link DateUtils}; text is matched without regard to case; and the
     * text is read strictly, so a day that the month does not have (February 30) is an error rather than being moved
     * to the end of the month.  Month names and AM/PM are English.
     */
    private static DateTimeFormatter buildFormatter(String pattern) {
        if (pattern == null || pattern.trim().isEmpty())
            throw new IllegalArgumentException("A date format must not be blank.");
        final DateTimeFormatterBuilder b = new DateTimeFormatterBuilder().parseCaseInsensitive();
        final StringBuilder seg = new StringBuilder();
        boolean quoted = false;
        int i = 0;
        final int n = pattern.length();
        while (i < n) {
            final char ch = pattern.charAt(i);
            if (ch == '\'') {
                quoted = !quoted;
                seg.append(ch);
                i++;
            } else if (!quoted && ch == 'y') {
                int j = i;
                while (j < n && pattern.charAt(j) == 'y')
                    j++;
                if (j - i == 2) {
                    if (seg.length() > 0) {
                        b.appendPattern(seg.toString());
                        seg.setLength(0);
                    }
                    b.appendValueReduced(ChronoField.YEAR, 2, 2, twoDigitYearBase());
                } else
                    for (int k = i; k < j; k++)
                        seg.append('u');
                i = j;
            } else {
                seg.append(ch);
                i++;
            }
        }
        if (seg.length() > 0)
            b.appendPattern(seg.toString());
        return b.toFormatter(Locale.ENGLISH).withResolverStyle(ResolverStyle.STRICT);
    }

    /** A number for a message: thousands separators, and exactly the decimals it has. */
    private static String fmt(BigDecimal v) {
        final DecimalFormat f = new DecimalFormat("#,##0.#", DecimalFormatSymbols.getInstance(Locale.US));
        f.setMaximumFractionDigits(Math.max(v.scale(), 0));
        return f.format(v);
    }

    /** The text of a cell for a message, shortened if it is very long. */
    private static String shown(String raw) {
        if (raw.length() <= 60)
            return raw;
        int end = 57;
        if (Character.isHighSurrogate(raw.charAt(end - 1)))
            end--;      //do not leave half of a character
        return raw.substring(0, end) + "...";
    }

    private static String chars(int n) {
        return n + (n == 1 ? " character" : " characters");
    }

    /** " from 0 to 1,000,000", " of at least 0", " of at most 9", or "" for no bounds. */
    private static String bounds(BigDecimal lo, BigDecimal hi) {
        if (lo != null && hi != null)
            return " from " + fmt(lo) + " to " + fmt(hi);
        if (lo != null)
            return " of at least " + fmt(lo);
        if (hi != null)
            return " of at most " + fmt(hi);
        return "";
    }

    /**
     * The rules of one declared column, returned by {@link DelimitedFileImport#addColumn(String)} and
     * {@link DelimitedFileImport#addColumn(String, boolean, String...)} so that they can be chained in
     * {@link DelimitedFileImport#initColumns()}:
     * <pre>
     *   addColumn("Last Name", true, "LNAME", "Surname").notBlank().maxLength(30);
     *   addColumn("Job Title").maxLength(() -&gt; widthOf("person", "job_title")).truncate();
     *   addColumn("# Employees").integer().range(0, 1_000_000);
     *   addColumn("Revenue").decimal().min(0);
     *   addColumn("Start Date").date("MM/dd/yyyy").range(LocalDate.of(2020, 1, 1), LocalDate.of(2030, 12, 31));
     *   addColumn("Activity Time").dateTime("MM-dd-yyyy hh:mm a");
     * </pre>
     * <b>What the rules do.</b>  The rules of every declared column run automatically, once per data row, in
     * {@link DelimitedFileImport#check()}, in the check phase of {@link DelimitedFileImport#runChecked()} and in the
     * check pass of {@link DelimitedFileImport#run()}, <i>before</i> {@link DelimitedFileImport#checkRow(int)} is
     * called for the row.  A rule that is not met is a row error ({@code "Row 5: ..."}) and every one is recorded;
     * nothing is capped.  The columns are named in the messages by the title the file itself uses
     * ({@link DelimitedFileImport#getFileColumnTitle(String)}), not by the logical name, so the user sees their own
     * header.  A rule applies only to a column the file has; a column the file lacks is not checked (use
     * {@link #required()} to require it).
     * <br><br>
     * <b>Blank values.</b>  A cell that is blank (empty or white space) is valid unless the column is
     * {@link #notBlank()}; a blank cell skips the length, number and date checks.  If a column is both
     * {@link #required()} and {@code notBlank()}, both rules apply: the file must have the column, and no row may
     * leave it blank.
     * <br><br>
     * <b>A column is text, a number or a date.</b>  Without a type it is text: {@link #maxLength} and
     * {@link #truncate()} apply.  {@link #integer()}, {@link #longInteger()}, {@link #decimal()}, {@link #date()} and
     * {@link #dateTime(String, String...)} give it a type, checked in every row, and the matching typed getter of
     * {@link DelimitedFileImport} ({@code getInt}, {@code getLong}, {@code getBigDecimal}, {@code getDouble},
     * {@code getLocalDate}, {@code getLocalDateTime}, {@code getIntDate}, {@code getIntTime}) returns the parsed
     * value.  {@code truncate()} cannot be combined with a type.  A type is declared before its
     * {@code min}/{@code max}/{@code range}.
     * <br><br>
     * <b>The unrecognized columns.</b>  {@link DelimitedFileImport#unknownColumnRules()} returns the same declaration for
     * the rules of every column that no declared column reads; every rule above applies to each of them separately,
     * except {@link #required()} and {@link #aliases(String...)}, which are about a declared column's title and throw
     * {@link IllegalStateException} there.
     * <br><br>
     * A mistake in the declaration itself (a bad date format, a range on a text column, two types) throws
     * {@link IllegalArgumentException} or {@link IllegalStateException} at once, from {@code initColumns()}.
     */
    public final class Column {
        private final String name;
        private boolean required;
        private boolean unrecognized;       // the rules of the unrecognized columns (see unknownColumnRules()), not a declared column
        private boolean notBlank;
        private IntSupplier maxLength;
        private boolean truncate;
        private Kind kind = Kind.TEXT;
        private BigDecimal minNumber, maxNumber;
        private LocalDateTime minTime, maxTime;     // date bounds are kept as the start of their day
        private List<DateTimeFormatter> formats = Collections.emptyList();
        private List<String> examples = Collections.emptyList();

        //Per run, set when the title line has been read.
        private int fileIndex = -1;
        private String title = "";
        private int limit = Integer.MAX_VALUE;

        //Per row, set before the row is checked or parsed.  text is null outside the rows.
        private String text;
        private Object value;

        private Column(String name) {
            this.name = name;
        }

        /** A copy of the rules of {@code rules}, to be bound to one file column (see {@code bindUnknownColumns}). */
        private Column(Column rules) {
            name = rules.name;
            unrecognized = rules.unrecognized;
            notBlank = rules.notBlank;
            maxLength = rules.maxLength;
            truncate = rules.truncate;
            kind = rules.kind;
            minNumber = rules.minNumber;
            maxNumber = rules.maxNumber;
            minTime = rules.minTime;
            maxTime = rules.maxTime;
            formats = rules.formats;
            examples = rules.examples;
        }

        // -------------------------------------------------------------------------- the header

        /**
         * The file must have this column; a file without it is refused with
         * {@code The file is missing the required column "name".}  Same as declaring the column with
         * {@code required = true}.  This is a rule about the title line only; see {@link #notBlank()} for the rows.
         *
         * @return this column
         * @throws IllegalStateException on the rules of {@link DelimitedFileImport#unknownColumnRules()}
         */
        public Column required() {
            return required(true);
        }

        /**
         * Say whether the file must have this column; {@link #required()} is {@code required(true)}.
         *
         * @param required true if the file must have this column
         * @return this column
         * @see #required()
         */
        public Column required(boolean required) {
            notForUnrecognized("required");
            this.required = required;
            return this;
        }

        /**
         * Other titles the file may use for this column; matching ignores case, white space, hyphens, underscores and
         * non-printable characters, like the logical name.
         *
         * @param aliases the other titles
         * @return this column
         * @throws IllegalStateException on the rules of {@link DelimitedFileImport#unknownColumnRules()}
         */
        public Column aliases(String... aliases) {
            notForUnrecognized("aliases");
            if (aliases.length > 0)
                df.addAlias(name, aliases);
            return this;
        }

        // -------------------------------------------------------------------------- every row

        /**
         * The column, when the file has it, must have a value in every data row: a blank cell, or a row too short to
         * reach the column, is the row error {@code "Last Name" must have a value.}  Independent of
         * {@link #required()}, which is about the title line.
         *
         * @return this column
         */
        public Column notBlank() {
            notBlank = true;
            return this;
        }

        // -------------------------------------------------------------------------- text

        /**
         * A text value may be at most {@code max} characters (Unicode code points, as a database column counts
         * them).  By default a longer value is the row error
         * {@code "Job Title" is 45 characters long; at most 30 are allowed.}  With {@link #truncate()} it is
         * shortened instead.  A negative limit is an {@link IllegalArgumentException}.
         *
         * @param max the longest allowed value; {@link Integer#MAX_VALUE} means no limit
         * @return this column
         */
        public Column maxLength(int max) {
            if (max < 0)
                throw new IllegalArgumentException("The maximum length of column \"" + name + "\" must not be negative.");
            return maxLength(() -> max);
        }

        /**
         * As {@link #maxLength(int)}, with a limit that is known only at run time, such as the width of a database
         * column.  The supplier is called once per run, when the title line has been accepted and before the first row
         * is checked, and only if the file has the column; it is not called again for the import pass.  Whatever it
         * throws propagates like an exception from {@code checkRow}.  A result of {@link Integer#MAX_VALUE} (for
         * instance {@code Connection.UNLIMITED_COLUMN_WIDTH}) means no limit; a negative result is an
         * {@link IllegalStateException}.
         * <pre>
         *   addColumn("Job Title").maxLength(() -&gt; widthOf("person", "job_title")).truncate();
         *   addColumn("City").maxLength(() -&gt; { try { return db.getColumnWidth("address", "city"); }
         *                                         catch (SQLException e) { throw new IllegalStateException(e); } });
         * </pre>
         *
         * @param max supplies the longest allowed value
         * @return this column
         */
        public Column maxLength(IntSupplier max) {
            maxLength = Objects.requireNonNull(max, "max");
            return this;
        }

        /**
         * A value longer than the {@link #maxLength} limit is cut to the limit instead of being an error.  The
         * cutting happens in one place, before the row is checked or parsed, so
         * {@link DelimitedFileImport#getString(String)} returns the shortened value in {@code checkRow} and
         * {@code parseRow} alike; {@code df.getString} is still the file's own text.  It is not an error, but it is
         * not silent: each shortened value is recorded once, in the check pass, as the warning
         * {@code Row 4: "Job Title" was 45 characters long and was shortened to 30 characters.}  Read the warnings
         * with {@link DelimitedFileImport#getWarnings()} (after {@code check()}) or {@link Result#getWarnings()}.
         * The cut never splits a character that is made of two UTF-16 units, and trailing white space the cut leaves
         * is kept.  Has no effect without a {@link #maxLength}.
         *
         * @return this column
         * @throws IllegalStateException if the column is a number or a date
         */
        public Column truncate() {
            if (kind != Kind.TEXT)
                throw new IllegalStateException("Column \"" + name + "\": truncate() applies to text only, and the column is declared as " + kind.what + ".");
            truncate = true;
            return this;
        }

        // -------------------------------------------------------------------------- numbers

        /**
         * The value must be a whole number that fits an {@code int}.  It is read leniently: dollar signs, commas and
         * white space are ignored ({@code "$1,500,000"}), a leading {@code +} or {@code -} is allowed, and a value
         * with a decimal point is rounded to the nearest whole number, halves up ({@code "1500.5"} is 1501), before
         * the range is checked.  Anything else (text, {@code "50-100"}, an exponent) is the row error
         * {@code "# Employees" must be a whole number from 0 to 1,000,000 (found "50-100").}, naming the range if one
         * was declared.  Read it with {@code getInt}, {@code getLong} or {@code getBigDecimal}.
         *
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column integer() {
            return type(Kind.INTEGER);
        }

        /**
         * As {@link #integer()}, for a value that fits a {@code long}; read it with {@code getLong} or
         * {@code getBigDecimal}.
         *
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column longInteger() {
            return type(Kind.LONG);
        }

        /**
         * The value must be a number, read as {@link #integer()} reads one but not rounded: {@code "$1,234.50"} is
         * exactly 1234.50.  Read it with {@code getBigDecimal} (exact) or {@code getDouble}.
         *
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column decimal() {
            return type(Kind.DECIMAL);
        }

        /**
         * The smallest value allowed (inclusive), for an {@link #integer()}, {@link #longInteger()} or
         * {@link #decimal()} column.  The declared type comes first.
         *
         * @param lowest the smallest allowed value
         * @return this column
         * @throws IllegalStateException if the column is not a number
         */
        public Column min(long lowest) {
            return min(BigDecimal.valueOf(lowest));
        }

        /**
         * As {@link #min(long)}.
         *
         * @param lowest the smallest allowed value
         * @return this column
         */
        public Column min(double lowest) {
            return min(toDecimal(lowest));
        }

        /**
         * As {@link #min(long)}, exactly.
         *
         * @param lowest the smallest allowed value; null for no minimum
         * @return this column
         */
        public Column min(BigDecimal lowest) {
            needNumber("min");
            minNumber = lowest;
            checkOrder(minNumber, maxNumber);
            return this;
        }

        /**
         * The largest value allowed (inclusive), for a number column.
         *
         * @param highest the largest allowed value
         * @return this column
         * @throws IllegalStateException if the column is not a number
         */
        public Column max(long highest) {
            return max(BigDecimal.valueOf(highest));
        }

        /**
         * As {@link #max(long)}.
         *
         * @param highest the largest allowed value
         * @return this column
         */
        public Column max(double highest) {
            return max(toDecimal(highest));
        }

        /**
         * As {@link #max(long)}, exactly.
         *
         * @param highest the largest allowed value; null for no maximum
         * @return this column
         */
        public Column max(BigDecimal highest) {
            needNumber("max");
            maxNumber = highest;
            checkOrder(minNumber, maxNumber);
            return this;
        }

        /**
         * Both bounds (inclusive) of a number column: {@code integer().range(0, 1_000_000)} is
         * {@code integer().min(0).max(1_000_000)}.
         *
         * @param lowest  the smallest allowed value
         * @param highest the largest allowed value
         * @return this column
         * @throws IllegalArgumentException if {@code lowest} is greater than {@code highest}
         * @throws IllegalStateException    if the column is not a number
         */
        public Column range(long lowest, long highest) {
            return range(BigDecimal.valueOf(lowest), BigDecimal.valueOf(highest));
        }

        /**
         * As {@link #range(long, long)}.
         *
         * @param lowest  the smallest allowed value
         * @param highest the largest allowed value
         * @return this column
         */
        public Column range(double lowest, double highest) {
            return range(toDecimal(lowest), toDecimal(highest));
        }

        /**
         * As {@link #range(long, long)}, exactly.  Either bound may be null (not limited); to leave one end open
         * without writing {@code null}, use {@link #min(BigDecimal)} or {@link #max(BigDecimal)}.
         *
         * @param lowest  the smallest allowed value, or null
         * @param highest the largest allowed value, or null
         * @return this column
         */
        public Column range(BigDecimal lowest, BigDecimal highest) {
            needNumber("range");
            checkOrder(lowest, highest);
            minNumber = lowest;
            maxNumber = highest;
            return this;
        }

        // -------------------------------------------------------------------------- dates

        /**
         * The value must be a date in one of the forms {@link DateUtils#parse(String)} reads: {@code M/d/yyyy},
         * {@code M/d/yy}, {@code M.d.yyyy}, {@code M.d.yy} and {@code yyyy-M-d}.  A two-digit year follows the window
         * of {@code DateUtils} (no more than 10 years ahead is 20yy, otherwise 19yy).  Unlike {@code DateUtils.parse},
         * a day the month does not have (2/30/2024) is an error.  See {@link #date(String, String...)}.
         *
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column date() {
            return setDate(Kind.DATE, DEFAULT_DATE_FORMATS);
        }

        /**
         * The value must be a date written in one of the given formats; the first that reads the whole cell is used.
         * A format is a {@link DateTimeFormatter} pattern ({@code MM/dd/yyyy}, {@code d MMM yyyy}), with these
         * differences: {@code yyyy} accepts exactly a four-digit year (so {@code 03/15/24} is refused, which is the
         * point of using it) and {@code yy} a two-digit year read with the window of {@link DateUtils}; {@code MM} and
         * {@code dd} need two digits where {@code M} and {@code d} accept one or two; letter case is ignored; the text is
         * read strictly (2/30/2024 is refused, trailing text is refused); month names and AM/PM are English.  A
         * format that is not a pattern, or cannot read the date it writes, is an {@link IllegalArgumentException}
         * from the declaration.
         * <br><br>
         * A bad value is the row error
         * {@code "Start Date" must be a date such as 03/25/2024 (found "13/45/2024").}, the example being the format
         * applied to a sample date.  Read the value with {@code getLocalDate} or {@code getIntDate}.
         *
         * @param format      the first accepted format
         * @param moreFormats other accepted formats
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column date(String format, String... moreFormats) {
            return setDate(Kind.DATE, join(format, moreFormats));
        }

        /**
         * As {@link #date(String, String...)}, for a date and a time of day, which the format must include
         * ({@code MM-dd-yyyy hh:mm a}; a 12-hour {@code h} needs {@code a}).  The row error is
         * {@code "Activity Time" must be a date and time such as 03-25-2024 02:30 PM (found "...").}  Read the
         * value with {@code getLocalDateTime}, {@code getLocalDate}, {@code getIntDate} and {@code getIntTime}.
         *
         * @param format      the first accepted format
         * @param moreFormats other accepted formats
         * @return this column
         * @throws IllegalStateException if the column already has a type or is {@link #truncate() truncated}
         */
        public Column dateTime(String format, String... moreFormats) {
            return setDate(Kind.DATETIME, join(format, moreFormats));
        }

        /**
         * The earliest date allowed (inclusive), for a {@link #date()} column.
         *
         * @param earliest the earliest allowed date; null for no minimum
         * @return this column
         * @throws IllegalStateException if the column is not a date
         */
        public Column min(LocalDate earliest) {
            needKind(Kind.DATE, "min");
            minTime = earliest == null ? null : earliest.atStartOfDay();
            checkOrder(minTime, maxTime);
            return this;
        }

        /**
         * The latest date allowed (inclusive), for a {@link #date()} column.
         *
         * @param latest the latest allowed date; null for no maximum
         * @return this column
         * @throws IllegalStateException if the column is not a date
         */
        public Column max(LocalDate latest) {
            needKind(Kind.DATE, "max");
            maxTime = latest == null ? null : latest.atStartOfDay();
            checkOrder(minTime, maxTime);
            return this;
        }

        /**
         * Both bounds (inclusive) of a date column; either may be null.
         *
         * @param earliest the earliest allowed date, or null
         * @param latest   the latest allowed date, or null
         * @return this column
         * @throws IllegalArgumentException if {@code earliest} is after {@code latest}
         * @throws IllegalStateException    if the column is not a date
         */
        public Column range(LocalDate earliest, LocalDate latest) {
            needKind(Kind.DATE, "range");
            final LocalDateTime lo = earliest == null ? null : earliest.atStartOfDay();
            final LocalDateTime hi = latest == null ? null : latest.atStartOfDay();
            checkOrder(lo, hi);
            minTime = lo;
            maxTime = hi;
            return this;
        }

        /**
         * The earliest date and time allowed (inclusive), for a {@link #dateTime(String, String...)} column.
         *
         * @param earliest the earliest allowed value; null for no minimum
         * @return this column
         * @throws IllegalStateException if the column is not a date-time
         */
        public Column min(LocalDateTime earliest) {
            needKind(Kind.DATETIME, "min");
            minTime = earliest;
            checkOrder(minTime, maxTime);
            return this;
        }

        /**
         * The latest date and time allowed (inclusive), for a {@link #dateTime(String, String...)} column.
         *
         * @param latest the latest allowed value; null for no maximum
         * @return this column
         * @throws IllegalStateException if the column is not a date-time
         */
        public Column max(LocalDateTime latest) {
            needKind(Kind.DATETIME, "max");
            maxTime = latest;
            checkOrder(minTime, maxTime);
            return this;
        }

        /**
         * Both bounds (inclusive) of a date-time column; either may be null.
         *
         * @param earliest the earliest allowed value, or null
         * @param latest   the latest allowed value, or null
         * @return this column
         * @throws IllegalArgumentException if {@code earliest} is after {@code latest}
         * @throws IllegalStateException    if the column is not a date-time
         */
        public Column range(LocalDateTime earliest, LocalDateTime latest) {
            needKind(Kind.DATETIME, "range");
            checkOrder(earliest, latest);
            minTime = earliest;
            maxTime = latest;
            return this;
        }

        // -------------------------------------------------------------------------- declaration helpers

        private void notForUnrecognized(String method) {
            if (unrecognized)
                throw new IllegalStateException("unknownColumnRules(): " + method + "() is about matching a title to a declared column, "
                        + "which the unrecognized columns, by definition, are not.");
        }

        private Column type(Kind k) {
            if (truncate)
                throw new IllegalStateException("Column \"" + name + "\": " + k.method + " cannot be combined with truncate(), which applies to text only.");
            if (kind != Kind.TEXT)
                throw new IllegalStateException("Column \"" + name + "\" is already declared as " + kind.what + "; a column has one type.");
            kind = k;
            return this;
        }

        private Column setDate(Kind k, String[] patterns) {
            type(k);
            final List<DateTimeFormatter> fs = new ArrayList<>();
            final List<String> ex = new ArrayList<>();
            for (String p : patterns) {
                try {
                    final DateTimeFormatter f = buildFormatter(p);
                    final String sample = f.format(SAMPLE);
                    if (k == Kind.DATE)
                        LocalDate.parse(sample, f);
                    else
                        LocalDateTime.parse(sample, f);
                    fs.add(f);
                    ex.add(sample);
                } catch (IllegalArgumentException | DateTimeException e) {
                    throw new IllegalArgumentException("Column \"" + name + "\": the format \"" + p + "\" cannot be used for "
                            + k.what + ": " + e.getMessage(), e);
                }
            }
            formats = fs;
            examples = ex;
            return this;
        }

        private void needNumber(String method) {
            if (kind != Kind.INTEGER && kind != Kind.LONG && kind != Kind.DECIMAL)
                throw new IllegalStateException("Column \"" + name + "\": " + method + " with a number applies to a column declared with "
                        + Kind.declaration(Kind.INTEGER, Kind.LONG, Kind.DECIMAL) + ", and this one is " + kind.what + ".");
        }

        private void needKind(Kind k, String method) {
            if (kind != k)
                throw new IllegalStateException("Column \"" + name + "\": " + method + " with a " + (k == Kind.DATE ? "LocalDate" : "LocalDateTime")
                        + " applies to a column declared with " + k.method + ", and this one is " + kind.what + ".");
        }

        private <T extends Comparable<? super T>> void checkOrder(T lo, T hi) {
            if (lo != null && hi != null && lo.compareTo(hi) > 0)
                throw new IllegalArgumentException("Column \"" + name + "\": the minimum " + lo + " is greater than the maximum " + hi + ".");
        }

        private BigDecimal toDecimal(double v) {
            if (Double.isNaN(v) || Double.isInfinite(v))
                throw new IllegalArgumentException("Column \"" + name + "\": " + v + " is not a usable bound.");
            return BigDecimal.valueOf(v);
        }

        private String[] join(String first, String[] more) {
            final String[] all = new String[more.length + 1];
            all[0] = first;
            System.arraycopy(more, 0, all, 1, more.length);
            return all;
        }

        // -------------------------------------------------------------------------- applying the rules to a row

        private boolean hasRules() {
            return notBlank || maxLength != null || kind != Kind.TEXT;
        }

        private String quoted() {
            return "\"" + title + "\"";
        }

        /**
         * Apply every rule to this row's cell.  Sets {@link #text} (the value the getters return) and, for a typed
         * column whose cell is valid, {@link #value}.
         *
         * @param raw    the trimmed cell
         * @param report true to record the problems and warnings
         */
        private void evaluate(String raw, boolean report) {
            text = raw;
            value = null;
            if (raw.isEmpty()) {
                if (notBlank && report)
                    addError(quoted() + " must have a value.");
                return;
            }
            switch (kind) {
                case TEXT:
                    evaluateText(raw, report);
                    break;
                case INTEGER:
                case LONG:
                case DECIMAL:
                    evaluateNumber(raw, report);
                    break;
                default:
                    evaluateDate(raw, report);
                    break;
            }
        }

        private void evaluateText(String raw, boolean report) {
            if (limit == Integer.MAX_VALUE)
                return;
            final int n = raw.codePointCount(0, raw.length());
            if (n <= limit)
                return;
            if (truncate) {
                text = cutTo(raw, limit);
                if (report)
                    addWarning(shortenedMessage(quoted(), n, limit));
            } else if (report)
                addError(quoted() + " is " + chars(n) + " long; at most " + limit + (limit == 1 ? " is" : " are") + " allowed.");
        }

        private void evaluateNumber(String raw, boolean report) {
            final String word = kind == Kind.DECIMAL ? "a number" : "a whole number";
            BigDecimal v = parseNumber(raw);
            if (v == null) {
                if (report)
                    addError(quoted() + " must be " + word + bounds(minNumber, maxNumber) + " (found \"" + shown(raw) + "\").");
                return;
            }
            BigDecimal lo = minNumber;
            BigDecimal hi = maxNumber;
            if (kind != Kind.DECIMAL) {
                v = v.setScale(0, RoundingMode.HALF_UP);
                final BigDecimal typeMin = kind == Kind.INTEGER ? INT_MIN : LONG_MIN;
                final BigDecimal typeMax = kind == Kind.INTEGER ? INT_MAX : LONG_MAX;
                lo = lo == null || lo.compareTo(typeMin) < 0 ? typeMin : lo;
                hi = hi == null || hi.compareTo(typeMax) > 0 ? typeMax : hi;
            }
            final boolean tooLow = lo != null && v.compareTo(lo) < 0;
            final boolean tooHigh = hi != null && v.compareTo(hi) > 0;
            if (tooLow || tooHigh) {
                if (report) {
                    //Name the bounds the declaration gave.  Only when the value breaks the limit of the type itself (lo and
                    //hi are then not the declared minNumber / maxNumber objects) are the type's limits named too.
                    final boolean typeLimitBroken = (tooLow && lo != minNumber) || (tooHigh && hi != maxNumber);
                    addError(quoted() + " must be " + word + (typeLimitBroken ? bounds(lo, hi) : bounds(minNumber, maxNumber))
                            + " (found \"" + shown(raw) + "\").");
                }
                return;
            }
            value = v;
        }

        private void evaluateDate(String raw, boolean report) {
            final boolean withTime = kind == Kind.DATETIME;
            LocalDateTime v = null;
            for (DateTimeFormatter f : formats)
                try {
                    v = withTime ? LocalDateTime.parse(raw, f) : LocalDate.parse(raw, f).atStartOfDay();
                    break;
                } catch (DateTimeException e) {
                    //try the next format
                }
            if (v == null) {
                if (report)
                    addError(quoted() + " must be " + kind.what + " such as " + String.join(" or ", examples)
                            + " (found \"" + shown(raw) + "\").");
                return;
            }
            if ((minTime != null && v.isBefore(minTime)) || (maxTime != null && v.isAfter(maxTime))) {
                if (report)
                    addError(quoted() + " must be " + kind.what + timeBounds() + " (found \"" + shown(raw) + "\").");
                return;
            }
            value = withTime ? v : v.toLocalDate();
        }

        /** " from A to B", " on or after A", " on or before B" (" at or ..." for a date and time). */
        private String timeBounds() {
            final DateTimeFormatter f = formats.get(0);
            final String at = kind == Kind.DATETIME ? "at" : "on";
            if (minTime != null && maxTime != null)
                return " from " + f.format(minTime) + " to " + f.format(maxTime);
            if (minTime != null)
                return " " + at + " or after " + f.format(minTime);
            return " " + at + " or before " + f.format(maxTime);
        }
    }

    /** The outcome of a run. */
    public static class Result {
        private final String type;
        private final String fileName;
        private final List<String> errors;
        private final List<String> warnings;
        private final int rowsRead, added, updated, deactivated;

        Result(String type, String fileName, List<String> errors, List<String> warnings, int rowsRead, int added, int updated, int deactivated) {
            this.type = type;
            this.fileName = fileName;
            this.errors = Collections.unmodifiableList(errors);
            this.warnings = Collections.unmodifiableList(warnings);
            this.rowsRead = rowsRead;
            this.added = added;
            this.updated = updated;
            this.deactivated = deactivated;
        }

        /** @return the errors (each prefixed with its row number where applicable) */
        public List<String> getErrors() { return errors; }
        /** @return the warnings */
        public List<String> getWarnings() { return warnings; }
        /** @return true if there were no errors */
        public boolean isSuccess() { return errors.isEmpty(); }
        /** @return rows read */
        public int getRowsRead() { return rowsRead; }
        /** @return records added */
        public int getRecordsAdded() { return added; }
        /** @return records updated */
        public int getRecordsUpdated() { return updated; }
        /** @return records deactivated */
        public int getRecordsDeactivated() { return deactivated; }

        /** @return formatted text: file, errors, warnings and counts */
        public String getSummary() {
            StringBuilder sb = new StringBuilder();
            sb.append(type).append(" import\nFile: ").append(fileName).append("\n\n");
            for (String e : errors)
                sb.append("ERROR: ").append(e).append('\n');
            for (String w : warnings)
                sb.append("WARNING: ").append(w).append('\n');
            sb.append('\n').append(rowsRead).append(" import records read\n");
            sb.append(errors.size()).append(" errors encountered\n");
            sb.append(warnings.size()).append(" warnings\n");
            sb.append(added).append(' ').append(type).append("s added\n");
            sb.append(updated).append(' ').append(type).append("s updated\n");
            sb.append(deactivated).append(' ').append(type).append("s deactivated\n");
            return sb.toString();
        }

        @Override
        public String toString() {
            return getSummary();
        }
    }
}
