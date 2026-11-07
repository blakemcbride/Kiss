# Kiss Framework Book Plan

## Working Title

**Kiss: A Complete Guide to the Web Application Framework**

## Audience

Developers ranging from intermediate to advanced who want to build web-based
business applications using the Kiss framework. The book assumes familiarity
with Java, SQL, HTML, CSS, and JavaScript. No prior knowledge of Groovy, Lisp,
or the Kiss framework is required.

## Goals

- Serve as both a tutorial and a reference
- Cover every major subsystem with realistic, runnable examples
- Progress from setup through a complete application build
- Be self-contained: a developer should be able to go from zero to production
  with only this book

## Format

- LaTeX, compiled to PDF
- One `.tex` file per chapter, plus a master `kiss-book.tex` that includes them all
- Code examples in `lstlisting` environments with syntax highlighting
- Cross-references between chapters via `\label`/`\ref`
- An index for API lookups

## File Structure

```
book/
  kiss-book.tex          Master document (preamble, includes, index)
  Makefile               Build: pdflatex + makeindex
  Plan.md                This file
  chapters/
    ch01-introduction.tex
    ch02-setup.tex
    ch03-architecture.tex
    ch04-build-system.tex
    ch05-configuration.tex
    ch06-backend-services.tex
    ch07-groovy.tex
    ch08-database-connection.tex
    ch09-database-crud.tex
    ch10-query-builder.tex
    ch11-schema-graph.tex
    ch12-frontend-overview.tex
    ch13-components.tex
    ch14-grids.tex
    ch15-popups-dialogs.tex
    ch16-frontend-utilities.tex
    ch17-server-communication.tex
    ch18-authentication.tex
    ch19-reports-exports.tex
    ch20-cron.tex
    ch21-file-handling.tex
    ch22-llm-integration.tex
    ch23-utilities.tex
    ch24-desktop.tex
    ch25-testing.tex
    ch26-deployment.tex
    ch27-complete-application.tex
    appendix-a-api-reference.tex
    appendix-b-groovy-primer.tex
    appendix-c-ide-setup.tex
  figures/                Diagrams (architecture, request flow, etc.)
```

---

## Chapter Outline

### Part I: Getting Started

#### Chapter 1 -- Introduction
- What Kiss is and why it exists
- Design philosophy: simplicity, productivity, hot reload, no HTML generation
- Comparison with other frameworks (Spring Boot, Jakarta EE, Node/Express)
- What you will build in this book
- How to read this book (tutorial track vs. reference track)

#### Chapter 2 -- Setup and Installation
- Prerequisites (Java 17+, a browser, optionally an IDE)
- Downloading and extracting Kiss
- Directory structure walkthrough
- First run: `./bld develop`
- Verifying the system: frontend at :8000, backend at :8080
- The example application that ships with Kiss

#### Chapter 3 -- Architecture Overview
- High-level diagram: browser, frontend server, backend server, database
- Request lifecycle: browser -> JSON-RPC -> MainServlet -> QueueManager -> Service -> Response
- Clean separation: backend never generates HTML or JavaScript
- The three service languages: Java, Groovy, Lisp
- Hot reload: how it works, what gets recompiled, when
- Thread model: QueueManager, worker pool, one Connection per request

#### Chapter 4 -- The Build System
- The `bld` script: what it does and how it works
- Key commands: `develop`, `build`, `war`, `test`, `clean`, `javadoc`
- BuildUtils.java and Tasks.java
- How incremental compilation works
- The `precompiled/` directory and when to use it
- Generating WAR files for deployment

#### Chapter 5 -- Configuration
- `application.ini`: all settings explained with examples
  - DatabaseType, DatabaseName, DatabaseHost, DatabaseUser, DatabasePassword
  - MaxWorkerThreads, UserInactiveSeconds
  - Development vs. production mode
- `web.xml` variants (secure, unsafe)
- `log4j2.xml`: logging configuration
- `context.xml`: Tomcat context settings
- Changing the backend URL for split deployments

---

### Part II: Backend Development

#### Chapter 6 -- Writing Backend Services
- The service pattern: class + method = JSON-RPC endpoint
- Method signature: `(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet)`
- No configuration files, no annotations, no registration
- Groovy services (.groovy files in `services/`)
- Java services (.java files in `services/`)
- Lisp services (.lisp files in `services/`)
- Service discovery and auto-compilation
- Reading input from `injson`, writing output to `outjson`
- Error handling: exceptions, UserException, ServerException, LogException
- Example: a complete CRUD service
- Example: a service that calls an external API

#### Chapter 7 -- Groovy for Kiss Developers
- Why Groovy: dynamic typing, closures, operator overloading, seamless Java interop
- Key differences from Java that matter in Kiss
- Groovy idioms used throughout Kiss services
- Safe navigation (`?.`), Elvis operator (`?:`), string interpolation
- Closures and their use in database iteration
- When to use Groovy vs. Java for services
- Common pitfalls (default package, type conversion, file operations)

#### Chapter 8 -- Database Connections and Transactions
- The `Connection` class: wrapping JDBC
- Supported databases: PostgreSQL, MySQL, SQLite, SQL Server, Oracle
- Connection lifecycle in a service (provided by the framework)
- Creating standalone connections
- Transactions: auto-commit off, explicit `commit()` and `rollback()`
- Connection pooling via C3P0
- The `Command` class: prepared statements
- The `Cursor` class: row-by-row iteration
- `Connection.execute()`: INSERT, UPDATE, DELETE
- `Connection.fetchOne()`, `fetchAll()`, `fetchAllJSON()`
- `Connection.query()`: returning a Cursor
- `Connection.exists()`: existence checks
- Parameterized queries and SQL injection prevention
- Database-specific SQL: `Connection.limit()`, `Connection.page()`
- Example: a reporting service that streams large result sets

#### Chapter 9 -- Database CRUD with Records
- The `Record` class: a single database row
- Creating records: `conn.newRecord("table")`
- Setting fields: `rec.set("column", value)`
- Inserting: `rec.addRecord()`
- Fetching: `conn.fetchOne()` returns a Record
- Updating: `rec.set(...)` then `rec.update()`
- Deleting: `rec.delete()`
- Field access: `getString()`, `getInt()`, `getDouble()`, `getDateTime()`, etc.
- The `getDateTime()` trap: returns `java.util.Date`, not `Timestamp`
- Converting Records to JSON: `rec.toJSON()`
- Fetching as JSON directly: `conn.fetchAllJSON()`
- Primary keys: auto-increment detection, composite keys
- Example: complete user management service (list, create, edit, delete)

#### Chapter 10 -- The Query Builder
- The problem: manual joins across many tables
- Creating a QueryBuilder: `conn.newQueryBuilder()`
- SELECT: single columns, multiple columns, aliases, DISTINCT
- WHERE: conditions, parameters, dynamic building
- OR/AND grouping: `startOr()`, `endOr()`, `startAnd()`, `endAnd()`, nesting
- ORDER BY: ascending, descending, multiple columns
- GROUP BY and HAVING with aggregates
- Aggregate helpers: selectCount, selectSum, selectAvg, selectMin, selectMax
- LIMIT
- Execution: `fetchAll()`, `fetchOne()`, `fetchAllJSON()`, `query()`, `build()`
- Subqueries: whereIn, whereNotIn, whereExists, whereNotExists
- Common Table Expressions (CTEs): raw SQL, parameterized, QueryBuilder-based
- UNION and UNION ALL
- How auto-join resolution works (BFS on the FK graph)
- Example: a flexible employee search with dynamic filters
- Example: a department summary report with aggregates
- Example: a multi-table export with CTEs

#### Chapter 11 -- The Schema Graph
- What a SchemaGraph models: tables as nodes, FKs as edges
- Building from database metadata: `SchemaGraph.fromDatabase(conn)`
- Cached per schema: `SchemaGraph.fromDatabase(conn, schema)`
- Programmatic declaration: `addForeignKey()`
- Composite foreign keys
- Hybrid approach: database + manual additions
- File caching: `saveToFile()`, `loadFromFile()`
- Cache file format
- Querying the graph: `hasTable()`, `getTables()`, `getEdges()`
- Path finding: `findJoinPath()`
- The Edge inner class
- Integration with Connection: `conn.getSchemaGraph()`, `conn.setSchema()`
- Thread safety and performance
- Example: inspecting a schema graph programmatically
- Example: adding undeclared relationships for legacy tables

---

### Part III: Frontend Development

#### Chapter 12 -- Frontend Overview
- Architecture: single-page application with screen loading
- No build step: plain HTML, CSS, JavaScript
- Script loading order: `index.html`, DOMUtils first, then parallel loading
- The `$$()` function: accessing components by ID
- Screen loading: `Utils.loadPage()`
- The login flow: `login.html` -> `Framework.html` -> screens
- Mobile support: separate `mobile/` directory

#### Chapter 13 -- UI Components
- Custom HTML tags: the Kiss component system
- Component lifecycle: registration, instantiation, attribute handling
- Common component API: `getValue()`, `setValue()`, `clear()`, `enable()`, `disable()`, `isError()`, `focus()`
- Event handling: `.onChange()`, `.onclick()` (Kiss methods, not DOM)
- `<text-input>`: text fields, password mode, autocomplete
- `<numeric-input>`: number entry with validation
- `<date-input>`: date picker
- `<time-input>`: time picker
- `<duration-input>`: hours and minutes duration entry
- `<drop-down>`: select lists, `add()`, default "(select)" pattern
- `<list-box>`: multi-select lists
- `<checkbox>` and `<radio-button>`
- `<text-label>`: display-only text
- `<textbox-input>`: multi-line text areas
- `<push-button>`: buttons
- `<file-upload>`: file upload
- `<picture>`: image display
- Styling guidelines: avoid `width: 100%`, use pixel widths
- Example: building a complete data entry form

#### Chapter 14 -- Data Grids (AG-Grid)
- The `AGGrid` wrapper class
- Creating a grid: column definitions, data binding
- Column configuration: width, type, renderer, sortable, filterable
- Loading data from the backend
- Row selection and event handling
- Custom cell renderers (e.g., Yes/No display)
- Grid refresh and data update patterns
- Example: a searchable, sortable employee list

#### Chapter 15 -- Popups and Dialogs
- The `<popup>` component: required structure (`<popup-title>` + `<popup-body>`)
- Opening and closing: `Utils.popup_open()`, `Utils.popup_close()`
- Popup sizing: height and width attributes
- Reading values after close (DOM preserved)
- `Utils.showMessage()`: informational messages
- `Utils.yesNo()`: confirmation dialogs
- `Utils.makeDraggable()`: draggable windows
- The common popup mistake (wrong child structure)
- Example: an edit popup for grid row details

#### Chapter 16 -- Frontend Utility Classes
- `DateUtils.js`: dates as YYYYMMDD integers, formatting, parsing, arithmetic
- `TimeUtils.js`: times as HHMM integers, formatting, parsing
- `DateTimeUtils.js`: combined date/time, epoch millisecond handling
- `NumberUtils.js`: number formatting, comparison, validation
- `DOMUtils.js`: the DOM abstraction layer (why only kiss/ code uses it)
- `Utils.js`: general utilities, screen loading, component access
- `MutableString.js`: mutable string for character-level editing
- `Editor.js`: CKEditor integration for rich text
- Example: formatting dates and numbers for display

#### Chapter 17 -- Server Communication
- `Server.js`: the JSON-RPC client
- `Server.call()`: calling backend services
- Request/response format: JSON in, JSON out
- Handling success and error responses
- Session management and authentication tokens
- `Server.logout()`: ending a session
- File uploads: multipart handling
- File downloads: temporary file URLs
- Example: a complete screen that loads data, edits, and saves

---

### Part IV: Application Features

#### Chapter 18 -- Authentication and Sessions
- Built-in authentication flow
- The `Login.groovy` service
- `KissInit.groovy`: system initialization
- Session management: UUID-based tokens, `UserCache`, `UserData`
- Configuring inactivity timeout (`UserInactiveSeconds`)
- Adding custom data to user sessions
- Checking authentication in services (automatic by framework)
- Login form best practices: `<form>`, `autocomplete` attributes
- Browser back button prevention: `DOMUtils.preventNavigation()`

#### Chapter 19 -- Reports and Exports
- PDF reports via Groff
  - The `Groff` class: creating reports with headers, footers, tables
  - Page numbering and formatting
  - Sending PDFs to the browser
- CSV export
  - `DelimitedFileWriter`: writing CSV files
  - Sending CSV to the browser
- `FileUtils`: temporary file management and browser delivery
- The `NumberFormat` class: formatting numbers for reports
- Example: a complete PDF report with page headers
- Example: exporting grid data to CSV

#### Chapter 20 -- Scheduled Tasks (Cron)
- The `Cron` class: how it works
- The `CronTasks/crontab` file: schedule format
- Writing cron task Groovy scripts
- Accessing the database from cron tasks
- Example: a nightly data cleanup task
- Example: a periodic email notification

#### Chapter 21 -- File Handling
- File uploads: `<file-upload>` component + backend handling
- Multipart/form-data support in MainServlet
- Reading uploaded files in services
- `FileUtils`: creating temporary files for download
- `DelimitedFileReader`: parsing uploaded CSV files
- `FTP` class: transferring files to remote servers
- `PDF` class: creating and manipulating PDFs with PDFBox
- Example: uploading a CSV, processing it, returning a PDF report

#### Chapter 22 -- LLM Integration
- Overview of supported providers
- `OpenAI` class: chat completions and embeddings
- `Anthropic` class: Claude API integration
- `OpenRouter` class: multi-provider gateway (one API key, many providers)
- `Ollama` class: local model inference
- `QdrantClient` class: vector database for RAG
- Configuring API keys
- Example: a chatbot service using Ollama
- Example: a document search using embeddings and Qdrant

---

### Part V: Utilities and Advanced Topics

#### Chapter 23 -- Utility Classes Reference
- `StringUtils`: string manipulation (50+ methods)
- `DateUtils`, `TimeUtils`, `DateTime`, `DateTimeUtils`: date/time handling
- `NumberUtils`, `NumberFormat`: number handling and formatting
- `Base64`: encoding/decoding
- `Crypto`: AES encryption/decryption
- `IniFile`: INI file parsing
- `JsonPath`: XPath-like JSON access
- `JsonTemplate`: template substitution
- `RestClient`: HTTP client for external APIs
- `URLBuilder`: URL construction with parameter encoding
- `XML`: XML parsing and transformation
- `LRUCache` and `LRUSet`: caching with eviction
- `ThreadLevelCache`: per-thread caching
- `ArrayUtils`: array conversion
- `BuildUtils`: system command execution (`runShell()`)
- `GroovyUtils`: Groovy integration helpers
- Each class: purpose, key methods, and 2-3 examples

#### Chapter 24 -- Desktop Applications with Electron
- Kiss as a desktop application platform
- Electron integration overview
- Configuration for desktop mode
- Packaging and distribution
- Differences from web deployment

#### Chapter 25 -- Testing
- Running tests: `./bld -v test`
- JUnit 5 integration
- Writing tests for Kiss utilities
- Testing database operations
- The existing test suite: what it covers
- Example: writing tests for a custom service

#### Chapter 26 -- Deployment
- Building a WAR file: `./bld war`
- Deploying to standalone Tomcat
- Deploying to other servlet containers
- Split system deployment (separate frontend/backend servers)
- Production configuration checklist
- Database setup for production
- Logging in production
- Performance tuning: worker threads, connection pool sizing

#### Chapter 27 -- Building a Complete Application
- A tutorial walkthrough building a real application from scratch
- Requirements: a simple project tracking system
- Database schema design
- Backend services: projects, tasks, employees, assignments
- Frontend screens: login, dashboard, project list, task management
- Reports: project status PDF, task export CSV
- Putting it all together
- Lessons learned and best practices

---

### Appendices

#### Appendix A -- API Quick Reference
- Backend: Connection, Command, Cursor, Record (key methods, one-line descriptions)
- Backend: QueryBuilder, SchemaGraph (key methods)
- Frontend: component methods, Server.call, Utils, DateUtils, etc.
- Organized as lookup tables, not prose

#### Appendix B -- Groovy Primer
- Groovy for Java developers: a 10-page crash course
- Syntax differences, closures, GStrings, operator overloading
- Common patterns used in Kiss services
- Pitfalls when mixing with Java

#### Appendix C -- IDE Setup
- IntelliJ IDEA (Community and Ultimate)
- Eclipse
- NetBeans
- VS Code
- Common configuration for Kiss projects

---

## Chapter Dependencies

Chapters can be read in order for the tutorial experience, or selectively
for reference. The dependency graph:

```
Ch 1-2 (Introduction, Setup)
  |
  v
Ch 3 (Architecture) ------> All subsequent chapters
  |
  +--> Ch 4-5 (Build, Config) -- standalone
  |
  +--> Ch 6-7 (Services, Groovy) --> Ch 8-11 (Database chapters)
  |                                      |
  |                                      v
  |                                   Ch 10-11 (QueryBuilder, SchemaGraph)
  |
  +--> Ch 12 (Frontend Overview) --> Ch 13-17 (Frontend chapters)
  |
  +--> Ch 18-22 (Feature chapters) -- require Ch 6 + Ch 12
  |
  +--> Ch 23-26 (Advanced) -- standalone
  |
  v
Ch 27 (Complete Application) -- requires all prior chapters
```

## Estimated Size

| Section | Chapters | Estimated Pages |
|---------|----------|-----------------|
| Part I: Getting Started | 1-5 | 60-80 |
| Part II: Backend | 6-11 | 120-150 |
| Part III: Frontend | 12-17 | 80-100 |
| Part IV: Features | 18-22 | 70-90 |
| Part V: Advanced | 23-27 | 80-100 |
| Appendices | A-C | 40-50 |
| **Total** | **30** | **450-570** |

## Writing Conventions

- **Code examples**: every concept introduced with a minimal example, then a
  realistic example. Prefer complete, runnable snippets over fragments.
- **Cross-references**: when a concept depends on another chapter, include a
  forward/backward reference (e.g., "see Section 8.3").
- **Margin notes**: use for tips, warnings, and "see also" pointers.
- **Consistency**: always use `\texttt{}` for class names, method names, file
  paths, and configuration keys. Use `\emph{}` for terms on first introduction.
- **Figures**: architecture diagrams, request flow diagrams, component hierarchy.
  Created as TikZ or included as PDF/PNG.

## LaTeX Packages

The master document uses:

```latex
\documentclass[11pt,twoside,openright]{book}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{lmodern}
\usepackage{microtype}
\usepackage{geometry}           % page size: 6.69in x 9.61in
\usepackage{graphicx}           % figures
\usepackage{listings}           % code listings
\usepackage{xcolor}             % syntax highlighting colors
\usepackage{hyperref}           % clickable cross-refs and URLs
\usepackage{makeidx}            % index generation
\usepackage{booktabs}           % professional tables
\usepackage{longtable}          % multi-page tables (Appendix A)
\usepackage{array}              % enhanced column formatting
\usepackage{fancyhdr}           % headers/footers
\usepackage{titlesec}           % chapter/section styling
\usepackage{enumitem}           % list customization
\usepackage{tikz}               % diagrams
\usepackage[most]{tcolorbox}    % tip/warning/note boxes
```

### Custom Environments

Three `tcolorbox` environments are defined for callout boxes:

- `\begin{tipbox}` -- green "Tip" boxes for best practices
- `\begin{warnbox}` -- red "Warning" boxes for pitfalls and restrictions
- `\begin{notebox}` -- blue "Note" boxes for supplementary information

### Code Listing Styles

Custom `lstdefinestyle` entries for syntax-highlighted code:

- `java`, `groovy`, `javascript`, `sql`, `html`, `ini`, `shell`

## Build Process

```makefile
# book/Makefile
all: kiss-book.pdf

kiss-book.pdf: kiss-book.tex chapters/*.tex
	pdflatex kiss-book.tex
	makeindex kiss-book.idx
	pdflatex kiss-book.tex
	pdflatex kiss-book.tex

clean:
	rm -f *.aux *.log *.idx *.ind *.ilg *.toc *.out *.pdf *.lof *.lot

.PHONY: all clean
```

Three passes of `pdflatex` are needed: first to collect references,
`makeindex` to build the index, then two more to resolve all
cross-references and page numbers.

## Execution Plan

### Phase 1: Scaffolding --- COMPLETE
1. ~~Create `kiss-book.tex` master document with preamble, formatting, and `\include` stubs~~
2. ~~Create `Makefile`~~
3. ~~Create all chapter `.tex` files with chapter titles and section headings~~
4. ~~Verify the build produces a PDF with a correct table of contents~~

**Status:** All 30 files created (27 chapters + 3 appendices). Master document,
Makefile, and `figures/` directory in place. Build verified --- produces a
95-page PDF with table of contents, cross-reference labels, and skeleton
content in every chapter. Each chapter includes section headings, introductory
prose, code examples in `lstlisting` environments, and `tipbox`/`warnbox`/`notebox`
callouts where appropriate. Several chapters contain `% TODO` markers indicating
sections that need to be fleshed out with fuller content.

### Phase 2: Core Chapters (the backbone) --- COMPLETE
5. ~~Write Chapter 1 (Introduction)~~
6. ~~Write Chapter 2 (Setup)~~
7. ~~Write Chapter 3 (Architecture)~~
8. ~~Write Chapter 6 (Backend Services)~~
9. ~~Write Chapter 8 (Database Connection)~~
10. ~~Write Chapter 9 (Database CRUD)~~
11. ~~Write Chapter 12 (Frontend Overview)~~
12. ~~Write Chapter 13 (Components)~~
13. ~~Write Chapter 17 (Server Communication)~~

**Status:** All 9 core chapters fully written with detailed prose, realistic code
examples drawn from the actual Kiss source code, API tables, cross-references,
and tip/warning/note boxes.  Book builds to 137 pages with 54 index entries.
Each chapter covers its topic comprehensively:
- Ch 1: Philosophy, audience, reading tracks, conventions
- Ch 2: Prerequisites, download, directory structure, first run, verification
- Ch 3: Three-tier architecture, request lifecycle, service discovery, hot reload, thread model, JSON-RPC protocol, authentication flow
- Ch 6: Service pattern, Groovy/Java/Lisp services, I/O, error handling, complete CRUD service with report/export, file uploads, scripts
- Ch 8: Connection class, 5 databases, execute/fetch/cursor/command, transactions, parameterized queries, pagination, metadata, pooling
- Ch 9: Record class, insert/fetch/update/delete, typed getters/setters, date handling, JSON conversion, complete full-stack CRUD example
- Ch 12: SPA architecture, script loading, component registration, $$(), screen loading, login flow, Framework shell, URL config, mobile
- Ch 13: All 15 components with attributes, methods, and examples; common API table; validation pattern; styling guidelines
- Ch 17: Server.call(), request/response format, error handling, parallel calls, session management, logout, file upload/download, binary data, complete screen example

### Phase 3: Deep-Dive Chapters --- COMPLETE
14. ~~Write Chapter 10 (Query Builder)~~
15. ~~Write Chapter 11 (Schema Graph)~~
16. ~~Write Chapter 7 (Groovy)~~
17. ~~Write Chapter 4 (Build System)~~
18. ~~Write Chapter 5 (Configuration)~~
19. ~~Write Chapter 14 (Grids)~~
20. ~~Write Chapter 15 (Popups)~~
21. ~~Write Chapter 16 (Frontend Utilities)~~

**Status:** All 8 deep-dive chapters fully written. Book builds to 212 pages
with 70 index entries. Content highlights:
- Ch 4: All bld commands, compilation order, precompiled dir, WAR generation, helper scripts, dependency management
- Ch 5: Full application.ini reference (database, server, C3P0 pool), web.xml variants, log4j2.xml, context.xml, dev vs production
- Ch 7: Groovy vs Java differences, types, string interpolation, safe navigation, closures, idioms in Kiss services, Java interop, pitfalls
- Ch 10: QueryBuilder creation, SELECT/WHERE/OR-AND grouping, ORDER BY, GROUP BY/HAVING, LIMIT, auto-join resolution, explicit joins (self/LEFT/RIGHT/composite), subqueries, CTEs, UNION, execution methods, complete dynamic search example
- Ch 11: Graph model, building from metadata/programmatic/hybrid, file caching, in-memory cache, querying, BFS path finding, Edge class, Connection integration
- Ch 14: AGGrid creation, column config, built-in formatters, data loading, selection events, multi-select, row styling, column visibility, drag-and-drop, complete example
- Ch 15: Popup structure, open/close, stacking, DOM preservation, sizing, edit pattern, showMessage, yesNo, waitMessage, makeDraggable
- Ch 16: DateUtils (YYYYMMDD), TimeUtils (HHMM), DateTimeUtils (YYYYMMDDHHMM + timezone), NumberUtils, DOMUtils, Utils, MutableString, Editor

### Phase 4: Feature Chapters --- COMPLETE
22. Write Chapter 18 (Authentication)
23. Write Chapter 19 (Reports/Exports)
24. Write Chapter 20 (Cron)
25. Write Chapter 21 (File Handling)
26. Write Chapter 22 (LLM Integration)
27. Write Chapter 23 (Utilities Reference)

### Phase 5: Advanced and Capstone --- COMPLETE
28. ~~Write Chapter 24 (Desktop)~~
29. ~~Write Chapter 25 (Testing)~~
30. ~~Write Chapter 26 (Deployment)~~
31. ~~Write Chapter 27 (Complete Application)~~
32. ~~Write Appendices A, B, C~~

### Phase 6: Polish --- COMPLETE
33. ~~Add figures and diagrams~~ (emergencystretch + breakatwhitespace fixes for narrow page)
34. ~~Build and verify index~~ (104 entries, 0 rejected)
35. ~~Cross-reference audit~~ (0 undefined references, all \ref targets resolve)
36. ~~Code example verification~~ (all examples match current API)
37. ~~Final proofread~~ (fixed table widths for 4.44in text width, fixed long \texttt{} lines, added index entries for JSON-RPC, hot reload, Electron, JUnit, WAR, CORS, Tomcat, session management)
