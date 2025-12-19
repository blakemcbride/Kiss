# KISS Web Development Framework Analysis

## Overview
KISS is a **Kiss Framework application** - a full-stack Java web framework designed for rapid business application development. It includes both front-end and back-end already integrated and running as a basic example application. The framework emphasizes simplicity, productivity, and the ability to make changes while the system is running without requiring compilation or restarts.

## Architecture

### Technology Stack
- **Backend:** Java 17+, Groovy, ABCL (Lisp support)
- **Frontend:** JavaScript/HTML/CSS with custom UI components (custom HTML tags)
- **Database:** PostgreSQL, MySQL, MS SQL Server, Oracle, SQLite
- **Build System:** Custom "bld" system (No Maven/Gradle required)
- **Server:** Tomcat 11.x (Jakarta EE 11, Servlet 6.1) - embedded

### Directory Structure
```
Stack360Management/
├── src/
│   ├── main/
│   │   ├── core/          # Core Java framework code (DO NOT MODIFY)
│   │   ├── backend/       # Backend application code (Groovy/Java/Lisp services)
│   │   ├── frontend/      # Frontend web application
│   │   │   └── kiss/      # Framework components (DO NOT MODIFY)
│   │   └── precompiled/   # Shared Java utilities accessible throughout application
│   └── test/              # Unit tests
├── libs/                  # Third-party JAR dependencies
├── manual/                # Documentation (main manual)
│   └── jsdoc/            # Frontend API documentation
├── work/                  # Build output directory
│   └── javadoc/          # Backend JavaDoc documentation
├── tomcat/                # Embedded Tomcat server
│   └── logs/             # Server logs (catalina.out)
└── target/                # Maven/IDE output
```

## Key Components

### Backend Structure
- **MainServlet.java**: Main JSON-RPC server entry point at `/rest` with async request handling
- **Service Layer**: Multi-language support (Java/Groovy/Lisp)
- **Database Layer**: Custom ORM-like abstraction (Connection, Command, Cursor, Record)
- **Queue Manager**: Asynchronous request handling with configurable worker threads
- **Authentication**: Built-in user authentication and session management
- **Cron Support**: Built-in task scheduler for periodic tasks

### Frontend Structure
- **Component System**: Custom UI components (TextInput, DateInput, DropDown, CheckBox, etc.)
- **Utility Libraries**:
  - DateUtils - Date manipulation (dates as YYYYMMDD integers)
  - TimeUtils - Time manipulation (times as HHMM integers)
  - DateTime - Combined date/time operations (wrapper around ZonedDateTime)
  - NumberUtils - Number formatting and validation
  - Server - AJAX communication with backend
  - Utils - General utilities and UI helpers
- **Grid Support**: AG-Grid integration for data tables
- **Editor**: CKEditor integration for rich text
- **Mobile**: Separate mobile-responsive pages

## Notable Features

1. **Hot Reload Development**
   - No recompilation needed during development
   - Automatic compilation and loading of changed files

2. **Multi-Language Services**
   - Write services in Java, Groovy, or Lisp
   - Services auto-compile on change

3. **Database Operations**
   - Simplified CRUD with Record/Connection pattern
   - Support for multiple database vendors
   - Connection pooling via C3P0

4. **Report Generation**
   - PDF reports via Groff integration
   - CSV export capabilities
   - Temporary file management

5. **File Upload Handling**
   - Native multipart/form-data support
   - Automatic file management

6. **LLM Integration**
   - Ollama integration ready
   - OpenAI API support

7. **Desktop Support**
   - Electron compatibility for desktop apps

## Configuration

### Application Configuration (application.ini)
```ini
DatabaseType =  <database type>
DatabaseName = <database name>
MaxWorkerThreads = 30
UserInactiveSeconds = 900
```

### Service Pattern Example
Services follow a standard pattern as shown in `Crud.groovy`:
```groovy
class ServiceName {
    void methodName(JSONObject injson, JSONObject outjson, 
                   Connection db, ProcessServlet servlet) {
        // Authentication already handled by framework
        // Process request from injson
        // Return response in outjson
    }
}
```

## Development Commands

- `./bld develop` - Start both frontend and backend development servers
- `./bld -v build` - Build the application (compiles all Java files including precompiled directory)
- `./bld war` - Create WAR file for deployment
- `./bld -v test` - Run unit tests
- `./bld clean` - Clean build artifacts
- `./bld javadoc` - Generate JavaDoc documentation

## System Integration

### Executing System Commands
- Use `org.kissweb.BuildUtils.runShell()` to execute system commands
- The method waits for command completion (synchronous)
- Example for database export/import:
  ```groovy
  import org.kissweb.BuildUtils
  String cmd = "pg_dump -h localhost -U user -d dbname -f output.sql"
  String exportCmd = "export PGPASSWORD=password && " + cmd
  BuildUtils.runShell(exportCmd)
  ```

### Database Connection Settings
- Access database configuration via `MainServlet.getEnvironment()`:
  ```groovy
  import org.kissweb.restServer.MainServlet
  String dbHost = MainServlet.getEnvironment("DatabaseHost") ?: "localhost"
  String dbName = MainServlet.getEnvironment("DatabaseName")
  String dbUser = MainServlet.getEnvironment("DatabaseUser")
  String dbPassword = MainServlet.getEnvironment("DatabasePassword")
  ```
- These values come from `application.ini` configuration

Note: The `bld` script automatically compiles:
1. Core framework files (`src/main/core/`)
2. Precompiled utilities (`src/main/precompiled/`)
3. Test files (`src/test/core/`)
All compiled classes go to `work/exploded/WEB-INF/classes/`

## Development Environment

### URLs
- **Frontend (Development):** http://localhost:8000
- **Backend (Development):** http://localhost:8080
- **Backend Log:** tomcat/logs/catalina.out

### Hot Reload
- **No compilation needed during development** - The system detects application code changes and automatically compiles and loads them while running
- Both backend and frontend code can be changed while the system is running
- Backend services (Java/Groovy/Lisp) auto-compile on change
- Frontend changes are immediately reflected

## Key Libraries

### Backend Dependencies
- Groovy 4.0.26 - Dynamic language support
- C3P0 0.11.2 - Database connection pooling
- Log4j 2.22.0 - Logging framework
- PDFBox 3.0.5 - PDF generation
- Database drivers for PostgreSQL, MySQL, SQLite, MS SQL, Oracle

### Frontend Dependencies
- jQuery 3.6.3 - DOM manipulation
- AG-Grid Community - Data grid component
- CKEditor - Rich text editor

## Authentication & Security

- Built-in session management
- Automatic authentication checking before service methods
- Configurable user inactivity timeout
- UUID-based session tracking

## Database Features

- **Record API**: Simplified database operations
  - `newRecord()` - Create new record
  - `fetchOne()` - Get single record
  - `fetchAll()` - Get multiple records
  - `fetchAllJSON()` - Get multiple records as JSON array
  - `addRecord()` - Insert new record
  - `update()` - Update existing record
  - `delete()` - Delete record
- **Connection Methods**:
  - `execute(String sql, Object... args)` - Execute parameterized SQL statements (INSERT, UPDATE, DELETE)
  - `exists(String sql, Object... args)` - Check if records exist
  - `fetchAllJSON(String sql, Object... args)` - Fetch results directly as JSON array
- **Field Access Methods**:
  - `getString()` - Get string value
  - `getInt()` - Get integer value
  - `getDateTime()` - Get timestamp/datetime value
    - **Important**: Returns `java.util.Date` object, NOT `Timestamp`
    - This is a common misconception that can lead to incorrect type casting
    - Always use: `Date date = record.getDateTime("column_name")`
    - Never cast to `Timestamp` or assume `Timestamp` type
  - `setDateTime()` - Set timestamp/datetime value
  - Similar getters/setters for all data types
- **Connection Management**: Automatic connection pooling via C3P0
- **Multi-Database Support**: Write once, run on any supported database
- **Transaction Support**: Built-in transaction management
- **Schema Support**: Can specify schema in table names (e.g., "admin.users")

## Database Record Insertion Pattern

Rather than using SQL INSERT commands to insert records, always use the Kiss pattern:

```java
Connection db = ...;
Record rec = db.newRecord("table_name");
rec.set("column1", val1);
rec.set("column2", val2);
...
rec.addRecord();
```

This pattern provides a cleaner, more maintainable approach to database record insertion compared to raw SQL INSERT statements.

## Report & Export Capabilities

- **PDF Reports**: Full-featured reports with Groff
  - Page numbering
  - Headers/footers
  - Tables and formatting
- **CSV Export**: Direct CSV file generation
- **Temporary File Management**: Automatic cleanup of generated files

## Development Mode Features

- Hot reload of services without restart
- Development vs production mode detection
- Cache control for debugging
- Separate frontend/backend servers for development

## Production Deployment

- WAR file deployment to standard servlet containers
- Support for separated frontend/backend deployment
- Configurable backend URL for distributed systems
- Built-in static file serving

## Frontend Components

### Custom HTML Tags
The framework provides custom HTML components that should be used:
- `<text-input>` - Text input field
- `<drop-down>` - Dropdown select
- `<push-button>` - Button element
- `<popup>` - Modal dialog
- `<popup-title>` - Popup header
- `<popup-body>` - Popup content area
- `<text-label>` - Text label
- `<date-input>` - Date picker
- `<time-input>` - Time picker
- `<numeric-input>` - Number input
- `<checkbox>` - Checkbox control
- `<radio-button>` - Radio button
- `<list-box>` - List selection
- `<file-upload>` - File upload control

### Frontend Utilities
- **Server.call()** - Make JSON-RPC calls to backend services
- **Utils.popup_open()** - Open popup dialogs
- **Utils.popup_close()** - Close popup dialogs
- **Utils.showMessage()** - Show message dialogs
- **Utils.yesNo()** - Show confirmation dialogs
- **Utils.loadPage()** - Load screen content
- **$$()** - Get component by ID (similar to jQuery)
- **AGGrid** - Data grid integration

### Grid Column Configuration
- Column widths can be specified as pixels (e.g., `width: 200`)
- Custom cell renderers supported (e.g., for formatting Yes/No display)

## Framework Philosophy

The Kiss Framework emphasizes:
- **Simplicity**: Minimal configuration required
- **Productivity**: Rapid development with hot reload
- **Flexibility**: Multi-language support for services
- **Completeness**: Built-in features for common business needs
- **Performance**: Compiled execution speed with dynamic convenience
- **No HTML/JS Generation**: Backend never generates HTML or JavaScript - clean separation of concerns

## Important Development Notes

### File Restrictions
- **DO NOT MODIFY** files under:
  - `src/main/frontend/kiss/` - Framework components
  - `src/main/core/` - Core framework code
- **SAFE TO MODIFY**:
  - `src/main/backend/` - Application services and business logic
  - `src/main/frontend/` - Application screens (except kiss/ subdirectory)
  - `src/main/precompiled/` - Shared utility classes

### Communication Architecture
- Backend and frontend communicate **only through JSON-RPC** (not REST, despite the `/rest` endpoint)
- Backend never generates HTML or JavaScript
- Frontend handles all UI rendering
- Clean separation between backend logic and frontend presentation
- Services are JSON-RPC methods, not REST endpoints

### Service Development Best Practices
- Services require minimal code - just a class with methods
- No configuration files needed for services
- Methods automatically become JSON-RPC endpoints
- Authentication handled automatically by framework
- Don't set database columns that have defaults (e.g., CURRENT_TIMESTAMP)
- Use appropriate data type methods (getString, getInt, getDateTime, etc.)
- **ALWAYS use Kiss framework utilities over standard Java/Groovy alternatives**:
  - Use `org.kissweb.DateTime` for date/time operations instead of `SimpleDateFormat` or `Date.format()`
  - Use `org.kissweb.NumberUtils` for number formatting instead of `DecimalFormat`
  - Use `org.kissweb.FileUtils` for file operations when available
  - The Kiss utilities are designed to work seamlessly with the framework and provide consistent behavior

### Precompiled Utilities Directory
The `src/main/precompiled/` directory is for shared Java utility classes that need to be accessible throughout the application:
- Place reusable Java utilities here to avoid code duplication
- Classes are automatically compiled by `./bld -v build` to `work/exploded/WEB-INF/classes/`
- Ideal for common functions like UUID generation, data formatting, validation utilities
- **IMPORTANT**: Classes must have a package declaration to be accessible from Groovy services
  - Classes in the default package (no package declaration) cannot be accessed from packaged Groovy services
  - This is a Java/Groovy language restriction, not a Kiss framework limitation
- Recommended approach: Create your own package structure (e.g., `io.yourcompany.utils`)
- Directory structure must match package declaration:
  - Example: `package io.stack360;` requires file at `src/main/precompiled/io/stack360/ClassName.java`
- After adding new classes, run `./bld -v build` to compile them
- Access from Groovy services via standard import:
  ```groovy
  import io.stack360.UUIDGenerator
  String id = UUIDGenerator.generateModifiedUUID()
  ```

### Groovy-Specific Notes
- Parentheses can be omitted for single statement blocks
- Safe navigation operator `?.` useful for null handling
- UUID generation: `UUID.randomUUID().toString()`
- File operations:
  - Use simple loops instead of closure-based filters to avoid type conversion issues
  - Example - listing files:
    ```groovy
    File[] allFiles = dir.listFiles()
    List<File> sqlFiles = []
    for (File file : allFiles) {
        if (file.isFile() && file.getName().endsWith(".sql"))
            sqlFiles.add(file)
    }
    ```
  - Sorting with closures works well:
    ```groovy
    sqlFiles.sort { File a, File b -> 
        Long.compare(b.lastModified(), a.lastModified())
    }
    ```

### Component Usage
- Use framework-provided custom HTML tags
- Access components with `$$('component-id')`
- Component methods:
  - `.getValue()` - Get component value
  - `.setValue()` - Set component value
  - `.clear()` - Clear component
  - `.enable()` / `.disable()` - Enable/disable component
  - `.isError()` - Validate and show error if invalid
  - `.focus()` - Set focus to component
  - `.onChange()` - Set change event handler (capital C, not onchange)
  - `.add(value, label)` - Add items to list-box or drop-down components
  - `.onclick()` - Set click event handler for buttons

### Event Handling
- **ALWAYS use Kiss component methods for events, not native DOM methods**:
  - Use `.onChange()` not `addEventListener('change', ...)`
  - Use `.onclick()` not `addEventListener('click', ...)`
  - These Kiss methods handle events consistently across the framework
  - Example for list-box selection:
    ```javascript
    $$('list-id').onChange(() => {
        const selected = $$('list-id').getValue();
        // handle selection
    });
    ```

### Dropdown Default Values
- When populating dropdowns that require a selection, use "(select)" as the default text instead of blank
- Example:
  ```javascript
  $$('dropdown-id').clear();
  $$('dropdown-id').add('', '(select)');  // Empty value with "(select)" display text
  for (let item of items)
      $$('dropdown-id').add(item.value, item.label);
  ```
- This provides better UX by clearly indicating that a selection is required

### Popup Sizing
- Specify both height and width attributes
- Size appropriately for content - avoid excess whitespace
- Test different screen sizes for responsive behavior

### Date and Time Utilities

Kiss provides three main utility classes for date and time manipulation:

#### DateUtils (org.kissweb.DateUtils)
Handles dates represented as integers in YYYYMMDD format:
- `DateUtils.toInt(Date)` - Convert Date object to YYYYMMDD integer
- `DateUtils.toDate(int)` - Convert YYYYMMDD integer to Date object
- `DateUtils.today()` - Get current date as YYYYMMDD integer
- `DateUtils.format(String fmt, int dt)` - Format date with custom pattern
- `DateUtils.addDays(int dt, int n)` - Add/subtract days from date
- `DateUtils.year(int)`, `month(int)`, `day(int)` - Extract date components

#### TimeUtils (org.kissweb.TimeUtils)
Handles times represented as integers in HHMM format:
- `TimeUtils.now()` - Get current time as HHMM integer
- `TimeUtils.formatMilitary(int)` - Format as "HH:MM" (24-hour)
- `TimeUtils.formatAMPM(int)` - Format as "H:MM AM/PM"
- `TimeUtils.parse(String)` - Parse time string to HHMM integer
- `TimeUtils.hour(int)`, `minutes(int)` - Extract time components

#### DateTime (org.kissweb.DateTime)
Wrapper around Java's ZonedDateTime for combined date/time operations:
- `new DateTime(Date)` - Create from Date object
- `getIntDate()` - Get date portion as YYYYMMDD integer
- `getIntTime()` - Get time portion as HHMM integer
- `getDate()` - Convert back to Date object
- `format()` - Format as "MM/dd/yyyy h:mm a"
- `addDays(int)`, `addHours(int)`, `addMinutes(int)` - Date/time arithmetic

**Common Pattern for Date to Time Conversion:**
```java
// Convert a Date object to HHMM integer format
Date myDate = ...;
DateTime dt = new DateTime(myDate);
int timeHHMM = dt.getIntTime();  // Returns time as HHMM integer

// Alternative using Calendar directly (if DateTime is not suitable)
Calendar cal = Calendar.getInstance();
cal.setTime(myDate);
int timeHHMM = cal.get(Calendar.HOUR_OF_DAY) * 100 + cal.get(Calendar.MINUTE);
```

## Browser Back Button Prevention

The Kiss framework implements browser back button prevention using the History API instead of `window.onbeforeunload` for better mobile browser compatibility.

### Implementation Details

**Login Handling** (in `login.js` and `mobile/login.js`):
- After successful login, sets `Server.isLoggedIn = true`
- Uses `history.pushState()` to add a state entry
- Adds a `popstate` event listener that:
  - Checks if user is logged in via `Server.isLoggedIn` flag
  - Re-pushes state to prevent navigation
  - Shows confirmation dialog asking user if they want to logout
  - Calls `Server.logout()` if user confirms

**Logout Handling** (in `Server.js`):
- `Server.logout()`: Sets `Server.isLoggedIn = false` before reload to disable back button protection
- `Server.checkTime()`: Sets `Server.isLoggedIn = false` before auto-logout due to inactivity

**State Management**:
- `Server.isLoggedIn` is a class variable initialized to `false`
- Set to `true` after successful login
- Set to `false` before any logout (manual or automatic)

This approach works reliably across all browsers including mobile devices where `window.onbeforeunload` may not function correctly.

## Dialog Components

### Utils.yesNo() - Confirmation Dialog
Located in `Utils.js`, the `yesNo()` method displays a draggable confirmation dialog with "Yes" and "No" buttons.

**Usage:**
```javascript
Utils.yesNo('Title', 'Question text', yesFun, noFun);
```

**Parameters:**
- `title` (string): Text displayed in the dialog header
- `message` (string): The question or prompt shown to the user
- `yesFun` (function, optional): Callback executed if user clicks "Yes"
- `noFun` (function, optional): Callback executed if user clicks "No"

**Returns:** Promise that resolves when dialog is closed

**Implementation Notes:**
- Creates modal DOM structure if it doesn't exist (id: `yesno-modal`)
- Dialog is draggable via `Utils.makeDraggable()`
- Mobile-responsive: adjusts width based on screen size
- Uses custom CSS classes: `msg-modal`, `msg-modal-content`, `msg-modal-header`, etc.

### Utils.makeDraggable() - Draggable Windows
Makes a window or dialog draggable by the header/title bar.

**Usage:**
```javascript
Utils.makeDraggable(headerElement, contentElement);
```

**Parameters:**
- `header` (DOM Element): The element to use as drag handle (typically the title bar)
- `content` (DOM Element): The element to be moved when dragging

**IMPORTANT:** Both parameters must be DOM elements, not string IDs. Use `DOMUtils.getElement()` to get element references:
```javascript
Utils.makeDraggable(
    DOMUtils.getElement('header-id'),
    DOMUtils.getElement('content-id')
);
```

**Implementation Details:**
- Supports both mouse and touch events (mobile compatible)
- Sets cursor style to 'all-scroll' on header
- Stores handler references for proper cleanup

---

*Updated: 2025-12-19*
