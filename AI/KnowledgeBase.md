# KISS Web Development Framework Knowledge Base

## Important Instructions
**This file must be updated whenever new information about the Kiss framework is discovered or learned. Any significant findings, patterns, configurations, or solutions should be documented here immediately.**

**CRITICAL: Kiss is a generic, application-neutral framework. All changes to Kiss core code, configuration options, documentation, and comments must remain completely free of references to any specific application, third-party product, or proprietary technology. Never reference specific application names, specific non-SQL databases, or any application-specific concepts in framework code or documentation. Use generic terms such as "alternative data store" rather than naming specific products.**

### Keeping Generic Code Generic (operational checklist)

The CRITICAL rule above is easy to state and easy to violate in practice. Apply these operational rules whenever a change would touch generic framework code.

**Generic (framework) — application-neutral; treat as read-only, change only when unavoidable and only generically:**
- `src/main/frontend/kiss/**` — frontend framework and components
- `src/main/core/**` — core Java framework
- `src/main/precompiled/**` — shared utilities (an application may add its own utility classes here, but must not alter framework-owned ones)
- `AI/KnowledgeBase.md` — this document: the generic Kiss-framework reference (see "Documentation and where knowledge lives" below)

**Application-specific — the normal work area; all app code, values, and branding go here:**
- `src/main/backend/**` — services and business logic
- `src/main/frontend/**` except `kiss/` — application screens, application theme CSS, application JavaScript
- `src/main/backend/application.ini` — all secrets, URLs, keys, and per-deployment config
- `AI/ApplicationDetails.md` — this application's own notes (per application; never shared)

**Documentation and where knowledge lives.** This file, `AI/KnowledgeBase.md`, is the generic Kiss-framework reference. It is itself a shared framework artifact: it must read **identically for every Kiss application** (kept in sync with the canonical framework repository) and must contain **only** generic, application-neutral material. Application-specific notes — anything that names or is tuned to one application — belong in that application's `AI/ApplicationDetails.md`, which is never shared. When generic framework knowledge or code first appears inside an application, **upstream it**: move the code into the framework tree and the documentation into this file, so every application benefits and the shared artifacts stay in sync; keep only the application-specific remainder in the app.

**Rules for any change that does touch a generic file:**
1. No application-specific references — no application, product, or company names, and no application-domain concepts, in code, comments, documentation, or identifiers. Use neutral generic terms.
2. No brand-specific values baked in — colors, logos, copy, fonts, URLs, or dimensions tuned to one application. (For example, never hardcode a brand color such as `rgba(r, g, b)` inside a framework component.)
3. No secrets or environment literals — URLs, API keys, passwords, and tokens belong in `application.ini`, read via `MainServlet.getEnvironment()`.
4. Parameterize instead of hardcoding — expose application-specific values as CSS custom properties, configuration keys, or override hooks WITH NEUTRAL DEFAULTS in the framework; the application supplies its specifics from the application layer (application theme CSS, `application.ini`). Example: the framework reads a `--glow-color` CSS variable with a neutral default, and the application sets its own brand color in its theme file.
5. The change must be a generic capability any Kiss application could use — never something that only makes sense for one application.
6. When a component's behavior or API changes, update its documentation in `src/main/frontend/kiss/component/components.js` in the same change.

**Before committing, self-check the diff of every generic file:**
- Grep it for the application's name and for any brand color/value — there must be ZERO matches in generic files.
- Confirm the change would make sense in a brand-new, unrelated Kiss application.
- In the commit message / PR description, explicitly list every generic framework file you changed and why, so it can be reviewed and synchronized across the framework and its applications.

If a change seems to require application-specific behavior inside a generic file: stop, move it to the application layer instead, or ask.

## Overview
KISS is a **Kiss Framework application** - a full-stack Java web framework designed for rapid business application development. It includes both front-end and back-end already integrated and running as a basic example application. The framework emphasizes simplicity, productivity, and the ability to make changes while the system is running without requiring compilation or restarts.

## Architecture

### Technology Stack
- **Backend:** Java 17+, Groovy, ABCL (Lisp support)
- **Frontend:** JavaScript/HTML/CSS with custom UI components (custom HTML tags)
- **Database:** PostgreSQL, MySQL, MS SQL Server, Oracle, SQLite
- **Build System:** Custom "bld" system (Maven/Gradle not used)
- **Server:** Tomcat 11.x (Jakarta EE 11, Servlet 6.1) - embedded

### Directory Structure
```
Kiss/
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
   - No recompilation needed during development for backend service files
   - Automatic compilation and loading applies **only to files under `src/main/backend/`**
   - Files under `src/main/core/` and `src/main/precompiled/` are NOT dynamically loaded — they are compiled by the `bld` build and require a server restart to take effect

2. **Multi-Language Services**
   - Write services in Java, Groovy, or Lisp
   - Services auto-compile on change (only when located under `src/main/backend/`)

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
   - Ollama integration ready (`org.kissweb.llm.Ollama`) — single-prompt `send()` (`/api/generate`) plus multi-turn, role-structured `chat(messages)` / `chat(messages, tools)` (`/api/chat`), which gives correct per-model chat templating, explicit system/user/assistant roles, and a `tools` hook for function calling. The blocking calls request `stream:false` (a single JSON object). Streaming overloads `send(prompt, Consumer<String> onToken)` and `chat(messages, onToken)` request `stream:true`, deliver each chunk to `onToken` as the model produces it, and also return the full assembled text (pass `null` for `onToken` to just assemble); they read Ollama's newline-delimited stream via `RestClient.streamCall`
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

`application.ini` supports named `[section]`s. `MainServlet.readIniFile("application.ini", "main")` (called from `KissInit.groovy`) flattens only the `[main]` section into the global environment read by `MainServlet.getEnvironment(key)`. Other sections are not in that flat map; read them directly with `IniFile.load("application.ini")` and `get(section, key)` / `getSectionNames()` (this is how the OAuth client reads its `[OAuthClient *]` sections).

### Ini File Path Resolution (org.kissweb.IniFile)

`IniFile.load(String)` and `IniFile.save(String)` resolve a filename the **same** way (a single shared rule): an absolute path is used verbatim; a relative path is resolved against `MainServlet.getApplicationPath()` (the backend directory), or against the current working directory when the application path is not set (null/empty). A file written with a given relative name is therefore read back with that same name. (Constructing `new IniFile(fname)` followed by no-arg `save()` writes to whatever path was supplied/loaded.)

### Database Connection Failure at Startup

When a database **is** configured in `application.ini` but cannot be reached at
startup (server down, database dropped/nonexistent, bad credentials), Kiss
logs an unmistakable multi-line FATAL banner naming the database and the
underlying error, and then fails this web application's deployment. The
servlet container (Tomcat) itself is deliberately left running — a bad
database must never take down the container or other applications it hosts on
a production system. Note that with the context disabled, requests get a bare
404/503 with no application headers, which browsers may misreport (e.g. as a
CORS policy failure) — the banner in `tomcat/logs/catalina.out` is where the
real cause is found. Running with **no** database configured remains supported
and is unaffected.

Implementation: `MainServlet.databaseConnectionFatal()` (logs the banner),
invoked from the `makeDatabaseConnection()` failure path in
`initializeSystem()`, which then throws to abort the context.

### Secrets and External Configuration

**All URLs, passwords, security keys, API keys, tokens, and user names must be kept in `src/main/backend/application.ini`.** Nothing of this kind should be hard-coded in source files (Java, Groovy, Lisp, JavaScript, HTML, or other configuration files). Whenever such a value is found outside `application.ini`, it must be moved into `application.ini` and accessed via the API below.

**Retrieval API:** Values stored in `application.ini` are retrieved at runtime using the `getEnvironment` method on `MainServlet`, defined in `src/main/core/org/kissweb/restServer/MainServlet.java`:

```groovy
import org.kissweb.restServer.MainServlet

String apiUrl     = MainServlet.getEnvironment("ExternalApiUrl")
String apiKey     = MainServlet.getEnvironment("ExternalApiKey")
String smtpUser   = MainServlet.getEnvironment("SmtpUser")
String smtpPass   = MainServlet.getEnvironment("SmtpPassword")
```

**Rationale:**
- Centralizes secrets so they can be rotated without code changes
- Keeps credentials out of version control when `application.ini` is excluded from the repository
- Avoids duplicating values across multiple files
- Makes per-environment configuration (dev, staging, production) straightforward — only `application.ini` differs

**Categories that must live in `application.ini`:**
- URLs of external services (REST endpoints, SMTP servers, LDAP servers, third-party APIs, webhooks)
- Passwords (database, SMTP, service accounts, third-party APIs)
- Security keys, API keys, tokens, client secrets, encryption keys, signing keys
- User names / account identifiers used by the application to authenticate to external systems

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

  `develop` (on keypress) and `stop-backend` stop Tomcat by sending the
  SHUTDOWN command directly to the configured shutdown port. If Tomcat is not
  running — e.g. it crashed or was never started — they print one clear line
  pointing at `tomcat/logs/catalina.out` instead of a SEVERE stack trace (with
  a single 2-second retry in case Tomcat was still starting).
- `./bld -v build` - Build the application (compiles all Java files including precompiled directory).
  Note: `build` also creates the **production** WAR and deploys it to the local
  tomcat as `webapps/ROOT.war`. Left in place, tomcat would re-explode that WAR
  over `ROOT` on the next startup, replacing the development tree — most visibly
  `web.xml`, whose production CORS allow-list rejects the cross-origin development
  front-end (the browser shows a CORS preflight failure on every service call).
  `develop` and `start-backend` therefore remove any stale `ROOT.war`
  (`Tasks.removeDeployedWar()`) before starting tomcat: development always runs
  from the exploded tree.
- `./bld war` - Create WAR file for deployment
- `./bld -v unit-tests` - Build `work/KissUnitTest.jar`, a self-contained JUnit 5 console-launcher jar
  (Kiss core + precompiled + test classes + JUnit + Log4j + jakarta.servlet-api, all unpacked and
  re-jarred together with a `Main-Class: org.junit.platform.console.ConsoleLauncher` manifest). There is
  no separate `test` task - `unit-tests` only builds the jar; run the tests with:
  ```
  java -jar work/KissUnitTest.jar --scan-class-path=work/KissUnitTest.jar
  ```
  The explicit `--scan-class-path=work/KissUnitTest.jar` argument is required. `java -jar` sets the
  running JVM's classpath to just that one jar, but a bare `--scan-class-path` (no argument) scans the
  JVM's classpath *entries* looking for directories/jars to open separately - it does not also imply
  "scan the jar this launcher is itself running from." Pointing it explicitly at the jar makes the
  launcher open and scan that jar's own classes.
- `./bld clean` - Clean build artifacts
- `./bld javadoc` - Generate JavaDoc documentation

### Build System Architecture

The `bld` system is three Java files, compiled together by the `bld` / `bld.cmd`
bootstrap scripts:

- `src/main/core/org/kissweb/BuildUtils.java` — fully generic build utilities
  (compile, jar, download, copy, run). Contains nothing Kiss- or Tomcat-specific,
  because it is also used as part of a generic build system unrelated to Kiss.
- `src/main/core/org/kissweb/KissBuildUtils.java` — build procedures common to
  every Kiss application: the four development ports and the `-dp/-bp/-sp/-fp`
  option parsing (`consumePortOptions`), single-call port-block assignment
  (`setPortBase`), Tomcat install and port stamping
  (`installTomcat(version)`), `shutdownTomcat()`, WAR cache-busting stamping
  (`stampVersion(explodedDir)`), `getTomcatPath()`, and `stopFrontendServer()`.
- `src/main/precompiled/Tasks.java` — this application's build tasks and
  dependency lists only (per-application; not shared).

When generic build functionality accumulates in `Tasks.java`, upstream it into
`KissBuildUtils.java` (Kiss-wide) or `BuildUtils.java` (universally generic) —
never Kiss/Tomcat-specific code into `BuildUtils.java`.

**Development port block.** All four development ports come from a single
variable at the top of `Tasks.java` — `private static int portBase = 8000;` —
which `Tasks.main()` always passes to `setPortBase` (before
`consumePortOptions`, so the `-dp/-bp/-sp/-fp` options still override
individual ports). The block is consecutive: **frontend = portBase, backend =
portBase+1, shutdown = portBase+2, debug = portBase+3** (default 8000 →
8000/8001/8002/8003). Give each Kiss application a unique `portBase` and they
all run in development mode simultaneously without port clashes. `bld help`
shows the live values.

The front end finds the back end with no configuration:

- A page served by the front-end dev server calls the back end at **its own
  port + 1** (the block convention; see `index.js`).
- Copies served by Tomcat itself are stamped `SystemInfo.sameOriginBackend =
  true` (`KissBuildUtils.stampSameOriginBackend`, applied to the WAR staging by
  `stampVersion` and to `tomcat/webapps/ROOT` by develop/start-backend), so
  those pages call the back end at the page's own origin — this makes browsing
  the Tomcat origin directly, and any production port, work for any base. The
  source `SystemInfo.js` ships `false` and is never stamped.
- `SystemInfo.backendUrl`, when set, overrides everything (split deployments,
  or when the `-fp`/`-bp` flags break the +1 convention).
- Electron (`file://`) defaults to `http://localhost:8001` (the default
  block's back end); applications with a different base set
  `SystemInfo.backendUrl`.

## Documentation System

The Kiss framework has three primary documentation vehicles:

### 1. Backend API Documentation (JavaDoc)
- Standard JavaDoc documentation for the backend Java/Groovy API
- Generated by the application programmer using: `./bld javadoc`
- Output location: `work/javadoc/`
- Documents all backend classes, methods, and services

### 2. Frontend API Documentation (JSDoc)
- Similar to JavaDoc but for JavaScript/Frontend API
- Generated by the Kiss framework developer (not the application programmer)
- Included as part of the framework's system documentation
- Output location: `manual/jsdoc/`

### 3. User Manual (TexInfo)
- Comprehensive user manual using GNU TexInfo format
- Produces both **HTML** and **PDF** output from the same source files
- Provides framework usage documentation, tutorials, and reference material

### Documentation Build Process
- Both JSDoc and the User Manual are generated from the `manual/` directory
- A standard `Makefile` orchestrates the documentation build process
- To build documentation, run `make` from the `manual/` directory

### Directory Structure
```
manual/
├── Makefile          # Orchestrates JSDoc and TexInfo builds
├── jsdoc/            # Generated frontend API documentation
├── *.texi            # TexInfo source files for user manual
└── (output files)    # Generated HTML and PDF manual
```

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
- **Frontend (Development):** http://localhost:8000 (portBase)
- **Backend (Development):** http://localhost:8001 (portBase+1)
- **Backend Log:** tomcat/logs/catalina.out

These are the defaults of the development port block (`portBase = 8000` in
`Tasks.java`; see "Build System Architecture").

### Hot Reload
- **Dynamic loading is limited to `src/main/backend/`** — Kiss does NOT dynamically load all source files. Only files under `src/main/backend/` are detected, compiled, and reloaded while the server is running.
- Backend services (Java/Groovy/Lisp) under `src/main/backend/` auto-compile on change
- Files under `src/main/core/` (framework code) and `src/main/precompiled/` (shared utilities) are compiled by `./bld` and require a server restart to pick up changes
- Frontend files (under `src/main/frontend/`) are served as static assets — changes are immediately reflected on browser reload, but this is not "dynamic loading" by the server
- Both backend services and frontend code can be changed while the system is running, subject to the scope above

## Key Libraries

### Backend Dependencies
- Groovy 4.0.26 - Dynamic language support
- C3P0 0.11.2 - Database connection pooling
- Log4j 2.25.3 - Logging framework (log4j 2.x API)
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

### Service-Side Authentication (the "service decides" pattern)

By default every web-service method is authenticated **before** it is dispatched: an invalid/expired session returns the standard "not logged in" response (`_ErrorCode = 2`) and the service never runs.

A method registered with `MainServlet.allowWithoutAuthentication("package/Class", "method")` (called from `KissInit.groovy`) instead runs **whether or not** the caller is logged in — the "call the service first, then let it decide" model. Inside such a service (signature `(JSONObject in, JSONObject out, Connection db, ProcessServlet servlet)`), use these `ProcessServlet` helpers:

| Method | Behavior |
|---|---|
| `servlet.isLoggedIn()` | `true` if a valid session is attached to this request — branch on it to do one thing when logged in, another when not |
| `servlet.getUserData()` | the `UserData` for the session, or `null` when not logged in |
| `servlet.getUserData(key)` | a per-session value, or `null` when the key is absent **or** not logged in (null-safe) |
| `servlet.requireLogin()` | abort the call with the standard `_ErrorCode = 2` response when not logged in (so the front-end routes to login); a no-op when logged in |

`requireLogin()` throws `org.kissweb.restServer.LoginRequiredException`; the framework detects it (in `errorReturn`, walking the cause chain) and converts it to the same `_ErrorCode = 2` response a normally-protected method produces — so it works uniformly across Groovy, Java, compiled-Java, and Lisp services without any per-runner code.

The front-end completes the loop: any `_ErrorCode = 2` triggers `Server.logout(true)`, which clears `AppState` and routes to `/login` **carrying the current location as the `return`** — so after re-authenticating, the user lands back on the page they were on (session-expiry resume). An intentional logout (`Server.logout()`) goes to `/login` without a return. See [Client-Side Routing](#client-side-routing-router).

### Per-Request Connection Preparation (RequestConnectionPreparer)

`org.kissweb.restServer.RequestConnectionPreparer` is an application-registered
hook for preparing each REST request's database connection. Register with
`MainServlet.setRequestConnectionPreparer(...)` (typically from
`KissInit.groovy`). `prepare(db, userData)` runs after authentication and
before the service method (userData is null for allowed-without-authentication
calls with no session); if it throws, the request aborts before any service
code runs (fail-closed). `release(db)` runs when the request connection is
closed, before it returns to the pool, to clear any per-connection state.
Typical use: multi-tenant applications selecting a per-tenant schema (e.g.
PostgreSQL `search_path`) so every web service stays tenant-neutral.
Connections obtained outside the request path (cron, `openNewConnection`) are
not passed to `prepare`.

**Server-restart detection (boot id).** Sessions live in the in-memory `UserCache`, so a back-end restart invalidates them all — but the *client's* token persists in `AppState`, so without help the browser would resume onto a dead session. To prevent that, `MainServlet.getBootId()` returns a UUID generated once per server start; it's included in every response as `_BootId`. The front-end records it at login (`Server.setBootId` from the `Login` response) and, at startup before routing, `Server.verifyServerInstance()` makes one unauthenticated `LoginRequired` call and compares: if the boot id changed, the back end was restarted, so it clears the persisted session and forces a clean re-login (no resume) with a "server was restarted" notice. A restart while the app is open is caught on the next call by the normal `_ErrorCode = 2` path, which logs the user out — and **logout performs a full page reload** of a fresh `index.html` (`Server.logout`), not an in-app re-login. Since a redeploy invalidates the session (routing an open tab through logout) and the reload re-boots on the cache-busted assets (see [Cache Busting](#cache-busting-force-refresh-every-file-on-upgrade)), a long-open tab can never keep running **stale front-end code** against a freshly deployed back end — it picks up the new release automatically. This needs no restart *detection* on the runtime path: reloading on every logout is harmless because cache-busting re-downloads only when `app-version` actually changed (an unchanged release just re-boots from cache), and the reload carries any session-expiry resume target into the login route.

### CORS and Reverse Proxies (web-secure.xml / web-unsafe.xml)

Kiss ships two web.xml variants in `src/main/core/WEB-INF/`:
- `web-unsafe.xml` — deployed by `buildSystem()` for development; `cors.allowed.origins = *` (needed because the dev frontend `:8000` and backend `:8001` are different origins)
- `web-secure.xml` — swapped in by `./bld war` for the production WAR; keeps a localhost-only allow-list (`http://localhost:8000,http://localhost:63342`)

**No per-deployment CORS configuration is needed in production.** In the normal single-WAR deployment, frontend and backend are same-origin. Browsers still send an `Origin` header on every POST (the Fetch spec attaches it to all non-GET requests, even same-origin ones), so Tomcat's CorsFilter engages — but it classifies the request as NOT_CORS via `RequestUtil.isSameOrigin()` and never consults the allow-list. The localhost allow-list in `web-secure.xml` is therefore harmless in production; it only blocks genuinely foreign origins.

**TLS-terminating reverse proxies would break same-origin detection** — the proxy forwards over plain http, so Tomcat would believe the scheme is http while the browser's `Origin` says https; the mismatch misclassifies every API call as cross-origin and CorsFilter returns 403. For this reason `web-secure.xml` includes Tomcat's `RemoteIpFilter`, mapped BEFORE the CorsFilter, with `protocolHeader = X-Forwarded-Proto`. It restores the original scheme/port and trusts the header only from loopback/private (RFC 1918) source addresses (the `internalProxies` default), so outside clients cannot spoof it. The proxy must send `X-Forwarded-Proto` and pass `Host` through — standard reverse-proxy practice. Deployments not behind a proxy are unaffected.

**Split deployments are the one true cross-origin case.** If the frontend is served from a different origin than the backend (`SystemInfo.backendUrl` set), the frontend origin must be added to `cors.allowed.origins` in `web-secure.xml` by hand — no same-origin bypass can apply there.

(An earlier `AllowedOrigins` application.ini key that the build stamped into the deployed web.xml was removed in favor of the RemoteIpFilter approach — it required per-deployment configuration for what the server can determine automatically.)

### Crypto, Hashing, and MACs — which class to use

Kiss provides four separate, flatly-namespaced classes for everything crypto-shaped, deliberately kept
distinct rather than merged behind one facade (a facade would put a reversible-encryption call and a
one-way-hash call one method name apart, which is exactly the ambiguity that invites misuse - e.g.
someone "just encrypting a password reversibly"). Pick by what you actually need:

| Need | Use |
|---|---|
| Recover the original value later (reversible) | `org.kissweb.Crypto` |
| Store a user's password (never recoverable) | `org.kissweb.PasswordHash` |
| A plain content fingerprint/digest, no secret key | `org.kissweb.Hash` |
| A keyed digest/MAC, or a deterministic "blind index" for equality search over an encrypted column | `org.kissweb.Hmac` |

Two small shared utilities support all four: `org.kissweb.ConstantTime` (timing-safe byte-array
comparison) and `org.kissweb.RandomUtil` (shared `SecureRandom`-backed helpers).

### Reversible Encryption (org.kissweb.Crypto)

`org.kissweb.Crypto` is the framework utility for reversible encryption of strings and byte arrays — data that a legitimate caller must later be able to recover in plaintext (e.g. a value another system needs verbatim). It is the wrong tool for passwords; use `org.kissweb.PasswordHash` for those (see below), since passwords must never be recoverable.

**Algorithm.** AES-GCM (`AES/GCM/NoPadding`), an authenticated cipher (AEAD): every ciphertext carries a 128-bit integrity tag, so tampering or corruption is detected on decrypt rather than silently producing garbage plaintext. Each encrypt call generates a fresh random 12-byte nonce from `SecureRandom` — nonces are never reused, which GCM requires for its security guarantees. Per-value non-determinism (the same plaintext encrypting to different ciphertext each time) comes solely from this nonce, regardless of which key-sourcing path below is used.

**Two ways to supply the key:**
- **Password-based** (`new Crypto(password)`, `Crypto.deriveOnce(password, salt)`): the AES key is derived from a human-choosable password (and optional salt) via `PBKDF2WithHmacSHA256` (65,536 iterations, 256-bit derived key) — container format `VERSION_1`. PBKDF2 exists to slow down brute-forcing a *low-entropy* secret; it makes sense for an actual password, not for a machine-generated key.
- **Raw key** (`Crypto.fromKey(byte[])`, `Crypto.fromKeyBase64(String)`): the caller supplies an already high-entropy 256-bit key directly — no PBKDF2, no salt, no per-call KDF cost — container format `VERSION_2`. **This is the recommended path for bulk/high-volume field encryption** (e.g. a master key sourced once from `application.ini` and reused for many rows): applying PBKDF2 to a key that already has nothing for it to usefully slow down only adds latency, and doing it *per row* rather than once is the single most common misuse of this class in practice. Use `Crypto.generateKey()` / `Crypto.generateKeyBase64()` to provision a fresh key. A `VERSION_2` value can only be decrypted by a `fromKey`/`fromKeyBase64`-constructed instance (a password-based instance throws `IllegalStateException`, since there is no password/derivation involved).

**Key-derivation caching.** For password-based instances, the PBKDF2-derived key for a given (password, salt) pair is computed at most once per instance and memoized for every later encrypt/decrypt call with that same salt — no construction path re-derives the key on every call. `Crypto.deriveOnce(password, salt)` additionally computes that derivation *eagerly*, at construction, rather than lazily on first use — useful when a caller wants the one-time PBKDF2 cost to land at a predictable point (e.g. once at the start of a batch) rather than on whichever row happens to run first. Its output is byte-for-byte `VERSION_1`-compatible with (and decryptable by) a plain `new Crypto(password)` instance — it changes only *when* PBKDF2 runs, not the format or the key. A caller who can instead source a genuinely high-entropy key should prefer `fromKey`, which removes the PBKDF2 cost entirely rather than just relocating it.

**`DEFAULT_KDF_SALT` fallback.** When a password-based encrypt/decrypt call supplies no salt, PBKDF2 (which requires a non-empty salt) falls back to a fixed, built-in salt. This remains fully supported for decryption and is unaffected by anything above, but it is a **compatibility fallback for pre-existing callers only** — new code should supply an explicit salt, or (better) a per-value random salt via `encryptWithRandomSalt`, or (better still) use `fromKey`/`fromKeyBase64` where salt/KDF do not apply at all. The first time any `Crypto` instance in a JVM falls back to `DEFAULT_KDF_SALT`, a one-time `WARN` is logged (Log4j) as a nudge to migrate — it never throws and never changes what gets decrypted.

**Self-describing versioned container.** Every value produced by the current format encodes everything decryption needs: `MAGIC(8) | VERSION(1) | saltLen(2) | salt | nonce(12) | ciphertext+tag`. String outputs are additionally prefixed `$KC1$` and Base64-encoded; byte-array outputs carry the same container with a leading `0x00` magic byte. `VERSION_1` (password/PBKDF2) and `VERSION_2` (raw key, `saltLen` always 0) share this same envelope shape; the versioning leaves room for the container format to evolve further without breaking data already at rest.

**Backward compatibility.** Older data written before the AES-GCM rework (plain `AES/ECB/PKCS5Padding` with a homegrown salt+password key derivation) is still transparently decryptable — decryption auto-detects the legacy format (absence of the `$KC1$` prefix / magic bytes) and falls back to the legacy path. All *new* encryptions always use one of the current AES-GCM formats; there is no way to opt back into the legacy format. Data produced via `fromKey`/`fromKeyBase64` (`VERSION_2`) cannot be read by code that predates that feature.

**API:**

| Method | Description |
|---|---|
| `new Crypto(String password)` | Password-based instance (throws if null/empty); PBKDF2-derived key, `VERSION_1` |
| `Crypto.deriveOnce(String password, String salt)` | Same as above, but the PBKDF2 derivation for `salt` runs immediately at construction instead of lazily |
| `Crypto.fromKey(byte[] key256)` / `Crypto.fromKeyBase64(String base64Key256)` | Raw-key instance, no PBKDF2/salt, `VERSION_2`; key must be exactly 256 bits (32 bytes) |
| `Crypto.generateKey()` / `Crypto.generateKeyBase64()` | Generate a fresh random 256-bit key, for provisioning a `fromKey` master key |
| `String encrypt(String valueToEnc)` / `String encrypt(String salt, String valueToEnc)` | Encrypt a string, no salt or caller-supplied salt (ignored in raw-key mode); returns Base64 |
| `String encryptWithRandomSalt(String valueToEnc)` | Encrypt a string with a fresh random salt embedded in the output (recommended when no natural salt exists) |
| `String decrypt(String encryptedValue)` / `String decrypt(String salt, String encryptedValue)` / `String decryptWithRandomSalt(String encryptedValue)` | Corresponding decrypt calls (must match how the value was encrypted) |
| `byte[] encrypt(byte[] valueToEnc)` / `byte[] encrypt(String salt, byte[] valueToEnc)` / `byte[] encryptWithRandomSalt(byte[] valueToEnc)` | Byte-array equivalents |
| `byte[] decrypt(byte[] encryptedValue)` / `byte[] decrypt(String salt, byte[] encryptedValue)` / `byte[] decryptWithRandomSalt(byte[] encryptedValue)` | Byte-array equivalents |

A single `Crypto` instance is safe to share/reuse across threads (each call creates its own `Cipher`; the key-derivation cache is a `ConcurrentHashMap`).

**Usage:**
```java
import org.kissweb.Crypto

// Password-based (low-volume / human passphrase)
Crypto crypto = new Crypto(masterKeyFromApplicationIni)   // never hardcode the password/key
String enc = crypto.encryptWithRandomSalt(plainTextValue)  // store 'enc'
String plain = crypto.decryptWithRandomSalt(enc)           // recover later

// Raw key (recommended for bulk/high-volume field encryption)
String key = Crypto.generateKeyBase64()          // provision once, store in application.ini
Crypto fast = Crypto.fromKeyBase64(keyFromApplicationIni)
String enc2 = fast.encryptWithRandomSalt(plainTextValue)
String plain2 = fast.decryptWithRandomSalt(enc2)
```

The password/key passed to `Crypto` is itself a secret and must come from `application.ini` (see "Secrets and External Configuration" above), never a literal in source.

### Password Storage (org.kissweb.PasswordHash)

`org.kissweb.PasswordHash` is the framework utility for storing user passwords. Passwords must be stored using this class — never as plain text, and never with reversible encryption.

**Hashed, not encrypted.** Passwords are hashed one-way with PBKDF2-HMAC-SHA256 (600,000 iterations, the OWASP 2023 floor) and a random per-password salt. There is intentionally no way to recover the original password; authentication only ever *verifies* a candidate, in constant time via `org.kissweb.ConstantTime`. (For reversible encryption of arbitrary data, use `org.kissweb.Crypto` instead — it is the wrong tool for passwords.) PBKDF2-HMAC-SHA256 remains the framework's only supported algorithm — Argon2id was evaluated and deliberately not added, to keep Kiss dependency-free (pure JDK) for every application; `needsRehash` below is nonetheless structured so a second, stronger algorithm could be added later as an additional recognized prefix without changing any caller-visible signature.

**API (all static):**

| Method | Description |
|---|---|
| `String hash(String password)` | Hash a plain-text password for storage |
| `boolean verify(String password, String stored)` | Constant-time verify a candidate against a stored hash |
| `boolean isHashed(String stored)` | True if a stored value is in the PasswordHash format (vs. a legacy/plain value) |
| `boolean needsRehash(String stored)` | True if `stored` should be re-hashed and re-saved (e.g. it was hashed at a lower iteration count than the current default, or isn't even in this format at all) |

**Stored format** is a self-describing string so the work factor can be raised over time without invalidating existing hashes:
```
pbkdf2$<iterations>$<Base64(salt)>$<Base64(hash)>
```
Defaults: 600,000 iterations, 16-byte salt, 256-bit derived key. The encoded value is ~80 characters, so the storage column must be at least `varchar(255)`.

**Usage — the standard "verify, then opportunistically rehash" pattern:**
```groovy
import org.kissweb.PasswordHash

// When setting or changing a password:
rec.set("user_password", PasswordHash.hash(plainTextPassword))

// When authenticating:
boolean ok = PasswordHash.verify(enteredPassword, rec.getString("user_password"))
if (ok && PasswordHash.needsRehash(rec.getString("user_password")))
    rec.set("user_password", PasswordHash.hash(enteredPassword))   // upgrade the work factor in place
```
`needsRehash` lets every stored password gradually migrate up to the current work factor as users log in successfully, with no bulk migration required.

### General-Purpose Hashing (org.kissweb.Hash)

`org.kissweb.Hash` provides unkeyed cryptographic digests — SHA-256/384/512 and their SHA-3
counterparts — for content fingerprints, dedup/change detection, checksums, and cache keys, where the
input is not secret and speed is a feature, not a liability. Pure JDK: `MessageDigest` has supported
SHA-3 natively since Java 9, so no external dependency is needed for any algorithm here. **Never use
`Hash` for passwords** (use `PasswordHash`) **or for anything needing a secret key/proof-of-origin**
(use `Hmac`) — a plain digest has neither property.

**API (all static):** for each of `sha256`/`sha384`/`sha512`/`sha3_256`/`sha3_512`, three forms exist —
`byte[] <algo>(byte[] data)` / `<algo>(String data)` (UTF-8), `String <algo>Hex(...)` (lowercase hex),
and `String <algo>Base64(...)` (standard Base64) — plus streaming/file helpers:

| Method | Description |
|---|---|
| `hash(InputStream in, String algorithm)` | Digest a stream in fixed-size chunks (does not load the whole input into memory); caller closes the stream |
| `hashFile(File file, String algorithm)` / `hashFileHex(File file, String algorithm)` | Digest a file's content, streamed |

**Usage:**
```java
import org.kissweb.Hash

byte[] digest = Hash.sha256("some content")
String hex     = Hash.sha256Hex("some content")
String b64     = Hash.sha256Base64("some content")
String fileSha = Hash.hashFileHex(new File("upload.bin"), "SHA-256")
```

### Keyed Digests / HMAC and the Blind-Index Pattern (org.kissweb.Hmac)

`org.kissweb.Hmac` provides HMAC-SHA256/384/512 (pure JDK, `javax.crypto.Mac` — no dependency). Unlike
`Hash`, every digest here is parameterized by a secret key: only someone holding the key can produce or
verify a matching tag for a given message. API shape mirrors `Hash`: `byte[] hmacSha256(byte[] key,
byte[]|String message)` plus `hmacSha256Hex`/`hmacSha256Base64` (and the `384`/`512` variants). SHA-256
is the default/recommended algorithm for new work; 384/512 are available when needed.

**The blind-index pattern.** `Crypto`'s AES-GCM is deliberately non-deterministic (a fresh random nonce
every call), which is exactly what makes it safe — but it also means `WHERE encrypted_column = ?` cannot
work directly against it. A **blind index** restores equality lookup: store a deterministic, keyed HMAC
of the (normalized) plaintext alongside the encrypted column, and query against that instead:
```java
// write path
row.encryptedValue = Crypto.fromKey(fieldEncryptionKey).encryptWithRandomSalt(value)   // reversible, for display/export
row.blindIndex      = Hmac.hmacSha256Hex(fieldIndexKey, normalize(value))              // deterministic, for lookup only

// read path — an indexed equality lookup with no decryption of any row
String candidateIndex = Hmac.hmacSha256Hex(fieldIndexKey, normalize(candidateValue))
// ... SELECT ... WHERE blind_index = candidateIndex ...
```
Two responsibilities stay with the caller, not `Hmac`, because they are application/data-shape decisions:
- **Key separation** — `fieldIndexKey` must be a *different* key from `fieldEncryptionKey`. Never reuse
  one secret for two primitives; each is its own `application.ini` entry.
- **Normalization** — normalize the plaintext before hashing (trim whitespace, strip punctuation, fix
  case, etc., as appropriate) so formatting variance doesn't defeat equality lookup. What "normalized"
  means is inherently specific to the value being indexed.

**Usage:**
```java
import org.kissweb.Hmac

byte[] mac = Hmac.hmacSha256(secretKey, "message to authenticate")
String hex = Hmac.hmacSha256Hex(secretKey, "message to authenticate")
```

### Constant-Time Comparison (org.kissweb.ConstantTime) and Shared Randomness (org.kissweb.RandomUtil)

`org.kissweb.ConstantTime.equals(byte[] a, byte[] b)` compares two byte arrays without an early return on
the first differing byte, so its running time depends only on array length, not content — use it (rather
than `Arrays.equals`/`String.equals`) whenever comparing a MAC, hash, or blind-index value against an
expected value. `PasswordHash` uses it internally for hash verification.

`org.kissweb.RandomUtil` provides `randomBytes(int n)` and `randomAlphaNumeric(int len)` backed by one
shared `SecureRandom` instance, so new code needing random key material or tokens has one obvious place
to get it rather than instantiating another `SecureRandom`.

### OAuth 2.1 (`org.kissweb.oauth`)

Kiss implements all three OAuth 2.1 roles. Each is inert unless configured (no runtime cost, no files created). Full reference: `OAuth.md`.

- **Resource server** (`org.kissweb.oauth`) — validates *incoming* bearer tokens (JWTs) and publishes the RFC 9728 protected-resource discovery document. Enabled by setting `OAuthAuthorizationServer` in `application.ini`; every `MCPServerBase` subclass then requires a valid token automatically. Holds no persistent state (JWKS is cached in memory).
- **Authorization server** (`org.kissweb.oauth.as`) — *issues* tokens: authorization-code flow with mandatory PKCE (S256), token endpoint with refresh-token rotation + reuse detection, RFC 7591 dynamic client registration, RFC 8414 metadata, and JWKS. Enabled with `OAuthAsEnabled = true`. The app must register an `org.kissweb.oauth.as.UserAuthenticator` (in `KissInit.groovy`). Persists keys/clients/refresh-tokens to a private SQLite database (`oauth.db` by default; `OAuthAsSqliteFile`), separate from the application's main database.
- **Client / relying party** (`org.kissweb.oauth.client`) — *obtains* tokens from a *remote* OAuth 2.1 server and presents them on outbound calls. Drives the authorization-code + PKCE flow with automatic discovery (RFC 9728 → RFC 8414 / OIDC) and DCR when no `ClientId` is configured; captures the redirect at the built-in `/oauth/client/callback` servlet; refreshes access tokens automatically (single-flight per provider, honoring rotation). Supports **multiple providers**, each declared in its own `application.ini` section:

  ```ini
  [OAuthClient myprovider]
  Url          = https://remote.example.com/mcp   ; required
  Scopes       = mcp:read mcp:write               ; optional
  ClientId     =                                  ; blank => Dynamic Client Registration
  ClientSecret =                                  ; only if pre-registered + confidential
  ```

  Usage:
  ```java
  import org.kissweb.oauth.client.OAuthClient
  OAuthClient client = OAuthClient.forProvider("myprovider")
  if (!client.isAuthorized())
      redirectBrowserTo(client.beginAuthorization("http://localhost:8001"))
  String token = client.getAccessToken()   // refreshes as needed (discovery/registration happen in beginAuthorization)
  ```
  `getAccessToken()` throws `OAuthAuthorizationRequiredException` when an interactive login is needed.

**Shared database:** the client persists its cached discovery, registration, and tokens in the **same** `oauth.db` the authorization server uses (reusing `OAuthSqliteStore`'s connection and lock) — there is no separate client database. All `oauth.db` access (AS and client) goes through the Kiss `org.kissweb.database` API, never raw JDBC.

### Client-Side State Persistence (`AppState`)

`src/main/frontend/kiss/AppState.js` is the framework's single client-side state store. It holds arbitrary front-end state that must survive page navigation and reload; the session token is not special — it is the reserved entry `_uuid` stored alongside everything else. It is the **only** module permitted to access `window.localStorage` / `window.sessionStorage` directly (the same layering rule DOMUtils applies to the DOM).

The backing store is chosen once at startup from `SystemInfo.stateStore` (in `src/main/frontend/SystemInfo.js`):

| Value | Backing store | Behavior |
|---|---|---|
| `'session'` (default) | sessionStorage | survives reload; **per-tab** (each tab is its own isolated session, so two tabs never clobber each other's state); cleared on tab close |
| `'local'` | localStorage | survives reload, new tabs, and Electron restarts; **shared** across tabs of the origin |
| `'memory'` | in-memory only | classic single-page behavior; lost on reload |

Per-tab (`'session'`) is the default. Consequence: a new browser tab starts with no token and so requires its own login — the session does **not** carry across tabs (reload within a tab still resumes). If Web Storage is unavailable (private mode, disabled, quota), `AppState` falls back to in-memory so the app still runs. Keys are namespaced (`kiss.`) so `clear()` removes only Kiss state. Values are JSON-serialized (no `Date`/`Map`/functions survive). (There is no cross-tab logout coordination — under the per-tab default it's unnecessary.)

API (all static): `init()`, `set(key, value)` (`undefined` removes), `get(key)`, `has(key)`, `remove(key)`, `keys()`, `clear()`, `backend()`.

**Cross-screen app data** — `Utils.saveData(key, val)` / `getData(key)` / `getAndEraseData(key)` are backed by `AppState` (under a `data.` sub-namespace, so they can't collide with `_uuid`/`_bootId`). So data passed between screens now survives reload and is per-tab — the old in-memory `Utils.globalData` (which a router-driven reload would wipe) is gone.

`Server.js` consumes it: `Server.setUUID()` writes `_uuid` through `AppState`, the `Server.uuid` getter reads it, and `Server.logout()` calls `AppState.clear()`. The bootstrap (`src/main/frontend/kiss/bootstrap.js`) loads `AppState.js` and calls `AppState.init()` before `Server.js`.

`AppState` exposes the persisted `_uuid`; the [Router](#client-side-routing-router) uses it for session resume.

### Client-Side Routing (`Router`)

`src/main/frontend/kiss/Router.js` is a hash-based client-side router that gives each screen its own URL. Hash routing (`#/controls`, `#/customer/123`) needs no server-side URL rewriting, so it works identically on the dev static server, `file://`, Electron, and a deployed WAR. The hash changes per screen (deep-linkable, bookmarkable) and the browser Back/Forward buttons become in-app navigation.

It maps the hash onto Kiss's two navigation levels: **full-body screens** (a shell or login, loaded by replacing `document.body`) and **sub-screens** (loaded into a shell region such as `app-screen-area`). Routes are registered in the application-owned **`src/main/frontend/routes.js`** via `Router.add(path, def)`; `index.js` calls `Router.start()` once components are loaded.

Route definition fields: `page` (loadPage path, or a function returning one — used for device-aware mobile/desktop selection), `tag` (container id; omit for full-body — defaults to the home shell's region when fallback routing is on), `shell` (the route hosting a sub-screen; **defaults to the default route**, so it's usually omitted), `focus`, and `auth` (default `true`; set `false` for public routes like login). Path params (`:id`) are parsed, passed to the screen via `loadPage`'s `argv`, and readable with `Router.params()`, so a deep-linked screen can rebuild its state from the URL. A `?tag=` on the URL (or `Router.go(path, tag)`) overrides a screen's region at navigation time.

**File-based fallback** (`Router.setScreenRoot(root, {shell, tag})`): with it enabled, a URL matching no registered route is loaded as a screen *path* under `root` — e.g. root `'screens'` makes `#/Reports/Daily` load `screens/Reports/Daily` into the configured shell/tag, no `Router.add` needed. Register routes only for screens needing a clean/stable URL, params, or special metadata; the rest resolve by convention. Fallback screens always require auth; `..`/empty segments are rejected (no traversal out of the root); and note this makes every screen file under the root URL-addressable (the route table is no longer an allowlist).

API (all static): `add`, `setDefault`, `setScreenRoot`, `start`, `isStarted`, `go(path, tag?)` (push history), `replace(path)` (no Back step), `params()`, `query()`, `returnTarget()`, `current()`.

Dispatch (on `start` and every `hashchange`): match the route → if `auth !== false` and `AppState.get('_uuid')` is absent, `replace('/login?return=<hash>')` → otherwise ensure the shell occupies the body (if any), then `loadPage` the screen. **Session resume** falls out of this: a deep link with a persisted `_uuid` loads straight through, validated lazily by the first `/rest` call (a stale session returns `_ErrorCode 2` → `Server.logout()` → login with a return-URL).

Login/logout integration: `login.js` calls `Router.replace(Router.query().return || '/')` on success (so Back doesn't return to login); `Server.logout()` performs a full page reload of a fresh `index.html`, landing on the login route via `Router.loginHash(captureReturn)` — the single place the login route's URL form (`#/login`, `#/login?return=...`) is defined, also used by `Router.gotoLogin` (see the boot-id section: the reload is what lets an open tab pick up a new release). The old global `DOMUtils.preventNavigation` back-button blocker was retired from the login screens — Back now navigates between screens (the intended behavior); `preventNavigation` remains available as an opt-in per-screen unsaved-changes guard.

### Browser Security: CSP and Security Headers

The entire Content-Security-Policy is delivered as an **HTTP response header** by `SecurityHeadersFilter` (`src/main/core/org/kissweb/restServer/SecurityHeadersFilter.java`). It self-registers via a `@WebFilter("/*")` annotation (no `web.xml` entry, the same way `MCPServerBase` servlets use `@WebServlet`), so it ships entirely inside the application WAR and needs no servlet-container configuration. CSP is **not** delivered via a `<meta>` tag: the `Content-Security-Policy-Report-Only` form used during rollout is invalid in a meta tag (browsers ignore it), and a header is the single source of truth for the production/Electron deployments (all served by Tomcat).

- The XSS-relevant policy is the constant `SecurityHeadersFilter.CONTENT_SECURITY_POLICY` (`script-src 'self'`, `object-src 'none'`, `style-src 'self' 'unsafe-inline'` for CKEditor/AG-Grid, `img-src 'self' data: blob:` for `binaryCall` images, `connect-src 'self'` — pages this filter covers are always served by the back end itself, etc.). A separated production back-end must be added to `connect-src`.
- Per-deployment overrides in `application.ini` (`[main]`): `SecurityHeaders = false` disables the filter entirely (for applications whose front end predates the bootstrap kernel whose hash the CSP pins), and `CspReportOnly = true` switches to report-only. Defaults (absent keys) are enabled + enforcing.
- It **ships enforcing**: the filter's `CSP_REPORT_ONLY` flag is `false`, sending the XSS policy as `Content-Security-Policy` (violations are blocked, not merely logged). To re-validate after a policy change, set `CSP_REPORT_ONLY = true` to return to `Content-Security-Policy-Report-Only` (violations logged, nothing blocked), exercise every screen until the console is clean, then flip back. **Validate by browsing the app via the Tomcat origin (the back-end port, `http://localhost:8001` with the default port block)** — the dev static server (a prebuilt `SimpleWebServer.jar`) cannot set headers, and `file://` has no server, so those contexts receive no CSP.
- The filter always also sends the header-only protections: `Content-Security-Policy: frame-ancestors 'none'` + `X-Frame-Options: DENY` (clickjacking, enforced regardless of rollout phase), `X-Content-Type-Options: nosniff`, `Referrer-Policy: no-referrer`, and `Strict-Transport-Security` (only when `request.isSecure()`).

The CSP allows **one** inline script: the byte-stable bootstrap kernel in `index.html`, pinned by a `'sha256-…'` in `script-src` (everything else is external, served under `'self'`). Inline `<style>` is allowed via `style-src 'unsafe-inline'`. The kernel carries no per-deployment values, so its hash is stable; if it is ever edited, recompute the hash and update `SecurityHeadersFilter`.

### Cache Busting (force-refresh every file on upgrade)

Kiss refreshes **all** downloaded files on a new release from a single version number, with no server configuration (works on the dev static server, `file://`, Tomcat, and Electron). It solves the chicken-and-egg problem — the version that drives busting can't itself be served stale — by keeping the version in the only perpetually-fresh file, `index.html`.

- **Version + mode** live in `index.html`: `<meta name="app-version">` (the version of the **application**, not of the Kiss framework) and `<meta name="app-mode">` (`development` or `production`; any other value behaves as development). In `production` mode the bust token is `app-version` (stable across a release, so the browser caches everything until the version changes). In `development` mode the kernel instead uses a fresh **per-page-load timestamp** as the token, so dev caches nothing and a plain reload always re-fetches every file. The `?now` freshness redirect for `index.html` itself runs in **both** modes. These are the only per-deploy cache values. The source ships with `EDIT-1` / `development`.
- **`./bld war` auto-stamps production values** (`Tasks.stampVersion()`): it sets `app-version` to a fresh UUID, `app-mode` to `production`, and `SystemInfo.releaseDate` to the build date — but only in the WAR's staged copies (`work/exploded`), so the source keeps its `EDIT` placeholders. It edits only the meta values, never the kernel, so the CSP hash is unaffected. Every WAR therefore force-refreshes all clients with no manual version bump. **After the WAR is jarred, `war()` restores the staged `index.html` and `SystemInfo.js` from source** (`copyForce`, mirroring its `web-secure.xml`→jar→`web-unsafe.xml` swap), so `work/exploded` is never left holding production busting. Without this, a later `develop`/`start-backend` — whose `copyTree` is mtime-incremental and won't overwrite the newer stamped files — would silently run dev with production busting (a fixed `?ver` pinning stale code, plus the `?now` double load).
- A **byte-stable inline kernel** in `index.html` keeps `index.html` itself uncached via the `?now` redirect, reads the version from the meta, exposes **`window.cacheBust(url)`** (appends `?ver=<version>`), and loads `kiss/bootstrap.js` busted.
- **`bootstrap.js`** loads everything else through `bust()`: stylesheets, third-party libs (AG-Grid/CKEditor), `SystemInfo.js`, and all framework + application JS. `getScript`/`getScripts` and `Utils.getHTML` (screens) are busted; `Utils.bustHtmlResources` rewrites relative `<img src>` in loaded HTML *before* insertion (so the un-busted URL is never fetched).
- **Why it's rock-solid:** the version sits in always-fresh `index.html` and the kernel is byte-stable, so a stale cached `index.html` self-heals — its identical kernel redirects to a fresh copy and re-reads the fresh version. No cacheable file in the chain can pin a stale version.
- `bootstrap.js` mirrors the meta values onto `SystemInfo.softwareVersion` / `SystemInfo.controlCache` after loading `SystemInfo.js`, so existing references keep working.
- **`DOMUtils.fetchHTML`** (a generic `$.get` replacement) participates too: a relative URL is fetched with the standard bust token, conservatively — pre-existing query arguments are preserved (the token is appended), a `#fragment` stays in place, and URLs that are absolute (`http:`, `https:`, `//`), `data:`, `blob:`, or that already carry a `ver=` argument are left untouched. In development it also fetches with `no-store` (mirroring `Utils.getHTML`); on a page not booted through the kernel its behavior is unchanged.
- **Not covered:** `background-image:url()` inside CSS (the browser resolves those static URLs) — those refresh when their stylesheet is re-fetched, so rename a CSS-referenced asset when changing it in place. Note that version-busting is global, so every release re-downloads even unchanged third-party libs.

## Model Context Protocol (MCP)

MCP is the open protocol AI assistants use to talk to external tools and data. Kiss provides app-neutral base classes for both sides of the protocol — exposing tools to an AI assistant (server) and consuming a remote MCP server's tools (client). Both speak JSON-RPC 2.0 over HTTP (`PROTOCOL_VERSION = "2025-06-18"`), interoperate directly with each other, and share the same JSON-RPC error-code constants. Each side intentionally leaves the same features unimplemented (resources, prompts, sampling, server-initiated requests, SSE/`text/event-stream` streaming, and `Mcp-Session-Id` session management) — each can be added incrementally.

### Server (`org.kissweb.MCPServerBase`)

Base class for **serving** an MCP endpoint. Handles the JSON-RPC envelope, the `initialize` handshake, `ping`, notifications, and dispatch to `tools/list` / `tools/call`. Extends `RestServerBase`, so a subclass is registered with a `@WebServlet` annotation (no `web.xml`) and **must reside under `src/main/precompiled/`** (discovered by annotation scanning at startup; no hot reload — run `./bld -v build` and restart after edits).

Required hooks: `getServerName()`, `getServerVersion()`, `listTools()` (the tool catalog), `callTool(String, JSONObject)` (execute a tool). Optional hooks: `authenticate(...)` (default: validate an OAuth 2.1 bearer token when `OAuthAuthorizationServer` is configured, otherwise allow all), `getCapabilities()`, `dispatchExtension(...)`. Static helpers: `textBlock`, `toolResult`, `toolError`, `buildSchema`.

Tool-specific failures should be returned with `toolError(...)` (`isError: true`) — which the model sees and can react to — rather than thrown (a thrown exception becomes a JSON-RPC internal error the model does not see). Template: `com.mycompany.MCPServerExample` (`urlPatterns="/mcp-example"`).

### Client (`org.kissweb.MCPClientBase`)

Base class for **consuming** a remote MCP server — the mirror image of `MCPServerBase`. Handles the client side of the same wire protocol: the `initialize` handshake plus `notifications/initialized`, `ping`, and `tools/list` / `tools/call` requests, over HTTP via `RestClient`. Unlike the server, it is core framework code under `src/main/core/` and is used by **instantiating or subclassing it from a backend service** — there is no servlet registration.

The `initialize` handshake runs **lazily** on the first request, so callers can go straight to `listTools()` / `callTool(...)` (or call `initialize()` explicitly to inspect the server's capabilities first).

| Member | Description |
|---|---|
| `getServerUrl()` | **Required** hook — full URL of the remote MCP endpoint (per Kiss convention, read from `application.ini`) |
| `getClientName()` / `getClientVersion()` | Optional — `clientInfo` sent during initialize (defaults provided) |
| `getOAuthProviderName()` | Optional — name of an `[OAuthClient <name>]` provider to obtain bearer tokens from (default: none) |
| `applyAuthentication(JSONObject headers)` | Optional — attach auth headers per request (default: OAuth bearer when a provider is set; override for basic auth, a static API token, etc.) |
| `getClientCapabilities()` | Optional — client capabilities sent during initialize (default: none) |
| `setTimeouts(connect, request)` | Connect / request timeouts (defaults 30 s / 60 s) |
| `initialize()` | Run (once) the handshake; returns the server's `initialize` result |
| `listTools()` | Fetch the tool catalog as a `JSONArray` (auto-follows `nextCursor` pagination) |
| `callTool(name, args)` | Call a tool; returns the raw result object (inspect with `isError` / `textOf`) |
| `callToolText(name, args)` | Call a tool and return its concatenated text; throws `MCPClientException` if `isError` |
| `ping()` | Liveness check |
| `call(method, params)` / `notify(method, params)` | Generic JSON-RPC request / notification for methods without a wrapper |
| `getServerInfo()` / `getServerCapabilities()` / `getNegotiatedProtocolVersion()` | Populated by the handshake |
| `isError(result)` / `textOf(result)` (static) | Read a `tools/call` result — error flag, and `text` content blocks joined with newlines |

**Authentication is the symmetric counterpart of the server's:** the server *validates* incoming bearer tokens (resource-server role); the client *obtains* them via the existing `OAuthClient` (relying-party role). Set `getOAuthProviderName()` to a configured provider and every request carries a transparently-refreshed `Authorization: Bearer …`. When an interactive login is required, `OAuthAuthorizationRequiredException` propagates out of the call — kick off `OAuthClient.beginAuthorization(...)` rather than treat it as an error.

Failures throw `MCPClientBase.MCPClientException` (a `RuntimeException`) — JSON-RPC errors from the server, non-2xx HTTP, unparseable bodies, or transport failures; `getCode()` carries the JSON-RPC error code when present (else `0`). A *tool* failure the model is meant to see arrives instead as `isError: true` in the result, not as this exception.

Template: `com.mycompany.MCPClientExample`.

```java
import org.kissweb.MCPClientBase
import org.kissweb.restServer.MainServlet

class MyMcpClient extends MCPClientBase {
    protected String getServerUrl()         { return MainServlet.getEnvironment("RemoteMcpUrl") }
    protected String getOAuthProviderName() { return "myprovider" }   // optional
}

MyMcpClient client = new MyMcpClient()
JSONArray tools = client.listTools()                                  // runs initialize on first call
String reply = client.callToolText("echo", new JSONObject().put("message", "hi"))
```

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
  - `exists(String sql, Object... args)` - Check if records exist. Implemented as `fetchOne(sql, args) != null` so it routes through the framework's per-vendor `limit()` and works uniformly across all five supported databases. Pass a plain WHERE-clause query (do not pre-wrap with `SELECT EXISTS(...)` or pre-add `LIMIT`)
  - `fetchCount(String sql, Object... args)` - Count rows that would be returned by the given select. Wraps the query as `SELECT COUNT(*) AS n FROM (...) tmp123`; the explicit `AS n` alias and bare table alias are portable across PostgreSQL, MySQL, SQLite, SQL Server, and Oracle
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

## SQL Query Generator

The Kiss framework includes an automatic SQL query generator that determines join paths from the database schema. Given a set of tables referenced in SELECT, WHERE, ORDER BY, etc., it finds the shortest join path using BFS on foreign key relationships and produces the correct SQL.

### Architecture

Three classes in `org.kissweb.database`:

- **SchemaGraph** — models tables as nodes and FK relationships as edges; finds join paths via BFS
- **QueryBuilder** — fluent API to specify select/where/order and produce SQL
- **SchemaGraph.Edge** — represents a single or composite foreign key relationship

File locations:
```
src/main/core/org/kissweb/database/SchemaGraph.java
src/main/core/org/kissweb/database/QueryBuilder.java
src/test/core/org/kissweb/database/QueryBuilderTest.java   (108 tests)
```

### Quick Start

```java
// Build the schema graph (once at startup)
SchemaGraph graph = SchemaGraph.fromDatabase(connection);

// Optionally cache it to a file
graph.saveToFile("schema-cache.txt");

// On subsequent startups, load from cache instead
SchemaGraph graph = SchemaGraph.loadFromFile("schema-cache.txt");
```

```java
// Build and execute a query via Connection
List<Record> records = conn.newQueryBuilder()
    .select("employee.first_name", "employee.last_name")
    .select("department.name AS dept_name")
    .select("building.address")
    .where("project.project_id = ?", projectId)
    .where("employee.active = 'Y'")
    .orderBy("employee.last_name")
    .orderBy("employee.first_name")
    .fetchAll();

// Returns Kiss Record objects — use existing Record API
for (Record r : records) {
    String name = r.getString("first_name");
    String dept = r.getString("dept_name");
}
```

The system automatically generates:
```sql
SELECT employee.first_name, employee.last_name, department.name AS dept_name, building.address
FROM project
JOIN project_assignment ON project_assignment.project_id = project.project_id
JOIN employee ON project_assignment.employee_id = employee.employee_id
JOIN department ON employee.department_id = department.department_id
JOIN building ON department.building_id = building.building_id
WHERE project.project_id = ?
  AND employee.active = 'Y'
ORDER BY employee.last_name, employee.first_name
```

### SchemaGraph

`SchemaGraph` models a database schema as a graph where tables are nodes and foreign key relationships are edges. Given a set of tables, it finds the shortest join path connecting them using BFS. All table and column names are case-insensitive (stored lowercase internally). After construction, a `SchemaGraph` is thread-safe for concurrent read operations.

**Construction:**

| Method | Description |
|---|---|
| `new SchemaGraph()` | Create empty graph for programmatic population |
| `SchemaGraph.fromDatabase(Connection conn)` | Build graph from JDBC metadata (reads all tables and FKs) |
| `SchemaGraph.fromDatabase(Connection conn, String schema)` | Build or retrieve cached graph for schema (thread-safe) |
| `SchemaGraph.loadFromFile(String path)` | Load graph from a cache file |

**Populating:**

| Method | Description |
|---|---|
| `addTable(String tableName)` | Declare a table (optional — auto-created by `addForeignKey`) |
| `addForeignKey(String fromTable, String fromColumn, String toTable, String toColumn)` | Declare a single-column FK |
| `addForeignKey(String fromTable, String[] fromColumns, String toTable, String[] toColumns)` | Declare a composite (multi-column) FK |

**Querying:**

| Method | Description |
|---|---|
| `hasTable(String tableName)` | Check if a table exists in the graph |
| `getTables()` | Return all table names (`Set<String>`) |
| `getEdges(String tableName)` | Return all FK edges incident on a table |
| `findJoinPath(Set<String> tables, String rootTable)` | Find shortest join path connecting all tables via BFS |

**Caching:**

| Method | Description |
|---|---|
| `saveToFile(String path)` | Write graph to a text file |
| `loadFromFile(String path)` | Read graph from a text file |
| `clearSchemaCache()` | Clear entire in-memory schema cache |
| `clearSchemaCache(String schema)` | Remove one entry from in-memory schema cache |

Cache file format:
```
# SchemaGraph cache v1
TABLE employee
TABLE department
FK employee department_id department department_id
FK order_line order_id,product_id order_product order_id,product_id
```

**Schema Loading Strategies:**

1. **Automatic from JDBC metadata (recommended):** `SchemaGraph.fromDatabase(connection)` — Uses `DatabaseMetaData.getImportedKeys()` for each table. Works on all five supported databases.
2. **Programmatic declaration:** Create `new SchemaGraph()` and call `addForeignKey()` manually.
3. **Hybrid:** Load from database then add additional FKs programmatically.
4. **Cached:** `SchemaGraph.loadFromFile("schema-cache.txt")`

**Edge:** `SchemaGraph.Edge` represents a foreign key relationship (single or composite). Methods: `getFromTable()`, `getToTable()`, `getFromColumn()`, `getToColumn()`, `getFromColumns()`, `getToColumns()`, `isComposite()`, `buildOnCondition(fromAlias, toAlias)`.

### QueryBuilder

**Construction:**

- **From Connection (recommended):** `conn.newQueryBuilder()` — creates a QueryBuilder using the connection's schema graph
- **From Command:** `cmd.newQueryBuilder()` — for concurrent queries on the same connection
- **From explicit SchemaGraph:** `new QueryBuilder(graph)` — pass connection explicitly to execute methods

**SELECT:**

| Method | Description |
|---|---|
| `select(String tableColumn)` | Add `table.column`, `table.column AS alias`, or `FUNC(table.column)` |
| `select(String... tableColumns)` | Add multiple columns |
| `distinct()` | Enable `SELECT DISTINCT` |

**Aggregate Helpers:** `selectCount()`, `selectSum()`, `selectAvg()`, `selectMin()`, `selectMax()` — each with optional alias parameter.

**WHERE:**

| Method | Description |
|---|---|
| `where(String condition, Object... params)` | Add condition (multiple calls combined with AND) |

**OR / AND Grouping:**

| Method | Description |
|---|---|
| `startOr()` / `endOr()` | Begin/end an OR group — conditions inside are joined with OR |
| `startAnd()` / `endAnd()` | Begin/end an AND group (useful inside OR groups) |

Empty OR/AND groups are silently omitted. Example:
```java
conn.newQueryBuilder()
    .select("employee.first_name", "employee.last_name")
    .startOr()
        .where("employee.department_id = ?", 10)
        .where("employee.department_id = ?", 20)
    .endOr()
    .where("employee.active = 'Y'")
    .fetchAll();
// WHERE (employee.department_id = ? OR employee.department_id = ?) AND employee.active = 'Y'
```

**Subqueries:** `whereIn()`, `whereNotIn()`, `whereExists()`, `whereNotExists()` — each accepting either a QueryBuilder subquery or raw SQL with parameters.

**ORDER BY:** `orderBy(tableColumn)` (ascending), `orderByDesc(tableColumn)` (descending).

**GROUP BY / HAVING:** `groupBy(tableColumn)`, `having(condition, params...)`.

**Explicit Joins:** `join()`, `leftJoin()`, `rightJoin()` — each with variants for single-column, composite, and aliased joins. Explicit join target tables are excluded from automatic join resolution. Required for self-joins and choosing a specific FK path when multiple exist.

Self-join example:
```java
conn.newQueryBuilder()
    .select("employee.first_name")
    .select("mgr.first_name AS manager_name")
    .join("employee", "manager_id", "employee", "employee_id", "mgr")
    .fetchAll();
```

**CTEs (Common Table Expressions):** `with(name, rawSQL, params...)` or `with(name, QueryBuilder)`. CTE parameters appear before WHERE parameters in the final parameter list.

**UNION:** `union(QueryBuilder)` (removes duplicates), `unionAll(QueryBuilder)` (keeps duplicates). ORDER BY and LIMIT on a UNION apply to the combined result.

**LIMIT:** `limit(int max)` — database-adapted via `Connection.limit()`.

**Build & Execute:** All execution methods come in three forms (no-arg using stored connection/command, explicit Connection, explicit Command):

| Method | Description |
|---|---|
| `build()` | Generate SQL string; populates `getParameters()` |
| `getParameters()` | Ordered `?` placeholder values (call after `build()`) |
| `fetchAll()` | Execute and return `List<Record>` |
| `fetchOne()` | Execute and return first `Record` or null |
| `fetchAllJSON()` | Execute and return `JSONArray` |
| `query()` | Execute and return `Cursor` |

### Connection and Command Integration

`Connection.newQueryBuilder()` creates a QueryBuilder using the connection's lazily-built schema graph. `Command.newQueryBuilder()` creates a QueryBuilder bound to a specific command for concurrent query scenarios.

**When to use Connection vs Command:**
- Use `conn.newQueryBuilder()` for isolated, one-shot queries (the common case)
- Use `cmd.newQueryBuilder()` when iterating over one query's results while executing another on the same connection

### How Join Path Finding Works

1. Collect all distinct tables mentioned in the query (from SELECT, WHERE, ORDER BY, GROUP BY, HAVING)
2. Pick the root table (first WHERE table, or first SELECT table)
3. For each remaining table, run BFS from already-connected tables to find shortest path
4. Merge overlapping paths. If any table is unreachable, throw `SQLException`

Auto-discovered joins are always `INNER JOIN`. Explicit joins use the specified type.

### Edge Cases and Limitations

- Tables with no FK path: `build()` throws `SQLException`
- Composite foreign keys: fully supported
- Self-joins: supported via explicit `join()` with alias
- Multiple FK paths between two tables: auto-join picks first found; use explicit `join()` to override
- Schema-qualified table names: supported
- FULL OUTER JOIN and INTERSECT/EXCEPT: not supported; use raw SQL

### Performance

- **SchemaGraph from DB:** ~1–3 seconds for 300 tables (one-time cost)
- **SchemaGraph from cache:** sub-millisecond
- **Path finding:** sub-millisecond for 300 nodes
- **Memory:** <1 MB for 300 tables with ~500 FK relationships
- **Thread safety:** SchemaGraph is safe for concurrent reads after construction

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

### Component Documentation
The file `Kiss/src/main/frontend/kiss/component/components.js` serves as the JSDoc documentation for all components under the `Kiss/src/main/frontend/kiss/component/` directory. Each component's API (methods, attributes, events) is documented there. When a component implementation is changed, `components.js` must be updated to reflect the change.

### Custom HTML Tags
The framework provides custom HTML components that should be used:
- `<text-input>` - Text input field
- `<drop-down>` - Dropdown select
- `<combo-box>` - Editable dropdown (Windows-style combobox; pick from the list or type free text)
- `<push-button>` - Button element
- `<popup>` - Modal dialog
- `<popup-title>` - Popup header
- `<popup-body>` - Popup content area
- `<text-label>` - Text label
- `<date-input>` - Date picker
- `<time-input>` - Time picker
- `<numeric-input>` - Number input
- `<check-box>` - Checkbox control
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

### DOM Access Rules

**CRITICAL: DOM access is strictly layered in the Kiss framework:**

1. **Only `DOMUtils.js` is allowed to access the HTML DOM directly** - All direct DOM manipulation (e.g., `element.style`, `element.setAttribute`, `document.getElementById`, etc.) must be done through DOMUtils.
2. **In general, only front-end files under the kiss directory should use DOMUtils.js** - Application screens and other code should use the Kiss component APIs, not DOMUtils directly.  If an exception arises yu must get explicit approval
3. **Existing DOMUtils functions must not be changed without explicit approval** - New functions may be added, but existing functions should remain stable to avoid breaking dependent code.

This layered architecture ensures:
- Consistent DOM manipulation across the framework and application code
- Easier maintenance and bug fixes (changes to DOM handling only need to be made in one place)
- Proper abstraction between application code and low-level DOM operations

### File Restrictions
- **DO NOT MODIFY** files under:
  - `src/main/frontend/kiss/` - Framework components
  - `src/main/core/` - Core framework code
- **SAFE TO MODIFY**:
  - `src/main/backend/` - Application services and business logic
  - `src/main/frontend/` - Application screens (except kiss/ subdirectory)
  - `src/main/precompiled/` - Shared utility classes

### Coding Style
- **If-statement bodies must never appear on the same line as the condition.** Always place the body on the next line (or in a block on the next line). This applies to all Java and Groovy code.
  ```java
  // WRONG
  if (x == null) return;
  if (c == '(') depth++;

  // CORRECT
  if (x == null)
      return;
  if (c == '(')
      depth++;
  // Also correct
  if (x == null) {
      return;
  }
  ```

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
- `format()` - Format as "MM/dd/yyyy h:mm a" (instance, hard-coded pattern)
- `format(String pattern)` - Format using caller-supplied pattern, e.g. `new DateTime().format("MM/dd/yyyy h:mm:ss a")`
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

### NumberFormat (org.kissweb.NumberFormat)

Utility class (private constructor, all static methods) for advanced numeric formatting. Converts doubles into formatted strings with fine-grained control over base, width, decimal places, and display options.

#### Key Methods

**`Formatb(double num, int base, String msk, int wth, int dp)`** - Full-featured formatter supporting any numeric base (2-36).
- `num` - the number to format
- `base` - numeric base (2=binary, 8=octal, 10=decimal, 16=hex, etc.)
- `msk` - format mask string combining any of these flags:
  - `B` = blank if zero (returns spaces)
  - `C` = add commas (or grouping separators)
  - `L` = left justify
  - `P` = parentheses around negative numbers (accounting style)
  - `Z` = zero fill
  - `D` = floating dollar sign
  - `U` = uppercase letters (for hex digits etc.)
  - `R` = append percent sign
- `wth` - total field width (0 = auto-size)
- `dp` - decimal places (-1 = auto-detect)
- Returns asterisks (`***`) if the number cannot fit in the specified width

**`Format(double num, String msk, int wth, int dp)`** - Convenience wrapper for base-10 formatting. Equivalent to `Formatb` with `base=10`.

**`FormatVWF(double n, String fmt, int wth, int dec)`** - Variable Width Format. Formats the number and adds extra spaces to account for comma positions, enabling visual alignment of columns in reports where some values have commas and others do not.

#### Usage Examples
```java
// Currency with commas, parentheses for negatives, dollar sign, 12-char width, 2 decimals
NumberFormat.Format(-12345.348, "CDP", 12, 2);   // "($12,345.35)"

// Simple comma and dollar sign formatting
NumberFormat.Format(12345.146, "CD", 10, 2);     // "$12,345.15"

// Left-justified with dollar sign
NumberFormat.Format(125.146, "CDL", 10, 2);      // "$125.15   "

// Auto width and auto decimal places
NumberFormat.Format(1236354545.146, "CD", 0, -1);

// Zero-filled with commas and parentheses for negatives
NumberFormat.Format(-5.146, "ZCP", 10, 2);       // "(00005.15)"
```

#### Key Behaviors
- Rounds the number to the specified decimal places before formatting
- If the formatted number exceeds the field width, it progressively drops formatting elements in order: leading zero, dollar sign, commas, parentheses, percent sign. If it still does not fit, returns asterisks (`***`)
- Supports numeric bases 2 through 36 using digits 0-9 and letters a-z
- `FormatVWF` doubles spaces and adds leading spaces equal to the comma count, enabling consistent visual alignment in tabular or report output

## Logging

Kiss uses **Log4j 2.x** for all logging. The framework uses the modern log4j 2.x API throughout.

### Required Imports
```java
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
```

### Logger Declaration
```java
private static final Logger logger = LogManager.getLogger(YourClass.class);
```

### Logging Methods
```java
logger.trace("Trace message");
logger.debug("Debug message");
logger.info("Info message");
logger.warn("Warning message");
logger.error("Error message");
logger.error("Error with exception", exception);
logger.fatal("Fatal message");
```

### Setting Log Level Programmatically
To change log levels at runtime, use `Configurator`:
```java
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.config.Configurator;

// Set level for a specific logger
Configurator.setLevel(logger, Level.ALL);
Configurator.setLevel(logger, Level.DEBUG);
Configurator.setLevel(logger, Level.INFO);
```

### Log4j JARs
The framework includes:
- `log4j-api-2.25.3.jar` - Log4j 2.x API
- `log4j-core-2.25.3.jar` - Log4j 2.x implementation

**Note:** Do NOT use the old log4j 1.x API (`org.apache.log4j.*`) or Java util logging (`java.util.logging.*`).

## Browser Back Button and Reload Prevention

The Kiss framework provides `DOMUtils.preventNavigation()` to prevent accidental browser back button usage and page reload/close.

### DOMUtils.preventNavigation(isActive, onBack)

**Parameters:**
- `isActive` (Function) - Returns `true` when navigation protection should be active
- `onBack` (Function, optional) - Called when the back button is pressed while active

**What it does:**
- Pushes a history entry and adds a `popstate` listener to intercept the browser back button. When triggered and `isActive()` returns true, pushes a new state to maintain protection and calls `onBack`.
- Uses `beforeunload` listener to show the browser's built-in "Leave site?" warning on page reload or close when `isActive()` returns true.

**Chrome user activation limitation:** Chrome blocks all script-based navigation prevention (pushState in popstate handlers, beforeunload dialogs, and Navigation API intercept) until the page has received a "user gesture" (click, keypress, or touch). This is enforced at the browser engine level and cannot be bypassed by any JavaScript technique. On a brand new tab before any interaction, the back button and reload cannot be prevented. After the user's first click or keypress anywhere on the page, full protection activates.

**Usage:**
```javascript
DOMUtils.preventNavigation(
    function () { return true; },
    function () {
        Utils.yesNo('Confirm', 'Are you sure you want to logout?', function () {
            Server.logout();
        });
    }
);
```

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
- Uses the Pointer Events API (`pointerdown`/`pointermove`/`pointerup`/`pointercancel`) so mouse, touch, and pen input are all handled by a single code path
- Calls `header.setPointerCapture(e.pointerId)` on `pointerdown` so all subsequent move/up events are delivered to the header element even when the pointer crosses into a child iframe; without capture, crossing into an iframe causes the parent document to lose mouse events and the drag "sticks" after the button is released
- `setPointerCapture` is guarded (`if (header.setPointerCapture)`) for safety, though all target browsers (Chrome, Firefox, Safari, Edge) support it
- Sets `header.style.touchAction = 'none'` to suppress browser pan/scroll gestures while dragging on touch devices (replaces the old `touchstart` `e.preventDefault()` approach)
- Sets cursor style to 'all-scroll' on header
- Stores per-drag handler references and removes them in `endDrag` (called on both `pointerup` and `pointercancel`) to prevent listener accumulation across repeated drags
- Calls `header.releasePointerCapture(e.pointerId)` in `endDrag` to cleanly release capture
- Guards `isPrimary` so multi-touch sequences (second finger down) do not start a second drag

## Script Loading Order

The boot chain is: the inline **kernel** in `index.html` → `kiss/bootstrap.js` → `loadUtils()`. The kernel only ensures freshness and loads `bootstrap.js` (see [Cache Busting](#cache-busting-force-refresh-every-file-on-upgrade)); `bootstrap.js` does the actual ordered loading and defines the global loaders:
- `getScript(url)` - Loads a single script file (version-busted; returns a Promise)
- `getScripts(urls)` - Loads multiple scripts **in parallel** via `Promise.all()`

### Correct Loading Pattern (`loadUtils` in `bootstrap.js`)

```javascript
async function loadUtils() {
    addStylesheet("normalize.css"); ...                 // stylesheets (busted)
    await getScripts(["lib/ag-grid-...js", "lib/ckeditor.js"]); // libs BEFORE AGGrid.js/Editor.js
    await getScript("SystemInfo.js");                   // config (needed by AppState.init)
    SystemInfo.softwareVersion = window.KissVersion;    // mirror meta values for back-compat
    SystemInfo.controlCache    = window.KissCacheOn;
    await getScript("kiss/DOMUtils.js");                // DOMUtils BEFORE Utils.js
    await getScript("kiss/AppState.js");                // AppState BEFORE Server.js uses it
    AppState.init();
    await getScripts(["kiss/Utils.js", ..., "kiss/Server.js", "kiss/Router.js", "kiss/AGGrid.js", "kiss/Editor.js", ...]);
    await getScript("routes.js");                       // routes BEFORE index.js calls Router.start()
    getScript("index.js");
}
```

**Ordering constraints that matter:**
- `getScripts()` loads in parallel, not sequentially — anything with a load-time dependency must be in an earlier `await`.
- **DOMUtils before Utils.js** — Utils.js checks for the DOMUtils object at load time; otherwise "DOMUtils object not found".
- **Third-party libs before `AGGrid.js`/`Editor.js`** (the kiss wrappers reference the lib globals).
- **`SystemInfo.js` before `AppState.init()`** (reads `SystemInfo.stateStore`); **`AppState.js` before `Server.js`** (the `uuid` getter); **`routes.js` before `index.js`** (`Router.start()`).

## Login Form Structure

To prevent browser warnings about password fields not being in forms, login pages must wrap input fields in a `<form>` tag:

**Pattern:**
```html
<form onsubmit="return false;" autocomplete="on">
    <text-input id="username" required autocomplete="username"></text-input>
    <text-input id="password" required password autocomplete="current-password"></text-input>
    <push-button id="login">Login</push-button>
</form>
```

**Key Points:**
- `onsubmit="return false;"` prevents actual form submission (login is handled via JavaScript)
- `autocomplete="on"` on form enables browser password manager
- `autocomplete="username"` on username field helps password managers identify the username
- `autocomplete="current-password"` on password field for existing credentials
- The TextInput component passes through the `autocomplete` attribute to the underlying `<input>` element

**Files with login forms:**
- `/src/main/frontend/login.html` - Desktop login page
- `/src/main/frontend/mobile/login.html` - Mobile login page

## Popup Structure Requirements

The `Utils.popup_open()` function requires popups to have exactly TWO direct children:
1. `<popup-title>` - Header element containing the popup title
2. `<popup-body>` - Body element containing the popup content

### Correct Structure
```html
<popup id="my-popup">
    <popup-title>Popup Title</popup-title>
    <popup-body>
        <!-- popup content here -->
    </popup-body>
</popup>
```

### Common Mistake (Causes Errors)
```html
<!-- WRONG: Single wrapper div causes "Cannot set properties of undefined" error -->
<popup id="my-popup">
    <div class="popup-content">
        <!-- This single wrapper div breaks the popup -->
    </div>
</popup>
```

### Usage
```javascript
Utils.popup_open('my-popup');   // Open popup
Utils.popup_close('my-popup');  // Close popup
```

### Popup Control Behavior After Close

**IMPORTANT: Popup controls remain valid after the popup is closed.**

When `Utils.popup_close()` is called, the popup is hidden but the DOM elements are not destroyed. This means:

- All controls and DOM elements within the popup remain accessible and valid
- Values can be read from popup controls even after the popup has been closed
- Control references (e.g., `$$('popup-control-id')`) continue to work
- The popup's DOM structure is preserved in the document

**Example:**
```javascript
// Open popup and let user interact
Utils.popup_open('my-popup');

// Later, close the popup
Utils.popup_close('my-popup');

// Controls are still accessible - values can be retrieved
const selectedValue = $$('popup-select').getValue();  // This works!
const inputText = $$('popup-input').getValue();        // This works too!
```

This behavior is useful for:
- Reading user selections after closing a popup
- Preserving state between popup open/close cycles
- Avoiding unnecessary re-initialization of controls

## DateTime Formatting

### Backend DateTime Formats
The backend may return datetime values in different formats:
- **Epoch milliseconds**: Large numbers like 1737292576000 (milliseconds since Jan 1, 1970)
- **Date integer**: YYYYMMDD format (e.g., 20260119)
- **Time integer**: HHMM format (e.g., 1430 for 2:30 PM)
- **DateTime integer**: YYYYMMDDHHMM format (e.g., 202601191430)

### Correct Approach
Always use `DateTimeUtils.formatDate()` to format datetime values:
```javascript
const formattedDateTime = DateTimeUtils.formatDate(msg.dateTime);
```

This method automatically detects if input is epoch milliseconds and formats according to user's locale preference.

### Common Mistake (Causes Wrong Dates)
```javascript
// WRONG: Manual substring parsing assumes specific format
// Produces garbage like "49/12/1768" when given epoch milliseconds
const dateTimeStr = msg.dateTime.toString();
const year = dateTimeStr.substring(0, 4);  // Don't do this!
```

## Input Control Styling

### Avoid width: 100% on Input Controls
Do not use `width: 100%` on input controls (textbox-input, text-input, etc.). This can cause layout issues. Use specific pixel widths or other sizing approaches instead.

```html
<!-- WRONG -->
<textbox-input style="width: 100%;"></textbox-input>

<!-- CORRECT -->
<textbox-input style="width: 300px;"></textbox-input>
```

## Known Issues & Solutions

### Empty `application.ini` values must be quoted (`Key = ""`, not `Key =`)

`MainServlet.getEnvironment` is backed by a `java.util.Hashtable`, which throws
`NullPointerException` on a null value. `IniFile.parse` calls `unquote` on each value,
and `unquote("")` returns **null** (an unquoted empty string), whereas `unquote("\"\"")`
returns an empty string. So a bare `Key =` line yields a null that `MainServlet.readIniFile`
tries to `Hashtable.put`, throwing an NPE that aborts `KissInit.init()` — after which the
database is never configured and every service receives a null `Connection` (`db`).

Symptom: at startup `* * * Error executing KissInit.groovy` with a bare
`java.lang.NullPointerException`, and services fail with `Cannot invoke method ... on null
object` (db is null) even though logins appear to "succeed" (the no-database fallback
accepts any credentials).

Fix: always give empty config keys an explicit empty-quoted value:
```ini
ServicePassword = ""    # correct
# ServicePassword =     # WRONG: parses to null -> Hashtable NPE
```

---

*Last Updated: 2026-07-12*
