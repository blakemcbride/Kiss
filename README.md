[![](Kiss-logo.svg)](https://kissweb.org)

# KISS Web Application Full Stack Framework

The KISS Framework is a Java-based, full-stack application development framework for
developing web-based business applications. KISS can also be used to
build command-line utilities, and, in conjunction with
[Electron](https://electronjs.org), desktop applications that are
portable to Windows, macOS, and Linux.

This project is in full release status (not a beta) and is used in production. Visit [kissweb.org](https://kissweb.org) for more details, or browse the source at [github.com/blakemcbride/Kiss](https://github.com/blakemcbride/Kiss).

Public discussion and support is available at [Kiss Support](https://github.com/blakemcbride/Kiss/discussions).

## Quick Start

Presuming you have the Java JDK (tested with Java 17, 21, 25), GIT, and an
Internet connection, you can run the following commands to download, install,
configure, and run Kiss, tomcat, and the required JAR files:

Be sure the JAVA_HOME and JRE_HOME environment variables are set correctly!

### Linux, macOS, BSD, etc.

    git clone https://github.com/blakemcbride/Kiss.git
    cd Kiss
    ./bld -v develop

### Windows

    git clone https://github.com/blakemcbride/Kiss.git
    cd Kiss
    bld -v develop

In either environment, you can then go to `http://localhost:8000`
within your browser to use the system.  After that, both the front-end
and the back-end code can be changed while the system is running. No need for
additional compiles or deploys.

## Integrations

In addition to enabling the rapid development of web and desktop applications,
KISS also provides the following integrations with external systems.

### Integrating with External Systems

* **REST Client** — make HTTP calls out to third-party APIs.
* **REST Server** — accept inbound HTTP calls from external systems and webhooks.

### LLM Integrations

* **LLM Providers** — built-in support for OpenAI, Anthropic, and Ollama.
* **MCP Server** — expose application tools to AI assistants via the Model Context Protocol.
* **MCP Client** — consume tools from a remote MCP server, with optional OAuth 2.1 authentication.
* **Embeddings Database** — vector storage for retrieval-augmented generation (RAG).

### Security

* **OAuth 2.1 Resource Server** — validate bearer tokens from any OAuth 2.1 / OpenID Connect authorization server (Auth0, Okta, Keycloak, etc.); MCP servers are automatically protected when configured.  See [OAuth.md](OAuth.md).
* **OAuth 2.1 Authorization Server** — issue tokens to MCP clients (or any OAuth client) directly from Kiss: authorization endpoint with PKCE, token endpoint with refresh-token rotation, dynamic client registration (RFC 7591), and the RFC 8414 / JWKS discovery endpoints.  Persists keys, clients, and refresh tokens to a private SQLite database independent of the application's main database — no shared schema or operator setup required.  See [OAuth.md](OAuth.md).
* **OAuth 2.1 Client** — consume a remote OAuth 2.1-protected server from Kiss: drives the authorization-code + PKCE flow, with automatic server discovery (RFC 9728 / RFC 8414), dynamic client registration when needed, a built-in redirect callback, and transparent access-token refresh with rotation.  Supports multiple remote providers configured in `application.ini`, and shares the authorization server's SQLite database.  See [OAuth.md](OAuth.md).

## Front-End

The KISS front-end is a single-page-application framework that includes:

* **Client-Side Routing** — every screen has its own deep-linkable, bookmarkable URL, with working browser Back/Forward navigation.  Routing is hash-based, so it needs no server-side URL rewriting and behaves identically on the development server, `file://`, Tomcat, and Electron.
* **Unified State Persistence** — a single client-side store keeps front-end state (including the session token) across navigation and reload, per browser tab by default.
* **Automatic Cache-Busting** — a single version bump force-refreshes every downloaded file on a new release, with no server configuration.
* **Browser Security (CSP)** — a Content-Security-Policy and the standard security headers are delivered by a self-registering filter that ships inside the WAR, requiring no servlet-container configuration.

## Training Videos

A 6-part training series on KISS is at:

* [Part 1 - Introduction](https://youtu.be/FAnL7dpMld4)
* [Part 2 - Setup & Configuration](https://youtu.be/xT-C-yQo0Ec)
* [Part 3 - Web Services](https://youtu.be/9zRZcxMjoW0)
* [Part 4 - Front-end](https://youtu.be/zMjrp-ft_Tc)
* [Part 5 - Data Persistence](https://youtu.be/pS7DezhYpGo)
* [Part 6 - Deployment](https://youtu.be/fGEzv7uuJCk)

## Documentation

* [Getting Started Manual (HTML)](https://blakemcbride.github.io/Kiss/manual/man) — also available as a [PDF](https://blakemcbride.us/software/kiss/GettingStarted.pdf).
* [Front-end API (JSDoc)](https://blakemcbride.github.io/Kiss/manual/jsdoc)
* [Kiss: A Complete Guide to the Web Application Framework](https://a.co/d/035V1VEl) — a comprehensive book by Blake McBride, available on Amazon.

The back-end JavaDoc must be built by you.  This can be done by typing:

    ./bld javadoc               [Linux, macOS, BSD, etc.]
        -or-
    bld javadoc                 [Windows]

You then get:  `work/javadoc/index.html`

## Support

Commercial support is available from [blake@mcbridemail.com](mailto:blake@mcbridemail.com)

Please help fund this project at [https://www.gofundme.com/kissweb](https://www.gofundme.com/kissweb)
