# OAuth 2.1 Support in Kiss

Kiss ships with all three OAuth 2.1 roles: a **resource server** that validates incoming bearer tokens, an **authorization server** that issues them, and a **client** (relying party) that obtains tokens from a remote server and presents them on outbound calls.  The roles run independently or together — a Kiss-based MCP server typically runs the resource and authorization servers in the same JVM, validating its own tokens, while the client role lets a Kiss app consume *other* OAuth-protected servers.

This document is a reference for a developer already familiar with OAuth 2.1.  For a slower, worked-example treatment, see the chapters on OAuth in the Kiss book.

> **Resource server** — Part 1, validates tokens issued by any RFC 8414 / OpenID Connect compliant authorization server (Auth0, Okta, Keycloak, Entra, Google, or your own Kiss-hosted AS).
>
> **Authorization server** — Part 2, runs the authorization-code flow with mandatory PKCE, the token endpoint with refresh-token rotation, dynamic client registration, the discovery endpoint, and the JWKS endpoint.  Persists all long-lived state to a private SQLite database that is independent of the application's main database — no shared schema, no shared connection pool, no operator setup beyond pointing at a file path.
>
> **Client** — Part 3, drives the authorization-code + PKCE flow *against a remote* OAuth 2.1 server, performing discovery and (when needed) dynamic client registration, capturing the redirect at a built-in callback servlet, and refreshing access tokens automatically.  Supports multiple remote providers, each configured in `application.ini`.  Persists its cached registrations and tokens in the **same** `oauth.db` the authorization server uses.

---

## Table of Contents

**Part 1 — Resource Server (validating tokens)**
- [What It Does](#what-it-does)
- [What It Does Not Do](#what-it-does-not-do)
- [Configuration](#configuration)
- [Automatic Enforcement for MCP Servers](#automatic-enforcement-for-mcp-servers)
- [Accessing the Validated Token](#accessing-the-validated-token)
- [Discovery Endpoint](#discovery-endpoint)
- [Customizing Authentication](#customizing-authentication)
- [Using OAuth from Non-MCP Servlets](#using-oauth-from-non-mcp-servlets)
- [JWKS Caching and Key Rotation](#jwks-caching-and-key-rotation)
- [Security Considerations](#security-considerations)
- [Troubleshooting](#troubleshooting)
- [API Reference](#api-reference)

**Part 2 — Authorization Server (issuing tokens)**
- [AS Overview](#as-overview)
- [AS Configuration](#as-configuration)
- [Required App Code: UserAuthenticator](#required-app-code-userauthenticator)
- [Optional App Code: ConsentProvider and HTML Overrides](#optional-app-code-consentprovider-and-html-overrides)
- [Endpoints](#endpoints)
- [The Authorization Code Flow End-to-End](#the-authorization-code-flow-end-to-end)
- [Dynamic Client Registration](#dynamic-client-registration)
- [Refresh-Token Rotation and Reuse Detection](#refresh-token-rotation-and-reuse-detection)
- [Persistence Model (oauth.db)](#persistence-model-oauthdb)
- [AS Security Considerations](#as-security-considerations)
- [AS Limitations and Omissions](#as-limitations-and-omissions)
- [AS API Reference](#as-api-reference)

**Part 3 — Client (obtaining tokens from a remote server)**
- [Client Overview](#client-overview)
- [Client Configuration](#client-configuration)
- [Using the Client from App Code](#using-the-client-from-app-code)
- [The Client Flow End-to-End](#the-client-flow-end-to-end)
- [Token Refresh and Rotation](#token-refresh-and-rotation)
- [Client Persistence (shared oauth.db)](#client-persistence-shared-oauthdb)
- [Client Security Considerations](#client-security-considerations)
- [Client Limitations](#client-limitations)
- [Client API Reference](#client-api-reference)

---

# Part 1 — Resource Server

## What It Does

When you configure an authorization-server URL in `application.ini`, the framework:

1. **Validates incoming bearer tokens** on every request to any `MCPServerBase` subclass. The validator parses `Authorization: Bearer <jwt>`, verifies the JWS signature against the authorization server's published JWKS, and checks the `iss`, `aud`, `exp`, `nbf`, and `scope` claims.
2. **Publishes a discovery document** at `/.well-known/oauth-protected-resource` per [RFC 9728](https://datatracker.ietf.org/doc/rfc9728/), so MCP clients can bootstrap their authentication flow.
3. **Returns proper challenges** on failure — HTTP 401 with `WWW-Authenticate: Bearer ...` for missing or invalid tokens, HTTP 403 with `error="insufficient_scope"` for valid tokens lacking required scopes — following [RFC 6750](https://datatracker.ietf.org/doc/rfc6750/).
4. **Surfaces the validated subject and scopes** to tool methods via a thread-local accessor, so authorization decisions can be made inside service logic.

All of this is enabled by setting a single `application.ini` key. No code change is required to protect an MCP server.

## What It Does Not Do

The framework implements the **resource-server role only**. The following responsibilities belong to a dedicated authorization server (Auth0, Keycloak, Okta, etc.) and are deliberately out of scope:

- Issuing tokens (token endpoint, refresh tokens)
- User authentication and consent UI
- Dynamic client registration (RFC 7591)
- Token revocation and introspection endpoints
- The JWKS publishing endpoint (you publish your keys on the authorization server, not the resource server)

If you need an authorization server, run one of the established open-source options (Keycloak, Hydra, Authentik) alongside Kiss and configure Kiss to point at it.

The framework also currently supports only **RSA** signing algorithms (RS256, RS384, RS512). ECDSA (ES256/ES384/ES512), EdDSA (EdDSA/Ed25519), and HMAC (HS*) algorithms are not yet supported. RS256 covers the vast majority of OAuth deployments, including all major commercial identity providers.

---

## Configuration

All settings live in `src/main/backend/application.ini` and are read once at startup via `MainServlet.getEnvironment()`.  Only one key contains a URL; the rest default to that value or to discovery against it, so for the common single-Kiss-app AS+RS deployment the URL appears only once in the file:

```ini
# Required to enable OAuth — issuer URL of the authorization server.
OAuthAuthorizationServer = https://myapp.example.com

# Optional.  Canonical URL of this resource server (the "aud" value
# tokens must carry).  Defaults to OAuthAuthorizationServer, which is
# correct when this Kiss app is both AS and RS.  Set this explicitly
# only when the RS is hosted at a URL different from the AS.
# OAuthResourceIdentifier = https://myapp.example.com

# Optional.  Comma- or space-separated scopes every token must carry.
# Tokens missing any of these scopes get HTTP 403 with error="insufficient_scope".
# OAuthRequiredScopes = mcp:read mcp:write

# Optional.  Explicit JWKS URL.  If omitted, the framework discovers
# it from <OAuthAuthorizationServer>/.well-known/oauth-authorization-server
# (RFC 8414, the OAuth 2.1 standard, which Kiss's own AS publishes),
# falling back to <OAuthAuthorizationServer>/.well-known/openid-configuration
# for OIDC-only authorization servers.  Set this only when neither
# metadata document is published.
# OAuthJwksUri = https://myapp.example.com/.well-known/jwks.json

# Optional.  How long to cache JWKS keys before refetching, in seconds.
# Default: 3600.
# OAuthJwksCacheSeconds = 3600

# Optional.  Comma-separated list of acceptable JWS algorithms.
# Default: RS256.  Supported: RS256, RS384, RS512.
# OAuthAllowedAlgorithms = RS256

# Optional.  Allowed clock skew when validating exp/nbf, in seconds.
# Default: 60.
# OAuthClockSkewSeconds = 60
```

If `OAuthAuthorizationServer` is **not** set, the OAuth subsystem is inert: the discovery endpoint returns 404, MCP servers accept all requests as before, and no JWKS fetch is ever attempted. There is zero runtime cost for applications that don't use OAuth.

---

## Automatic Enforcement for MCP Servers

When OAuth is configured, **every `MCPServerBase` subclass automatically requires a valid bearer token on every request.** No code change to the MCP servlet is needed — the default `authenticate()` method in `MCPServerBase` consults `OAuthConfig` and, if enabled, calls `BearerTokenValidator.requireBearerToken()`.

This is by design. MCP is OAuth 2.1's primary use case, and the MCP spec mandates bearer-token authentication; making developers write the same one-line override in every MCP servlet would be friction without benefit.

**Behavior at a glance:**

| Request situation                                  | Response                                                            |
|----------------------------------------------------|---------------------------------------------------------------------|
| No `Authorization` header                          | 401 with bare `WWW-Authenticate: Bearer realm=..., resource_metadata=...` |
| `Authorization` uses a non-Bearer scheme           | 401 with bare challenge (per RFC 6750 §3.1)                         |
| Malformed JWT, bad signature, expired, etc.        | 401 with `error="invalid_token", error_description="..."`           |
| Valid JWT missing a scope listed in `OAuthRequiredScopes` | 403 with `error="insufficient_scope"`                               |
| Valid JWT with all required scopes                 | Request proceeds; token is available via `BearerTokenValidator.currentToken()` |

The `resource_metadata` parameter in the `WWW-Authenticate` header points the client at the discovery endpoint so it knows where to obtain a token.

---

## Accessing the Validated Token

Inside any tool method (or anywhere downstream of `authenticate()`), you can read the validated token via a static accessor:

```java
import org.kissweb.oauth.BearerTokenValidator;
import org.kissweb.oauth.ValidatedToken;

@Override
protected JSONObject callTool(String name, JSONObject args) {
    ValidatedToken tok = BearerTokenValidator.currentToken();
    String userId = tok.getSubject();        // "sub" claim
    String email  = tok.getClaimString("email");

    if (!tok.hasScope("mcp:write"))
        return toolError("This tool requires the mcp:write scope.");

    // ... your tool logic ...
}
```

The `ValidatedToken` is held in a `ThreadLocal` for the duration of the request and is cleared automatically in `MCPServerBase.doPost`'s `finally` block — there is no token leakage onto subsequent requests reusing the same servlet-container thread.

**`ValidatedToken` exposes:**

| Method                          | Description                                              |
|---------------------------------|----------------------------------------------------------|
| `getSubject()`                  | The `sub` claim (the principal the token represents)     |
| `getIssuer()`                   | The `iss` claim                                          |
| `getAudience()`                 | The `aud` claim values, as a `Set<String>`               |
| `getScopes()`                   | Granted scopes from either `scope` (space-separated) or `scp` (array) |
| `hasScope(String)`              | Convenience scope check                                  |
| `getExpiresAtEpochSeconds()`    | `exp` claim                                              |
| `getIssuedAtEpochSeconds()`     | `iat` claim                                              |
| `getClaimString(String)`        | Provider-specific string claim (e.g. `email`, `preferred_username`) |
| `getClaims()`                   | The full verified claim set as a `JSONObject`            |

---

## Discovery Endpoint

The framework auto-registers `ProtectedResourceMetadataServlet` at `/.well-known/oauth-protected-resource` via `@WebServlet` annotation scanning. When OAuth is enabled it returns the RFC 9728 metadata document:

```json
{
  "resource": "https://myapp.example.com",
  "authorization_servers": ["https://myapp.example.com"],
  "bearer_methods_supported": ["header"],
  "scopes_supported": ["mcp:read", "mcp:write"],
  "resource_signing_alg_values_supported": ["RS256"]
}
```

(In a split deployment where the RS and AS live at different URLs, `resource` reflects this server's `OAuthResourceIdentifier` and `authorization_servers` reflects `OAuthAuthorizationServer`.)

MCP clients call this endpoint when they receive a 401 with `WWW-Authenticate: Bearer ..., resource_metadata="..."` and use the returned `authorization_servers` value to start an OAuth flow.

When OAuth is **not** configured, the endpoint returns HTTP 404 — the same response any non-OAuth resource would give, signaling to clients that this resource is not protected by OAuth.

---

## Customizing Authentication

The default OAuth-when-configured behavior is fine for almost all cases. Override `authenticate()` only if you need something different:

### Make an MCP server explicitly public despite OAuth being configured globally

```java
@Override
protected boolean authenticate(HttpServletRequest req, HttpServletResponse resp) {
    return true;  // public endpoint
}
```

### Use a different scheme (e.g. HTTP basic auth)

```java
@Override
protected boolean authenticate(HttpServletRequest req, HttpServletResponse resp) throws IOException {
    return basicAuthenticate(req, resp, "MyRealm", "user", "secret");
}
```

### Layer extra checks on top of the OAuth default

```java
@Override
protected boolean authenticate(HttpServletRequest req, HttpServletResponse resp) throws IOException {
    if (!super.authenticate(req, resp))
        return false;
    ValidatedToken tok = BearerTokenValidator.currentToken();
    if (!tok.getClaimString("tenant").equals(expectedTenant(req)))
        return false;
    return true;
}
```

---

## Using OAuth from Non-MCP Servlets

`BearerTokenValidator` is not MCP-specific. Any servlet — including subclasses of `RestServerBase` or your own `HttpServlet` — can validate a bearer token by calling:

```java
ValidatedToken tok = BearerTokenValidator.requireBearerToken(req, resp);
if (tok == null)
    return;   // 401/403 already written
// proceed with the request, knowing the token has been validated
```

If you use this from outside `MCPServerBase`, **call `BearerTokenValidator.clearCurrentToken()` in a `finally` block** to prevent the validated token from leaking onto a subsequent request that reuses the same servlet-container thread. `MCPServerBase` does this automatically.

For programmatic validation without the HTTP machinery (e.g., from a background job that receives a token via another channel), call `BearerTokenValidator.validate(String token)` directly. It throws `TokenValidationException` on validation failure and `InsufficientScopeException` if scopes are missing.

---

## JWKS Caching and Key Rotation

The framework fetches the authorization server's JWKS lazily on the first request and caches each public key by its `kid` (key id). Two events trigger a refetch:

1. **TTL expiry** — by default keys are cached for one hour (`OAuthJwksCacheSeconds`). Adjust if your authorization server rotates keys more aggressively.
2. **Unknown `kid`** — when a token arrives whose `kid` is not in the cache, the framework treats this as a possible key rotation, refetches the JWKS under a lock, and re-checks. Only one thread fetches at a time even under concurrent load.

This handles routine key rotation (issuer adds a new key, then later removes the old one) without the operator having to restart Kiss.

If you have an explicit `OAuthJwksUri`, the framework uses it directly.  Otherwise it resolves the JWKS through a three-step waterfall against the issuer URL:

1. **`<OAuthAuthorizationServer>/.well-known/oauth-authorization-server`** — the RFC 8414 OAuth Authorization Server Metadata document.  This is the OAuth 2.1 / MCP standard, and the path Kiss's own AS publishes, so this step succeeds for both Kiss-hosted AS and any third-party OAuth 2.1 server.
2. **`<OAuthAuthorizationServer>/.well-known/openid-configuration`** — the OpenID Connect Discovery document.  Tried only when step 1 returns 404 (or returns a 2xx body that does not contain `jwks_uri`).  Kept as a fallback for OIDC-only authorization servers that do not publish RFC 8414.
3. If neither metadata document publishes a `jwks_uri`, the resolver throws an `IOException` whose message names both URLs and points the operator at the `OAuthJwksUri` override.

Hard failures during discovery — network errors, timeouts, HTTP 401/403, or 5xx — are surfaced as `IOException` with the specific status or cause, **not** silently masked by falling through to the next step (which would typically fail the same way against the same host and produce a misleading "tried everything" diagnostic).

---

## Security Considerations

- **Make sure `OAuthResourceIdentifier` reflects this resource server's canonical URL.** The validator rejects any token whose `aud` claim does not contain this value, which prevents token-substitution attacks where a token issued for a different resource gets replayed against yours. RFC 8707 (Resource Indicators) requires authorization servers to bind tokens to specific resources via the `aud` claim; Kiss enforces the resource side. When the AS and RS are the same Kiss app the default (which equals `OAuthAuthorizationServer`) is correct; set it explicitly only when the RS is hosted at a URL different from the AS.
- **`alg=none` is always rejected**, even if you list `none` in `OAuthAllowedAlgorithms` (you can't — the dispatch only handles RSA algorithms). This avoids the classic JWT vulnerability where a forged unsigned token would otherwise be accepted.
- **The token cache is keyed by SHA-256 of the token string.** Token bodies are not stored in the cache key, and the cache TTL is short (60 seconds), so a brief skew between "this token was valid" and "we know it was revoked" is possible. If your application needs immediate revocation, lower the cache TTL or call `BearerTokenValidator.validate()` directly with cache bypass logic (the cache is not exposed externally — re-validating the same token within 60 seconds simply uses the cached `ValidatedToken`, but the `exp` claim is re-checked on every cache hit).
- **JWKS fetch failures fall back to 401.** A 5xx from the authorization server while validating a token surfaces to the client as 401 `invalid_token` rather than 5xx, to avoid leaking infrastructure details. The actual cause is logged at `error` level.
- **Run over HTTPS in production.** Bearer tokens are bearer credentials; anyone who intercepts one can use it until it expires. TLS termination is the caller's responsibility — Kiss does not enforce it.
- **The validator does not check token type (`typ`) or use (`use`).** It accepts any JWT whose header `alg` is in the allowed list, signed by a key in the JWKS. Configure your authorization server to issue access tokens with the right audience; do not configure it to issue access tokens that are reusable as ID tokens or vice versa.
- **Refresh tokens are not handled.** The resource server only validates access tokens. Refresh tokens live entirely between the client and the authorization server.

---

## Troubleshooting

**Discovery endpoint returns 404 even though OAuth is configured.**
Check that `OAuthAuthorizationServer` is uncommented in `application.ini` and that the server was restarted after the change. Configuration is read once at startup.

**All requests get 401 `invalid_token` with "No JWKS key found for kid=...".**
The authorization server is signing tokens with a key it has not published in its JWKS, or the JWKS URL the framework discovered is wrong. Check the catalina log for the `Looking for jwks_uri in ...` lines (one per discovery step actually attempted) and the eventual `Fetching JWKS from ...` line, and verify that URL serves the expected JWKS. Override with explicit `OAuthJwksUri` if neither metadata document points where you expect.

**Server fails to start validating tokens with `IOException: Could not fetch OAuth metadata from .../.well-known/oauth-authorization-server`.**
This is a hard failure on the RFC 8414 step (HTTP 401/403, 5xx, network unreachable, timeout) — *not* a "the doc is missing" condition.  Resolve it at the AS (the doc is genuinely unreachable or the AS is refusing the request) rather than treating it as a discovery problem.  Setting `OAuthJwksUri` bypasses discovery entirely if the AS won't be made to cooperate.

**`IOException: Could not resolve JWKS URI for issuer ... Neither .../.well-known/oauth-authorization-server nor .../.well-known/openid-configuration published a jwks_uri.`**
Both metadata documents were reachable (so neither is a network problem) but neither contained a `jwks_uri` — the AS is not publishing OAuth metadata in either standard form. Set `OAuthJwksUri` explicitly to the AS's JWKS endpoint.

**All requests get 401 with "Token iss '...' does not match expected '...'".**
The `OAuthAuthorizationServer` value in `application.ini` must match the `iss` claim in tokens exactly (modulo a trailing slash, which the validator normalizes). Some authorization servers add a path component to the issuer (e.g. Auth0 uses `https://tenant.auth0.com/`, Keycloak uses `https://kc.example/realms/myrealm`). Set `OAuthAuthorizationServer` to the exact `iss` value your tokens carry.

**All requests get 401 with "Token aud does not include resource identifier '...'".**
The token's `aud` claim does not include the value of `OAuthResourceIdentifier`. Either reconfigure the authorization server to issue tokens with the right audience (typically the resource indicator from RFC 8707) or set `OAuthResourceIdentifier` to a value the tokens actually carry.

**`BearerTokenValidator.currentToken()` returns null inside a tool method.**
The OAuth path was not taken for this request — most commonly because OAuth is not configured (`OAuthAuthorizationServer` is unset), or because the MCP server overrides `authenticate()` with a non-OAuth scheme.

**Tokens validate the first time but fail after the authorization server rotates keys.**
The framework refetches the JWKS on encountering an unknown `kid`, so this should self-heal on the next request. If it does not, check that the authorization server's old `kid` was removed from the JWKS before tokens signed with the new key arrived (an overlap window during rotation is normal and supported).

---

## API Reference

All classes live in `org.kissweb.oauth`:

| Class                              | Purpose                                                              |
|------------------------------------|----------------------------------------------------------------------|
| `OAuthConfig`                      | Reads OAuth settings from `application.ini`.  Lazy singleton.        |
| `JwksCache`                        | Fetches and caches the authorization server's JWKS, keyed by `kid`.  |
| `BearerTokenValidator`             | Validates JWTs and writes RFC 6750 / RFC 9728 challenges on failure. |
| `ValidatedToken`                   | Immutable holder for a successfully validated token's claims.        |
| `ProtectedResourceMetadataServlet` | Serves `/.well-known/oauth-protected-resource` per RFC 9728.         |

Full JavaDoc is generated by `./bld javadoc` and lives under `work/javadoc/org/kissweb/oauth/`.

### Quick reference: `BearerTokenValidator`

| Method                                                                                                | Description                                                                  |
|-------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| `static ValidatedToken requireBearerToken(HttpServletRequest, HttpServletResponse)`                   | Validate token from `Authorization` header; write 401/403 + challenge on failure; stash result for `currentToken()`. |
| `static ValidatedToken validate(String token)`                                                        | Validate a token string directly.  Throws on failure.                       |
| `static ValidatedToken currentToken()`                                                                | Get the token validated earlier in the current request.                     |
| `static void clearCurrentToken()`                                                                     | Clear the thread-local.  `MCPServerBase` calls this automatically.          |

### Exceptions

| Exception                                              | When thrown                                                       |
|--------------------------------------------------------|-------------------------------------------------------------------|
| `BearerTokenValidator.TokenValidationException`        | Malformed JWT, bad signature, claim check failure                 |
| `BearerTokenValidator.InsufficientScopeException`      | Token otherwise valid but missing a required scope                |

---

# Part 2 — Authorization Server

## AS Overview

The authorization server lives in `org.kissweb.oauth.as` and exposes five endpoints, all auto-registered via `@WebServlet`:

| Path                                                 | Spec      | Purpose                                      |
|------------------------------------------------------|-----------|----------------------------------------------|
| `GET /.well-known/oauth-authorization-server`        | RFC 8414  | Authorization-server metadata                |
| `GET /oauth/jwks`                                    | RFC 7517  | Publishes the AS's RSA-2048 signing key      |
| `GET /oauth/authorize`                               | RFC 6749  | Authorization-code flow with PKCE            |
| `POST /oauth/token`                                  | RFC 6749  | Code exchange + refresh-token rotation       |
| `POST /oauth/register`                               | RFC 7591  | Dynamic client registration                  |

**Grant types supported:** `authorization_code`, `refresh_token`.
**Response types supported:** `code`.
**PKCE methods supported:** `S256` only (`plain` rejected per OAuth 2.1).
**Token endpoint auth methods:** `client_secret_basic`, `client_secret_post`, `none`.
**Signing algorithm:** RS256 (single RSA-2048 key, generated and persisted on first start).
**Issued access tokens:** RFC 9068 `at+jwt` profile — JWTs containing `iss`, `sub`, `aud`, `exp`, `iat`, `nbf`, `jti`, `scope`, `client_id`, plus any extra claims the `UserAuthenticator` returned.

The AS is **disabled by default**.  Setting `OAuthAsEnabled = true` in `application.ini` turns it on.

## AS Configuration

All keys go in `src/main/backend/application.ini`:

```ini
# Required
OAuthAsEnabled = true
OAuthAsIssuer = https://myapp.example.com  # iss claim and metadata base URL

# Optional (defaults shown)
OAuthAsSqliteFile             = oauth.db   # relative to applicationPath, or an absolute path
OAuthAccessTokenTtlSeconds    = 3600       # 1 hour
OAuthRefreshTokenTtlSeconds   = 2592000    # 30 days
OAuthAuthCodeTtlSeconds       = 60         # 1 minute
OAuthPruneIntervalSeconds     = 900        # 15 minutes
OAuthAllowDynamicRegistration = true       # required for MCP
OAuthSessionTtlSeconds        = 1800       # AS login session cookie
OAuthKeyId                    = kiss-key-1 # JWT kid header
```

If `OAuthAsIssuer` is not set, the value of `OAuthAuthorizationServer` (from the resource-server config) is used as a fallback — convenient when the AS and RS share the same canonical URL.

`OAuthAsSqliteFile` accepts either form:

- **Relative path** (default `oauth.db`) — resolved against `MainServlet.getApplicationPath()`, i.e. the deployed `WEB-INF/backend/` directory or, in dev mode, `src/main/backend/`.  This colocates the runtime state file with `application.ini`.
- **Absolute path** (e.g. `/var/lib/myapp/oauth.db`) — used verbatim.  Useful for placing the runtime state file outside the deployed webapp tree so that a WAR redeploy cannot touch it.

The file is created and its schema initialized automatically on first startup; no setup tool is needed.

## Required App Code: `UserAuthenticator`

The AS refuses to issue tokens until a `UserAuthenticator` is registered.  The interface has one method:

```java
public interface UserAuthenticator {
    AuthenticatedUser authenticate(String username, String password);
}
```

Return an `AuthenticatedUser` on success (subject + optional extra claims that ride along on issued tokens), or `null` on any failure (bad creds, locked account, store unavailable).  **Never throw** — the framework cannot distinguish "wrong password" from "LDAP is down" if you throw, and both should render as a generic login-failed to the user.  Log the underlying cause inside your implementation.

Wrap whatever credential store the app uses.  For a Kiss app that already has `Login.groovy`, a 10-line Groovy class drops in:

```groovy
// src/main/backend/MyUserAuthenticator.groovy
package com.mycompany

import org.kissweb.oauth.as.AuthenticatedUser
import org.kissweb.oauth.as.UserAuthenticator
import org.kissweb.json.JSONObject

class MyUserAuthenticator implements UserAuthenticator {
    AuthenticatedUser authenticate(String username, String password) {
        def row = Login.verifyPassword(username, password)   // your existing logic
        if (!row) return null
        def extras = new JSONObject()
        extras.put("email", row.email)
        extras.put("name",  row.full_name)
        return new AuthenticatedUser(String.valueOf(row.user_id), extras)
    }
}
```

Register it at startup, in `KissInit.groovy`:

```groovy
AsExtensions.setUserAuthenticator(new com.mycompany.MyUserAuthenticator())
```

The `sub` claim on every issued token is whatever string the authenticator returns from `getSubject()`.  Use a stable identifier (database PK) rather than a username that could be renamed.

## Optional App Code: `ConsentProvider` and HTML Overrides

### `ConsentProvider`

Supplies human-readable text on the consent page.  Default returns scope names verbatim — usable but ugly.  Override:

```java
public interface ConsentProvider {
    String describeScope(String scope);
    default String getDisplayName() { ... }
}
```

```groovy
AsExtensions.setConsentProvider(new ConsentProvider() {
    String describeScope(String s) {
        switch (s) {
            case "mcp:read":  return "Read your orders, customers, and reports."
            case "mcp:write": return "Create, update, and delete records on your behalf."
            default:          return s
        }
    }
    String getDisplayName() { return "Acme Corp" }
})
```

### HTML Overrides

The login and consent pages have minimal built-in HTML.  To replace them, drop your own files at:

- `src/main/frontend/oauth/login.html`
- `src/main/frontend/oauth/consent.html`

Both use simple `{placeholder}` substitution.  Supported placeholders:

| Placeholder      | Available in       | Value                                                          |
|------------------|--------------------|----------------------------------------------------------------|
| `{asName}`       | login, consent     | `ConsentProvider.getDisplayName()`                             |
| `{clientName}`   | login, consent     | The DCR `client_name` (or `client_id` if name is absent)       |
| `{flowId}`       | login, consent     | Opaque flow id — must round-trip in the form as `name="flow"`  |
| `{userSubject}`  | consent            | Subject of the user who just logged in                         |
| `{scopeList}`    | consent            | Pre-rendered `<li>` items for each requested scope             |
| `{errorBlock}`   | login, consent     | Error `<div>` (empty when there's nothing to show)             |

Both forms must POST back to `/oauth/authorize` with a hidden `stage=login` or `stage=consent` and the `flow` id.  See the defaults in `AuthorizationServlet.java` for a working template.

## Endpoints

### `GET /.well-known/oauth-authorization-server`

RFC 8414 metadata document.  Static once config is loaded; cached one hour at the client.  Returns 404 when the AS is disabled.

### `GET /oauth/jwks`

Single-key JWKS (`kty=RSA, use=sig, alg=RS256, kid=<OAuthKeyId>`).  Cached five minutes.  Returns 404 when the AS is disabled.

### `GET /oauth/authorize`

Standard authorization endpoint.  Required params: `response_type=code`, `client_id`, `redirect_uri` (must match a registered URI exactly), `code_challenge`, `code_challenge_method=S256`.  Optional: `scope`, `state`, `resource` (RFC 8707), `nonce`.

The endpoint validates the request, then renders the login page (if no session) or consent page (if the user has an `oauth_session` cookie).  POST-back with `stage=login` runs the `UserAuthenticator`; with `stage=consent&decision=allow` it generates an auth code and 302-redirects to `redirect_uri?code=<code>&state=<state>`.

Errors that the client can recover from (unknown scope, denied consent) redirect to `redirect_uri` with `error=...&error_description=...&state=...`.  Errors that prevent the redirect (unknown client, redirect URI mismatch) render as plain HTML with HTTP 400 — the framework refuses to redirect to a non-registered URI.

### `POST /oauth/token`

Two grants:

**`grant_type=authorization_code`** — params: `code`, `redirect_uri`, `client_id`, `code_verifier`.  Public clients omit `client_secret`; confidential clients supply it via HTTP Basic or as a body param.  Returns:

```json
{
  "access_token": "<at+jwt>",
  "token_type":   "Bearer",
  "expires_in":   3600,
  "refresh_token": "<opaque random>",
  "scope":        "mcp:read mcp:write"
}
```

**`grant_type=refresh_token`** — params: `refresh_token`, `client_id`, optional `scope` (must subset original), optional `resource` (must match original).  Rotates the refresh token: issues a brand-new `refresh_token` value in the same family, marks the old one as rotated.  If a rotated token is ever presented again, the entire family is revoked (see below).

All responses include `Cache-Control: no-store`.

### `POST /oauth/register`

RFC 7591 dynamic client registration.  Request body is a JSON client-metadata document; response is the registered client.  Anyone can POST — there is no initial-access-token gate in v1.  Disable by setting `OAuthAllowDynamicRegistration = false` if your deployment does not need dynamic registration.

```bash
curl -X POST http://localhost:8080/oauth/register \
  -H "Content-Type: application/json" \
  -d '{
    "redirect_uris": ["http://localhost:54545/cb"],
    "client_name": "Claude Desktop",
    "scope": "mcp:read mcp:write",
    "token_endpoint_auth_method": "none"
  }'
```

`token_endpoint_auth_method=none` produces a public client (PKCE-only).  Any other supported value (`client_secret_basic`, `client_secret_post`) produces a confidential client; the server generates a 256-bit secret, returns it in the response, and stores only its SHA-256 hash.

## The Authorization Code Flow End-to-End

1. Client `GET /oauth/authorize?response_type=code&client_id=…&code_challenge=…&code_challenge_method=S256&…`
2. AS renders login page → user submits → `POST /oauth/authorize` with `stage=login&flow=…&username=…&password=…`
3. AS authenticates via `UserAuthenticator`, sets `oauth_session` cookie (scoped to `/oauth/`, HttpOnly, Secure when over TLS, 30-min TTL), renders consent page
4. User clicks Allow → `POST /oauth/authorize` with `stage=consent&flow=…&decision=allow`
5. AS generates auth code (60-s TTL, single-use), 302-redirects to `redirect_uri?code=…&state=…`
6. Client `POST /oauth/token` with `grant_type=authorization_code&code=…&redirect_uri=…&client_id=…&code_verifier=…`
7. AS verifies PKCE, consumes code, mints access token + refresh token, returns JSON

Subsequent `/authorize` requests from the same browser within the session window skip step 2 (the cookie identifies the user) — only consent is shown.  Logout is implicit: the session cookie expires, or the user clears cookies.

## Dynamic Client Registration

DCR is enabled by default because the MCP spec requires it.  Anyone reachable from your AS can register; the response includes a fresh `client_id` and, for confidential clients, a `client_secret`.  Registered clients persist across restarts in the AS's SQLite database (`oauth.db` by default).

For production, consider whether to:

- Leave DCR open (typical for MCP servers — clients you've never met need to onboard).
- Disable DCR (`OAuthAllowDynamicRegistration = false`) and provision clients programmatically by calling `ClientStore.register(new RegisteredClient(...))` from your startup code.

There is no DCR management endpoint (RFC 7592) in v1 — to update or delete a registered client, use `ClientStore.remove(clientId)` from app code.

## Refresh-Token Rotation and Reuse Detection

Every successful `refresh_token` grant:

1. Issues a brand-new refresh token in the same **family** (a UUID assigned when the original auth-code grant happened).
2. Marks the old refresh token as **rotated**, pointing at the successor.
3. Returns the new pair (access + refresh) to the client.

If a **rotated** refresh token is ever presented again — which an honest client never does, because it has already moved to the successor — the AS treats this as evidence of token theft and **revokes the entire family**.  Every refresh token in the family is deleted from the AS database; the next refresh attempt by anyone in the family (including the legitimate client) returns `invalid_grant`.  The legitimate user has to re-authenticate.

This is the OAuth 2.1 stolen-refresh-token detection mechanism described in RFC 6749bis §6.1.  It does not prevent the initial theft, but it bounds the damage: the attacker cannot keep refreshing indefinitely without the legitimate client noticing the next time it tries.

Implementation note: rotation issues two writes (mark the old token rotated; insert the successor) inside one SQLite transaction so the on-disk state is never observed mid-rotation.  Family revocation is a single indexed `DELETE`.

## Persistence Model (`oauth.db`)

The AS persists three kinds of long-lived state in a SQLite database that is **separate from the application's main database** — its own file, its own JDBC connection, no shared connection pool or schema.  The default location is `oauth.db` in the application root (`src/main/backend/oauth.db` in dev mode, `WEB-INF/backend/oauth.db` in a deployed WAR).  Set `OAuthAsSqliteFile` to an absolute path to place the file outside the webapp tree so a WAR redeploy cannot touch it.

The schema is materialized via `CREATE TABLE IF NOT EXISTS` on every open, so a fresh install creates the file automatically.  The tables are:

| Table                  | Purpose                                                                                           |
|------------------------|---------------------------------------------------------------------------------------------------|
| `oauth_keys`           | Signing keypair(s): `kid` (PK), `private_key` (base64 PKCS#8), `public_key` (base64 X.509), `created_at`. |
| `oauth_clients`        | Registered clients: `client_id` (PK), `client_secret_hash` (SHA-256, null for public clients), `client_name`, `redirect_uris` (space-joined), `allowed_scopes` (space-joined), `allowed_grant_types` (space-joined), `created_at`. |
| `oauth_refresh_tokens` | Refresh tokens: `jti` (PK), `family_id` (indexed), `client_id`, `user_sub`, `user_extra_claims` (base64 JSON), `scopes` (space-joined), `audience`, `created_at`, `expires_at` (indexed), `rotated_to_jti` (null for active, set for rotated). |
| `oauth_meta`           | Schema bookkeeping: `meta_key` (PK), `meta_value`.  Currently holds `schema_version=1`.            |

Authorization codes (60-s TTL) live in memory only — too short-lived to be worth persisting.

When the **client** role (Part 3) is used, it persists its own state in this same `oauth.db` file, reusing `OAuthSqliteStore`'s connection and lock rather than opening a second database.  Its tables (`oauth_client_discovery`, `oauth_client_registration`, `oauth_client_tokens`) are created on first client use and sit alongside the AS tables above; see [Client Persistence](#client-persistence-shared-oauthdb).  These tables are absent until the client is actually used.

**Transactions.**  Every mutating operation commits as a normal SQLite transaction.  Refresh-token rotation issues both writes (mark the old token, insert the successor) inside one transaction.

**Pruning is lazy.**  Every OAuth endpoint call checks the in-memory `last_pruned_at` timestamp; if `OAuthPruneIntervalSeconds` has elapsed, it issues `DELETE WHERE expires_at <= ?` for refresh tokens and drops expired in-memory codes.  The `expires_at` column is indexed so the sweep is cheap regardless of table size.  The throttle timestamp is in memory only — a restart re-prunes immediately, which is desirable.

**Build safety.**  `./bld build` excludes `oauth.db` from the source-tree copy.  An accidental `oauth.db` in `src/main/backend/` will never overwrite the deployed runtime file.

**Concurrency.**  Single-process only.  SQLite serializes writers internally, and the Kiss `Connection` is shared across threads under a JVM-wide monitor on `OAuthSqliteStore`.  Two Kiss JVMs writing to the same `oauth.db` will fail with SQLite's standard `database is locked` error rather than silently corrupting state.  For HA, run a single AS instance behind a load balancer, or fork the store implementations to point at a shared server-class database (`oauth.db` is just a SQLite file — the higher-level store classes can be rewritten to talk to PostgreSQL etc.).

**Scale.**  Per-row overhead is well under a kilobyte; tens of thousands of active refresh tokens fit comfortably in a single-MB file with sub-millisecond point lookups.

**Inspection.**  Operators with the `sqlite3` CLI can inspect the file in place: `sqlite3 /path/to/oauth.db '.tables'`, `sqlite3 /path/to/oauth.db 'SELECT client_id, client_name FROM oauth_clients;'`, etc.  Take a copy first if the AS is running.

## AS Security Considerations

- **HTTPS in production.**  The session cookie carries the user's identity for the duration of the consent flow; redirect URIs and refresh tokens are bearer credentials.  TLS termination is the operator's responsibility — Kiss does not enforce it.  Cookies are marked `Secure` automatically when the request scheme is HTTPS.
- **PKCE is mandatory.**  S256 only.  `plain` is rejected even if explicitly requested.  Public clients depend entirely on PKCE for authentication.
- **Exact redirect-URI matching.**  No wildcards, no scheme/host normalization, no fragment.  Per OAuth 2.1 §4.1.2.1, the `redirect_uri` on `/authorize` must equal one of the client's registered URIs byte-for-byte, and the same value must come back on `/token`.
- **Refresh-token rotation is mandatory** for the `refresh_token` grant.  See the section above.
- **Audience binding.**  If the client supplied a `resource` parameter on `/authorize`, the issued tokens carry it as the `aud` claim and the audience is enforced on `/token` refresh.  Otherwise, tokens are bound to the AS issuer URL (any RS that trusts this AS will accept them).
- **Client secret storage.**  Only SHA-256 of the secret is stored.  The full secret is returned to the client exactly once during DCR; there is no recovery if it's lost (the client must re-register).
- **No `alg=none`.**  The token issuer signs with `SHA256withRSA` directly; there is no algorithm-from-header dispatch on the issuing side.
- **Auth-code single-use.**  `AuthorizationCodeStore.consume()` returns the code and removes it in one synchronized operation.  A replay attempt finds nothing.
- **`alg=none` and weak ciphers on the validator side** are handled by the resource-server config (`OAuthAllowedAlgorithms`).  This AS issues only RS256.

## AS Limitations and Omissions

The framework deliberately ships a minimal-but-spec-compliant AS.  Out of scope for v1:

- **Other algorithms.**  Only RS256 is issued (and only RS{256,384,512} can be validated).  ECDSA (ES256), EdDSA, HMAC are not implemented.  Adding ES256 is a localized change to `TokenIssuer` and `BearerTokenValidator`.
- **Key rotation tooling.**  The framework supports a single current key.  The JWKS structure can accommodate a published "previous" key during a rotation window, but the rotation UX (generate new, keep old until last refresh expires, drop old) is not built.
- **RFC 9449 DPoP** (proof-of-possession tokens).  All issued tokens are bearer tokens.
- **RFC 8705 mTLS** client authentication / token binding.
- **RFC 7592** DCR management endpoint (update/delete a registered client via HTTP).  Use `ClientStore` from Java code.
- **RFC 7009 revocation** and **RFC 7662 introspection** endpoints.  Revocation can be done by deleting the refresh token from `oauth.db` (or by calling `RefreshTokenStore.revokeFamily()` from Java); introspection is generally unnecessary when access tokens are JWTs that resource servers can validate locally.
- **OpenID Connect.**  No `openid` scope handling, no ID tokens, no userinfo endpoint.  The framework is OAuth 2.1 only; if you need OIDC, layer it on top or run a separate OIDC server.
- **Multi-tenancy / realms.**  Single AS per Kiss instance.
- **Shared/clustered AS storage.**  The AS backs onto a private SQLite file (`oauth.db`).  This is intentional — the AS does not couple to the application's main database, and operators do not need to provision a schema.  For a multi-instance HA deployment, swap in alternative implementations of `ClientStore`, `RefreshTokenStore`, and `KeyManager` that talk to a shared server-class database; the rest of the AS code does not need to change.

## AS API Reference

All classes live in `org.kissweb.oauth.as`:

| Class                                  | Purpose                                                                  |
|----------------------------------------|--------------------------------------------------------------------------|
| `AuthorizationServerConfig`            | Reads AS config from `application.ini`. Lazy singleton.                  |
| `OAuthSqliteStore`                     | Opens (and initializes the schema of) the AS's private SQLite database; provides the shared JDBC connection and the prune-throttle timestamp. |
| `KeyManager`                           | Generates/persists the RSA signing keypair in `oauth_keys`; builds the JWKS document. |
| `ClientStore`                          | CRUD for `RegisteredClient`s in `oauth_clients`.                         |
| `RefreshTokenStore`                    | CRUD for `RefreshToken`s in `oauth_refresh_tokens`; handles rotation and family revocation. |
| `AuthorizationCodeStore`               | In-memory single-use codes with TTL expiry.                              |
| `TokenIssuer`                          | Builds and signs RS256 JWTs per RFC 9068; generates random opaque tokens.|
| `PkceValidator`                        | RFC 7636 S256 verifier.                                                  |
| `AsExtensions`                         | Registry where the app installs its `UserAuthenticator` / `ConsentProvider`. |
| `UserAuthenticator` *(interface)*      | App implements: verify credentials, return `AuthenticatedUser` or null.  |
| `ConsentProvider` *(interface)*        | App implements: per-scope human description + AS display name.           |
| `AuthenticatedUser`                    | Subject + extra claims to embed in tokens.                               |
| `RegisteredClient`                     | Immutable client record.                                                 |
| `RefreshToken`, `AuthorizationCode`    | Immutable token records.                                                 |
| `MetadataServlet`                      | `/.well-known/oauth-authorization-server`.                               |
| `JwksPublishServlet`                   | `/oauth/jwks`.                                                           |
| `AuthorizationServlet`                 | `/oauth/authorize` --- login, consent, code issuance.                    |
| `TokenServlet`                         | `/oauth/token` --- code exchange + refresh rotation.                     |
| `DynamicClientRegistrationServlet`     | `/oauth/register` --- RFC 7591.                                          |

### Quick reference: `AsExtensions`

```java
// Required
AsExtensions.setUserAuthenticator(UserAuthenticator);
// Optional
AsExtensions.setConsentProvider(ConsentProvider);
```

Register both in `KissInit.groovy` after `MainServlet.readIniFile(...)`.

### Quick reference: `RefreshTokenStore`

```java
RefreshTokenStore store = RefreshTokenStore.get();
store.store(token);                 // initial issuance
store.rotate(oldJti, successor);    // on each refresh
store.revokeFamily(familyId);       // on detected reuse
store.pruneExpired();               // called automatically; safe to call manually
RefreshToken t = store.get(jti);    // lookup
```

### Quick reference: `ClientStore`

```java
ClientStore store = ClientStore.get();
store.register(client);              // DCR or programmatic
RegisteredClient c = store.get(id);
store.remove(id);
Collection<RegisteredClient> all = store.all();
```

---

# Part 3 — Client (Relying Party)

## Client Overview

The client role lets a Kiss application act as a **client of a remote** OAuth 2.1-protected server: it obtains access tokens via the authorization-code + PKCE flow, refreshes them, and makes them available for outbound calls (typically as an `Authorization: Bearer ...` header).  It is the inverse of the resource/authorization-server roles — here Kiss *consumes* a remote server's tokens rather than validating or issuing its own.

When at least one client provider is configured, the framework:

1. **Discovers** the remote authorization server from the resource URL (RFC 9728 protected-resource metadata → RFC 8414 / OpenID Connect metadata), caching the endpoints.
2. **Registers** the client — using a pre-configured `client_id`, or via RFC 7591 Dynamic Client Registration (as a public, PKCE-only client) when none is configured — caching the result.
3. **Drives the authorization-code + PKCE flow**: builds the authorization URL, captures the redirect at a built-in callback servlet (`/oauth/client/callback`), and exchanges the code for tokens.
4. **Refreshes** access tokens automatically, single-flight per provider, honoring refresh-token rotation.

Everything is **inert** until a provider is configured: with no `[OAuthClient *]` section, the callback servlet returns 404, no database is touched, and no network call is made.

The role is generic — it has no knowledge of what the remote server does (MCP, a plain REST API, anything else).

## Client Configuration

Each remote server is declared in its own section named `[OAuthClient <name>]`, where `<name>` is your label (used in code and to key the stored state).  Multiple providers are supported — one section each.  Unlike the resource/AS keys (which live in `[main]`), these are their own sections, read directly from `application.ini` by `OAuthClientConfig` (the framework's flat environment only carries `[main]`).

```ini
[OAuthClient myprovider]
Url          = https://remote.example.com/mcp   ; remote resource/server URL (required)
Scopes       = mcp:read mcp:write               ; space- or comma-separated (optional)
ClientId     =                                  ; blank => Dynamic Client Registration (RFC 7591)
ClientSecret =                                  ; only if pre-registered + confidential
```

Optional `[main]` keys (defaults shown):

```ini
# Base URL used to build the redirect_uri (<base>/oauth/client/callback).
# If unset, it is derived from the request that starts the flow.  The
# remote server must accept this redirect_uri (registered automatically
# when using Dynamic Client Registration).
# OAuthClientRedirectBaseUrl = http://localhost:8080

# Treat an access token as expired this many seconds before its real
# expiry, so a refresh happens slightly early.  Default 60.
# OAuthClientRefreshSkewSeconds = 60

# The ini file the [OAuthClient *] sections are read from.  Default
# application.ini.  Exists mainly so tests can point at a fixture.
# OAuthClientConfigFile = application.ini
```

There is no client state-file key: cached registrations and tokens go into the same `oauth.db` the authorization server uses (see [Client Persistence](#client-persistence-shared-oauthdb)).

## Using the Client from App Code

```java
import org.kissweb.oauth.client.OAuthClient;
import org.kissweb.oauth.client.OAuthAuthorizationRequiredException;

OAuthClient client = OAuthClient.forProvider("myprovider");

// Before making outbound calls, ensure we are authorized:
if (!client.isAuthorized()) {
    // Send the browser to this URL to log in / consent.  The base URL
    // becomes <base>/oauth/client/callback; pass null to use
    // OAuthClientRedirectBaseUrl from application.ini.
    String authorizeUrl = client.beginAuthorization("http://localhost:8080");
    // ... redirect the browser to authorizeUrl ...
}

// On every outbound call — refreshes the access token as needed (discovery and registration happened in beginAuthorization):
try {
    String token = client.getAccessToken();
    JSONObject headers = new JSONObject();
    headers.put("Authorization", "Bearer " + token);
    // ... call the remote API with RestClient ...
} catch (OAuthAuthorizationRequiredException e) {
    // No usable token and none obtainable without a fresh login —
    // kick off beginAuthorization() again.
}
```

`getAccessToken()` returns a currently-valid access token, refreshing transparently when the cached one is expired.  When no token is held and none can be minted without interactive login, it throws `OAuthAuthorizationRequiredException` (a normal control-flow signal, not an error).  `clearTokens()` discards stored tokens for the provider (logout) while keeping the cached discovery and registration.

## The Client Flow End-to-End

1. App calls `client.beginAuthorization(base)`.  The framework resolves discovery (cached), ensures a client registration (configured `ClientId` or DCR, cached), generates a PKCE verifier/challenge and a random `state`, records them in the in-memory `PendingAuthorization` registry, and returns the authorization URL — `response_type=code`, `client_id`, `redirect_uri=<base>/oauth/client/callback`, `scope`, `state`, `code_challenge`, `code_challenge_method=S256`, and `resource=<provider Url>` (RFC 8707 audience binding).
2. The browser visits the remote `authorization_endpoint`; the user logs in and consents.
3. The remote server 302-redirects to `/oauth/client/callback?code=…&state=…`.
4. `OAuthCallbackServlet` validates `state` against the pending registry (CSRF protection; the entry is single-use and short-lived), then calls `OAuthClient.completeAuthorization`, which `POST`s to the remote `token_endpoint` with `grant_type=authorization_code`, the code, the matching `redirect_uri`, the `client_id`, and the PKCE `code_verifier`.  The returned tokens are persisted.  The servlet renders a small "you may close this window" page.
5. Subsequent `client.getAccessToken()` calls return the stored access token, refreshing it when needed.

## Token Refresh and Rotation

When `getAccessToken()` finds the access token expired (within `OAuthClientRefreshSkewSeconds` of expiry), it performs a `refresh_token` grant, **single-flight per provider** — concurrent callers for the same provider serialize on a per-provider lock, and a thread that wakes after another has already refreshed simply uses the fresh token.

The client **honors refresh-token rotation**: if the token response includes a new `refresh_token`, it replaces the stored one; if it does not, the existing refresh token is kept.  If the remote server rejects the refresh with `invalid_grant` (e.g. the refresh token was revoked or its family was invalidated by the AS's reuse detection), the client deletes the stored tokens and throws `OAuthAuthorizationRequiredException`, so the next attempt starts a fresh login.

## Client Persistence (shared `oauth.db`)

The client does **not** open its own database.  It persists into the same SQLite file the authorization server uses (`oauth.db` by default; configured by `OAuthAsSqliteFile`), reusing `OAuthSqliteStore`'s single JDBC connection and JVM-wide monitor.  All access goes through the Kiss `org.kissweb.database` API (no raw JDBC).

Three tables, keyed by provider name, are created lazily (`CREATE TABLE IF NOT EXISTS`) on first client use and sit alongside the AS tables:

| Table                        | Purpose                                                                                  |
|------------------------------|------------------------------------------------------------------------------------------|
| `oauth_client_discovery`     | Cached endpoints: `provider` (PK), `issuer`, `authorization_endpoint`, `token_endpoint`, `registration_endpoint`, `fetched_at`. |
| `oauth_client_registration`  | Cached client identity: `provider` (PK), `client_id`, `client_secret` (null for public), `redirect_uri`, `created_at`. |
| `oauth_client_tokens`        | Current token set: `provider` (PK), `access_token`, `refresh_token`, `token_type`, `scope`, `expires_at`, `created_at`. |

The in-flight `PendingAuthorization` (state + PKCE verifier during a single login) is held in memory only — single-use and short-lived (10-minute TTL), so it is not worth persisting.

Because the client shares the AS's file and lock, the concurrency, build-safety, and inspection notes in the AS [Persistence Model](#persistence-model-oauthdb) apply equally.  A client-only deployment (no AS) still uses `oauth.db`; the AS tables are created but remain empty.

## Client Security Considerations

- **PKCE is mandatory** (S256).  DCR registers a public client (`token_endpoint_auth_method=none`) whose security rests on PKCE.
- **`state` is a CSRF token.**  The callback proceeds only when the returned `state` matches a pending, unexpired, single-use entry.
- **Redirect capture is a framework servlet** at `/oauth/client/callback`.  The remote server must accept that `redirect_uri`; DCR registers it automatically, otherwise whitelist it when you pre-register the client.
- **Audience binding.**  Each authorization and token request carries `resource=<provider Url>` (RFC 8707) so the issued token's audience is bound to the intended remote server.
- **Token storage.**  Access and refresh tokens are stored at rest in `oauth.db` (unencrypted, like the AS's refresh tokens).  Protect the file with filesystem permissions and place it outside the webapp tree in production (`OAuthAsSqliteFile`).
- **HTTPS in production.**  Tokens and the authorization code are bearer credentials in transit.

## Client Limitations

- **Redirect capture is via the callback servlet only.**  The RFC 8252 loopback-listener pattern (for CLI/desktop tools with no web server) is not implemented; it can be added later behind the same API.
- **Protected-resource discovery uses the origin-root well-known path** (`<origin>/.well-known/oauth-protected-resource`); RFC 9728 path-based insertion is not implemented.  If no protected-resource document is published, the provider URL's origin is treated as the issuer and RFC 8414 / OIDC discovery is attempted directly.
- **Discovery is cached indefinitely** (re-fetched only when absent) — there is no TTL-based refresh of endpoint metadata.
- **Single-process**, sharing the AS's `oauth.db` concurrency model.
- **RSA-only / bearer tokens**, consistent with the rest of the Kiss OAuth support; no DPoP or mTLS.

## Client API Reference

All classes live in `org.kissweb.oauth.client`:

| Class                                  | Purpose                                                                  |
|----------------------------------------|--------------------------------------------------------------------------|
| `OAuthClient`                          | Facade: `forProvider`, `beginAuthorization`, `isAuthorized`, `getAccessToken`, `clearTokens`. |
| `OAuthClientConfig`                    | Reads `[OAuthClient *]` sections + client `[main]` keys. Lazy singleton. |
| `OAuthClientProvider`                  | Immutable parsed provider (name, url, scopes, optional client id/secret).|
| `OAuthMetadataDiscovery`               | RFC 9728 → RFC 8414 / OIDC endpoint discovery via `RestClient`.          |
| `DynamicClientRegistration`            | RFC 7591 registration of a public client.                               |
| `Pkce`                                 | Client-side S256 verifier/challenge generator.                          |
| `OAuthClientStore`                     | Persists discovery, registration, and tokens into the shared `oauth.db`.|
| `PendingAuthorization`                 | In-memory, single-use state + PKCE verifier for an in-flight login.      |
| `OAuthCallbackServlet`                 | `/oauth/client/callback` — validates state, exchanges code for tokens.   |
| `DiscoveryMetadata`, `ClientRegistration`, `TokenSet` | Immutable value records.                                  |
| `OAuthClientException`                 | Unrecoverable client error (discovery/registration/token failure).      |
| `OAuthAuthorizationRequiredException`  | Signal that interactive login is required.                              |

### Quick reference: `OAuthClient`

```java
OAuthClient client = OAuthClient.forProvider("myprovider");
String url   = client.beginAuthorization(baseUrl);  // null baseUrl => OAuthClientRedirectBaseUrl
boolean ok   = client.isAuthorized();
String token = client.getAccessToken();             // refreshes as needed; may throw OAuthAuthorizationRequiredException
client.clearTokens();                               // logout
```
