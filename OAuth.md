# OAuth 2.1 Support in Kiss

Kiss ships with both halves of OAuth 2.1: a **resource server** that validates incoming bearer tokens, and an **authorization server** that issues them.  The two run independently or together — a Kiss-based MCP server typically runs both in the same JVM, validating its own tokens.

This document is a reference for a developer already familiar with OAuth 2.1.  For a slower, worked-example treatment, see the chapters on OAuth in the Kiss book.

> **Resource server** — Part 1, validates tokens issued by any RFC 8414 / OpenID Connect compliant authorization server (Auth0, Okta, Keycloak, Entra, Google, or your own Kiss-hosted AS).
>
> **Authorization server** — Part 2, runs the authorization-code flow with mandatory PKCE, the token endpoint with refresh-token rotation, dynamic client registration, the discovery endpoint, and the JWKS endpoint.  Persists all long-lived state to a single ini file — no database required.

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
- [Persistence Model (oauth.ini)](#persistence-model-oauthini)
- [AS Security Considerations](#as-security-considerations)
- [AS Limitations and Omissions](#as-limitations-and-omissions)
- [AS API Reference](#as-api-reference)

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
OAuthAsIniFile                = oauth.ini  # relative to applicationPath
OAuthAccessTokenTtlSeconds    = 3600       # 1 hour
OAuthRefreshTokenTtlSeconds   = 2592000    # 30 days
OAuthAuthCodeTtlSeconds       = 60         # 1 minute
OAuthPruneIntervalSeconds     = 900        # 15 minutes
OAuthAllowDynamicRegistration = true       # required for MCP
OAuthSessionTtlSeconds        = 1800       # AS login session cookie
OAuthKeyId                    = kiss-key-1 # JWT kid header
```

If `OAuthAsIssuer` is not set, the value of `OAuthAuthorizationServer` (from the resource-server config) is used as a fallback — convenient when the AS and RS share the same canonical URL.

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

DCR is enabled by default because the MCP spec requires it.  Anyone reachable from your AS can register; the response includes a fresh `client_id` and, for confidential clients, a `client_secret`.  Registered clients persist across restarts via `oauth.ini`.

For production, consider whether to:

- Leave DCR open (typical for MCP servers — clients you've never met need to onboard).
- Disable DCR (`OAuthAllowDynamicRegistration = false`) and provision clients programmatically by calling `ClientStore.register(new RegisteredClient(...))` from your startup code.

There is no DCR management endpoint (RFC 7592) in v1 — to update or delete a registered client, use `ClientStore.remove(clientId)` from app code.

## Refresh-Token Rotation and Reuse Detection

Every successful `refresh_token` grant:

1. Issues a brand-new refresh token in the same **family** (a UUID assigned when the original auth-code grant happened).
2. Marks the old refresh token as **rotated**, pointing at the successor.
3. Returns the new pair (access + refresh) to the client.

If a **rotated** refresh token is ever presented again — which an honest client never does, because it has already moved to the successor — the AS treats this as evidence of token theft and **revokes the entire family**.  Every refresh token in the family is deleted from `oauth.ini`; the next refresh attempt by anyone in the family (including the legitimate client) returns `invalid_grant`.  The legitimate user has to re-authenticate.

This is the OAuth 2.1 stolen-refresh-token detection mechanism described in RFC 6749bis §6.1.  It does not prevent the initial theft, but it bounds the damage: the attacker cannot keep refreshing indefinitely without the legitimate client noticing the next time it tries.

Implementation note: rotation and revocation are both single disk writes (the whole `oauth.ini` is rewritten atomically) and complete in milliseconds at MCP-scale traffic.

## Persistence Model (`oauth.ini`)

The AS persists three kinds of long-lived state to a single ini file at `src/main/backend/oauth.ini` (deployed runtime location: `WEB-INF/backend/oauth.ini`):

```ini
[keys]
current_kid     = kiss-key-1
current_private = <base64 PKCS#8>
current_public  = <base64 X.509>
created_at      = 1747200000

[client.<id>]
client_secret_hash  = sha256:<hex>   # empty for public clients
client_name         = Claude Desktop
redirect_uris       = http://localhost:54545/cb
allowed_scopes      = mcp:read mcp:write
allowed_grant_types = authorization_code refresh_token
created_at          = 1747201234

[refresh.<jti>]
family_id         = <uuid>
client_id         = <client_id>
user_sub          = <subject>
user_extra_claims = <base64 JSON>     # ride-along claims for access tokens
scopes            = mcp:read mcp:write
audience          = https://myapp.example.com
created_at        = 1747201234
expires_at        = 1749793200
rotated_to_jti    =                   # set when this token has been rotated
```

Authorization codes (60-s TTL) live in memory only — too short-lived to be worth persisting.

**Saves are atomic.**  `OAuthIniStore.save()` writes to `oauth.ini.tmp`, then atomically renames onto `oauth.ini`.  A crash mid-write leaves the previous file intact.

**Pruning is lazy.**  Every OAuth endpoint call checks the in-memory `last_pruned_at` timestamp; if `OAuthPruneIntervalSeconds` has elapsed, it removes expired refresh tokens and codes and saves once.  The timestamp is in memory only — restart re-prunes immediately, which is desirable.

**Build safety.**  `./bld build` excludes `oauth.ini` from the source-tree copy.  An accidental `oauth.ini` in `src/main/backend/` will never overwrite the deployed runtime file.

**Concurrency.**  Single-process only.  Two Kiss JVMs writing to the same `oauth.ini` will clobber each other.  For HA, swap in DB-backed implementations of `ClientStore` and `RefreshTokenStore` (interfaces TBD).

**Scale.**  Each registered client is ~500 bytes; each active refresh token is ~300 bytes.  10K active refresh tokens ≈ 3 MB file.  Beyond that the per-call rewrite cost becomes noticeable — but you're well past MCP server territory by then.

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
- **RFC 7009 revocation** and **RFC 7662 introspection** endpoints.  Revocation can be done by deleting the refresh token from `oauth.ini` (or by calling `RefreshTokenStore.revokeFamily()` from Java); introspection is generally unnecessary when access tokens are JWTs that resource servers can validate locally.
- **OpenID Connect.**  No `openid` scope handling, no ID tokens, no userinfo endpoint.  The framework is OAuth 2.1 only; if you need OIDC, layer it on top or run a separate OIDC server.
- **Multi-tenancy / realms.**  Single AS per Kiss instance.
- **Database-backed storage.**  In-memory + `oauth.ini` only.  Implementing DB-backed alternatives is straightforward but requires changing the stores' singletons to a swap-in mechanism.

## AS API Reference

All classes live in `org.kissweb.oauth.as`:

| Class                                  | Purpose                                                                  |
|----------------------------------------|--------------------------------------------------------------------------|
| `AuthorizationServerConfig`            | Reads AS config from `application.ini`. Lazy singleton.                  |
| `OAuthIniStore`                        | Wraps `IniFile` with atomic save + in-memory prune-throttle timestamp.   |
| `KeyManager`                           | Generates/persists the RSA signing keypair; builds the JWKS document.    |
| `ClientStore`                          | CRUD for `RegisteredClient`s in `oauth.ini`.                             |
| `RefreshTokenStore`                    | CRUD for `RefreshToken`s; handles rotation and family revocation.        |
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
