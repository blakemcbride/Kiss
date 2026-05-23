# OAuth 2.1 Resource-Server Support

Kiss includes built-in support for acting as an **OAuth 2.1 resource server** — validating bearer tokens issued by an external authorization server (Auth0, Okta, Keycloak, Microsoft Entra, Google, or any RFC 8414 / OpenID Connect compliant provider). The primary use case is securing **MCP (Model Context Protocol) servers** built on `MCPServerBase`, which the MCP specification mandates be protected by OAuth 2.1.

This document describes what the resource-server provides, how to configure it, how to use it from MCP and non-MCP servlets, and the security considerations to keep in mind.

---

## Table of Contents

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

---

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

All settings live in `src/main/backend/application.ini` and are read once at startup via `MainServlet.getEnvironment()`:

```ini
# Required to enable OAuth — issuer URL of the authorization server.
OAuthAuthorizationServer = https://auth.example.com

# Canonical URL of this resource server.  Tokens must carry this value
# in their "aud" claim.  Real deployments should always set this.
OAuthResourceIdentifier = https://mcp.myapp.com

# Optional.  Comma- or space-separated scopes every token must carry.
# Tokens missing any of these scopes get HTTP 403 with error="insufficient_scope".
OAuthRequiredScopes = mcp:read mcp:write

# Optional.  Explicit JWKS URL.  If omitted, the framework discovers it from
# <OAuthAuthorizationServer>/.well-known/openid-configuration
OAuthJwksUri = https://auth.example.com/.well-known/jwks.json

# Optional.  How long to cache JWKS keys before refetching, in seconds.
# Default: 3600.
OAuthJwksCacheSeconds = 3600

# Optional.  Comma-separated list of acceptable JWS algorithms.
# Default: RS256.  Supported: RS256, RS384, RS512.
OAuthAllowedAlgorithms = RS256

# Optional.  Allowed clock skew when validating exp/nbf, in seconds.
# Default: 60.
OAuthClockSkewSeconds = 60
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
  "resource": "https://mcp.myapp.com",
  "authorization_servers": ["https://auth.example.com"],
  "bearer_methods_supported": ["header"],
  "scopes_supported": ["mcp:read", "mcp:write"],
  "resource_signing_alg_values_supported": ["RS256"]
}
```

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

If you have an explicit `OAuthJwksUri`, the framework uses it directly. Otherwise it fetches `<OAuthAuthorizationServer>/.well-known/openid-configuration` and reads the `jwks_uri` field. Both OpenID Connect Discovery and OAuth 2.0 Authorization Server Metadata (RFC 8414) use the same URL and field name, so this works against either kind of authorization server.

---

## Security Considerations

- **Always set `OAuthResourceIdentifier`** to your resource server's canonical URL. The validator rejects any token whose `aud` claim does not contain this value, which prevents token-substitution attacks where a token issued for a different resource gets replayed against yours. RFC 8707 (Resource Indicators) requires authorization servers to bind tokens to specific resources via the `aud` claim; Kiss enforces the resource side.
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
The authorization server is signing tokens with a key it has not published in its JWKS, or the JWKS URL the framework discovered is wrong. Check the catalina log for the "Fetching JWKS from ..." line and verify that URL serves the expected JWKS. Override with explicit `OAuthJwksUri` if discovery picks the wrong one.

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
