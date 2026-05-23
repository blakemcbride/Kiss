package org.kissweb.oauth;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The successfully validated contents of an OAuth 2.1 bearer token.
 * <br><br>
 * Immutable.  Returned by
 * {@link BearerTokenValidator#requireBearerToken(jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse)}
 * on success and accessible during the rest of the request via
 * {@link BearerTokenValidator#currentToken()}.
 * <br><br>
 * The raw JWT claims are available through {@link #getClaims()} for any
 * provider-specific fields (e.g. {@code email}, {@code groups}, custom
 * claims) not exposed by named getters.
 */
public final class ValidatedToken {

    private final String subject;
    private final String issuer;
    private final Set<String> audience;
    private final Set<String> scopes;
    private final long expiresAtEpochSeconds;
    private final long issuedAtEpochSeconds;
    private final JSONObject claims;

    /**
     * Build a validated token from claims that have already passed
     * signature and claim-set validation.  Application code should not
     * construct this directly --- use
     * {@link BearerTokenValidator#requireBearerToken}.
     *
     * @param claims the verified JWT claims
     */
    ValidatedToken(JSONObject claims) {
        this.claims                 = claims;
        this.subject                = claims.getString("sub", null);
        this.issuer                 = claims.getString("iss", null);
        this.audience               = readStringOrArray(claims, "aud");
        this.scopes                 = readScopes(claims);
        this.expiresAtEpochSeconds  = claims.getLong("exp", 0L);
        this.issuedAtEpochSeconds   = claims.getLong("iat", 0L);
    }

    /**
     * Get the {@code sub} claim --- the principal the token represents.
     *
     * @return the subject, or null if absent
     */
    public String getSubject() {
        return subject;
    }

    /**
     * Get the {@code iss} claim --- the authorization server that issued the token.
     *
     * @return the issuer URL, or null if absent
     */
    public String getIssuer() {
        return issuer;
    }

    /**
     * Get the {@code aud} claim values as a set.
     *
     * @return the audience values (one entry if the claim was a string, more if it was an array)
     */
    public Set<String> getAudience() {
        return audience;
    }

    /**
     * Get the granted scopes, parsed from either the {@code scope} claim
     * (space-separated string) or the {@code scp} claim (array) ---
     * whichever the authorization server uses.
     *
     * @return the scopes carried by the token
     */
    public Set<String> getScopes() {
        return scopes;
    }

    /**
     * Test whether the token carries a given scope.
     *
     * @param scope the scope to test
     * @return true if the token carries the given scope
     */
    public boolean hasScope(String scope) {
        return scope != null && scopes.contains(scope);
    }

    /**
     * Get the {@code exp} claim as Unix epoch seconds.
     *
     * @return the expiration time, or 0 if absent
     */
    public long getExpiresAtEpochSeconds() {
        return expiresAtEpochSeconds;
    }

    /**
     * Get the {@code iat} claim as Unix epoch seconds.
     *
     * @return the issued-at time, or 0 if absent
     */
    public long getIssuedAtEpochSeconds() {
        return issuedAtEpochSeconds;
    }

    /**
     * Get the full verified claim set --- use for provider-specific claims
     * (e.g. {@code email}, {@code preferred_username}, custom claims) not
     * exposed by the named getters.  The returned object is the live claim
     * set; callers should not mutate it.
     *
     * @return the verified JWT claims
     */
    public JSONObject getClaims() {
        return claims;
    }

    /**
     * Convenience accessor for a string-valued claim.
     *
     * @param name the claim name
     * @return the value as a string, or null if the claim is absent
     */
    public String getClaimString(String name) {
        return claims.getString(name, null);
    }

    // ------------------------------------------------------------------

    private static Set<String> readStringOrArray(JSONObject o, String key) {
        if (!o.has(key))
            return Collections.emptySet();
        final Object v = o.opt(key);
        final Set<String> out = new LinkedHashSet<>();
        if (v instanceof JSONArray) {
            final JSONArray arr = (JSONArray) v;
            for (int i = 0; i < arr.length(); i++) {
                String s = arr.optString(i, null);
                if (s != null && !s.isEmpty())
                    out.add(s);
            }
        } else {
            String s = v.toString();
            if (!s.isEmpty())
                out.add(s);
        }
        return Collections.unmodifiableSet(out);
    }

    private static Set<String> readScopes(JSONObject claims) {
        // RFC 8693 / OAuth practice: scopes may arrive in "scope" as a
        // space-separated string or in "scp" as a JSON array.  Some IdPs
        // (Azure AD, older Okta) use one; some (most modern AS) use the
        // other.  Accept both.
        final Set<String> out = new LinkedHashSet<>();
        final String s = claims.getString("scope", null);
        if (s != null && !s.isEmpty())
            for (String tok : s.split("\\s+"))
                if (!tok.isEmpty())
                    out.add(tok);
        if (claims.has("scp")) {
            final Object scp = claims.opt("scp");
            if (scp instanceof JSONArray) {
                final JSONArray arr = (JSONArray) scp;
                for (int i = 0; i < arr.length(); i++) {
                    String tok = arr.optString(i, null);
                    if (tok != null && !tok.isEmpty())
                        out.add(tok);
                }
            }
        }
        return Collections.unmodifiableSet(out);
    }
}
