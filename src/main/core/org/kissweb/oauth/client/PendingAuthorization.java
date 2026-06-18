package org.kissweb.oauth.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * One in-flight authorization-code login: the {@code state} value, the
 * PKCE {@code code_verifier} to be presented at the token exchange, the
 * provider it belongs to, and the {@code redirect_uri} used.  Created by
 * {@link OAuthClient#beginAuthorization(String)} and consumed by
 * {@link OAuthCallbackServlet} when the browser returns.
 * <br><br>
 * The {@code state} value doubles as the CSRF token: the callback only
 * proceeds if the returned {@code state} matches a pending entry.
 * <br><br>
 * Entries are held in memory only (this static registry).  They are
 * single-use and short-lived (default 10 minutes); losing them on a JVM
 * restart only costs the user a re-click of the login button, so there is
 * no reason to persist them.
 */
public final class PendingAuthorization {

    /** How long a pending authorization remains valid, in seconds. */
    public static final long TTL_SECONDS = 600;

    private static final Map<String, PendingAuthorization> PENDING = new HashMap<>();

    private final String provider;
    private final String state;
    private final String codeVerifier;
    private final String redirectUri;
    private final long   createdAtEpochSeconds;

    /**
     * Construct a pending authorization stamped with the current time.
     *
     * @param provider     the provider name
     * @param state        the random state / CSRF value
     * @param codeVerifier the PKCE code verifier
     * @param redirectUri  the redirect URI used in the authorize request
     */
    public PendingAuthorization(String provider, String state, String codeVerifier, String redirectUri) {
        this.provider              = provider;
        this.state                 = state;
        this.codeVerifier          = codeVerifier;
        this.redirectUri           = redirectUri;
        this.createdAtEpochSeconds = System.currentTimeMillis() / 1000L;
    }

    /**
     * Get the provider name.
     * @return the provider name
     */
    public String getProvider() {
        return provider;
    }

    /**
     * Get the state / CSRF value.
     * @return the state value
     */
    public String getState() {
        return state;
    }

    /**
     * Get the PKCE code verifier.
     * @return the code verifier
     */
    public String getCodeVerifier() {
        return codeVerifier;
    }

    /**
     * Get the redirect URI used in the authorize request.
     * @return the redirect URI
     */
    public String getRedirectUri() {
        return redirectUri;
    }

    /**
     * Whether this entry has passed its time-to-live.
     * @return true if expired
     */
    public boolean isExpired() {
        final long now = System.currentTimeMillis() / 1000L;
        return now - createdAtEpochSeconds > TTL_SECONDS;
    }

    // ==================================================================
    // In-memory registry
    // ==================================================================

    /**
     * Register a freshly-created pending authorization, keyed by its state.
     *
     * @param p the pending authorization to store
     */
    public static synchronized void store(PendingAuthorization p) {
        pruneExpired();
        PENDING.put(p.getState(), p);
    }

    /**
     * Look up and atomically remove the pending authorization for a state.
     * Returns null if the state is unknown, already consumed, or expired.
     *
     * @param state the state value returned by the authorization server
     * @return the pending authorization, or null
     */
    public static synchronized PendingAuthorization consume(String state) {
        if (state == null)
            return null;
        final PendingAuthorization p = PENDING.remove(state);
        if (p == null || p.isExpired())
            return null;
        return p;
    }

    /**
     * Drop expired entries.  Called opportunistically on {@link #store}.
     */
    public static synchronized void pruneExpired() {
        final List<String> dead = new ArrayList<>();
        for (Map.Entry<String, PendingAuthorization> e : PENDING.entrySet())
            if (e.getValue().isExpired())
                dead.add(e.getKey());
        for (String k : dead)
            PENDING.remove(k);
    }
}
