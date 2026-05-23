package org.kissweb.oauth.as;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * In-memory store of authorization codes.  Codes are issued by
 * {@code /authorize}, consumed by {@code /token}, and live for at most
 * 60 seconds (configurable via {@code OAuthAuthCodeTtlSeconds}).
 * <br><br>
 * Not persisted to disk: the cost of losing one on restart is the user
 * clicking the consent button again, which costs nothing.  Avoiding the
 * write also keeps the token endpoint cheap on the common path.
 * <br><br>
 * Codes are single-use: {@link #consume} atomically returns the code
 * and removes it from the store, so a replay attempt finds nothing.
 */
public final class AuthorizationCodeStore {

    private static final AuthorizationCodeStore INSTANCE = new AuthorizationCodeStore();

    private final Map<String, AuthorizationCode> codes = new HashMap<>();

    private AuthorizationCodeStore() { }

    /** @return the process-wide singleton */
    public static AuthorizationCodeStore get() {
        return INSTANCE;
    }

    /**
     * Store a freshly-issued code.
     *
     * @param code the code to store, keyed by {@link AuthorizationCode#getCode()}
     */
    public synchronized void store(AuthorizationCode code) {
        codes.put(code.getCode(), code);
    }

    /**
     * Look up and atomically remove a code.  Returns null if the code
     * is unknown, has already been consumed, or has expired (expired
     * codes are removed on lookup).
     *
     * @param codeValue the code value the client submitted
     * @return the stored code, or null
     */
    public synchronized AuthorizationCode consume(String codeValue) {
        if (codeValue == null)
            return null;
        final AuthorizationCode c = codes.remove(codeValue);
        if (c == null)
            return null;
        if (c.isExpired())
            return null;
        return c;
    }

    /**
     * Drop expired codes from the store.  Called opportunistically by
     * the prune cycle.
     *
     * @return the number of codes removed
     */
    public synchronized int pruneExpired() {
        final long now = System.currentTimeMillis() / 1000L;
        final List<String> toRemove = new ArrayList<>();
        for (Map.Entry<String, AuthorizationCode> e : codes.entrySet())
            if (e.getValue().getExpiresAtEpochSeconds() <= now)
                toRemove.add(e.getKey());
        for (String key : toRemove)
            codes.remove(key);
        return toRemove.size();
    }

    /** @return the number of codes currently held (intended for tests/monitoring) */
    public synchronized int size() {
        return codes.size();
    }
}
