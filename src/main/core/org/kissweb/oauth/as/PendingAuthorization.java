package org.kissweb.oauth.as;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Holds the original parameters of an in-flight {@code /authorize}
 * request while the user logs in and grants consent.  Kept in memory
 * only --- if Kiss restarts mid-flow, the user simply clicks the link
 * from their MCP client again.
 * <br><br>
 * The auth flow assigns each pending request a random {@code flowId};
 * the login and consent forms POST this id back so the servlet can
 * recover the original request without re-validating every parameter.
 */
final class PendingAuthorization {

    final String      flowId;
    final String      clientId;
    final String      redirectUri;
    final Set<String> requestedScopes;
    final String      audience;             // RFC 8707 resource indicator, or null
    final String      state;
    final String      codeChallenge;
    final String      codeChallengeMethod;
    final String      nonce;
    final long              createdAtEpochSeconds;
    /** Set after the user logs in (so consent stage can read who's approving). */
    volatile AuthenticatedUser authenticatedUser;

    PendingAuthorization(String flowId,
                         String clientId,
                         String redirectUri,
                         Set<String> requestedScopes,
                         String audience,
                         String state,
                         String codeChallenge,
                         String codeChallengeMethod,
                         String nonce) {
        this.flowId                = flowId;
        this.clientId              = clientId;
        this.redirectUri           = redirectUri;
        this.requestedScopes       = Collections.unmodifiableSet(new LinkedHashSet<>(requestedScopes));
        this.audience              = audience;
        this.state                 = state;
        this.codeChallenge         = codeChallenge;
        this.codeChallengeMethod   = codeChallengeMethod;
        this.nonce                 = nonce;
        this.createdAtEpochSeconds = System.currentTimeMillis() / 1000L;
    }
}
