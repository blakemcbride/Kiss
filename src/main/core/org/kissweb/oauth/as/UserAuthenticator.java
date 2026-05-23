package org.kissweb.oauth.as;

/**
 * The one piece of the authorization server that cannot be generic: the
 * actual ``is this person who they claim to be?'' check.
 * <br><br>
 * The framework calls {@link #authenticate} when a user submits the
 * login form on the AS.  Implementations check the credentials against
 * whatever user store the application owns --- the Kiss
 * {@code users} table, an LDAP directory, an external IdP, a magic-link
 * sender, etc.  On success, return an {@link AuthenticatedUser} whose
 * subject identifier will go into the {@code sub} claim of every token
 * issued for this session.  On failure, return {@code null}.
 * <br><br>
 * <h2>Where the implementation lives</h2>
 * Under {@code src/main/backend/} (so it can be hot-reloaded) or
 * {@code src/main/precompiled/} (so it can be a Java class with package
 * structure).  Register it at startup, typically from
 * {@code KissInit.groovy}:
 * <pre>
 * AsExtensions.setUserAuthenticator(new MyUserAuthenticator())
 * </pre>
 * <h2>What the implementation must do</h2>
 * <ul>
 *   <li>Verify the credentials --- using whatever password hashing /
 *       MFA / federation logic the application requires.</li>
 *   <li>On success, return an {@link AuthenticatedUser} with the user's
 *       stable subject identifier.  The {@code sub} claim is the
 *       primary way every other component (the resource server, audit
 *       logs, etc.) identifies the user, so prefer an immutable ID over
 *       a username that could be renamed.</li>
 *   <li>Optionally include extra claims (email, group memberships,
 *       custom application claims) that should ride along on every
 *       access token issued for this user.</li>
 *   <li>On failure, return {@code null}.  Do not throw --- the
 *       framework cannot distinguish ``bad password'' from ``LDAP
 *       server is down'' if you throw, and both should render as
 *       login-failed to the user without leaking detail.  Log the
 *       underlying cause inside your implementation if you need to
 *       diagnose it.</li>
 * </ul>
 */
public interface UserAuthenticator {

    /**
     * Verify the supplied credentials.
     *
     * @param username the username the user entered on the login form
     * @param password the password the user entered on the login form
     * @return an {@link AuthenticatedUser} on success, or null on
     *         any kind of failure (wrong credentials, locked account,
     *         user store unavailable)
     */
    AuthenticatedUser authenticate(String username, String password);
}
