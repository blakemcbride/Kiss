package org.kissweb.oauth.as;

/**
 * Supplies the human-readable text the AS consent page shows for each
 * scope a client is requesting.
 * <br><br>
 * The framework knows scope \emph{names} ({@code mcp:read},
 * {@code orders:write}, {@code admin}) but has no idea what they mean
 * in your application's domain.  An implementation maps each scope name
 * to a sentence the end user can understand.  Defaults to returning the
 * scope name unchanged --- which is barely usable; almost every
 * deployment should provide its own.
 * <br><br>
 * <h2>Where the implementation lives</h2>
 * Same conventions as {@link UserAuthenticator}: under
 * {@code src/main/backend/} or {@code src/main/precompiled/}, registered
 * at startup via {@link AsExtensions#setConsentProvider}.
 */
public interface ConsentProvider {

    /**
     * Return a short, user-facing description of the named scope.  The
     * consent page renders this verbatim inside a bullet list, so plain
     * sentence text is the right register.  Example:
     * <pre>
     *   describeScope("mcp:read")  -&gt; "Read your orders, customers, and reports."
     *   describeScope("mcp:write") -&gt; "Create, update, and delete records on your behalf."
     * </pre>
     *
     * @param scope the scope name
     * @return a sentence describing what consent the scope conveys
     */
    String describeScope(String scope);

    /**
     * Return the human-readable display name for this AS, used at the
     * top of the login and consent pages (``Sign in to \emph{Acme
     * Corp}'').  Default returns the AS issuer URL --- override to
     * brand the experience.
     *
     * @return the display name
     */
    default String getDisplayName() {
        return AuthorizationServerConfig.get().getIssuer();
    }

    /**
     * The default implementation: returns the scope name verbatim.
     * Useful for tests and during initial development; production apps
     * should provide a real implementation.
     */
    final class Default implements ConsentProvider {
        /** Construct the default ConsentProvider. */
        public Default() {
        }

        @Override
        public String describeScope(String scope) {
            return scope;
        }
    }
}
