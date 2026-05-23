package org.kissweb.oauth.as;

/**
 * Registry for the two app-provided OAuth AS extension points:
 * {@link UserAuthenticator} and {@link ConsentProvider}.
 * <br><br>
 * The application registers its implementations at startup --- typically
 * from {@code KissInit.groovy} --- and the AS servlets retrieve them at
 * request time:
 * <pre>
 * // KissInit.groovy
 * AsExtensions.setUserAuthenticator(new com.mycompany.MyUserAuthenticator())
 * AsExtensions.setConsentProvider(new com.mycompany.MyConsentProvider())
 * </pre>
 * If no {@link UserAuthenticator} is registered, the
 * {@code /authorize} endpoint returns 500 with a configuration-error
 * message; the AS cannot function without one.  If no
 * {@link ConsentProvider} is registered, a no-op default is used that
 * shows scope names unchanged --- functional but ugly, so override it.
 */
public final class AsExtensions {

    private static volatile UserAuthenticator userAuthenticator;
    private static volatile ConsentProvider   consentProvider = new ConsentProvider.Default();

    private AsExtensions() { }

    /**
     * Register the application's {@link UserAuthenticator}.  Required
     * for the AS to function.
     *
     * @param authenticator the implementation
     */
    public static void setUserAuthenticator(UserAuthenticator authenticator) {
        userAuthenticator = authenticator;
    }

    /**
     * @return the registered {@link UserAuthenticator}, or null if none
     *         has been registered (the AS will refuse to issue tokens
     *         in that state)
     */
    public static UserAuthenticator getUserAuthenticator() {
        return userAuthenticator;
    }

    /**
     * Register the application's {@link ConsentProvider}.  Optional ---
     * a no-op default is used if none is registered.
     *
     * @param provider the implementation
     */
    public static void setConsentProvider(ConsentProvider provider) {
        consentProvider = provider != null ? provider : new ConsentProvider.Default();
    }

    /**
     * @return the registered {@link ConsentProvider}; never null --- a
     *         no-op default is returned if the application has not
     *         registered one
     */
    public static ConsentProvider getConsentProvider() {
        return consentProvider;
    }
}
