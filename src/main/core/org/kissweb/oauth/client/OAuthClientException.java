package org.kissweb.oauth.client;

/**
 * Thrown when an outbound OAuth 2.1 client operation fails in a way the
 * application cannot recover from automatically --- a discovery document
 * that cannot be fetched or parsed, a token endpoint that returns an
 * error, a misconfigured provider, and the like.
 * <br><br>
 * Distinct from {@link OAuthAuthorizationRequiredException}, which is not
 * an error at all but the normal signal that the user must complete an
 * interactive login before a token can be obtained.
 */
public class OAuthClientException extends RuntimeException {

    /**
     * Create an exception with a human-readable message.
     *
     * @param message the detail message
     */
    public OAuthClientException(String message) {
        super(message);
    }

    /**
     * Create an exception with a message and an underlying cause.
     *
     * @param message the detail message
     * @param cause   the underlying cause
     */
    public OAuthClientException(String message, Throwable cause) {
        super(message, cause);
    }
}
