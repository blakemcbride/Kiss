package org.kissweb.restServer;

/**
 * Thrown by {@link ProcessServlet#requireLogin()} when a service that is permitted
 * to run without authentication determines that it actually needs a logged-in user.
 * <br><br>
 * The framework converts it into the standard "not logged in" response
 * (<code>_ErrorCode = 2</code>) — the same response a normally-protected method
 * produces — so the front-end routes the user to the login screen.
 */
public class LoginRequiredException extends RuntimeException {

    /**
     * Creates a new LoginRequiredException with the specified message.
     *
     * @param msg the exception message (sent to the front-end as _ErrorMessage)
     */
    public LoginRequiredException(String msg) {
        super(msg);
    }
}
