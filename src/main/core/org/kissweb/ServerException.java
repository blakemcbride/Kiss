package org.kissweb;

/**
 * Throwing a KissExceptions cause two things:
 * <p>
 * 1.  specify an error string and error code for the front-end
 * 2.  print an error and stacktrace on the back-end log
 * <p>
 * Throwing a KissWarning does only one thing:
 * <p>
 * 1.  specify an error string and error code for the front-end
 * <p>
 * KissWarning does not print an error or stacktrace in the back-end log
 * <p>
 * In either case, the web service call handler on the front-end (Server class) automatically handles the error
 * by displaying the error to the user in a popup and returning _Success = false.  The front-end also receives
 * the error message and error code from the back-end via _ErrorMessage and _ErrorCode.
 */
public class ServerException extends RuntimeException {

    /**
     * error code returned to the front-end as _ErrorCode
     */
    private int errorCode = -1;

    /**
     * This method causes the error string and error code to be returned to the front-end and logged on the back-end.
     * The message and return code are sent to the front-end as _ErrorMessage and _ErrorCode.
     *
     * @param errorCode error code returned to the front-end as _ErrorCode
     * @param msg error message returned to the front-end as _ErrorMessage
     */
    public ServerException(int errorCode, final String msg) {
        super(msg);
        this.errorCode = errorCode;
        try {
            // You can email the message to support if you like
            //final StringWriter sw = new StringWriter();
            //printStackTrace(new PrintWriter(sw));
            //Mail.send("admin@abc.com", "admin@abc.com", "Exception", s+"\n"+sw.toString());
        } catch (final Throwable e) {
        }
    }

    /**
     * This method causes the error string and error code to be returned to the front-end and logged on the back-end.
     * The message sent to the front-end arrives as _ErrorMessage.   _ErrorCode defaults to -1.
     *
     * @param msg error message returned to the front-end as _ErrorMessage
     */
    public ServerException(final String msg) {
        this(-1, msg);
    }

    /**
     * Get the error code that will be sent to the front-end.
     *
     * @return the error code
     */
    public int getErrorCode() {
        return errorCode;
    }
}
