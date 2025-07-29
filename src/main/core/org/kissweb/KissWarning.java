package org.kissweb;

import java.io.Serial;

/**
 * Throwing a KissExceptions cause two things:
 * <p>
 * 1.  specify an error string and error code for the front-end
 * 2.  print an error and stacktrace on the back-end log
 * <p>
 * KissWarning does only one thing:
 * <p>
 * 1.  specify an error string and error code for the front-end
 * <p>
 * Throwing a KissWarning does not print an error or stacktrace in the back-end log
 * <p>
 * In either case, the web service call handler on the front-end (Server class) automatically handles the error
 * by displaying the error to the user in a popup and returning _Success = false.  The front-end also receives
 * the error message and error code from the back-end via _ErrorMessage and _ErrorCode.
 */
public class KissWarning extends KissException {

    @Serial
    private static final long serialVersionUID = -6401392132905555830L;

    /**
     * This method causes the error string and error code to be returned to the front-end.
     * Nothing is logged on the back-end.
     * The message sent to the front-end arrives as _ErrorMessage.   _ErrorCode defaults to -1.
     *
     * @param msg error message returned to the front-end as _ErrorMessage
     */
    public KissWarning(final String msg) {
        super(msg);
    }

    /**
     * This method causes the error string and error code to be returned to the front-end.
     * Nothing is logged on the back-end.
     * The message sent to the front-end arrives as _ErrorMessage.
     * The errorCode arrives at the front-end as _ErrorCode.
     *
     * @param errorCode error code returned to the front-end as _ErrorCode
     * @param msg error message returned to the front-end as _ErrorMessage
     */
    public KissWarning(int errorCode, String msg) {
        super(errorCode, msg);
    }

}