package org.kissweb;

/**
 * This class is used to signify an error that deserves a log but not a backtrace.
 * It also sends the error message to the front-end.
 */
public class LogException extends RuntimeException {

    /**
     * error code returned to the front-end as _ErrorCode
     */
    private int errorCode = -1;

    /**
     * Creates a new LogException with the specified message.
     *
     * @param msg the exception message
     */
    public LogException(String msg) {
        super(msg);
    }

    /**
     * Creates a new LogException with the specified error code and message.
     * This is an exception that is sent to the front-end and logged on the back-end.
     * The message and return code are sent to the front-end as _ErrorMessage and _ErrorCode.
     *
     * @param errorCode the error code sent to the front-end
     * @param msg the error message sent to the front-end
     */
    public LogException(int errorCode, String msg) {
        super(msg);
        this.errorCode = errorCode;
    }

    /**
     * Returns the error code associated with the exception.
     *
     * @return the error code
     */
    public int getErrorCode() {
        return errorCode;
    }
}
