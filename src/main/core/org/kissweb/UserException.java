package org.kissweb;

/**
 *  This class is used to signify an error that is sent to the front-end but does
 *  not produce a back-end log.
 */
public class UserException extends RuntimeException {

    /**
     * error code returned to the front-end as _ErrorCode
     */
    private int errorCode = -1;

    /**
     * Creates a new FrontendException with the specified message.
     * This is an exception that is sent to the front-end but does not get logged on the back-end.
     * The message is sent to the front-end as _ErrorMessage. _ErrorCode is -1.
     *
     * @param msg the exception message
     */
    public UserException(String msg) {
        super(msg);
    }

    /**
     * Creates a new FrontendException with the specified error code and message.
     * This is an exception that is sent to the front-end but does not get logged on the back-end.
     * The message and return code are sent to the front-end as _ErrorMessage and _ErrorCode.
     *
     * @param errorCode the error code sent to the front-end
     * @param msg the error message sent to the front-end
     */
    public UserException(int errorCode, String msg) {
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
