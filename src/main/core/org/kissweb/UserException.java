package org.kissweb;

/**
 *  This class is used to signify an error that is sent to the front-end but does
 *  not produce a back-end log.
 */
public class UserException extends Exception {

    private int errorCode = -1;

    /**
     * Creates a new FrontendException with the specified message.
     * This is an exception that is sent to the front-end but does not get logged on the back-end.
     *
     * @param msg the exception message
     */
    public UserException(String msg) {
        super(msg);
    }

    /**
     * Creates a new FrontendException with the specified error code and message.
     * This is an exception that is sent to the front-end but does not get logged on the back-end.
     *
     * @param errorCode the error code sent to the front-end
     * @param msg the eror message sent to the front-end
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
