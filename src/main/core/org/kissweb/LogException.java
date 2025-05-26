package org.kissweb;

/**
 * This class is used to signify an error that deserves a log but not a backtrace.
 * It also sends the error message to the front-end.
 */
public class LogException extends Exception {

    /**
     * Creates a new LogException with the specified message.
     *
     * @param msg the exception message
     */
    public LogException(String msg) {
        super(msg);
    }
}
