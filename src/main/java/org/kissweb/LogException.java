package org.kissweb;

/**
 * Author: Blake McBride
 * Date: 12/24/21
 *
 * This class is used to signify an error that deserves a log but not a backtrace.
 * It also sends the error message to the front-end.
 */
public class LogException extends Exception {

    public LogException(String msg) {
        super(msg);
    }
}
