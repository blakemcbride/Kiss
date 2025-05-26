package org.kissweb;

/**
 *  This class is used to signify an error that is sent to the front-end but does
 *  not produce a back-end log.
 */
public class FrontendException extends Exception {

    /**
     * Creates a new FrontendException with the specified message.
     *
     * @param msg the exception message
     */
    public FrontendException(String msg) {
        super(msg);
    }

}
