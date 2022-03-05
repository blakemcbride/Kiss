package org.kissweb;

/**
 * Author: Blake McBride<br>
 * Date: 1/30/22
 * <br><br>
 *  This class is used to signify an error that is sent to the front-end but does
 *  not produce a back-end log.
 */
public class FrontendException extends Exception {

    public FrontendException(String msg) {
        super(msg);
    }

}
