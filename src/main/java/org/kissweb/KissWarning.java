package org.kissweb;

/**
 * Author: Blake McBride
 * Date: 12/24/21
 *
 * This class is used to signify an error that deserves a log but not a backtrace.
 */
public class KissWarning extends Exception {

    public KissWarning(String msg) {
        super(msg);
    }
}
