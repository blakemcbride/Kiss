package scripts

/**
 * Author: Blake McBride
 * Date: 7/16/20
 *
 * Groovy scripts get compiled and cached so they are run at the same speed as compiled code.
 * They are also auto-recompiled and reloaded when they change so that the system can be changed
 * while it is running.
 */
class MyScript {

    public static Object myMethod(Integer a, Integer b) {
        return a + b + 100
    }
}
