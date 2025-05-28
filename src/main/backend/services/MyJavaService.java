

package services;

import org.kissweb.json.JSONObject;
import org.kissweb.database.Connection;
import org.kissweb.restServer.ProcessServlet;

/**
 * <code>MyJavaService</code> is the name of the web service.  This is all you need.
 * There is no build process or configuration.  It can be added, changed, or deleted on a running system.
 * It gets auto-compiled and loaded whenever it changes.  It always runs at full compiled speed.
 */
public class MyJavaService {

    /**
     * <code>addNumbers</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    public void addNumbers(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Integer num1 = injson.getInt("num1");
        Integer num2 = injson.getInt("num2");
        if (num1 == null || num2 == null)
            outjson.put("num3", 0);
        else
            outjson.put("num3", num1 + num2);
    }

}
