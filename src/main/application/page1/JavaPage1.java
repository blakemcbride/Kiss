

package page1;

import org.json.JSONObject;
import org.kissweb.database.Connection;
import org.kissweb.rest.MainServlet;

public class JavaPage1 {

    public static void main(JSONObject injson, JSONObject outjson, Connection db, MainServlet servlet) {
        int num1 = injson.getInt("num1");
        int num2 = injson.getInt("num2");
        outjson.put("num3", (num1 + num2));
        Object x;
    }

}