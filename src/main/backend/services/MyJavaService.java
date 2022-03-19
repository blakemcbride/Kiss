

package services;

import org.json.JSONObject;
import org.kissweb.database.Connection;
import org.kissweb.restServer.ProcessServlet;

public class MyJavaService {

    public void addNumbers(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Integer num1 = injson.getInt("num1");
        Integer num2 = injson.getInt("num2");
        if (num1 == null || num2 == null)
            outjson.put("num3", 0);
        else
            outjson.put("num3", num1 + num2);
    }

}