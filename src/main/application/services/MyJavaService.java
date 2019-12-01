

package services;

import org.json.JSONObject;
import org.kissweb.database.Connection;
import org.kissweb.rest.ProcessServlet;

public class MyJavaService {

    public void addNumbers(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        int num1 = injson.getInt("num1");
        int num2 = injson.getInt("num2");
        outjson.put("num3", (num1 + num2));
    }

}