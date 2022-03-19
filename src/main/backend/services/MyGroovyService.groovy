package services

import org.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.restServer.GroovyService
import org.kissweb.restServer.ProcessServlet
import org.kissweb.restServer.MainServlet

class MyGroovyService {

    /*
             Keep in mind that this is a compiled and cached script so it runs as fast as regular compiled Java.
             It gets auto-recompiled and loaded whenever it changes.
     */
    void addNumbers(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Integer num1 = injson.getInt("num1")
        Integer num2 = injson.getInt("num2")
        if (num1 == null  ||  num2 == null)
            outjson.put("num3", 0)
        else
            outjson.put("num3", num1 + num2)


        /*  We could perform all of the processing here (as above).  Or, we can call a Groovy script as follows.   */
        /*
        Integer r = (Integer) GroovyService.run(false, "~/scripts", "MyScript", "myMethod", null, num1, num2)
        outjson.put("num3", (int) r)
         */
    }

    void hasDatabase(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        outjson.put("hasDatabase", MainServlet.hasDatabase())
    }

}