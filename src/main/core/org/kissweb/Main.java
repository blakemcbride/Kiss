
package org.kissweb;

import org.kissweb.database.Command;
import org.kissweb.database.Connection;
import org.kissweb.database.Connection.ConnectionType;
import org.kissweb.database.Cursor;
import org.kissweb.database.Record;

import java.util.ArrayList;
import java.util.Objects;

/**
 * This class is only used for command-line execution of Kiss.
 * So, if
 * Kiss is run from the command-line, this is the class that gets executed.
 * This class is not used in any of the web environments (including develop).
 * <br><br>
 * The command-line jar file is built with:
 *     <code>./bld kisscmd</code>
 * <br><br>
 * This creates the file: work/kisscmd.jar
 * <br><br>
 * To execute do:
 *     <code>java -jar kisscmd.jar</code>
 */
public class Main {

    public static void main(String [] argv) throws Exception {
        Connection db = new Connection(ConnectionType.PostgreSQL, "localhost", null, "waytogo", "postgres", "postgres");
        Command cmd = db.newCommand();
        Cursor c = cmd.query("select * from hr_employee_event order by employee_id, event_date, summary, detail");
        Record prec = null;
        int kept=0, deleted=0;
        ArrayList<String> lst = new ArrayList<>();
        while (c.isNext()) {
            Record r = c.getRecord();
            if (prec == null) {
                prec = r;
                kept++;
                continue;
            }
            if (Objects.equals(r.getString("employee_id"), prec.getString("employee_id"))
                && Objects.equals(r.getString("supervisor_id"), prec.getString("supervisor_id"))
                && Objects.equals(r.getInt("event_date"), prec.getInt("event_date"))
                && Objects.equals(r.getString("summary"), prec.getString("summary"))) {
                lst.add(r.getString("event_id"));
                deleted++;
            } else {
                prec = r;
                kept++;
            }
        }
        for (String id : lst) {
            System.out.println("deleting " + id);
            cmd.execute("delete from hr_employee_event where event_id=?", id);
        }
        db.close();
        System.out.println("Number deleted = " + deleted);
        System.out.println("Number kept = " + kept);
    }

}
