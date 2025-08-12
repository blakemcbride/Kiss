
import org.kissweb.DateUtils;
import org.kissweb.database.Command;
import org.kissweb.database.Connection;
import org.kissweb.database.Cursor;
import org.kissweb.database.Record;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.HashMap;

/**
 * Test class for database operations.
 */
public class Test {

    /**
     * Constructs a new Test instance.
     */
    public Test() {
    }

    /**
     * Main method for testing database operations.
     *
     * @param args command line arguments
     * @throws Exception if any error occurs
     */
    public static void main(String [] args) throws Exception {
        int hasId = 0;
        int hasNoId = 0;
        int recsAdded = 0;
        int today = DateUtils.today();
        Date dt = new Date();
        Connection db = new Connection(Connection.ConnectionType.PostgreSQL, "localhost", null, "waytogo", "postgres", "postgres");
        Cursor c = db.query("select adp_id, es.active, p.fname, p.mname, p.lname, p.person_id\n" +
                "from employee e\n" +
                "\n" +
                "join (\n" +
                "select t4.* from hr_empl_status_history t4\n" +
                "inner join\n" +
                "\n" +
                "(select distinct t2.employee_id, t2.effective_date, max(t2.status_hist_id) status_hist_id\n" +
                "from hr_empl_status_history t2\n" +
                "inner join\n" +
                "\n" +
                "(select distinct t0.employee_id, max(t0.effective_date) effective_date\n" +
                "from hr_empl_status_history t0\n" +
                "where t0.effective_date <= ?\n" +
                "group by t0.employee_id) t1\n" +
                "\n" +
                "  on t2.employee_id = t1.employee_id and t2.effective_date = t1.effective_date\n" +
                "group by t2.employee_id, t2.effective_date) t3\n" +
                "\n" +
                "on t4.status_hist_id = t3.status_hist_id) s\n" +
                "  on e.person_id = s.employee_id\n" +
                "  \n" +
                "join hr_employee_status es\n" +
                "  on s.status_id = es.status_id\n" +
                "  \n" +
                "join person p\n" +
                "  on e.person_id = p.person_id\n", today);
        while (c.isNext()) {
            Record r = c.getRecord();
            String active = r.getString("active");
            String adpId = r.getString("adp_id");
            String person_id = r.getString("person_id");
            if ((adpId == null  ||  adpId.isEmpty()) && active.equals("Y") && !person_id.equals("00000-0000000000")) {
                Record sr = db.newRecord("hr_empl_status_history");
                String status_hist_id = IDGeneratorKiss.generate(db, "hr_empl_status_history", "status_hist_id");
                sr.set("status_hist_id", status_hist_id);
                sr.set("employee_id", person_id);
                sr.set("effective_date", today);
                sr.set("status_id", "00001-0000000016"); // Purged
                sr.set("notes", "Automated process");
                sr.setDateTime("record_change_date", dt);
                sr.addRecord();
                recsAdded++;
            }
/*
            if (active.equals("N") || adpId == null  ||  adpId.isEmpty())
                hasNoId++;
            else {
                hasId++;
                String name = r.getString("lname") + ", " + r.getString("fname");
                String mname = r.getString("mname");
                if (mname != null  &&  !mname.isEmpty())
                    name += " " + mname;
 //               System.out.println(name);
            }
 */
        }
        System.out.println(recsAdded + " status records added");
        db.close();
 //       System.out.println("Total with ADP ID and active:    " + hasId);
 //       System.out.println("Total with no ADP ID or inactive: " + hasNoId);
    }

    static class IDGeneratorKiss {

        private static String dbid;
        private static final HashMap<String, Integer> idCache = new HashMap<>();

        public static String generate(final Connection db, final String tableName, final String id_column) throws Exception {
            if (dbid == null) {
                try {
                    Record r = db.fetchOne("select prop_value from property where prop_name='DBID'");
                    if (r == null)
                        throw new Exception("");
                    dbid = String.format("%5s", r.getString("prop_value")).replace(' ', '0');
                    r.close();
                } catch (Exception e) {
                    dbid = "00001";
                }
            }

            int top;
            Integer otop = idCache.get(tableName);
            if (otop != null)
                top = otop;
            else {
                    /* The following line works for PostgreSQL, MySQL, and SQLite but does not work for Microsoft Server or Oracle
					   because of the limit 1.  See org.kissweb.database.Connection.limit()
					*/
                String x = "select " + id_column + " from " + tableName + " where " + id_column + " like '" + dbid + "-%' order by " + id_column + " desc limit 1";
                Record r = db.fetchOne("select " + id_column + " from " + tableName + " where " + id_column + " like '" + dbid + "-%' order by " + id_column + " desc");
                if (r == null)
                    top = 0;
                else {
                    String key = r.getString(id_column);
                    top = Integer.parseInt(key.substring(6));
                }
            }
            idCache.put(tableName, ++top);
            return dbid + "-" + String.format("%010d", top);
        }
    }

}
