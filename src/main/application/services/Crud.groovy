package services

import org.json.JSONArray
import org.json.JSONObject
import org.kissweb.Groff
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.rest.ProcessServlet

/**
 * Author: Blake McBride
 * Date: 7/21/20
 */
class Crud {

    void getRecords(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        List<Record> recs = db.fetchAll("select * from phone order by last_name, first_name")
        JSONArray rows = new JSONArray();
        for (Record rec : recs) {
            JSONObject row = new JSONObject()
            row.put("id", rec.getInt("rec_id"))
            row.put("firstName", rec.getString("first_name"))
            row.put("lastName", rec.getString("last_name"))
            row.put("phoneNumber", rec.getString("phone_number"))
            rows.put(row)
        }
        outjson.put("rows", rows)
    }

    void addRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.newRecord("phone")
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.addRecord()
    }

    void updateRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.fetchOne("select * from phone where rec_id=?", injson.getInt("id"))
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.update();
    }

    void deleteRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        db.execute("delete from phone where rec_id=?", injson.getInt("id"))
    }

    void runReport(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {

        // This will only work if you have Groff installed on your computer

        Groff rpt = new Groff("PhoneList-", "Phone List", false)

        rpt.out(".TS H")
        rpt.out("Lw(1i) Lw(1i) Lw(1i) .")
        rpt.out("")
        rpt.out("Last Name\tFirst Name\tPhone Number")
        rpt.out("\\_\t\\_\t\\_")
        rpt.out(".TH")

        List<Record> recs = db.fetchAll("select * from phone order by last_name, first_name")
        for (Record rec : recs) {
            rpt.out(rec.getString("last_name") + "\t" + rec.getString("first_name") + "\t" + rec.getString("phone_number"))
        }

        rpt.out(".TE")

        outjson.put("reportUrl", rpt.process())
    }

}
