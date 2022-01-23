package services

import org.json.JSONArray
import org.json.JSONObject
import org.kissweb.DelimitedFileWriter
import org.kissweb.FileUtils
import org.kissweb.Groff
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.restServer.ProcessServlet

/**
 * Author: Blake McBride
 * Date: 7/21/20
 */
class Crud {

    static void getRecords(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
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

    static void addRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.newRecord("phone")
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.addRecord()
    }

    static void updateRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.fetchOne("select * from phone where rec_id=?", injson.getInt("id"))
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.update();
    }

    static void deleteRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        db.execute("delete from phone where rec_id=?", injson.getInt("id"))
    }

    static void runReport(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {

        // This will only work if you have Groff installed on your computer

        Groff rpt = new Groff("PhoneList-", "Phone List", false)

        rpt.startTable("Lw(1i) Lw(1i) Lw(1i)")
        rpt.column("Last Name")
        rpt.column("First Name")
        rpt.column("Phone Number")
        rpt.endTitle()

        List<Record> recs = db.fetchAll("select * from phone order by last_name, first_name")
        for (Record rec : recs) {
            rpt.column(rec.getString("last_name"))
            rpt.column(rec.getString("first_name"))
            rpt.column(rec.getString("phone_number"))
        }

        outjson.put("reportUrl", rpt.process())
    }

    static void runExport(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        File f = FileUtils.createReportFile("PhoneList-", ".csv")
        DelimitedFileWriter dfw = new DelimitedFileWriter(f.getAbsolutePath())

        // write header
        dfw.writeField("Last Name")
        dfw.writeField("First Name")
        dfw.writeField("Phone Number")
        dfw.endRecord()

        List<Record> recs = db.fetchAll("select * from phone order by last_name, first_name")
        for (Record rec : recs) {
            dfw.writeField(rec.getString("last_name"))
            dfw.writeField(rec.getString("first_name"))
            dfw.writeField(rec.getString("phone_number"))
            dfw.endRecord()
        }
        dfw.close()
        outjson.put("exportUrl", FileUtils.getHTTPPath(f))
    }
}

