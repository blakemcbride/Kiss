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
 * <code>Crud</code> is the name of the web service.  This is all you need.
 * There is no build process or configuration.  It can be added, changed, or deleted on a running system.
 * It gets auto-compiled and loaded whenever it changes.  It always runs at full compiled speed.
 */
class Crud {

    /**
     * <code>getRecords</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * This method illustrates how to perform a query and iterate over the records.
     * Actually, this can be accomplished with KISS in a single line of code but was done this way as an example.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void getRecords(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        if (db == null) {
            outjson.put("nodb", true)
            return
        }
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

    /**
     * <code>addRecord</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void addRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.newRecord("phone")
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.addRecord()
    }

    /**
     * <code>updateRecord</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void updateRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.fetchOne("select * from phone where rec_id=?", injson.getInt("id"))
        rec.set("first_name", injson.getString("firstName"))
        rec.set("last_name", injson.getString("lastName"))
        rec.set("phone_number", injson.getString("phoneNumber"))
        rec.update();
    }

    /**
     * <code>deleteRecord</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void deleteRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        db.execute("delete from phone where rec_id=?", injson.getInt("id"))
    }

    /**
     * <code>runReport</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * This method requires your system to have groff installed.  It produces a PDF report with full paging, page numbering,
     * titles, and tables.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void runReport(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {

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

    /**
     * <code>runExport</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * This method produces a CSV file locally and then passes the front-end the URL of the file.  The back-end
     * auto-removes it after about a day.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void runExport(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
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

