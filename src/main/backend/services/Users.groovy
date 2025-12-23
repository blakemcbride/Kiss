package services

import org.kissweb.json.JSONArray
import org.kissweb.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.restServer.ProcessServlet

/**
 * Users service for CRUD operations on the users table.
 */
class Users {

    /**
     * Get all user records.
     */
    void getRecords(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        if (db == null) {
            outjson.put("nodb", true)
            return
        }
        List<Record> recs = db.fetchAll("select * from users order by user_name")
        JSONArray rows = new JSONArray()
        for (Record rec : recs) {
            JSONObject row = new JSONObject()
            row.put("id", rec.getInt("user_id"))
            row.put("userName", rec.getString("user_name"))
            row.put("userPassword", rec.getString("user_password"))
            row.put("userActive", rec.getString("user_active"))
            rows.put(row)
        }
        outjson.put("rows", rows)
    }

    /**
     * Add a new user record.
     */
    void addRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.newRecord("users")
        rec.set("user_name", injson.getString("userName"))
        rec.set("user_password", injson.getString("userPassword"))
        rec.set("user_active", injson.getString("userActive"))
        rec.addRecord()
    }

    /**
     * Update an existing user record.
     */
    void updateRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        Record rec = db.fetchOne("select * from users where user_id=?", injson.getInt("id"))
        rec.set("user_name", injson.getString("userName"))
        rec.set("user_password", injson.getString("userPassword"))
        rec.set("user_active", injson.getString("userActive"))
        rec.update()
    }

    /**
     * Delete a user record.
     */
    void deleteRecord(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        db.execute("delete from users where user_id=?", injson.getInt("id"))
    }
}
