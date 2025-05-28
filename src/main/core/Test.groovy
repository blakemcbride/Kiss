import org.kissweb.database.Command
import org.kissweb.database.Connection
import org.kissweb.database.Cursor
import org.kissweb.database.Record


static void main(String [] args) {
    Connection db = new Connection(Connection.ConnectionType.PostgreSQL, "localhost", null, "waytogo", "postgres", "postgres")
    Command cmd = db.newCommand()
    Cursor c = cmd.query("select * from person")
    int i = 1
    while (c.isNext()) {
        Record rec = c.getRecord()
        println(i + " " + rec.getString("lname"))
        rec.set("lname", rec.getString("lname") + "*")
        rec.update()
        db.commit()
        i++
    }
}
