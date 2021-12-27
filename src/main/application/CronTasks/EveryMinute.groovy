package CronTasks

import org.kissweb.DateTime
import org.kissweb.database.Connection
import org.kissweb.database.Record


/**
 * Author: Blake McBride
 * Date: 12/24/21
 */
class EveryMinute {

    /**
     * Entry point for cron jobs.  db can be cast to an application specific type.
     *
     * @param db
     */
    static void main(Object obj) {
        Connection db = (Connection) obj
        println("EveryMinute is running at " + DateTime.currentDateTimeFormatted())
        List<Record> recs = db.fetchAll("select * from phone order by last_name, first_name")
        recs.forEach(rec -> {
            println(rec.getString("first_name"))
        })
    }

}
