package CronTasks

import org.kissweb.DateTime
import org.kissweb.database.Connection



/**
 * Author: Blake McBride
 * Date: 12/24/21
 */
class EveryMinute {

    static void main(Connection db) {
        println("EveryMinute is running at " + DateTime.currentDateTimeFormatted())
    }

}
