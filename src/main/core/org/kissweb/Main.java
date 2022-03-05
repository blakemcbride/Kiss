/*
 *  Copyright (c) 2006-2015 Blake McBride (blake@mcbridemail.com)
 *  All rights reserved.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright
 *  notice, this list of conditions, and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.kissweb;

import org.kissweb.database.Command;
import org.kissweb.database.Connection;
import org.kissweb.database.Connection.ConnectionType;
import org.kissweb.database.Cursor;
import org.kissweb.database.Record;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Objects;

/**
 * Author: Blake McBride
 * Date: 4/10/20
 * <br><br>
 * This class is only used for command-line execution of Kiss.  So, if
 * Kiss is run from the command-line, this is the class that gets executed.
 * This class is not used in any of the web environments (including develop).
 * <br><br>
 * The command-line jar file is built with:
 *     <code>./bld kisscmd</code>
 * <br><br>
 * This creates the file: build.work/kisscmd.jar
 * <br><br>
 * To execute do:
 *     <code>java -jar kisscmd.jar</code>
 */
public class Main {

    public static void main(String [] argv) throws SQLException, ClassNotFoundException {
        Connection db = new Connection(ConnectionType.PostgreSQL, "localhost", "waytogo", "postgres", "postgres");
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
