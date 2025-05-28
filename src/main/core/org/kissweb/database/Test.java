package org.kissweb.database;

/**
 * User: Blake McBride
 * Date: 9/30/16
 */
public class Test {


    /**
     * Main method for testing database connectivity.
     *
     * @param argv command line arguments
     */
    public static void main(String [] argv) {
        Connection DB;
        Command cmd = null;
        Cursor cursor = null;
        String pw;

        try {
            DB = new Connection(Connection.ConnectionType.PostgreSQL, "localhost", null, "fos", "postgres", "postgres");
            cmd = DB.newCommand();
            Record rec;

            cursor = cmd.query("select username, password, role_type from person p left join prole r on p.rid = r.rid where username=?", "root");
            if ((rec=cursor.fetchOne()) == null)
                throw new Exception("User not found");
            pw = (String) rec.get("password");


            cursor = cmd.query("select username, password, role_type from person p left join prole r on p.rid = r.rid where username=?", "root");
            if ((rec=cursor.fetchOne()) == null)
                throw new Exception("User not found");
            pw = (String) rec.get("password");


            cursor.close();
            cmd.close();
            DB.close();

        } catch (Exception e)  {
            e.printStackTrace();
        }
    }
}
