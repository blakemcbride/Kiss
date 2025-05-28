
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.DelimitedFileReader

class FileUpload {

    public static void main(String[] argv) {

        String tabName = argv[0]

        DelimitedFileReader fileReader = new DelimitedFileReader(tabName)
        Connection connection = new Connection(Connection.ConnectionType.PostgreSQL, "localhost", "temp", "postgres", "postgres")
        int i = 0
        fileReader.readHeader()

        while (fileReader.nextLine()) {
            Record record = connection.newRecord("leads")
            record.set("first_name", fileReader.getString("First Name"))
            record.set("last_name", fileReader.getString("Last Name"))
            record.set("company_name", fileReader.getString("Company Name - Cleaned"))
            record.set("title", fileReader.getString("Title"))
            record.set("email_address", fileReader.getString("Email 1"))
            record.set("company_website", fileReader.getString("Website"))
            record.set("linkedin_url", fileReader.getString("Contact LI Profile URL"))
            record.set("contact_phone", fileReader.getString("Contact Phone 1"))
            record.set("company_phone", fileReader.getString("Company Phone 1"))
            record.set("location", fileReader.getString("Company Location"))
            record.set("location", fileReader.getString("Company Street 1"))
            record.set("country", fileReader.getString("Company Country (Alpha 2)"))
            //record.set("country", fileReader.getString("Company Country (Alpha 3)"))
            record.set("industry", fileReader.getString("Company Industry"))
            record.set("state", fileReader.getString("Company State Abbr"))
            record.set("size", fileReader.getInt("Company Staff Count"))
            record.set("source_tab", tabName)

            record.addRecord()
            if (++i % 40 == 0)
                connection.commit()
        }

        connection.commit()
        connection.close()
    }
}
