package org.kissweb;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class IniFileTest {

     private IniFile iniFile;
     private final String testFilename = "test.ini";

     @BeforeEach
     void setUp() {
         iniFile = new IniFile();
     }

     @Test
     void testCreateIniFile() {
         iniFile.put("section1", "key1", "value1");
         iniFile.put("section1", "key2", "value2");

         assertEquals("value1", iniFile.get("section1", "key1"));
         assertEquals("value2", iniFile.get("section1", "key2"));
     }

     @Test
     void testGetInt() {
         iniFile.put("section1", "key1", 42);

         assertEquals(42, iniFile.getInt("section1", "key1"));
     }

     @Test
     void testGetChar() {
         iniFile.put("section1", "key1", 'A');

         assertEquals('A', iniFile.getChar("section1", "key1"));
     }

     @Test
     void testGetBoolean() {
         iniFile.put("section1", "key1", true);
         iniFile.put("section1", "key2", "yes");
         iniFile.put("section1", "key3", "1");
         iniFile.put("section1", "key4", "false");

         assertTrue(iniFile.getBoolean("section1", "key1"));
         assertTrue(iniFile.getBoolean("section1", "key2"));
         assertTrue(iniFile.getBoolean("section1", "key3"));
         assertFalse(iniFile.getBoolean("section1", "key4"));
     }

     @Test
     void testGetDouble() {
         iniFile.put("section1", "key1", 3.14);

         assertEquals(3.14, iniFile.getDouble("section1", "key1"));
     }

     @Test
     void testGetDateInt() {
         iniFile.put("section1", "key1", "8/30/2024");

         // Assuming DateUtils.parse(s) returns an integer representation of the date.
         assertEquals(20240830, iniFile.getDateInt("section1", "key1"));
     }

     @Test
     void testGetTimeInt() {
         iniFile.put("section1", "key1", 1230);

         // Assuming TimeUtils.parse(s) returns an integer representation of the time.
         assertEquals(1230, iniFile.getTimeInt("section1", "key1"));
     }

     @Test
     void testRemoveValue() {
         iniFile.put("section1", "key1", "value1");
         iniFile.removeValue("section1", "key1");

         assertNull(iniFile.get("section1", "key1"));
     }

     @Test
     void testRemoveKey() {
         iniFile.put("section1", "key1", "value1");
         iniFile.removeKey("section1", "key1");

         assertNull(iniFile.get("section1", "key1"));
     }

     @Test
     void testSaveAndLoadIniFile() throws IOException {
         iniFile.put("section1", "key1", "value1");
         iniFile.put("section1", "key2", "value2");
         iniFile.save(testFilename);

         IniFile loadedIniFile = IniFile.load(testFilename);
         assertNotNull(loadedIniFile);
         assertEquals("value1", loadedIniFile.get("section1", "key1"));
         assertEquals("value2", loadedIniFile.get("section1", "key2"));

         // Clean up
         new File(testFilename).delete();
     }

     @Test
     void testGetAndPutInNullSection() {
         iniFile.put("key1", "value1");

         assertEquals("value1", iniFile.get("key1"));
     }

     @Test
     void testPutWithVariousDataTypes() {
         iniFile.put("section1", "intKey", 42);
         iniFile.put("section1", "doubleKey", 3.14);
         iniFile.put("section1", "booleanKey", true);
         iniFile.put("section1", "charKey", 'C');
         iniFile.put("section1", "longKey", 123456789L);
         iniFile.put("section1", "dateKey", new Date());

         assertEquals(42, iniFile.getInt("section1", "intKey"));
         assertEquals(3.14, iniFile.getDouble("section1", "doubleKey"));
         assertTrue(iniFile.getBoolean("section1", "booleanKey"));
         assertEquals('C', iniFile.getChar("section1", "charKey"));
         assertEquals(123456789L, Long.parseLong(iniFile.get("section1", "longKey")));
     }
}
