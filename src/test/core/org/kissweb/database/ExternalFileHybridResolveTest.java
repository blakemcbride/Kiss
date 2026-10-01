package org.kissweb.database;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Plain (no database) tests for {@link ExternalFile#isExternal} and
 * {@link ExternalFile#resolveHybridColumn}, which work on an already-fetched raw value and never
 * query the connection (so a null connection is passed).
 */
class ExternalFileHybridResolveTest {

    private static final String MARKER = "\u0001KFX\u0001";
    private static final String ESCAPE = "\u0001KFI\u0001";

    @TempDir
    Path tempDir;

    @BeforeEach
    void setUp() {
        ExternalFile.setRootSupplier(() -> tempDir.toString());
        ExternalFile.setDirectoryMapper((table, pk) -> table + "/" + pk);
    }

    @AfterEach
    void tearDown() {
        ExternalFile.setRootSupplier(null);
        ExternalFile.setDirectoryMapper(null);
    }

    @Test
    void testIsExternal() {
        assertTrue(ExternalFile.isExternal(MARKER));
        assertFalse(ExternalFile.isExternal(null));
        assertFalse(ExternalFile.isExternal(""));
        assertFalse(ExternalFile.isExternal("hello"));
        assertFalse(ExternalFile.isExternal(ESCAPE + MARKER), "escaped data that looks like the marker is inline data");
        assertFalse(ExternalFile.isExternal(MARKER + "x"));
    }

    @Test
    void testResolveInlinePaths() throws Exception {
        assertNull(ExternalFile.resolveHybridColumn(null, "t", "c", "1", null));
        assertEquals("", ExternalFile.resolveHybridColumn(null, "t", "c", "1", ""));
        assertEquals("hello", ExternalFile.resolveHybridColumn(null, "t", "c", "1", "hello"));
        assertEquals("abc", ExternalFile.resolveHybridColumn(null, "t", "c", "1", ESCAPE + "abc"));
        assertEquals(MARKER, ExternalFile.resolveHybridColumn(null, "t", "c", "1", ESCAPE + MARKER));
        assertEquals(ESCAPE + "z", ExternalFile.resolveHybridColumn(null, "t", "c", "1", ESCAPE + ESCAPE + "z"));
        assertEquals("\u0001other", ExternalFile.resolveHybridColumn(null, "t", "c", "1", "\u0001other"));
    }

    @Test
    void testResolveExternal() throws Exception {
        assertEquals("", ExternalFile.resolveHybridColumn(null, "t", "c", "7", MARKER), "missing file yields empty string");
        File f = new File(tempDir.toFile(), "t/7/7-c.col");
        f.getParentFile().mkdirs();
        Files.writeString(f.toPath(), "big data");
        assertEquals("big data", ExternalFile.resolveHybridColumn(null, "t", "c", "7", MARKER));
    }
}
