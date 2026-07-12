package org.kissweb;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.HexFormat;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link Hash}, using published known-answer digest vectors:
 * <ul>
 *   <li>FIPS 180-4 (NIST) SHA-256/384/512 test vectors for the ASCII inputs {@code "abc"} and
 *       {@code ""} (the empty string).</li>
 *   <li>NIST SHA-3 (FIPS 202) SHA3-256/512 test vectors for the same two inputs.</li>
 * </ul>
 * All vectors below were independently cross-checked (RFC/NIST-published hex compared against
 * Python's standard-library {@code hashlib}) before being hardcoded here.
 */
class HashTest {

    private static final String ABC = "abc";
    private static final String EMPTY = "";

    // FIPS 180-4 / SHA-3 known-answer values for "abc"
    private static final String ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";
    private static final String ABC_SHA384 = "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7";
    private static final String ABC_SHA512 = "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f";
    private static final String ABC_SHA3_256 = "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532";
    private static final String ABC_SHA3_512 = "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0";

    // Known-answer values for the empty string
    private static final String EMPTY_SHA256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    private static final String EMPTY_SHA384 = "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b";
    private static final String EMPTY_SHA512 = "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e";
    private static final String EMPTY_SHA3_256 = "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a";
    private static final String EMPTY_SHA3_512 = "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26";

    @Test
    void sha256_abc_matchesKnownAnswer() {
        assertEquals(ABC_SHA256, Hash.sha256Hex(ABC));
        assertArrayEquals(HexFormat.of().parseHex(ABC_SHA256), Hash.sha256(ABC));
    }

    @Test
    void sha384_abc_matchesKnownAnswer() {
        assertEquals(ABC_SHA384, Hash.sha384Hex(ABC));
    }

    @Test
    void sha512_abc_matchesKnownAnswer() {
        assertEquals(ABC_SHA512, Hash.sha512Hex(ABC));
    }

    @Test
    void sha3_256_abc_matchesKnownAnswer() {
        assertEquals(ABC_SHA3_256, Hash.sha3_256Hex(ABC));
    }

    @Test
    void sha3_512_abc_matchesKnownAnswer() {
        assertEquals(ABC_SHA3_512, Hash.sha3_512Hex(ABC));
    }

    @Test
    void allAlgorithms_emptyString_matchKnownAnswers() {
        assertEquals(EMPTY_SHA256, Hash.sha256Hex(EMPTY));
        assertEquals(EMPTY_SHA384, Hash.sha384Hex(EMPTY));
        assertEquals(EMPTY_SHA512, Hash.sha512Hex(EMPTY));
        assertEquals(EMPTY_SHA3_256, Hash.sha3_256Hex(EMPTY));
        assertEquals(EMPTY_SHA3_512, Hash.sha3_512Hex(EMPTY));
    }

    @Test
    void byteArrayAndStringOverloads_agree() {
        assertArrayEquals(Hash.sha256(ABC.getBytes(StandardCharsets.UTF_8)), Hash.sha256(ABC));
        assertArrayEquals(Hash.sha384(ABC.getBytes(StandardCharsets.UTF_8)), Hash.sha384(ABC));
        assertArrayEquals(Hash.sha512(ABC.getBytes(StandardCharsets.UTF_8)), Hash.sha512(ABC));
        assertArrayEquals(Hash.sha3_256(ABC.getBytes(StandardCharsets.UTF_8)), Hash.sha3_256(ABC));
        assertArrayEquals(Hash.sha3_512(ABC.getBytes(StandardCharsets.UTF_8)), Hash.sha3_512(ABC));
    }

    @Test
    void base64Encoding_decodesBackToSameDigestBytes() throws Exception {
        String b64 = Hash.sha256Base64(ABC);
        assertArrayEquals(Hash.sha256(ABC), Base64.decode(b64));
    }

    @Test
    void hexEncoding_isLowercase() {
        String hex = Hash.sha256Hex(ABC);
        assertEquals(hex.toLowerCase(), hex);
    }

    @Test
    void digestLengths_areCorrect() {
        assertEquals(32, Hash.sha256(ABC).length);
        assertEquals(48, Hash.sha384(ABC).length);
        assertEquals(64, Hash.sha512(ABC).length);
        assertEquals(32, Hash.sha3_256(ABC).length);
        assertEquals(64, Hash.sha3_512(ABC).length);
    }

    @Test
    void hashFile_streamedResult_matchesInMemoryDigest(@TempDir Path tempDir) throws Exception {
        File file = tempDir.resolve("hash-test.bin").toFile();
        byte[] content = "The quick brown fox jumps over the lazy dog.".getBytes(StandardCharsets.UTF_8);
        try (FileOutputStream fos = new FileOutputStream(file)) {
            fos.write(content);
        }
        byte[] expected = Hash.sha256(content);
        assertArrayEquals(expected, Hash.hashFile(file, "SHA-256"));
        assertEquals(HexFormat.of().formatHex(expected), Hash.hashFileHex(file, "SHA-256"));
    }

    @Test
    void hash_streamOverload_matchesInMemoryDigest() throws Exception {
        byte[] content = "streaming input for Hash.hash(InputStream, algorithm)".getBytes(StandardCharsets.UTF_8);
        byte[] expected = Hash.sha256(content);
        try (java.io.ByteArrayInputStream in = new java.io.ByteArrayInputStream(content)) {
            assertArrayEquals(expected, Hash.hash(in, "SHA-256"));
        }
    }
}
