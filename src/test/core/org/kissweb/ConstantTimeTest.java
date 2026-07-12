package org.kissweb;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link ConstantTime}.
 * <br><br>
 * As documented on {@link ConstantTime#equals(byte[], byte[])}, a genuine timing side-channel
 * measurement is not practical or reliable inside a unit test - these tests cover functional
 * correctness only (the real safeguard is the branchless, no-early-return implementation itself).
 */
class ConstantTimeTest {

    @Test
    void equalContent_differentArrayInstances_areEqual() {
        byte[] a = {1, 2, 3, 4, 5};
        byte[] b = {1, 2, 3, 4, 5};
        assertNotSame(a, b);
        assertTrue(ConstantTime.equals(a, b));
    }

    @Test
    void sameArrayInstance_isEqualToItself() {
        byte[] a = {9, 8, 7};
        assertTrue(ConstantTime.equals(a, a));
    }

    @Test
    void singleBitDifference_isNotEqual() {
        byte[] a = {0b0000_0001, 2, 3};
        byte[] b = {0b0000_0000, 2, 3};
        assertFalse(ConstantTime.equals(a, b));
    }

    @Test
    void differenceInLastByte_isNotEqual() {
        byte[] a = {1, 2, 3};
        byte[] b = {1, 2, 4};
        assertFalse(ConstantTime.equals(a, b));
    }

    @Test
    void differentLength_isNotEqual() {
        byte[] a = {1, 2, 3};
        byte[] b = {1, 2, 3, 4};
        assertFalse(ConstantTime.equals(a, b));
        assertFalse(ConstantTime.equals(b, a));
    }

    @Test
    void emptyArrays_areEqual() {
        assertTrue(ConstantTime.equals(new byte[0], new byte[0]));
    }

    @Test
    void allZeroBytesOfSameLength_areEqual() {
        assertTrue(ConstantTime.equals(new byte[32], new byte[32]));
    }
}
