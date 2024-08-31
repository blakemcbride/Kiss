package org.kissweb;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ArrayUtilsTest {

    @Test
    void testToPrimitiveByteArray_withValidByteObjects() {
        Byte[] byteObjects = {1, 2, 3, 4, 5};
        byte[] expected = {1, 2, 3, 4, 5};

        byte[] result = ArrayUtils.toPrimitiveByteArray(byteObjects);

        assertArrayEquals(expected, result);
    }

    @Test
    void testToPrimitiveByteArray_withNullByteObject() {
        Byte[] byteObjects = {1, null, 3, null, 5};
        byte[] expected = {1, 0, 3, 0, 5}; // nulls should be converted to 0

        byte[] result = ArrayUtils.toPrimitiveByteArray(byteObjects);

        assertArrayEquals(expected, result);
    }

    @Test
    void testToPrimitiveByteArray_withEmptyArray() {
        Byte[] byteObjects = {};
        byte[] expected = {};

        byte[] result = ArrayUtils.toPrimitiveByteArray(byteObjects);

        assertArrayEquals(expected, result);
    }

    @Test
    void testToPrimitiveByteArray_withNullArray() {
        Byte[] byteObjects = null;

        byte[] result = ArrayUtils.toPrimitiveByteArray(byteObjects);

        assertNull(result);
    }

    @Test
    void testToWrapperByteArray_withValidBytesPrimitive() {
        byte[] bytesPrimitive = {1, 2, 3, 4, 5};
        Byte[] expected = {1, 2, 3, 4, 5};

        Byte[] result = ArrayUtils.toWrapperByteArray(bytesPrimitive);

        assertArrayEquals(expected, result);
    }

    @Test
    void testToWrapperByteArray_withEmptyArray() {
        byte[] bytesPrimitive = {};
        Byte[] expected = {};

        Byte[] result = ArrayUtils.toWrapperByteArray(bytesPrimitive);

        assertArrayEquals(expected, result);
    }

    @Test
    void testToWrapperByteArray_withNullArray() {
        byte[] bytesPrimitive = null;

        Byte[] result = ArrayUtils.toWrapperByteArray(bytesPrimitive);

        assertNull(result);
    }
}
