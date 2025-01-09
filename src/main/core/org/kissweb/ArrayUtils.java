package org.kissweb;

/**
 * Utility methods to help deal with arrays.
 *
 * Author: Blake McBride
 * Date: 11/5/23
 */
public class ArrayUtils {

    /**
     * Converts a Byte array to a byte array.
     *
     * @param byteObjects The Byte array to convert.
     * @return A primitive byte array corresponding to the Byte array.
     */
    public static byte[] toPrimitiveByteArray(Byte[] byteObjects) {
        if (byteObjects == null)
            return null;

        final byte[] bytesPrimitive = new byte[byteObjects.length];
        for (int i = 0; i < byteObjects.length; i++)
            // Handle nulls to avoid NullPointerException
            bytesPrimitive[i] = (byteObjects[i] != null) ? byteObjects[i] : 0;
        return bytesPrimitive;
    }

    /**
     * Converts a byte array to a Byte array.
     *
     * @param bytesPrimitive The primitive byte array to convert.
     * @return A Byte array corresponding to the primitive byte array.
     */
    public static Byte[] toWrapperByteArray(byte[] bytesPrimitive) {
        if (bytesPrimitive == null)
            return null;

        final Byte[] byteObjects = new Byte[bytesPrimitive.length];
        for (int i = 0; i < bytesPrimitive.length; i++)
            byteObjects[i] = bytesPrimitive[i];  // Autoboxing
        return byteObjects;
    }

}
