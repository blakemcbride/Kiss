package org.kissweb;

import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Metadata;
import com.drew.metadata.exif.ExifIFD0Directory;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageTree;
import org.apache.pdfbox.pdmodel.PDResources;
import org.apache.pdfbox.pdmodel.graphics.PDXObject;
import org.apache.pdfbox.pdmodel.graphics.form.PDFormXObject;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;

/**
 * Utility class for image manipulation operations including resizing, rotating,
 * EXIF orientation correction, and PDF image compression.
 *
 * <p>Supported raster formats for resize/rotate operations include JPEG, BMP, PNG, and GIF.
 * PDF compression works by recompressing every embedded image in each page at the
 * specified quality level.</p>
 *
 * <p>Author: Blake McBride<br>
 * Date: 2/23/19</p>
 */
public class Image {

    /** Utility class — not intended for instantiation. */
    private Image() {}

    /**
     * Compress an image byte array.
     *
     * @param type    files type "jpg", "png", etc.
     * @param inbytea the uncompress image
     * @param quality a number between 0 and 1.  Smaller means more compression.
     * @return the compressed image
     * @throws IOException when error
     */
    public static byte[] compressImage(String type, byte[] inbytea, float quality) throws IOException {
        byte[] res;

        if (type == null)
            return inbytea;
        if ("pdf".equalsIgnoreCase(type))
            return shrinkPDF(quality, inbytea);

        // get all image writers for JPG format
        Iterator<ImageWriter> writers = ImageIO.getImageWritersByFormatName(type);

        if (!writers.hasNext())  // can't compress
            return inbytea;

        try (InputStream is = new ByteArrayInputStream(inbytea); ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            // create a BufferedImage as the result of decoding the supplied InputStream
            BufferedImage image = ImageIO.read(is);

            ImageWriter writer = null;
            ImageOutputStream ios = null;
            try {
                writer = writers.next();
                ios = ImageIO.createImageOutputStream(os);
                writer.setOutput(ios);

                ImageWriteParam param = writer.getDefaultWriteParam();

                // compress to a given quality
                param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
                param.setCompressionQuality(quality);

                // appends a complete image stream containing a single image and
                //associated stream and image metadata and thumbnails to the output
                writer.write(null, new IIOImage(image, null, null), param);
                res = os.toByteArray();
            } finally {
                if (ios != null)
                    ios.close();
                if (writer != null)
                    writer.dispose();
            }
        }
        return res;
    }

    private static byte[] shrinkPDF(float compQual, byte[] input) throws IOException {
        final PDDocument doc = Loader.loadPDF(input);
        try {
            final PDPageTree pages = doc.getDocumentCatalog().getPages();
            for (Object p : pages) {
                if (!(p instanceof PDPage))
                    continue;
                PDPage page = (PDPage) p;
                scanResources(page.getResources(), doc, compQual);
            }
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            doc.save(os);
            return os.toByteArray();
        } finally {
            if (doc != null)
                doc.close();
        }
    }

    private static void scanResources(final PDResources rList, final PDDocument doc, float compQual) throws IOException {
        if (rList == null)
            return;
        final Iterable<COSName> names = rList.getXObjectNames();
        for (COSName name : names) {
            final PDXObject xObj = rList.getXObject(name);
            if (xObj instanceof PDFormXObject)
                scanResources(((PDFormXObject) xObj).getResources(), doc, compQual);
            if (!(xObj instanceof PDImageXObject))
                continue;
            PDImageXObject img = (PDImageXObject) xObj;
            final Iterator<ImageWriter> jpgWriters = ImageIO.getImageWritersByFormatName("jpeg");
            final ImageWriter jpgWriter = jpgWriters.next();
            final ImageWriteParam iwp = jpgWriter.getDefaultWriteParam();
            iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
            iwp.setCompressionQuality(compQual);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            jpgWriter.setOutput(ImageIO.createImageOutputStream(baos));
            jpgWriter.write(null, new IIOImage(img.getImage(), null, null), iwp);
            PDImageXObject im = PDImageXObject.createFromByteArray(doc, baos.toByteArray(), name.getName());
            rList.put(name, im);
        }
    }

    /**
     * Write a byte array to a file, creating or overwriting the file at the given path.
     *
     * @param fname the path of the file to write
     * @param out   the bytes to write to the file
     * @throws IOException if the file cannot be written
     */
    public static void writefile(String fname, byte[] out) throws IOException {
        Files.write(Paths.get(fname), out);
    }

    /**
     * Read the entire contents of a file into a byte array.
     *
     * @param fname the path of the file to read
     * @return the complete file contents as a byte array
     * @throws IOException if the file cannot be read
     */
    public static byte[] readfile(String fname) throws IOException {
        return Files.readAllBytes(Paths.get(fname));
    }

    // convert BufferedImage to byte[]
    private static byte[] toByteArray(BufferedImage bi, String format) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(bi, format, baos);
        byte[] bytes = baos.toByteArray();
        return bytes;
    }

    // convert byte[] to BufferedImage
    private static BufferedImage toBufferedImage(byte[] bytes) throws IOException {
        InputStream is = new ByteArrayInputStream(bytes);
        BufferedImage bi = ImageIO.read(is);
        return bi;
    }

    /**
     * Reduce the size of JPEG, BMP, PNG,and GIF files.
     *
     * <code>size</code> is the resulting size of the larger axis (height or width)
     * <p>
     * The aspect ratio is retained.
     * <p>
     * If the type is not handled, the image passed is simply returned.
     *
     * @param in   the raw image bytes to resize
     * @param type file type; JPG, JPEG, BMP, PNG, or GIF (case doesn't matter)
     * @param size the target size in pixels for the longer axis (height or width)
     * @return the resized image bytes, or the original bytes if no resize was needed or the type is unsupported
     */
    public static byte[] resizeImage(byte[] in, String type, int size) {
        if (type == null)
            return in;
        type = type.toLowerCase();
        if (in == null || !type.equals("jpg") && !type.equals("jpeg") && !type.equals("bmp") && !type.equals("png") && !type.equals("gif"))
            return in;
        try {
            BufferedImage bi = toBufferedImage(in);
            int h = bi.getHeight();
            int w = bi.getWidth();
            int h2, w2;
            int s2 = (int) ((double) size * 1.1);  // allow some flex
            if (h < s2 && w < s2)
                return in;
            if (h > w) {
                h2 = size;
                w2 = (w * h2) / h;
            } else {
                w2 = size;
                h2 = (h * w2) / w;
            }
            java.awt.Image im = bi.getScaledInstance(w2, h2, java.awt.Image.SCALE_DEFAULT);
            BufferedImage bi2 = new BufferedImage(w2, h2, BufferedImage.TYPE_INT_RGB);
            bi2.getGraphics().drawImage(im, 0, 0, null);
            return toByteArray(bi2, type);
        } catch (IOException e) {
            e.printStackTrace();
            return in;
        }
    }

    /**
     * Extract the file extension (including the dot) from a file name, lower-cased.
     * Returns an empty string if the file name has no extension.
     */
    private static String fileExtension(String fname) {
        if (fname == null || fname.isEmpty())
            return "";
        int lastPeriodIndex = fname.lastIndexOf('.');
        if (lastPeriodIndex == -1)
            return "";
        return fname.substring(lastPeriodIndex).toLowerCase();
    }

    /**
     * Rotate the image stored in the given file 90 degrees counter-clockwise (left),
     * writing the result back to the same file.
     * Any exception during processing is silently ignored and the file is left unchanged.
     *
     * @param fileName the path of the image file to rotate
     */
    public static void rotateLeft(String fileName) {
        try {
            final String ext = fileExtension(fileName);

            byte[] image = FileUtils.readFileBytes(fileName);
            ByteArrayInputStream bis = new ByteArrayInputStream(image);
            BufferedImage bImage = ImageIO.read(bis);

            int w = bImage.getWidth();
            int h = bImage.getHeight();
            BufferedImage dest = new BufferedImage(h, w, bImage.getType());
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                    dest.setRGB(y, w - x - 1, bImage.getRGB(x, y));

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final String ext2 = ext.length() > 1 ? ext.substring(1) : ext; // remove .
            ImageIO.write(dest, ext2, bos);
            image = bos.toByteArray();

            FileUtils.write(fileName, image);
        } catch (Exception ignored) {

        }
    }

    /**
     * Rotate the image stored in the given file 90 degrees clockwise (right),
     * writing the result back to the same file.
     * Any exception during processing is silently ignored and the file is left unchanged.
     *
     * @param fileName the path of the image file to rotate
     */
    public static void rotateRight(String fileName) {
        try {
            final String ext = fileExtension(fileName);

            byte[] image = FileUtils.readFileBytes(fileName);
            ByteArrayInputStream bis = new ByteArrayInputStream(image);
            BufferedImage bImage = ImageIO.read(bis);

            BufferedImage dest = rotateImage(bImage, 90);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final String ext2 = ext.length() > 1 ? ext.substring(1) : ext; // remove .
            ImageIO.write(dest, ext2, bos);
            image = bos.toByteArray();

            FileUtils.write(fileName, image);
        } catch (Exception ignored) {

        }
    }

    /**
     * Rotate a {@link BufferedImage} by an arbitrary angle.
     *
     * <p>For 90-degree and 270-degree rotations the output dimensions are swapped
     * (width becomes height and vice versa). For all other angles the output
     * dimensions match the input.</p>
     *
     * @param src           the source image to rotate
     * @param rotationAngle the clockwise rotation angle in degrees (e.g. 90, 180, 270)
     * @return a new {@link BufferedImage} containing the rotated image
     */
    public static BufferedImage rotateImage(BufferedImage src, int rotationAngle) {
        double theta = (Math.PI * 2) / 360 * rotationAngle;
        int width = src.getWidth();
        int height = src.getHeight();
        BufferedImage dest;
        if (rotationAngle == 90 || rotationAngle == 270) {
            dest = new BufferedImage(src.getHeight(), src.getWidth(), src.getType());
        } else {
            dest = new BufferedImage(src.getWidth(), src.getHeight(), src.getType());
        }

        Graphics2D graphics2D = dest.createGraphics();

        if (rotationAngle == 90) {
            graphics2D.translate((height - width) / 2, (height - width) / 2);
            graphics2D.rotate(theta, height / 2, width / 2);
        } else if (rotationAngle == 270) {
            graphics2D.translate((width - height) / 2, (width - height) / 2);
            graphics2D.rotate(theta, height / 2, width / 2);
        } else {
            graphics2D.translate(0, 0);
            graphics2D.rotate(theta, width / 2, height / 2);
        }
        graphics2D.drawRenderedImage(src, null);
        return dest;
    }

    /**
     * Read the EXIF orientation tag from raw image bytes.
     * Returns the EXIF orientation value (1-8), where 1 means no rotation needed.
     * Returns 1 on any failure (missing EXIF, no orientation tag, unsupported format, etc.).
     *
     * @param imageData the raw image bytes
     * @return the EXIF orientation value (1-8); 1 is the identity/no-op value
     */
    public static int getExifOrientation(byte[] imageData) {
        try {
            Metadata md = ImageMetadataReader.readMetadata(new ByteArrayInputStream(imageData));
            ExifIFD0Directory dir = md.getFirstDirectoryOfType(ExifIFD0Directory.class);
            if (dir == null)
                return 1;
            if (!dir.containsTag(ExifIFD0Directory.TAG_ORIENTATION))
                return 1;
            return dir.getInt(ExifIFD0Directory.TAG_ORIENTATION);
        } catch (Exception e) {
            return 1;
        }
    }

    /**
     * Apply EXIF orientation transformation to a BufferedImage.
     * EXIF orientation values:
     * 1 = Normal (no rotation needed)
     * 2 = Flip horizontal
     * 3 = Rotate 180 degrees
     * 4 = Flip vertical
     * 5 = Rotate 90 degrees CW + flip horizontal
     * 6 = Rotate 90 degrees CW
     * 7 = Rotate 90 degrees CCW + flip horizontal
     * 8 = Rotate 90 degrees CCW
     *
     * @param image the BufferedImage to transform
     * @param orientation the EXIF orientation value (1-8)
     * @return the transformed BufferedImage
     */
    public static BufferedImage applyExifOrientation(BufferedImage image, int orientation) {
        if (orientation == 1 || orientation < 1 || orientation > 8) {
            return image;
        }

        int width = image.getWidth();
        int height = image.getHeight();
        BufferedImage result;

        switch (orientation) {
            case 2:
                result = new BufferedImage(width, height, image.getType());
                for (int y = 0; y < height; y++)
                    for (int x = 0; x < width; x++)
                        result.setRGB(width - x - 1, y, image.getRGB(x, y));
                return result;

            case 3:
                return rotateImage(image, 180);

            case 4:
                result = new BufferedImage(width, height, image.getType());
                for (int y = 0; y < height; y++)
                    for (int x = 0; x < width; x++)
                        result.setRGB(x, height - y - 1, image.getRGB(x, y));
                return result;

            case 5:
                BufferedImage rotated5 = rotateImage(image, 90);
                result = new BufferedImage(rotated5.getWidth(), rotated5.getHeight(), rotated5.getType());
                for (int y = 0; y < rotated5.getHeight(); y++)
                    for (int x = 0; x < rotated5.getWidth(); x++)
                        result.setRGB(rotated5.getWidth() - x - 1, y, rotated5.getRGB(x, y));
                return result;

            case 6:
                return rotateImage(image, 90);

            case 7:
                BufferedImage rotated7 = rotateImage(image, 270);
                result = new BufferedImage(rotated7.getWidth(), rotated7.getHeight(), rotated7.getType());
                for (int y = 0; y < rotated7.getHeight(); y++)
                    for (int x = 0; x < rotated7.getWidth(); x++)
                        result.setRGB(rotated7.getWidth() - x - 1, y, rotated7.getRGB(x, y));
                return result;

            case 8:
                return rotateImage(image, 270);

            default:
                return image;
        }
    }

    /**
     * Apply EXIF orientation transformation to image byte array.
     *
     * @param imageData the image byte array
     * @param orientation the EXIF orientation value (1-8)
     * @param format the image format (jpg, png, etc.)
     * @return the transformed image byte array
     * @throws IOException if there is an error processing the image
     */
    public static byte[] applyExifOrientation(byte[] imageData, int orientation, String format) throws IOException {
        if (orientation == 1 || orientation < 1 || orientation > 8) {
            return imageData;
        }

        BufferedImage image = toBufferedImage(imageData);
        BufferedImage transformed = applyExifOrientation(image, orientation);
        return toByteArray(transformed, format);
    }

    /**
     * External tool that will be used to decode HEIC.  Resolved lazily on
     * the first conversion call and cached for the life of the JVM.
     */
    private enum HeicTool {
        SIPS,           // macOS built-in
        HEIF_CONVERT,   // Linux: libheif-tools / libheif-examples
        MAGICK,         // ImageMagick 7 (cross-platform, needs HEIC delegate)
        FFMPEG,         // ffmpeg (cross-platform fallback)
        NONE
    }

    private static HeicTool cachedHeicTool;

    /**
     * Convert HEIC image bytes to PNG image bytes.
     *
     * <p>HEIC decoding requires an HEVC decoder, which no pure-Java library
     * provides at production quality.  This method therefore delegates to
     * whichever external converter is available on the host's <code>PATH</code>.
     * The first available tool from the following list is used:</p>
     *
     * <ol>
     *   <li><code>sips</code> &mdash; macOS (built-in, no install needed)</li>
     *   <li><code>heif-convert</code> &mdash; Linux (<code>libheif-tools</code>
     *       or <code>libheif-examples</code> package)</li>
     *   <li><code>magick</code> &mdash; ImageMagick 7 with the HEIC delegate
     *       (typical Windows install path)</li>
     *   <li><code>ffmpeg</code> &mdash; universal fallback; official prebuilt
     *       binaries are available for Linux, macOS, and Windows</li>
     * </ol>
     *
     * <p>The chosen tool is detected once per JVM and cached.</p>
     *
     * @param heicBytes raw HEIC file bytes
     * @return PNG-encoded image bytes
     * @throws IOException if the input is null, no converter is available on
     *                     <code>PATH</code>, or conversion fails
     */
    public static byte[] convertHeicToPng(byte[] heicBytes) throws IOException {
        if (heicBytes == null)
            throw new IOException("HEIC input bytes are null");
        File in = File.createTempFile("kiss-heic-", ".heic");
        File out = File.createTempFile("kiss-heic-", ".png");
        try {
            Files.write(in.toPath(), heicBytes);
            runHeicToPng(in.getAbsolutePath(), out.getAbsolutePath());
            return Files.readAllBytes(out.toPath());
        } finally {
            in.delete();
            out.delete();
        }
    }

    /**
     * Convert a HEIC file to a PNG file.
     *
     * <p>See {@link #convertHeicToPng(byte[])} for the tool-detection rules
     * and the list of supported external converters.</p>
     *
     * @param heicPath path to the source HEIC file
     * @param pngPath  path of the destination PNG file to create or overwrite
     * @throws IOException if no converter is available or conversion fails
     */
    public static void convertHeicToPng(String heicPath, String pngPath) throws IOException {
        runHeicToPng(heicPath, pngPath);
    }

    private static void runHeicToPng(String heicPath, String pngPath) throws IOException {
        HeicTool tool = detectHeicTool();
        String[] cmd;
        switch (tool) {
            case SIPS:
                cmd = new String[] { "sips", "-s", "format", "png", heicPath, "--out", pngPath };
                break;
            case HEIF_CONVERT:
                cmd = new String[] { "heif-convert", heicPath, pngPath };
                break;
            case MAGICK:
                cmd = new String[] { "magick", heicPath, pngPath };
                break;
            case FFMPEG:
                cmd = new String[] { "ffmpeg", "-y", "-i", heicPath, pngPath };
                break;
            default:
                throw new IOException("No HEIC converter found on PATH. Install one of: "
                        + "sips (macOS built-in), heif-convert (Linux libheif-tools), "
                        + "ImageMagick 7 with HEIC delegate (magick), or ffmpeg.");
        }
        ProcessBuilder pb = new ProcessBuilder(cmd).redirectErrorStream(true);
        Process p = pb.start();
        byte[] output;
        try (InputStream in = p.getInputStream()) {
            output = readFully(in);
        }
        boolean done;
        try {
            done = p.waitFor(5, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            p.destroyForcibly();
            throw new IOException("HEIC conversion interrupted", e);
        }
        if (!done) {
            p.destroyForcibly();
            throw new IOException("HEIC conversion timed out: " + cmd[0]);
        }
        if (p.exitValue() != 0)
            throw new IOException("HEIC conversion failed (" + cmd[0] + " exit "
                    + p.exitValue() + "): " + new String(output, StandardCharsets.UTF_8));
    }

    private static synchronized HeicTool detectHeicTool() {
        if (cachedHeicTool != null)
            return cachedHeicTool;
        if (probeCommand("sips", "--version"))
            cachedHeicTool = HeicTool.SIPS;
        else if (probeCommand("heif-convert", "--version"))
            cachedHeicTool = HeicTool.HEIF_CONVERT;
        else if (probeCommand("magick", "-version"))
            cachedHeicTool = HeicTool.MAGICK;
        else if (probeCommand("ffmpeg", "-version"))
            cachedHeicTool = HeicTool.FFMPEG;
        else
            cachedHeicTool = HeicTool.NONE;
        return cachedHeicTool;
    }

    private static boolean probeCommand(String... args) {
        try {
            Process p = new ProcessBuilder(args).redirectErrorStream(true).start();
            try (InputStream in = p.getInputStream()) {
                readFully(in);
            }
            if (!p.waitFor(10, TimeUnit.SECONDS))
                p.destroyForcibly();
            return true;
        } catch (IOException e) {
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    private static byte[] readFully(InputStream in) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        byte[] buf = new byte[4096];
        int n;
        while ((n = in.read(buf)) > 0)
            bos.write(buf, 0, n);
        return bos.toByteArray();
    }
}
