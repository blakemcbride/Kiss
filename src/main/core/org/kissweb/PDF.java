package org.kissweb;


import org.apache.log4j.Logger;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.PDPageContentStream.AppendMode;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDFont;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.font.Standard14Fonts;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.util.Matrix;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

/**
 * This class creates PDF files with text, images, and line graphics.
 * <br><br>
 * There are two classes in Kiss used to create PDF reports: PDF and Groff.
 * PDF is used when graphics and images are needed.  Groff is used when text and tables are needed.
 * It is possible to use both in the same report by creating the PDF with one and appending with the other.
 *
 * Author: Blake McBride
 * Date: 3/5/16
 */
public class PDF {
    private final static Logger logger = Logger.getLogger(PDF.class);

    private static final PDType1Font COURIER_FONT = new PDType1Font(Standard14Fonts.FontName.COURIER);

    private PDDocument doc;
    private PDPageContentStream contentStream = null;
    private float posx=0, posy=0;
    private float fontSize = 11f;
    private PDFont font = COURIER_FONT;
    private PDPage page;
    private boolean landscape = false;
    private float pageHeight, pageWidth;
    private final String outputFilename;
    private PDRectangle pageSize = PDRectangle.LETTER;
    private boolean inText = false;
    private float defaultLineThickness = 1f;

    /**
     * Begin a new PDF file
     *
     * @param fname the file name to be saved to (include the .pdf)
     */
    public PDF(String fname) {
        outputFilename = fname;
        doc = new PDDocument();
    }

    /**
     * Begin a new PDF file with an existing PDF file as its starting point.
     *
     * @param infile the name of the input PDF template file
     * @param outfile the file name to be saved to (include the .pdf)
     * @throws IOException if an error occurs loading the input file
     */
    public PDF(String infile, String outfile) throws IOException {
        outputFilename = outfile;
        doc = Loader.loadPDF(new File(infile));
    }

    /**
     * Set font style and size
     *
     * @param fnt font style
     * @param fs font size in points
     */
    public void setFont(PDFont fnt, float fs) {
        font = fnt;
        fontSize = fs;
        if (contentStream != null) {
            try {
                contentStream.setFont(font, fontSize);
            } catch (IOException e) {
                logger.error("Error", e);
            }
        }
    }

    /**
     * Set the page orientation to landscape.
     */
    public void landscape() {
        landscape = true;
    }

    /**
     * Set the page orientation to portrait.
     */
    public void portrait() {
        landscape = false;
    }

    /**
     * Set the default line thickness.  The default is 1.
     *
     * @param thickness the line thickness in points
     * @return the old line thickness
     */
    public float setDefaultLineThickness(float thickness) {
        final float old = defaultLineThickness;
        defaultLineThickness = thickness;
        return old;
    }

    /**
     * Set the page size.
     *
     * @param ps the page size rectangle
     * @return the previous page size
     */
    public PDRectangle setPageSize(PDRectangle ps) {
        PDRectangle old = pageSize;
        pageSize = ps;
        return old;
    }

    /**
     * Output txt at line y, column x
     * Lines and column numbers take font into account so, for example, typically letter paper would
     * give 66 lines and 80 columns.
     *
     * @param y absolute line position top to bottom, line 1 to line ...
     * @param x absolute column position left to right, column 1 to ...
     * @param txt the text to be written
     */
    public void textOut(int y, int x, String txt) {
        float absx, absy, relx, rely;
        if (fontSize > 11.9f  &&  fontSize < 12.1f)
            absx = (x+1) * 7.2f;  //  12pt Courier font
        else  if (fontSize > 10.9f  &&  fontSize < 11.1f)
            absx = (x+3) * 6.6f;  //  11pt Courier font
        else
            absx = (x+6) * 6f;  //  10pt Courier font
        absy = pageHeight - (y * 12f) + 2f;
        try {
            startText();
            relx = absx - posx;
            rely = absy - posy;
            contentStream.newLineAtOffset(relx, rely);
            contentStream.showText(txt);
        } catch (Exception e) {
            logger.error("Error", e);
        }
        posx = absx;
        posy = absy;
    }

    /**
     * Start output of text.
     * <br><br>
     * You cannot intermix graphics with text output (although both can be on the same page).
     * Text must be surrounded with startText() end endText().
     * However, when you start a new page, the system automatically starts in text mode.
     */
    public void startText() {
        if (!inText) {
            try {
                contentStream.beginText();
                contentStream.setNonStrokingColor(Color.BLACK);
                inText = true;
                posy = posx = 0;
            } catch (Exception e) {
                logger.error("Error", e);
            }
        }
    }

    /**
     * End text mode and go into graphics mode.
     * <br><br>
     * Text and graphics cannot be output at the same time without switching between text and graphics mode.
     */
    public void endGraphics() {
        startText();
    }

    /**
     * Output txt at dot position y, dot position x
     *
     * @param absy absolute dot position, top to bottom
     * @param absx absolute dot position, left to right
     * @param txt the text to be written
     */
    public void textOutpx(float absy, float absx, String txt) {
        float relx, rely;
        absy = pageHeight - absy;
        try {
            startText();
            relx = absx - posx;
            rely = absy - posy;
            contentStream.newLineAtOffset(relx, rely);
            contentStream.showText(txt);
        } catch (Exception e) {
            logger.error("Error", e);
        }
        posx = absx;
        posy = absy;
    }

    /**
     * End current page and start a new page
     */
    public void newPage() {
        try {
            if (contentStream != null) {
                endText();
                contentStream.close();
            }
            page = new PDPage(pageSize);
            contentStream = new PDPageContentStream(doc, page, AppendMode.OVERWRITE, false);
            contentStream.setFont(font, fontSize);
            doc.addPage(page);
            startText();
            if (landscape) {
                page.setRotation(90);
                PDRectangle pageSize = page.getMediaBox();
                pageHeight = pageSize.getWidth();  //  height <- width is correct!
                pageWidth  = pageSize.getHeight(); //  width <- height is correct!
                try {
                    contentStream.transform(new Matrix(0, 1, -1, 0, pageHeight, 0));
                } catch (Exception e) {
                    logger.error("Error", e);
                }
            } else {
                PDRectangle pageSize = page.getMediaBox();
                pageHeight = pageSize.getHeight();
                pageWidth = pageSize.getWidth();
            }
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * Get an existing page.
     *
     * @param n the page number to get (starting at 0)
     */
    public void getPage(int n) {
        try {
            if (contentStream != null) {
                endText();
                contentStream.close();
            }
            page = doc.getPage(n);
            contentStream = new PDPageContentStream(doc, page, AppendMode.APPEND, false);
            contentStream.setFont(font, fontSize);
            startText();
            if (landscape) {
                page.setRotation(90);
                PDRectangle pageSize = page.getMediaBox();
                pageHeight = pageSize.getWidth();  //  height <- width is correct!
                pageWidth  = pageSize.getHeight(); //  width <- height is correct!
                try {
                    contentStream.transform(new Matrix(0, 1, -1, 0, pageHeight, 0));
                } catch (Exception e) {
                    logger.error("Error", e);
                }
            } else {
                PDRectangle pageSize = page.getMediaBox();
                pageHeight = pageSize.getHeight();
                pageWidth = pageSize.getWidth();
            }
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * Draw a line
     *
     * @param ya upper left  y point
     * @param xa upper left x point
     * @param yb lower right y point
     * @param xb lower right x point
     * @param thickness line thickness (-1 == no outside line)
     */
    public void drawLine(float ya, float xa, float yb, float xb, float thickness) {
        try {
            endText();
            contentStream.setLineWidth(thickness);
            contentStream.moveTo(xa, pageHeight-ya);
            contentStream.lineTo(xb, pageHeight-yb);
            contentStream.stroke();
        } catch (Exception e) {
            logger.error("Error", e);
        }
    }

    /**
     * Draw a horizontal line using the default line thinkness.
     *
     * @param y the line
     * @param xa starting row
     * @param xb ending row
     */
    public void drawHLine(float y, float xa, float xb) {
        drawLine(y, xa, y, xb, defaultLineThickness);
    }

    /**
     * Draw a vertical line using the default line thinkness.
     *
     * @param x the line
     * @param ya starting column
     * @param yb ending column
     */
    public void drawVLine(float x, float ya, float yb) {
        drawLine(ya, x, yb, x, defaultLineThickness);
    }

    /**
     * Draw a rectangle
     *
     * @param ya upper left y point
     * @param xa upper left x point
     * @param yb lower right y point
     * @param xb lower right x point
     * @param thickness line thickness (-1 == no outside line)
     * @param color fill color, hex RGB value
     */
    public void drawRect(float ya, float xa, float yb, float xb, float thickness, int color) {
        try {
            endText();
            contentStream.addRect(xa, pageHeight-ya, xb-xa, ya-yb);

            final Color fillColor = new Color(color);
            if (thickness > .01) {
                contentStream.setLineWidth(thickness);
                contentStream.setNonStrokingColor(fillColor);
                contentStream.fillAndStroke();
            } else {
                contentStream.setNonStrokingColor(fillColor);
                contentStream.fill();
            }
            if (thickness > .01) {
                contentStream.setLineWidth(thickness);
                contentStream.stroke();
            }
        } catch (Exception e) {
            logger.error("Error", e);
        }
    }

    /**
     *  Output image file to PDF
     *  <br><br>
     *  Positioning starts at upper left corner of paper.
     *
     * @param ypos place lower left corner of image at vertical position
     * @param xpos place lower left corner of image at horizontal position
     * @param scale scale image (1.0f means no scaling)
     * @param filename name of file holding image
     */
    public void imageOut(float ypos, float xpos, float scale, String filename) {
        try {
            endText();
            PDImageXObject pdImage = PDImageXObject.createFromFile(filename, doc);
            contentStream.drawImage(pdImage, xpos, pageHeight-ypos, pdImage.getWidth()*scale, pdImage.getHeight()*scale);
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     *  Output scaled image to PDF.
     *  <br><br>
     *  Positioning starts at upper left corner of paper.
     *
     * @param ypos place lower left corner of image at vertical position
     * @param xpos place lower left corner of image at horizontal position
     * @param scale scale image (1.0f means no scaling)
     * @param image the image
     */
    public void imageOut(float ypos, float xpos, float scale, byte [] image) {
        try {
            endText();
            ByteArrayInputStream bais = new ByteArrayInputStream(image);
            BufferedImage bim = ImageIO.read(bais);
            bais.close();
            PDImageXObject pdImage = LosslessFactory.createFromImage(doc, bim);
            contentStream.drawImage(pdImage, xpos, pageHeight-ypos, pdImage.getWidth()*scale, pdImage.getHeight()*scale);
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * Outputs an image with a specific width to the PDF document retaining the image aspect ratio.
     *
     * @param  ypos      place lower left corner of image at vertical position
     * @param  xpos      place lower left corner of image at horizontal position
     * @param  width     the width of the image
     * @param  filename  the filename of the image
     */
    public void imageOutWidth(float ypos, float xpos, float width, String filename) {
        try {
            endText();
            PDImageXObject pdImage = PDImageXObject.createFromFile(filename, doc);
            float aspectRatio = (float) pdImage.getHeight() / pdImage.getWidth();
            float height = width * aspectRatio;
            contentStream.drawImage(pdImage, xpos, pageHeight - ypos, width, height);
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * Outputs an image with a specific height to the PDF document retaining the image aspect ratio.
     *
     * @param  ypos      place lower left corner of image at vertical position
     * @param  xpos      place lower left corner of image at horizontal position
     * @param  height    the height of the image
     * @param  filename  the filename of the image
     */
    public void imageOutHeight(float ypos, float xpos, float height, String filename) {
        try {
            endText();
            PDImageXObject pdImage = PDImageXObject.createFromFile(filename, doc);
            float aspectRatio = (float) pdImage.getWidth() / pdImage.getHeight();
            float width = height * aspectRatio;
            contentStream.drawImage(pdImage, xpos, pageHeight - ypos, width, height);
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     *  Output image to defined square on the page.
     *
     * @param ypos place lower left corner of image at vertical position
     * @param xpos place lower left corner of image at horizontal position
     * @param ypos2 place upper right corner of image at vertical position
     * @param xpos2 place upper right corner of image at horizontal position
     * @param image the image
     */
    public void imageOut(float ypos, float xpos, float ypos2, float xpos2, byte [] image) {
        try {
            endText();
            ByteArrayInputStream bais = new ByteArrayInputStream(image);
            BufferedImage bim = ImageIO.read(bais);
            bais.close();
            PDImageXObject pdImage = LosslessFactory.createFromImage(doc, bim);

            // Figure out how to scale the image to fit in the box while keeping the aspect ratio
            int width = pdImage.getWidth();
            int height = pdImage.getHeight();
            float maxHeight = ypos - ypos2;
            float maxWidth = xpos2 - xpos;
            float yscale = maxHeight / height;
            float xscale = maxWidth / width;
            float scale = Math.min(yscale, xscale);

            contentStream.drawImage(pdImage, xpos, pageHeight-ypos, width*scale, height*scale);
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * End text mode and go into graphics mode.
     * Text and graphics cannot be output at the same time without switching between text and graphics mode.
     */
    public void endText() {
        try {
            if (inText) {
                contentStream.endText();
                inText = false;
            }
        } catch (IOException e) {
            logger.error("Error", e);
        }
    }

    /**
     * End text mode and go into graphics mode.
     * Text and graphics cannot be output at the same time without switching between text and graphics mode.
     */
    public void startGraphics() {
        endText();
    }

    /**
     * End current page and end document
     */
    public void endDocument() {
        try {
            if (contentStream != null) {
                endText();
                contentStream.close();
            }
            doc.save(outputFilename);
        } catch (Exception e) {
            logger.error("Error", e);
        } finally {
            try {
                if (doc != null) {
                    doc.close();
                    doc = null;
                }
            } catch (Exception e) {
                logger.error("Error", e);
            }
        }
    }

    /**
     * Get the underlying PDDocument.
     *
     * @return the PDDocument instance
     */
    public PDDocument getDoc() {
        return doc;
    }

    /**
     * Get the current page.
     *
     * @return the current PDPage instance
     */
    public PDPage getPage() {
        return page;
    }

    /**
     * Get the content stream for the current page.
     *
     * @return the PDPageContentStream instance
     */
    public PDPageContentStream getContentStream() {
        return contentStream;
    }

    /**
     * Draw a grid on the current page for layout assistance.
     */
    public void grid() {
        float y, x;
        float yo = 16, xo = 16;  // page offsets

        setFont(COURIER_FONT, 7);
        // left side marks
        for (y=0 ; y <= pageHeight ; y += 10)
            if (0 == y%100) {
                if (y != 0)
                    textOutpx(y-1, xo+3, "" + (int) y);
                drawLine(y, xo, y, xo+20, .5f);
            } else if (0 == y%50) {
                drawLine(y, xo, y, xo+15, .5f);
            } else
                drawLine(y, xo, y, xo+10, .5f);
        // right side marks
        for (y=0 ; y <= pageHeight ; y += 10)
            if (0 == y%100) {
                if (y != 0)
                    textOutpx(y-1, pageWidth-xo-18, "" + (int) y);
                drawLine(y, pageWidth-xo-20, y, pageWidth-xo, .5f);
            } else if (0 == y%50) {
                drawLine(y, pageWidth-xo-15, y, pageWidth-xo, .5f);
            } else
                drawLine(y, pageWidth-xo-10, y, pageWidth-xo, .5f);
        //  top marks
        for (x=0 ; x <= pageWidth ; x += 10)
            if (0 == x%100) {
                if (x != 0)
                    textOutpx(yo+16, x+2, "" + (int) x);
                drawLine(yo, x, yo+20, x, .5f);
            } else if (0 == x%50) {
                drawLine(yo, x, yo+15, x, .5f);
            } else
                drawLine(yo, x, yo+10, x, .5f);
        //  bottom marks
        for (x=0 ; x <= pageWidth ; x += 10)
            if (0 == x%100) {
                if (x != 0)
                    textOutpx(pageHeight-yo-16, x+2, "" + (int) x);
                drawLine(pageHeight-yo-20, x, pageHeight-yo, x, .5f);
            } else if (0 == x%50) {
                drawLine(pageHeight-yo-15, x, pageHeight-yo, x, .5f);
            } else
                drawLine(pageHeight-yo-10, x, pageHeight-yo, x, .5f);
    }

    /**
     * Test method for the PDF class.
     *
     * @param args command line arguments (not used)
     * @throws IOException if an error occurs during PDF operations
     */
    public static void main(String[] args) throws IOException {
        test4();
    }

    private static void test1() {
        PDF pdf = new PDF("mypdf.pdf");

//        pdf.landscape();

        pdf.newPage();

        pdf.textOutpx(100, 100, "100x100");
        pdf.textOutpx(300, 100, "300x100");
        pdf.textOutpx(200, 100, "200x100");
        pdf.textOutpx(300, 300, "300x300");

        pdf.imageOut(100, 200, 1, "WayToGo.png");

        pdf.newPage();
        pdf.textOutpx(400, 200, "400x200");

        pdf.endDocument();
    }

    private static void test2() {
        PDF pdf = new PDF("mypdf.pdf");
        pdf.newPage();

        for (int line=1 ; line <= 66 ; line++)
            pdf.textOut(line, 20, line + "x20");
        pdf.textOut(25, 40, "25x40x");

        pdf.drawLine(100, 100, 200, 200, 2);
        pdf.drawRect(300, 300, 400, 400, 5, 200);
        pdf.drawLine(250, 200, 250, 250, 2);

        pdf.textOut(1, 1, "1234567890123456789");

        pdf.endDocument();
    }

    private static void test3() {
        PDF pdf = new PDF("mypdf.pdf");
//        pdf.landscape();
        pdf.newPage();
        pdf.grid();

        pdf.imageOut(300, 100, .2f, "WayToGo.png");

        pdf.endDocument();
    }

    private static void test4() throws IOException {
        PDF pdf = new PDF("/home/blake/Desktop/20200320 Vendor Letter KNH.pdf", "/home/blake/Desktop/res.pdf");
        pdf.getPage(0);
        //       pdf.grid();
        pdf.setFont(COURIER_FONT, 12);
        pdf.textOutpx(230, 250, "Blake McBride");
        pdf.endDocument();
    }

}
