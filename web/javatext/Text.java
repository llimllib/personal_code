import java.applet.*;
import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.util.*;
import netscape.javascript.*;

public class Text extends Applet
{
    private String FONT_NAME;
    private Font CURFONT;
    private int FONT_SIZE;
    
    public void init() {
        setFont(10, "sans-serif");
    }

    public void setFont(int fontSize, String fontName) {
        //TODO implement the CSS font spec http://www.w3.org/TR/CSS2/fonts.html#font-shorthand
        //TODO handle errors
        FONT_NAME = fontName;
        if (FONT_NAME == "sans-serif") {
            CURFONT = new Font("SansSerif", Font.PLAIN, fontSize);
        }
        else if (FONT_NAME == "serif") {
            CURFONT = new Font("Serif", Font.PLAIN, fontSize);
        }
        else {
            CURFONT = new Font(fontName, Font.PLAIN, fontSize);
        }
    }

    public String renderString(String s) {
        Graphics2D g2 = (Graphics2D)this.getGraphics();
        g2.setFont(CURFONT);
        g2.setColor(Color.black);

        GlyphVector glyphVector = CURFONT.createGlyphVector(g2.getFontRenderContext(), s);
        g2.drawGlyphVector(glyphVector, 100,150);

        AffineTransform a = new AffineTransform(1,0,0,1,0,0);

        Shape shape = a.createTransformedShape(glyphVector.getOutline());
        PathIterator pi = shape.getPathIterator(null);
        StringBuilder sb = new StringBuilder();
        sb.append("this.beginPath();");

        float[] coords = new float[6];
        float[] fstpoint = new float[2];
        while (!pi.isDone()) {
            int type = pi.currentSegment(coords);
            switch (type) {
            case PathIterator.SEG_MOVETO:
                fstpoint[0] = coords[0];
                fstpoint[1] = coords[1];
                sb.append("this.moveTo(");
                sb.append(coords[0]);
                sb.append(",");
                sb.append(coords[1]);
                sb.append(");\n");
                break;
            case PathIterator.SEG_LINETO:
                sb.append("this.lineTo(");
                sb.append(coords[0]);
                sb.append(",");
                sb.append(coords[1]);
                sb.append(");\n");
                break;
            case PathIterator.SEG_QUADTO:
                sb.append("this.quadraticCurveTo(");
                sb.append(coords[0]);
                sb.append(",");
                sb.append(coords[1]);
                sb.append(",");
                sb.append(coords[2]);
                sb.append(",");
                sb.append(coords[3]);
                sb.append(");\n");
                break;
            case PathIterator.SEG_CUBICTO:
                sb.append("this.bezierCurveTo(");
                sb.append(coords[0]);
                sb.append(",");
                sb.append(coords[1]);
                sb.append(",");
                sb.append(coords[2]);
                sb.append(",");
                sb.append(coords[3]);
                sb.append(",");
                sb.append(coords[4]);
                sb.append(",");
                sb.append(coords[5]);
                sb.append(");\n");
                break;
            case PathIterator.SEG_CLOSE:
                sb.append("this.lineTo(");
                sb.append(fstpoint[0]);
                sb.append(",");
                sb.append(fstpoint[1]);
                sb.append(");\n");
            }

            pi.next();
        }

        sb.append("\nthis.closePath();\n");

        return sb.toString();
    }
}
