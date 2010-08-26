/**
Get a font and display glyphs with additional information
(bounding boxes, anchor & control points...)

by Philippe Lhoste <PhiLho(a)GMX.net> http://Phi.Lho.free.fr & http://PhiLho.deviantART.com
*/
/* File/Project history:
 1.01.000 -- 2008/08/28 (PL) -- Some improvements.
 1.00.000 -- 2008/08/28 (PL) -- Creation.
*/
/* Copyright notice: For details, see the following file:
http://Phi.Lho.free.fr/softwares/PhiLhoSoft/PhiLhoSoftLicence.txt
This program is distributed under the zlib/libpng license.
Copyright (c) 2008 Philippe Lhoste / PhiLhoSoft
*/

import java.awt.font.*;
import java.awt.geom.*;

final static int DEMO_ID = 2;
final static String FONT_NAME = "Times New Roman";
final static String STRING = "p@";

int FONT_SIZE, SCALE;
Graphics2D g2;
AffineTransform transform;
HashMap colorList = new HashMap();

void setup()
{
  size(1000, 700);

  smooth();
  noLoop();
  background(150);
  noFill();

  g2 = ((PGraphicsJava2D) g).g2;

  // Move drawing to a convenient place
  transform = new AffineTransform();
  if (DEMO_ID == 1)
  {
    FONT_SIZE = 1;
    SCALE = 300;
    transform.translate(40, 320);
    transform.scale(SCALE, SCALE);
  }
  else if (DEMO_ID == 2)
  {
    FONT_SIZE = 2;
    SCALE = 300;
    transform.translate(50, 500);
    transform.scale(SCALE, SCALE);
  }
  else if (DEMO_ID == 3)
  {
    FONT_SIZE = 2;
    SCALE = 300;
    transform.translate(50, 500);
    transform.scale(SCALE, SCALE);
  }
//  g2.setTransform(transform);

  // And show the origin (and scale) of drawing
  SetColor(#FF0000); // Update g2
  strokeWeight(5);
  Line2D.Double line = new Line2D.Double(-0.1, -0.1, 0.1, 0.1);
  g2.draw(transform.createTransformedShape(line));
  line = new Line2D.Double(-0.1, 0.1, 0.1, -0.1);
  g2.draw(transform.createTransformedShape(line));
  strokeWeight(2);

  // Now, we get the font
  Font font = new Font(FONT_NAME, Font.PLAIN, FONT_SIZE);
  g2.setFont(font);
  FontRenderContext frc = g2.getFontRenderContext();

  if (DEMO_ID == 1)
  {
//~     String str = "%&@";
    GlyphVector glyphVector = font.createGlyphVector(frc, STRING);
    for (int i = 0; i < STRING.length(); i++)
    {
      strokeWeight(2);

      SetColor(#00FF00);
      Shape vbs = glyphVector.getGlyphVisualBounds(i);
      g2.draw(transform.createTransformedShape(vbs));

      SetColor(#005000);
      Rectangle r = glyphVector.getGlyphPixelBounds(i, null, 0.0f, 0.0f);
      g2.draw(transform.createTransformedShape(r));

      SetColor(#A0A000);
      Shape lbs = glyphVector.getGlyphLogicalBounds(i);
      g2.draw(transform.createTransformedShape(lbs));

      SetColor(#0050F0);
      strokeWeight(5);

      Shape shape = glyphVector.getGlyphOutline(i);
      g2.draw(transform.createTransformedShape(shape));
    }

    // Draw the whole string at once
    SetColor(#F0A000);
    strokeWeight(1);

    Shape shape = glyphVector.getOutline();
    g2.draw(transform.createTransformedShape(shape));
  }
  else if (DEMO_ID == 2)
  {
    GlyphVector glyphVector = font.createGlyphVector(frc, STRING);
    for (int i = 0; i < STRING.length(); i++)
    {
      SetColor(#0050F0);
      strokeWeight(5);

      Shape shape = glyphVector.getGlyphOutline(i);
      g2.draw(transform.createTransformedShape(shape));

      HighlightPoints(shape);
    }
  }
  else if (DEMO_ID == 3) // To test the SEG_CUBICTO case!
  {
    SetColor(#0050F0);
    strokeWeight(5);

    GeneralPath shape = new GeneralPath();
    shape.moveTo(1.5, 0.0);
    shape.lineTo(1.8, 0.0);
    shape.quadTo(2.0, 0.3, 1.5, 0.3);
    shape.lineTo(1.1, 0.3);
    shape.curveTo(0.7, 0.3, 0.5, -0.5, 1.2, -0.2);
    shape.lineTo(1.5, -0.3);
    shape.closePath();

    g2.draw(transform.createTransformedShape(shape));

    HighlightPoints(shape);
  }
}

void HighlightPoints(Shape shape)
{
  strokeWeight(1);

  PathIterator iterator = shape.getPathIterator(null);
  float[] coords = new float[6];
  float px = 0, py = 0;
  while (!iterator.isDone())
  {
    int type = iterator.currentSegment(coords);
    switch (type)
    {
    case PathIterator.SEG_MOVETO: // One point
      print("Move ");
      px = coords[0];
      py = coords[1];
      DrawMovePoint(px, py);
      break;
    case PathIterator.SEG_LINETO:
      print("Line ");
      px = coords[0];
      py = coords[1];
      DrawLinePoint(px, py);
      break;
    case PathIterator.SEG_QUADTO: // Two points
      print("Quad ");
      DrawControlLine(coords[0], coords[1], px, py);
      px = coords[2];
      py = coords[3];
      DrawControlLine(coords[0], coords[1], px, py);
      DrawControlPoint(coords[0], coords[1]);
      DrawQuadPoint(px, py);
      break;
    case PathIterator.SEG_CUBICTO: // Three points
      print("Cubic "); // Not seen yet...
      DrawControlLine(coords[0], coords[1], px, py);  // Connect to last point
      px = coords[4];
      py = coords[5];
      DrawControlLine(coords[2], coords[3], px, py);
      DrawControlPoint(coords[0], coords[1]);
      DrawControlPoint(coords[2], coords[3]);
      DrawCubicPoint(px, py);
      break;
    case PathIterator.SEG_CLOSE:  // No points
    }
    iterator.next();
  }
}

void DrawMovePoint(float x, float y)
{
  float radius = 2.5 / SCALE;
  Ellipse2D.Float e = new Ellipse2D.Float(x - radius, y - radius, 2 * radius, 2 * radius);
  SetColor(#FF00A0);
  g2.fill(transform.createTransformedShape(e));
  SetColor(#FF00FF);
  g2.draw(transform.createTransformedShape(e));
}

void DrawLinePoint(float x, float y)
{
  float radius = 4.5 / SCALE;
  Ellipse2D.Float e = new Ellipse2D.Float(x - radius, y - radius, 2 * radius, 2 * radius);
  SetColor(#FF00FF);
  g2.draw(transform.createTransformedShape(e));
}

void DrawQuadPoint(float x, float y)
{
  SetColor(#80FF00);
  float radius = 3.0 / SCALE;
  Rectangle2D.Float r = new Rectangle2D.Float(x - radius, y - radius, 2 * radius, 2 * radius);
  g2.draw(transform.createTransformedShape(r));
}

void DrawCubicPoint(float x, float y)
{
  SetColor(#FF8000);
  float radius = 5.0 / SCALE;
  Rectangle2D.Float r = new Rectangle2D.Float(x - radius, y - radius, 2 * radius, 2 * radius);
  g2.draw(transform.createTransformedShape(r));
}

void DrawControlPoint(float x, float y)
{
  SetColor(#D0E000);
  float radius = 3.0 / SCALE;
  Ellipse2D.Float e = new Ellipse2D.Float(x - radius, y - radius, 2 * radius, 2 * radius);
  g2.draw(transform.createTransformedShape(e));
}

void DrawControlLine(float x1, float y1, float x2, float y2)
{
  SetColor(#FFA000);
  Line2D.Float l = new Line2D.Float(x1, y1, x2, y2);
  g2.draw(transform.createTransformedShape(l));
}

Color GetColor(color c)
{
  Integer ic = Integer.valueOf(c);  // New to 1.5! Cache values
  Color k = (Color) colorList.get(ic);
  if (k == null)
  {
    k = new Color(ic);
    colorList.put(ic, k);
  }
  return k;
}
void SetColor(color c)
{
  Color k = GetColor(c);
  g2.setPaint(k);
}
