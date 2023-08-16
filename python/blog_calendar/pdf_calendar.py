#!/usr/bin/env python
"""Create a PDF calendar.

This script requires Python and Reportlab
( http://reportlab.org/rl_toolkit.html ). Tested only with Python 2.4 and
Reportlab 1.2.

See bottom of file for an example of usage. No command-line interface has been
added, but it would be trivial to do so. Furthermore, this script is pretty
hacky, and could use some refactoring, but it works for what it's intended
to do.

Created by Bill Mill on 11/16/05, this script is in the public domain. There
are no express warranties, so if you mess stuff up with this script, it's not
my fault.

If you have questions or comments or bugfixes or flames, please drop me a line 
at bill.mill@gmail.com .
"""
from reportlab.lib import pagesizes
from reportlab.pdfgen.canvas import Canvas
import calendar, time, datetime
from math import floor

NOW = datetime.datetime.now()
SIZE = pagesizes.landscape(pagesizes.letter)


class NoCanvasError(Exception):
    pass


def nonzero(row):
    return len([x for x in row if x != 0])


def createCalendar(month, year=NOW.year, canvas=None, filename=None, size=SIZE):
    """
    Create a one-month pdf calendar, and return the canvas

    month: can be an integer (1=Jan, 12=Dec) or a month abbreviation (Jan, Feb,
            etc.
    year: year in which month falls. Defaults to current year.
    canvas: you may pass in a canvas to add a calendar page to the end.
    filename: String containing the file to write the calendar to
    size: size, in points of the canvas to write on
    """
    if type(month) == type(""):
        month = time.strptime(month, "%b")[1]
    if canvas is None and filename is not None:
        canvas = Canvas(filename, size)
    elif canvas is None and filename is None or not canvas:
        raise NoCanvasError
    monthname = time.strftime("%B", time.strptime(str(month), "%m"))
    cal = calendar.monthcalendar(year, month)

    width, height = size

    # draw the month title
    title = monthname + " " + str(year)
    canvas.drawCentredString(width / 2, height - 27, title)
    height = height - 40

    # margins
    wmar, hmar = width / 50, height / 50

    # set up constants
    width, height = width - (2 * wmar), height - (2 * hmar)
    rows = len(cal)
    lastweek = nonzero(cal[-1])
    firstweek = nonzero(cal[0])
    rowheight = floor(height / rows)
    boxwidth = floor(width / 7)

    # draw the bottom line
    canvas.line(wmar, hmar, wmar + (boxwidth * lastweek), hmar)
    # now, for all complete rows, draw the bottom line
    for row in range(1, len(cal[1:-1]) + 1):
        y = hmar + (row * rowheight)
        canvas.line(wmar, y, wmar + (boxwidth * 7), y)
    # now draw the top line of the first full row
    y = hmar + ((rows - 1) * rowheight)
    canvas.line(wmar, y, wmar + (boxwidth * 7), y)
    # and, then the top line of the first row
    startx = wmar + (boxwidth * (7 - firstweek))
    endx = startx + (boxwidth * firstweek)
    y = y + rowheight
    canvas.line(startx, y, endx, y)

    # now draw the vert lines
    for col in range(8):
        # 1 = don't draw line to first or last; 0 = do draw
        last, first = 1, 1
        if col <= lastweek:
            last = 0
        if col >= 7 - firstweek:
            first = 0
        x = wmar + (col * boxwidth)
        starty = hmar + (last * rowheight)
        endy = hmar + (rows * rowheight) - (first * rowheight)
        canvas.line(x, starty, x, endy)

    # now fill in the day numbers and any data
    x = wmar + 6
    y = hmar + (rows * rowheight) - 15
    for week in cal:
        for day in week:
            if day:
                canvas.drawString(x, y, str(day))
            x = x + boxwidth
        y = y - rowheight
        x = wmar + 6

    # finish this page
    canvas.showPage()

    return canvas


if __name__ == "__main__":
    # create a December, 2005 PDF
    c = createCalendar(8, 2023, filename="blog_calendar.pdf")
    for i in range(9, 13):
        createCalendar(i, 2023, canvas=c)
    c.save()
