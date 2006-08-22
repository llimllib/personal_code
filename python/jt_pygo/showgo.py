### SHOWGO.PY

#    Copyright (C) 2002 James Tauber
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# TODO
# partial boards
# numbers need to make blank if not on stone
# allow resizing of canvas with automatic scaling of board

import Tkinter
import tkFont

class ShowGo:
    def __init__(self, canvas, size=19, scale=30):
        canvas.delete(Tkinter.ALL)
        self.scale = scale
        self.canvas = canvas
        self.size = size

        self.contents = []        
        self.item = []
        for i in range(size + 1):
            self.contents.append([None] * (size + 1))
            self.item.append([None] * (size + 1))
        
        margin = scale * 1.5
        padding = scale / 1.5

        number_fontfamily = "arial narrow"
        number_fontsize = int(scale / 2.5)
        number_fontweight = tkFont.BOLD
        self.number_font = tkFont.Font(family=number_fontfamily, size=number_fontsize, weight=number_fontweight)

        letter_fontfamily = "arial narrow"
        self.letter_fontsize = letter_fontsize = int(scale / 2)
        letter_fontweight = tkFont.BOLD
        self.letter_font = tkFont.Font(family=letter_fontfamily, size=letter_fontsize, weight=letter_fontweight)

        self.start_x = start_x = margin
        self.start_y = start_y = margin
        end_x = margin + (scale * (size - 1))
        end_y = margin + (scale * (size - 1))

        starpoint_radius = int(scale / 10)
        self.stone_radius = int(scale / 2)

        self.line_thickness = int(scale / 20)
        edge_thickness = int(scale / 15)

        canvas.pack(side=Tkinter.TOP, fill=Tkinter.BOTH, expand=1)

        canvas.create_rectangle(start_x - padding, start_y - padding, end_x + padding, end_y + padding, width=edge_thickness)

        for i in range(size):
            # horizontal line
            canvas.create_line(int(start_x + (i * scale)), int(start_y), int(start_x +(i * scale)), int(end_y))
            # vertical line
            canvas.create_line(int(start_x), int(start_y + (i * scale)), int(end_x), int(start_y + (i * scale)))

        def draw_starpoint(x, y):
            x1 = (scale * (x - 1)) - starpoint_radius
            y1 = (scale * (y - 1)) - starpoint_radius
            x2 = (scale * (x - 1)) + starpoint_radius
            y2 = (scale * (y - 1)) + starpoint_radius
            canvas.create_oval(int(start_x + x1), int(start_y + y1), int(start_x + x2), int(start_y + y2), fill="black")
        
        def draw_starpoints(list):
            for x in list:
                for y in list:
                    draw_starpoint(x,y)
        
        if size == 19:
            point_list = [4, 10, 16]
        elif size == 13:
            point_list = [4, 7, 10]
        elif size == 9:
            point_list = [3, 7]
        else:
            point_list = []

        draw_starpoints(point_list)

    TRIANGLE = "triangle"
    SQUARE = "square"
    CIRCLE = "circle"
    SELECT = "select"
    MARKER = "marker"

    BLACK = "black"
    WHITE = "white"

    def get_x(self, x):
        return int(((x - self.start_x) / self.scale) + 1.5)

    def get_y(self, y):
        return int(((y - self.start_y) / self.scale) + 1.5)

    def draw_stone(self, x, y, color):
        centre_x = int(self.start_x + (self.scale * (x - 1)))
        centre_y = int(self.start_y + (self.scale * (y - 1)))
        x1 = centre_x - self.stone_radius
        y1 = centre_y - self.stone_radius
        x2 = centre_x + self.stone_radius
        y2 = centre_y + self.stone_radius
        self.contents[x][y] = color
        self.item[x][y] = self.canvas.create_oval(x1, y1, x2, y2, fill=color, width=self.line_thickness)

    def erase_stone(self, x, y):
        self.canvas.delete(self.item[x][y])
        self.contents[x][y] = None
        self.item[x][y] = None
        
    def draw_number(self, x, y, number):
        centre_x = self.start_x + (self.scale * (x - 1))
        centre_y = self.start_y + (self.scale * (y - 1))

        if self.contents[x][y] == self.BLACK:
            inverse_color = self.WHITE
        else:
            inverse_color = self.BLACK

        text_x = centre_x
        text_y = centre_y
        self.canvas.create_text(text_x, text_y, text=number, fill=inverse_color, font=self.number_font)    

    def draw_shape(self, x, y, shape):
        centre_x = int(self.start_x + (self.scale * (x - 1)))
        centre_y = int(self.start_y + (self.scale * (y - 1)))
        color = self.contents[x][y]
        
        if color == self.BLACK:
            inverse_color = self.WHITE
        else:
            inverse_color = self.BLACK

        r = int(self.stone_radius / 1.7)
        r1 = int(self.stone_radius / 1.8)
        r2 = int(self.stone_radius / 2.3)
        r3 = int(self.stone_radius / 4)
        if shape == self.CIRCLE:
            item = self.canvas.create_oval(centre_x - r, centre_y - r, centre_x + r, centre_y + r,
                                    fill=None, outline=inverse_color, width=self.line_thickness)
        if shape == self.TRIANGLE:
            item = self.canvas.create_polygon(centre_x, centre_y - r, 
                                       centre_x - r1, centre_y + r2,
                                       centre_x + r1, centre_y + r2,
                                       fill=None, outline=inverse_color, width=self.line_thickness)
        if shape == self.SQUARE:
            item = self.canvas.create_rectangle(centre_x - r2, centre_y - r2,
                                         centre_x + r2, centre_y + r2,
                                         fill=None, outline=inverse_color, width=self.line_thickness)
        if shape == self.MARKER:
            item = self.canvas.create_polygon(centre_x - r2, centre_y - r2,
                                              centre_x, centre_y,
                                              centre_x + r2, centre_y - r2,
                                              centre_x, centre_y,
                                              centre_x + r2, centre_y + r2,
                                              centre_x, centre_y,
                                              centre_x - r2, centre_y + r2,
                                              centre_x, centre_y,
                                         fill=None, outline=inverse_color, width=self.line_thickness)
        if shape == self.SELECT:
            item = self.canvas.create_rectangle(centre_x - r3, centre_y - r3,
                                         centre_x + r3, centre_y + r3,
                                         fill=inverse_color, outline=inverse_color, width=self.line_thickness)
        return item
    
    def draw_territory(self, x, y, color):
        centre_x = int(self.start_x + (self.scale * (x - 1)))
        centre_y = int(self.start_y + (self.scale * (y - 1)))
        
        r2 = int(self.stone_radius / 2.3)
        self.canvas.create_polygon(centre_x, centre_y - r2,
                                   centre_x + r2, centre_y,
                                   centre_x, centre_y + r2,
                                   centre_x - r2, centre_y,
                                   fill=color, outline=color)
     
    def draw_label(self, x, y, label):
        blank = self.letter_fontsize / 1.5
        text_x = self.start_x + (self.scale * (x - 1))
        text_y = self.start_y + (self.scale * (y - 1))
        x1 = text_x - blank
        y1 = text_y - blank
        x2 = text_x + blank
        y2 = text_y + blank

        if self.contents[x][y] == self.BLACK:
            inverse_color = self.WHITE
        else:
            inverse_color = self.BLACK

        if not self.contents[x][y]:
            background = self.canvas.config("background")[4]
            self.canvas.create_rectangle(x1, y1, x2, y2,
                                fill=background, outline=background)
        self.canvas.create_text(text_x, text_y,
                           text=label, fill=inverse_color, font=self.letter_font)



