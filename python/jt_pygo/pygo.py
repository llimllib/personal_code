### PYGO.PY

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

from Tkinter import *
import tkFileDialog
import tkSimpleDialog
import tkMessageBox
import ScrolledText

import sys

import gogame
import showgo
import sgf

app_name = "PyGo"
version = "0.10.1"


class Game:
    def __init__(self, play_go, collection, size=19, handicap=0, scale=25):
        self.play_go = play_go
        play_go.variations.config(state=NORMAL)
        play_go.variations.delete("1.0", END)
        play_go.variations.config(state=DISABLED)
        self.scale = scale
        self.dead_points = []
        self.play_go.root.bind("<Left>", self.prev_node)
        self.play_go.root.bind("<Right>", self.next_node)
        self.play_go.root.bind("<Up>", self.prev_variation)
        self.play_go.root.bind("<Down>", self.next_variation)
        self.play_go.root.bind("v", self.make_variation)
        self.play_go.root.bind("p", self.toggle_placement)
        self.play_go.root.bind("u", self.undo)
        self.play_go.root.bind("t", self.toggle_territory)
        self.play_go.root.bind("i", self.insert_node)
        self.play_go.root.bind("<Shift-Left>", self.first_node_in_variation)
        self.play_go.root.bind("<Shift-Right>", self.last_node_in_variation)
        self.play_go.canvas.bind("<Button-1>", self.handle_move_event)
        self.placement = 0
        self.show_territory = 0

        if collection:
            self.is_playable = 0
            self.play_go.root.bind("c", self.continue_game)
            self.collection = collection
            self.current_node = collection.children[0].nodes[0]
            self.draw_current_node()
        else:
            self.is_playable = 1
            self.size = size
            self.play_go.root.unbind("c")
            self.collection = sgf.Collection()
            self.current_gametree = sgf.GameTree(self.collection)
            self.collection.children.append(self.current_gametree)
            self.current_node = sgf.Node(self.current_gametree, None)
            self.current_gametree.nodes.append(self.current_node)
            self.current_node.properties["SZ"] = [str(size)]
            if handicap:
                self.current_node.properties["HA"] = [str(handicap)]
            self.current_node.properties["FF"] = ["4"]
            self.current_node.properties["GM"] = ["1"]
            self.current_node.properties["AP"] = ["%s:%s" % (app_name, version)]
        self.place_handicap_stones(handicap)
        self.draw_current_node()

    def toggle_placement(self, event):
        if self.placement == 0:
            self.placement = 1
        elif self.placement == 1:
            self.placement = 2
        elif self.placement == 2:
            self.placement = 0
        self.draw_current_node()

    def toggle_territory(self, event):
        if self.show_territory == 0:
            self.show_territory = 1
        else:
            self.show_territory = 0
        self.draw_current_node()

    def make_variation(self, event):
        # @@@ much of this method could be in sgf library
        
        self.current_gametree = self.current_node.parent

        pos = self.current_gametree.nodes.index(self.current_node)
        nodes1 = self.current_gametree.nodes[:pos]
        nodes2 = self.current_gametree.nodes[pos:]

        tree1 = sgf.GameTree(self.current_gametree)
        tree2 = sgf.GameTree(self.current_gametree)
        
        self.current_gametree.nodes = nodes1
        self.current_gametree.children.append(tree1)
        self.current_gametree.children.append(tree2)

        tree1.nodes = nodes2

        new_node = sgf.Node(tree2, nodes1[-1])
        tree2.nodes.append(new_node)

        self.current_node.previous.variations = [self.current_node, new_node]

        self.current_node.next_variation = new_node
        new_node.previous_variation = self.current_node

        self.current_node = new_node
        self.current_gametree = tree2
        self.is_playable = 1
        self.draw_current_node()

    def continue_game(self, event):
        # don't do anything if not at end of variation
        if self.current_node.next:
            self.display_result("NOT AT END OF VARIATION")
            return

        self.is_playable = 1
        self.current_gametree = self.current_node.parent
        self.draw_current_node()

    def place_handicap_stones(self, handicap):
        # uses: handicap, size
        # changes: contents
        handicap_positions = [
            [], [], [], [], [], [], [], [], [],
            [(7,3),(3,7),(7,7),(3,3),(5,5),(3,5),(7,5),(5,3),(5,7)],
            [], [], [],
            [(10,4),(4,10),(10,10),(4,4),(7,7),(4,7),(10,7),(7,4),(7,10)],
            [], [], [], [], [],
            [(16,4),(4,16),(16,16),(4,4),(10,10),(4,10),(16,10),(10,4),(10,16)]
            ]
        h = handicap
        if h == 6 or h == 8:
            h = h + 1
        for i in range(h):
            (x, y) = handicap_positions[self.size][i]
            if handicap == 6 or handicap == 8:
                if (x, y) == ((self.size+1)/2, (self.size+1)/2):
                    continue
            result = self.go_game.place_stone(x, y, gogame.BLACK)
            sgf_node = sgf.Node(self.current_gametree, self.current_node)
            self.current_gametree.nodes.append(sgf_node)
            sgf_node.properties["AB"] = ["%s%s" % (sgf.SGF_POS[x], sgf.SGF_POS[y])]
            self.current_node.next = sgf_node
            self.current_node = sgf_node


    def output(self, f):
        self.collection.output(f)

    ### STATUS DISPLAY

    def display_state(self):
        if self.placement == 1:
            self.play_go.label1.config(text = "PLACE BLACK STONES")
        elif self.placement == 2:
            self.play_go.label1.config(text = "PLACE WHITE STONES")
        elif not self.is_playable:
            self.play_go.label1.config(text = "")
        elif self.current_node.next: # not at end of a variation
            self.play_go.label1.config(text = "")
        elif self.go_game.game_completed:
            self.play_go.label1.config(text = "GAME OVER")
        elif self.go_game.to_play == gogame.BLACK:
            self.play_go.label1.config(text = "%s: BLACK TO PLAY" % self.go_game.move_number)
        else:
            self.play_go.label1.config(text = "%s: WHITE TO PLAY" % self.go_game.move_number)

    def display_prisoner_count(self):
        self.play_go.label2.config(text = "black: %s  white: %s" % (self.go_game.white_prisoners, self.go_game.black_prisoners))

    def display_result(self, message=""):
        self.play_go.label3.config(text = message)
        
    ### NODE NAVIGATION

    def prev_node(self, event):
        if self.current_node.previous:
            self.current_node = self.current_node.previous
        self.draw_current_node()
        
    def next_node(self, event):
        if self.current_node.next:
            self.current_node = self.current_node.next
        self.draw_current_node()

    def prev_variation(self, event):
        if self.current_node.previous_variation:
            self.current_node = self.current_node.previous_variation
        self.draw_current_node()
        
    def next_variation(self, event):
        if self.current_node.next_variation:
            self.current_node = self.current_node.next_variation
        self.draw_current_node()

    def first_node_in_variation(self, event):
        while not self.current_node.first:
            self.current_node = self.current_node.previous
        self.draw_current_node()

    def last_node_in_variation(self, event):
        while self.current_node.next:
            self.current_node = self.current_node.next
        self.draw_current_node()


    ### DRAW TERRITORY
        
    def draw_territory(self):
        (b, w) = self.go_game.calculate_score(self.dead_points)
        self.display_result("black: %s  white: %s" % (b, w))
        t = self.go_game.calculate_territory(self.dead_points)
        for (x, y) in t[0]:
            self.show_go.draw_shape(x, y, self.show_go.SQUARE)
        for (x, y) in t[1]:
            self.show_go.draw_territory(x, y, self.show_go.BLACK)
        for (x, y) in t[2]:
            self.show_go.draw_territory(x, y, self.show_go.WHITE)        
    
    ### TOGGLE GROUP ALIVE/DEAD

    def toggle_group_alive_dead(self, x, y):
        group = self.go_game.calculate_group(x, y)
        if (x,y) in self.dead_points:
            new_dead_points = []
            for point in self.dead_points:
                if not point in group:
                    new_dead_points.append(point)
            self.dead_points = new_dead_points
        else:
            self.dead_points.extend(group)

    ###

    def move_property(self):
        if self.go_game.to_play == gogame.BLACK:
            return "B"
        else:
            return "W"

    ### HANDLE INSERT NODE

    def insert_node(self, event):
        # @@@ much of this method could be in sgf library

        self.current_gametree = self.current_node.parent

        next = self.current_node.next

        new_node = sgf.Node(self.current_gametree, self.current_node)

        if next:
            new_node.next = next
            new_node.next.previous = new_node

            pos = self.current_gametree.nodes.index(self.current_node)
            nodes1 = self.current_gametree.nodes[:pos]
            nodes2 = self.current_gametree.nodes[pos:]
            self.current_gametree.nodes = nodes1
            self.current_gametree.nodes.append(self.current_node)
            self.current_gametree.nodes.append(new_node)
            self.current_gametree.nodes.extend(nodes2[1:])
        else:
            self.current_gametree.nodes.append(new_node)

        self.current_node = new_node

        self.draw_current_node()

    ### HANDLE UNDO

    def undo(self, event):
        # @@@ much of this method could be in sgf library

        # don't do anything if game not playable
        if not self.is_playable:
            self.display_result("GAME NOT PLAYABLE")
            return

        # don't do anything if not at end of variation
        if self.current_node.next:
            self.display_result("NOT AT END OF VARIATION")
            return

        if self.current_node.previous:
            self.current_node = self.current_node.previous
            self.current_node.next = None
            self.current_node.parent.nodes = self.current_node.parent.nodes[:-1]
            # @@@ possibly still need to remove variation links
        else:
            self.display_result("CAN'T UNDO START NODE")
            return
        self.draw_current_node()

    ### HANDLE CLICK

    def handle_move_event(self, event):

        # get point coordinates clicked on
        x = self.show_go.get_x(event.x)
        y = self.show_go.get_y(event.y)

        # if showing territory...
        if self.show_territory:
            # ignore clicks outside board area
            if x < 1 or y < 1 or x > self.size or y > self.size:
                return
            # otherwise toggle alive/dead status of group clicked on
            self.toggle_group_alive_dead(x, y)
            # draw current node
            self.draw_current_node()
            return

        if self.placement:
            self.handle_placement(event)
            return

        # don't do anything if game not playable
        if not self.is_playable:
            self.display_result("GAME NOT PLAYABLE")
            return

        # don't do anything if not at end of variation
        if self.current_node.next:
            self.display_result("NOT AT END OF VARIATION")
            return

        # @@@ does the following need to be here?
        if self.go_game.last_move_pass:
            self.display_result("PASS")
        else:
            self.display_result()

        # if the current_node is empty, use it otherwise
        # tentatively create new SGF node for this move
        if len(self.current_node.properties.keys()) == 0:
            sgf_node = self.current_node
        else:
            sgf_node = sgf.Node(self.current_gametree, self.current_node)

        # if click outside board area...
        if x < 1 or y < 1 or x > self.size or y > self.size:
            # treat as pass
            sgf_node.properties[self.move_property()] = [""]
            if sgf_node != self.current_node:
                self.current_gametree.nodes.append(sgf_node)
                self.current_node.next = sgf_node
                self.current_node = sgf_node
            self.go_game.pass_move()
            # draw current node
            self.draw_current_node()
            return

        # if point already occupied...
        if self.go_game.is_occupied(x, y):
            # report to user
            self.display_result("ILLEGAL MOVE (ALREADY OCCUPIED)")
            self.current_node.next = None
            return

        # set up some variables depending on whose go it is
        # @@@ could factor this out and use it elsewhere
        if self.go_game.to_play == gogame.BLACK:
            move_property = "B"
            prisoner_colour_name = "WHITE"
        else:
            move_property = "W"
            prisoner_colour_name = "BLACK"

        # attempt move (subject to ko and suicide rules)
        result = self.go_game.move_stone(x, y)

        if result.status == gogame.KO:
            self.display_result("ILLEGAL MOVE (KO)")
            self.current_node.next = None
        elif result.status == gogame.SUICIDE:
            self.display_result("ILLEGAL MOVE (SUICIDE)")
            self.current_node.next = None
        elif result.status == gogame.VALID:
            sgf_node.properties[move_property] = ["%s%s" % (sgf.SGF_POS[x], sgf.SGF_POS[y])]
            if sgf_node != self.current_node:
                self.current_gametree.nodes.append(sgf_node)
                self.current_node.next = sgf_node
                self.current_node = sgf_node
            self.draw_current_node()
            if len(result.prisoners) > 0: # if pieces captured...
                self.display_result("%s %s CAPTURED" % (len(result.prisoners), prisoner_colour_name))
                self.display_prisoner_count()

    def handle_placement(self, event):

        # can't place stones on a node with moves
        if self.current_node.properties.has_key("B") or self.current_node.properties.has_key("W"):
            self.display_result("CAN'T PLACE STONES ON A NODE WITH A MOVE")
            return

        # get point coordinates clicked on
        x = self.show_go.get_x(event.x)
        y = self.show_go.get_y(event.y)

        sgf_node = self.current_node

        # if click outside board area...
        if x < 1 or y < 1 or x > self.size or y > self.size:
            # do nothing
            return

        if self.placement == 1:
            colour = gogame.BLACK
            property = "AB"
        elif self.placement == 2:
            colour = gogame.WHITE
            property = "AW"

        self.go_game.place_stone(x, y, colour)

        if not sgf_node.properties.has_key(property):
            sgf_node.properties[property] = []
        sgf_node.properties[property].append("%s%s" % (sgf.SGF_POS[x], sgf.SGF_POS[y]))

        self.draw_current_node()

    def draw_current_node(self):
        node = self.current_node

        # display property info
        self.play_go.properties.config(state=NORMAL)
        self.play_go.properties.delete("1.0", END)
        for property in node.properties.keys():
            self.play_go.properties.insert(END, property+"\n")
            for value in node.properties[property]:
                self.play_go.properties.insert(END, "\t"+value+"\n")
        self.play_go.properties.config(state=DISABLED)

        # display variation info
        self.play_go.variations.config(state=NORMAL)
        self.play_go.variations.delete("1.0", END)
        if node.previous:
            for variation in node.previous.variations:
                if variation.properties.has_key("N"):
                    node_name = variation.properties["N"][0]
                elif variation.properties.has_key("B"):
                    node_name = "B" + variation.properties["B"][0]
                elif variation.properties.has_key("W"):
                    node_name = "W" + variation.properties["W"][0]
                else:
                    node_name = "node"
                self.play_go.variations.insert(END, node_name)
                if variation==node:
                    self.play_go.variations.insert(END, "<---")
                self.play_go.variations.insert(END, "\n")
        else:
            for gametree in self.collection.children:
                self.play_go.variations.insert(END, "Game")
                if gametree == self.current_node.parent:
                    self.play_go.variations.insert(END, "<---")
                self.play_go.variations.insert(END, "\n")
                
        self.play_go.variations.insert(END, "--------\n")
        for variation in node.variations:
            if variation.properties.has_key("N"):
                node_name = variation.properties["N"][0]
            elif variation.properties.has_key("B"):
                node_name = "B" + variation.properties["B"][0]
            elif variation.properties.has_key("W"):
                node_name = "W" + variation.properties["W"][0]
            else:
                node_name = "node"
            self.play_go.variations.insert(END, node_name+"\n")
        if not self.current_node.next:
            self.play_go.variations.insert(END, "***END***")
        self.play_go.variations.config(state=DISABLED)

        # construct a list of nodes that apply to this game up until
        # (but not including) node being displayed,
        # finding last size and handicap        
        # @@@ should this be in SGF module?
        node_list = []
        size = 0
        handicap = 0

        if node.properties.has_key("SZ") and not size:
            size = int(node.properties["SZ"][0])
        if node.properties.has_key("HA") and not handicap:
            handicap = int(node.properties["HA"][0])

        node = node.previous
        while node:
            node_list.insert(0, node)
            if node.properties.has_key("SZ") and not size:
                size = int(node.properties["SZ"][0])
            if node.properties.has_key("HA") and not handicap:
                handicap = int(node.properties["HA"][0])
            node = node.previous
        # if no size specified assume 19
        if not size:
            size = 19

        self.size = size

        # create a new ShowGo and GoGame
        # @@@ this suggests that some of the calls to gogame in handle_move_event are redundant
        self.show_go = s = showgo.ShowGo(self.play_go.canvas, size, self.scale)
        self.go_game = g = gogame.GoGame(size, handicap=handicap)

        def move_stone(move_property, s_colour, g_colour):
            pos = node.properties[move_property][0]
            if pos == "" or pos == "tt":
                g.pass_move() # @@@ should it give colour?
            else:
                x = ord(pos[0]) - 96
                y = ord(pos[1]) - 96
                s.draw_stone(x, y, s_colour)
                result = g.move_stone(x, y, g_colour, allow_suicide=1)
                if result.status == gogame.VALID:
                    for (xx,yy) in result.prisoners:
                        s.erase_stone(xx,yy)

        def place_stones(move_property, s_colour, g_colour):
            for pos in node.properties[move_property]:
                if ":" in pos:
                    sx = ord(pos[0])-96
                    sy = ord(pos[1])-96
                    ex = ord(pos[3])-96
                    ey = ord(pos[4])-96
                    for xx in range(sx, ex + 1):
                        for yy in range(sy, ey + 1):
                            s.draw_stone(xx, yy, s_colour)
                            g.place_stone(xx, yy, g_colour)
                else:    
                    xx = ord(pos[0])-96
                    yy = ord(pos[1])-96
                    s.draw_stone(xx, yy, s_colour)
                    g.place_stone(xx, yy, g_colour)

        def erase_stones(property):
            for pos in node.properties["AE"]:
                if ":" in pos:
                    sx = ord(pos[0])-96
                    sy = ord(pos[1])-96
                    ex = ord(pos[3])-96
                    ey = ord(pos[4])-96
                    for xx in range(sx, ex + 1):
                        for yy in range(sy, ey + 1):
                            s.erase_stone(xx, yy)
                            g.erase_stone(xx, yy)
                else:    
                    xx = ord(pos[0])-96
                    yy = ord(pos[1])-96
                    s.erase_stone(xx, yy)
                    g.erase_stone(xx, yy)

        def draw_territory(property, s_colour):
            for pos in node.properties[property]:
                if ":" in pos:
                    sx = ord(pos[0])-96
                    sy = ord(pos[1])-96
                    ex = ord(pos[3])-96
                    ey = ord(pos[4])-96
                    for xx in range(sx, ex + 1):
                        for yy in range(sy, ey + 1):
                            s.draw_territory(xx, yy, s_colour)
                else:    
                    xx = ord(pos[0])-96
                    yy = ord(pos[1])-96
                    s.draw_territory(xx, yy, s_colour)

        def draw_shape(property, s_shape):
            for pos in node.properties[property]:
                if pos == "" or pos == "tt":
                    continue
                if ":" in pos:
                    sx = ord(pos[0])-96
                    sy = ord(pos[1])-96
                    ex = ord(pos[3])-96
                    ey = ord(pos[4])-96
                    for xx in range(sx, ex + 1):
                        for yy in range(sy, ey + 1):
                            s.draw_shape(xx, yy, s_shape)
                else:    
                    xx = ord(pos[0])-96
                    yy = ord(pos[1])-96
                    s.draw_shape(xx, yy, s_shape)

        def draw_label():
            for value in node.properties["LB"]:
                pos = value[:2]
                if len(value) == 4:
                    label = value[3]
                else:
                    label = value[3:5]
                xx = ord(pos[0])-96
                yy = ord(pos[1])-96
                s.draw_label(xx, yy, label)

        # go through each node in list
        for node in node_list:
            if node.properties.has_key("B"):
                move_stone("B", s.BLACK, gogame.BLACK)
            elif node.properties.has_key("W"):
                move_stone("W", s.WHITE, gogame.WHITE)
            else:
                if node.properties.has_key("AB"):
                    place_stones("AB", s.BLACK, gogame.BLACK)
                if node.properties.has_key("AW"):
                    place_stones("AW", s.WHITE, gogame.WHITE)
                if node.properties.has_key("AE"):
                    erase_stones("AE")

        # display current_node
        node = self.current_node
        if node.properties.has_key("B"):
            move_stone("B", s.BLACK, gogame.BLACK)
            draw_shape("B", s.SELECT)
        elif node.properties.has_key("W"):
            move_stone("W", s.WHITE, gogame.WHITE)
            draw_shape("W", s.SELECT)
        else:
            if node.properties.has_key("AB"):
                place_stones("AB", s.BLACK, gogame.BLACK)
            if node.properties.has_key("AW"):
                place_stones("AW", s.WHITE, gogame.WHITE)
            if node.properties.has_key("AE"):
                erase_stones("AE")
        if node.properties.has_key("TB"):
            draw_territory("TB", s.BLACK)
        if node.properties.has_key("TW"):
            draw_territory("TW", s.WHITE)
        if node.properties.has_key("TR"):
            draw_shape("TR", s.TRIANGLE)
        if node.properties.has_key("CR"):
            draw_shape("CR", s.CIRCLE)
        if node.properties.has_key("SQ"):
            draw_shape("SQ", s.SQUARE)
        if node.properties.has_key("SL"):
            draw_shape("SL", s.SELECT)
        if node.properties.has_key("MA"):
            draw_shape("MA", s.MARKER)
        if node.properties.has_key("LB"):
            draw_label()


        self.display_state()
        self.display_prisoner_count()
        if self.go_game.last_move_pass:
            self.display_result("PASS")
        else:
            self.display_result()
        if self.show_territory:
            self.draw_territory()


class PlayGameDialog(tkSimpleDialog.Dialog):
    def __init__(self, master):
        self.board_size = 0
        self.handicap = 0
        tkSimpleDialog.Dialog.__init__(self, master, "Play Game")

    def body(self, master):
        label_1 = Label(master, text="Board Size", justify=LEFT)
        self.board_size_entry = Entry(master, name="board_size") # @@@ make a drop down
        self.board_size_entry.insert(0, 19)
        self.board_size_entry.select_range(0, END)
        label_2 = Label(master, text="Handicap", justify=LEFT)
        self.handicap_entry = Entry(master, name="handicap")
        self.handicap_entry.insert(0, 0)
        label_1.pack()
        self.board_size_entry.pack()
        label_2.pack()
        self.handicap_entry.pack()
        return self.board_size_entry

    def validate(self):
        try:
            board_size = int(self.board_size_entry.get())
        except ValueError:
            tkMessageBox.showerror("Invalid Board Size", "Board Size must be a number", parent=self)
            return 0
        if board_size not in [9,13,19]:
            tkMessageBox.showerror("Invalid Board Size", "Board Size must be 9, 13 or 19", parent=self)
            return 0
        try:
            handicap = int(self.handicap_entry.get())
        except ValueError:
            tkMessageBox.showerror("Invalid Handicap", "Handicap must be a number", parent=self)
            return 0
        if handicap < 0 or handicap > 9:
            tkMessageBox.showerror("Invalid Handicap", "Handicap must be between 0 and 9", parent=self)
            return 0
        self.board_size = board_size
        self.handicap = handicap
        return 1


class PyGo:
    def __init__(self, filename=None):
        self.scale = 25
        self.root = Tk()
        self.root.title(app_name + " " + version)
        self.create_menu()

        frame = Frame(self.root)
        self.canvas = Canvas(frame, width=self.scale*21, height=self.scale*21, background="#DDD")
        self.canvas.pack(side=TOP)
        status = Frame(frame)
        self.label1 = Label(status, relief="sunken")
        self.label1.pack(side=LEFT, expand=YES, fill=BOTH)
        self.label2 = Label(status, relief="sunken")
        self.label2.pack(side=RIGHT, expand=YES, fill=BOTH)
        self.label3 = Label(status, relief="sunken")
        self.label3.pack(side=BOTTOM, expand=YES, fill=BOTH)
        status.pack(side=BOTTOM, fill=BOTH)
        frame.grid(column=0, row=0, rowspan=2)

        self.variations = ScrolledText.ScrolledText(self.root, width=40, height=20, state=DISABLED, background="#EEE", relief="flat")
        self.variations.grid(column=1, row=0)

        self.properties = ScrolledText.ScrolledText(self.root, width=40, height=20, state=DISABLED, background="#EEE" , relief="flat")
        self.properties.grid(column=1, row=1)

        self.game = None
        if filename:
            self.open_sgf(filename)

    def create_menu(self):
        menu_bar = Menu(self.root)
        file_menu = Menu(menu_bar, name="file", tearoff=0)
        menu_bar.add_cascade(menu=file_menu, label="File", underline=0)

        file_menu.add_command(label="Open SGF", command=self.open_sgf, underline=0)
        file_menu.add_command(label="Play Game", command=self.play_game, underline=0)
        file_menu.add_command(label="Save SGF", command=self.save_sgf, underline=0) # @@@ should be disabled
        file_menu.add_command(label="Quit", command=self.quit, underline=0)
        help_menu = Menu(menu_bar, name="help", tearoff=0)
        menu_bar.add_cascade(menu=help_menu, label="Help", underline=0)
        help_menu.add_command(label="Keys...", command=self.show_help, underline=0)
        help_menu.add_command(label="About...", command=self.show_about, underline=0)
        self.root.config(menu=menu_bar)

    ### MENU ACTIONS
        
    def open_sgf(self, filename=None):
        if filename:
            fn = filename
        else:
            fn = tkFileDialog.askopenfilename(filetypes=[("Smart Game Format","*.sgf"),("All files","*")])
        if fn:
            try:
                parser = sgf.Parser()
                f = file(fn)
                x = f.read()
                f.close()
                collection = sgf.Collection(parser)
                parser.parse(x)
                self.game = Game(self, collection, scale=self.scale)
            except sgf.ParseException, x:
                tkMessageBox.showerror("Invalid File", "The SGF file could not be parsed [%s]" % x)
                
    def play_game(self):
        play_game_dialog = PlayGameDialog(self.root)
        if play_game_dialog.board_size:
            self.game = Game(self, None, size=play_game_dialog.board_size, handicap=play_game_dialog.handicap, scale=self.scale)

    def save_sgf(self):
        if self.game:
            fn = tkFileDialog.asksaveasfilename(filetypes=[("Smart Game Format", "*.sgf"), ("All files", "*")])
            if fn:
                f = file(fn, "w")
                self.game.output(f)
                f.close()

    def quit(self):
        self.root.destroy()

    def show_help(self):
        tkMessageBox.showinfo("Help",
"""click to move stones\n
left-arrow to go to previous node
right-arrow to go to next node
up-arrow to go to previous variation
down-arrow to go to next variation
shift-left-arrow to go to first node in variation
shift-right-arrow to go to last node in variation\n
'v' to make a new variation
'u' to undo a move
'p' to toggle play / place black / place white
'c' to continue playing from end of a loaded game
't' to toggle territory/score
'i' to insert a new node
""")

    def show_about(self):
        tkMessageBox.showinfo("About", "%s %s\nby James Tauber\nhttp://jtauber.com/" % (app_name, version))

if len(sys.argv) > 1:
    p = PyGo(sys.argv[1])
else:
    p = PyGo()
p.root.mainloop()        
