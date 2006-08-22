### SGF.PY

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

# module for representing, parsing and writing out SGF files
#
# TO PARSE
# given a file_name
#     parser = sgf.Parser()
#     f = file(file_name)
#     sgf_string = f.read()
#     f.close()
#     collection = sgf.Collection(parser)
#     parser.parse(x)
# collection now represents the SGF collection
#
# TO SAVE
# given a file_name
#     f = file(file_name, "w")
#     collection.output(f)
#     f.close()
#
# THE OBJECTS
# Collection has children[] each of which is a GameTree
# GameTree has nodes[] each of which is a Node
#          and children[] each of which is a GameTree
# Node has properties[] dictionary with string keys and values
#      and previous - previous node in SGF
#          next - next node in SGF
#          previous_variation - previous variation (if first node in a variation)
#          next_variation - next variation (if first node in a variation)
#          first - boolean indicating when first node in a variation
#          variations[] - list of variations immediately from this node


### IMPORTS

import string


### CONSTANTS

# map from numerical coordinates to letters used by SGF
SGF_POS = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


### SGF OBJECTS

class Collection:
    def __init__(self, parser=None):
        self.parser = parser
        if parser:
            self.setup()
        self.children = []

    def setup(self):
        self.parser.start_gametree = self.my_start_gametree
        
    def my_start_gametree(self):
        self.children.append(GameTree(self, self.parser))

    def output(self, f):
        for child in self.children:
            child.output(f)

         
class GameTree:
    def __init__(self, parent, parser=None):
        self.parent = parent
        self.parser = parser
        if parser:
            self.setup()
        self.nodes = []
        self.children = []
        
    def setup(self):
        self.parser.start_gametree = self.my_start_gametree
        self.parser.end_gametree = self.my_end_gametree
        self.parser.start_node = self.my_start_node

    def my_start_node(self):
        if len(self.nodes) > 0:
            previous = self.nodes[-1]
        elif self.parent.__class__ == GameTree:
            previous = self.parent.nodes[-1]
        else:
            previous = None
        node = Node(self, previous, self.parser)
        if len(self.nodes) == 0:
            node.first = 1
            if self.parent.__class__ == GameTree:
                if len(previous.variations) > 0:
                    previous.variations[-1].next_variation = node
                    node.previous_variation = previous.variations[-1]
                previous.variations.append(node)
            else:
                if len(self.parent.children) > 1:
                    node.previous_variation = self.parent.children[-2].nodes[0]
                    self.parent.children[-2].nodes[0].next_variation = node
                
        self.nodes.append(node)

    def my_start_gametree(self):
        self.children.append(GameTree(self, self.parser))

    def my_end_gametree(self):
        self.parent.setup()

    def output(self, f):
        f.write("(")
        for node in self.nodes:
            node.output(f)
        for child in self.children:
            child.output(f)
        f.write(")")


class Node:
    def __init__(self, parent, previous, parser=None):
        self.parent = parent
        self.previous = previous
        self.parser = parser
        if parser:
            self.setup()
        self.properties = {}
        self.next = None
        self.previous_variation = None
        self.next_variation = None
        self.first = 0
        self.variations = []
        if previous and not previous.next:
            previous.next = self

    def setup(self):
        self.parser.start_property = self.my_start_property
        self.parser.add_prop_value = self.my_add_prop_value
        self.parser.end_property = self.my_end_property
        self.parser.end_node = self.my_end_node

    def my_start_property(self, identifier):
        # @@@ check for duplicates
        self.current_property = identifier
        self.current_prop_value = []
        
    def my_add_prop_value(self, value):
        self.current_prop_value.append(value)
        
    def my_end_property(self):
        self.properties[self.current_property] = self.current_prop_value
        
    def my_end_node(self):
        self.parent.setup()

    def output(self, f):
        f.write(";")
        for property in self.properties.keys():
            f.write(property)
            for value in self.properties[property]:
                if "\\" in value:
                    value = string.join(value.split("\\"), "\\\\")
                if "]" in value:
                    value = string.join(value.split("]"), "\]")
                f.write("[%s]" % value)


### SGF PARSER

class ParseException(Exception):
    pass


class Parser:

    def parse(self, sgf_string):

        def whitespace(ch):
            return ch in " \t\r\n"

        def ucletter(ch):
            return ch in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        def lcletter(ch):
            return ch in "abcdefghijklmnopqrstuvwxyz"

        state = 0

        for ch in sgf_string:
            if state == 0:
                if whitespace(ch):
                    state = 0
                elif ch == '(':
                    self.start_gametree()
                    state = 1
                else:
                    state = 0 # ignore everything up to first (
                    # raise ParseException, (ch, state)
            elif state == 1:
                if whitespace(ch):
                    state = 1
                elif ch == ";":
                    self.start_node()
                    state = 2
                else:
                    raise ParseException, (ch, state)
            elif state == 2:
                if whitespace(ch):
                    state = 2
                elif ucletter(ch):
                    prop_ident = ch
                    state = 3
                elif ch == ";":
                    self.end_node()
                    self.start_node()
                    state = 2
                elif ch == "(":
                    self.end_node()
                    self.start_gametree()
                    state = 1
                elif ch == ")":
                    self.end_node()
                    self.end_gametree()
                    state = 4
                else:
                    raise ParseException, (ch, state)
            elif state == 3:
                if ucletter(ch):
                    prop_ident = prop_ident + ch
                    state = 3
                elif lcletter(ch): # @@@ currently ignoring lowercase
                    state = 3
                elif ch == "[":
                    self.start_property(prop_ident)
                    prop_value = ""
                    state = 5
                else:
                    raise ParseException, (ch, state)
            elif state == 4:
                if ch == ")":
                    self.end_gametree()
                    state = 4
                elif whitespace(ch):
                    state = 4
                elif ch == "(":
                    self.start_gametree()
                    state = 1
                else:
                    raise ParseException, (ch, state)
            elif state == 5:
                if ch == "\\":
                    state = 6
                elif ch == "]":
                    self.add_prop_value(prop_value)
                    state = 7
                else:
                    prop_value = prop_value + ch
            elif state == 6:
                prop_value = prop_value + ch
                state = 5
            elif state == 7:
                if whitespace(ch):
                    state = 7
                elif ch == "[":
                    prop_value = ""
                    state = 5
                elif ch == ";":
                    self.end_property()
                    self.end_node()
                    self.start_node()
                    state = 2
                elif ucletter(ch):
                    self.end_property()
                    prop_ident = ch
                    state = 3
                elif ch == ")":
                    self.end_property()
                    self.end_node()
                    self.end_gametree()
                    state = 4
                elif ch == "(":
                    self.end_property()
                    self.end_node()
                    self.start_gametree()
                    state = 1
                else:
                    raise ParseException, (ch, state)
            else:
                raise ParseException, (ch, state)
                pass

        if state != 4:
            raise ParseException, (ch, state)
