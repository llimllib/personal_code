#! /usr/bin/python
############################################################################
#                go_random.py  -  Generates random legal Go moves          #
#                             -------------------                          #
#    date                 : January 12 2003                                #
#    copyright            : (C) 2003 by Bill Mill                          #
#    email                : llimllib@f2o.org                               #
#                                                                          #
#   This program is free software; you can redistribute it and/or modify   #
#   it under the terms of the GNU General Public License as published by   #
#   the Free Software Foundation; either version 2 of the License, or      #
#   (at your option) any later version.                                    #
#                                                                          #
############################################################################

import random

class rand_engine:
    """This class generates a random legal move on a go board."""
    def __init__(self, debugfile, size):
        self.debugfile = debugfile
        self.color = ""
        self.board = [[]]
        self.size = size
        self.rand = random.Random()     #random number generator
        self.name = "Go Random!!!"
        self.version = ".01"

        self.clear_board()
    
    def get_name(self):
        return self.name
    
    def get_version(self):
        return self.version

    def play(self, color, x, y):
        """play a piece of color at board coords x,y"""
        if(self.color == ""):
            self.color = color
        self.board[x][y] = self.color
    
    def remove(self, move):
        """remove a single stone from the board
        
        move is a tuple of the form (x,y)
        """
        self.board[move[0]][move[1]] = " "
        
    def clear_board(self):
        self.board = []
        for i in range(self.size):
            self.board.append([])    #append a row
            for j in range(self.size):
                self.board[i].append(' ')
        return 1
        
    def genmove(self, color):
        """generate a random legal move"""
        self.x = self.rand.randint(0,self.size - 1)
        self.y = self.rand.randint(0,self.size - 1)
        while self.board[self.x][self.y] != " ":
            self.x = self.rand.randint(0,self.size - 1)
            self.y = self.rand.randint(0,self.size - 1)
        move = (self.x, self.y)
        self.play(color, self.x, self.y)
        self.debugfile.write("rand_engine generated: " +  str(self.x) + "|" + str(self.y) + "\n")
        return move
    
    def should_pass(self):
        """the random engine always passes if the opponent has,
        because it doesn't implement any logic
        """
        return 1
