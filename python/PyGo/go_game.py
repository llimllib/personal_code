#! /usr/bin/python
############################################################################
#                go_game.py  -  Manages a go board
#                             -------------------
#    date                 : January 12 2003
#    copyright            : (C) 2003 by Bill Mill
#    email                : llimllib@f2o.org
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
############################################################################

import sys

DEBUG=1         #debugging constant

#Define exceptions
class GoError(Exception): pass
class InvalidMoveSyntaxError(GoError): pass
class PlayCrossError(GoError): pass

class go_game:
    """
    This class handles the representation of a go board, of
    removing dead groups, playing pieces, and determining territory for
    each player.
    """
    def __init__(self,debugfile):
        self.color = ""              #should be deletable...
        self.ko = (-1,-1)            #most recent ko coordinate
        self.komi = 0.0              #komi number
        self.black_handicap = 0      #black's handicap
        self.is_pass = 0             #whether the last move was a pass or not
        self.size = 0                #the board size (# of squares per side)
        self.board = [[]]            #2d array; self.size rows and columns
        self.debugfile = debugfile
        self.move_history = []       #array of all the moves played thus far
        self.groups = []             #array of group objects
        self.blank_group = []        #used by makeBlankGroup
        self.scanned_blanks = []     #blanks already scanned by makeBlankGroup
        self.removed_pieces = []     #pieces removed by removedead()
        self.white_score = 0         #white's score
        self.black_score = 0         #black's score
        
        self.clear_board()           #make sure our board is empty

    def set_komi(self, komi):
        """sets the number of komi. Komi are points given to the white
        player, who goes second and thus is at a disadvantage in the game.
        A typical number of komi is 5.5
        """
        self.komi = float(komi)
        return 1

    def set_boardsize(self, size):
        self.size = int(size)
        self.clear_board()
        return 1
    
    def set_handicap(self, stones):
        """
        Place a fixed handicap between 2 and 9 pieces. This functions is only
        implemented for common board sizes of 9, 13, and 19.
        Handicap functions adapted from Wallyplus, in turn adapted from Todd R.
        Johnson's AmiGo program. I haven't checked it for correctness, so 
        if there's a problem with it, let me know.
        """
        stones = int(stones)
        if stones < 2 or stones > 9:
            return 0
        if self.size == 9:
            response = self.place_handicap9(stones)
            return response
        elif self.size == 13:
            response = self.place_handicap13(stones)
            return response
        elif self.size == 19:
            response = self.place_handicap19(stones)
            return response
        else:
            return 0
    
    def place_handicap9(self, stones):
        """Place handicap stones on a 9x9 board"""
        self.play("BLACK C3")
        self.play("BLACK G7")
        response = "C3 G7"

        if stones > 2:
            self.play("BLACK C7")
            response += " C7"
        if stones > 3:
            self.play("BLACK G3")
            response += " G3"

        return response
        
    def place_handicap13(self, stones):
        """Place handicap stones on a 13x13 board"""
        self.play("BLACK D4")
        self.play("BLACK K10")
        response = "D4 K10"
        
        if stones > 2:
            self.play("BLACK D10")
            response += " D10"
        if stones > 3:
            self.play("BLACK K4")
            response += " K4"
        if stones == 5 or stones == 7 or stones == 9:
            self.play("BLACK G7")
            response += " G7"
        if stones > 5:
            self.play("BLACK K7")
            self.play("BLACK D7")
            response += " K7 D7"
        if stones > 7:
            self.play("BLACK G10")
            self.play("BLACK G4")
            response += " G10 G4"
        
        return response
    
    def place_handicap19(stones):
        self.play("BLACK D4")
        self.play("BLACK Q16")
        response = "D4 Q16"
        
        if stones > 2:
            self.play("BLACK D16")
            response += " D16"
        if stones > 3:
            self.play("BLACK Q4")
            response += " Q4"
        if stones == 5 or stones == 7 or stones == 9:
            self.play("BLACK K10")
            response += " K10"
        if stones > 5:
            self.play("BLACK D10")
            self.play("BLACK Q10")
            response += " D10 Q10"
        if stones > 7:
            self.play("BLACK K4")
            self.play("BLACK K16")
            response += " K4 K16"
        
        return response

    def clear_board(self):
        self.board = []
        for i in range(self.size):
            self.board.append([])
            for j in range(self.size):
                self.board[i].append(' ')
        return 1
    
    def translate_from_gtp(move):
        """
        takes a move in GTP format and returns a move in native format
        
        move is a string containing first a vertical board coordinate
        followed by a horizontal one. The vertical board coordinates are
        letters from A-Z(case insensitive) except for the letter 'I', which
        is reserved. They are enumerated left to right. the horizontal coord
        is a number 1-25, enumerated bottom to top.
        
        native format is a tuple (x, y) where (0,0) is the top left of the
        board and (self.size, 0) is the top right.
        
        If gtp_convert finds an error, it throws an InvalidMoveSyntaxError.
        """
        move = str(move).strip().upper()
        play_color, vertex = move.split(" ")
        x = vertex[0]
        if x > "H":                          #exclude "I"
            x = ord(x) - 66
        else:
            x = ord(x) - 65
        y = self.size - int(vertex[1:])
        play_color = play_color[0]

        if play_color != "W" and play_color != "B":
            raise InvalidMoveSyntaxError, "color is invalid"
        elif 0 < x < (self.size - 1):
            raise InvalidMoveSyntaxError, "x is invalid"
        elif 0 < y < (self.size - 1):
            raise InvalidMoveSyntaxError, "y is invalid"
        else: return (play_color, (x, y))

    def play(self, move, color=""):
        """
        Plays a piece of color at cross
        
        if color is not set, play assumes that move is in GTP format.
        
        First, play_cross writes the piece to the board, catching 
        errors in the move parsing and writing them to debugfile.
        
        Finally, the board is parsed into groups, and groups that are
        dead are removed from the board. The removed pieces are returned.
        
        If there is a problem, raise a playCrossError.
        """
        self.is_pass -= 1   #TODO: make a better passing system
        if color == "":
            color, move = translate_from_gtp(move)

        try:
            self.board[move[0]][move[1]] = color
        except:
            self.print_board()
            self.debugfile.write("board coordinates error in play_cross()\n")
            self.debugfile.write("x_coord: %d y_coord: %d\n" % (cross[0],cross[1]))
            self.debugfile.write('color: "%s"\n' % (str(color)))
            self.debugfile.write("self.size: %d\n" % (int(self.size)))
            self.debugfile.write("len(board): %d\n" % (len(self.board)))
            raise PlayCrossError

        self.move_history.append(move)

        self.ko = cross #TODO: make ko work, dumbass

        self.make_groups(color, move)
        self.count_liberties()
        self.remove_dead()
        
        if(DEBUG):
            self.print_board()

        return self.removed_pieces
        
        return 1
    
    def remove_piece(self, move):
        self.board[move[0]][move[1]] = " "
        self.removed_pieces.append(move)

    def remove_group(self, pos):
        """recursively remove every stone in the group beginning at  a given position
        
        pos is a tuple of the form (x,y) containing board coords.
        
        How the algorithm works:
            given a position with a piece of a given color, that piece is
            first removed. Then, if an adjacent square has the same color,
            remove_group is called on that position.
        
        Idea taken from wallyplus, modified for python. Wallyplus is at:
        ftp://ibiblio.org/pub/linux/games/strategy/wallyplus-0.1.2.tar.gz
        """
        if not self.onboard(pos):
            return 0
        
        cur_color = self.board[pos[0]][pos[1]]
        if cur_color == " ":
            return 0
        
        self.remove_piece(pos)
        
        if self.onboard((pos[0]+1, pos[1])) and \
            self.board[pos[0] + 1][pos[1]] == cur_color:
            self.remove_group((pos[0]+1, pos[1]))
        
        if self.onboard((pos[0]-1, pos[1])) and \
            self.board[pos[0] - 1][pos[1]] == cur_color:
            self.remove_group((pos[0]-1, pos[1]))
            
        if self.onboard((pos[0], pos[1]+1)) and \
            self.board[pos[0]][pos[1] + 1] == cur_color:
            self.remove_group((pos[0], pos[1]+1))
            
        if self.onboard((pos[0], pos[1]-1)) and \
            self.board[pos[0]][pos[1] - 1] == cur_color:
            self.remove_group((pos[0], pos[1]-1))
            
    def remove_dead(self):
        """remove all dead groups from the board.
        
        for each group, if it has no liberties, remove it from the board
        then remove the group from the groups array
        """
        removed_pieces = []
        remove_queue = []
        for i in range(len(self.groups)):
            g = self.groups[i]
            if g.liberties == 0:
                pos = self.groups[i].return_piece()
                if(DEBUG):
                    self.debugfile.write("piece " + str(pos) + " needed to be removed")
                self.remove_group(pos)
                self.count_liberties()
                remove_queue.append(self.groups[i])
        for i in range(len(remove_queue)):
            self.groups.remove(remove_queue[i])

    def make_groups(self, color, move):
        """
        parse the board into groups, making changes based on move
        
        move is a tuple of board position (x,y) that represents the most
        recent move.
        
        A group is a string of pieces on adjacent squares. This function
        checks the squares adjacent to move and merges groups if move has
        connected two previously unconnected groups.
        """
        indices = []    #indices of groups to which piece belongs
        
        #this could be faster if we added up how many 
        if is_adjacent_piece(color, move):
            for i in range(len(self.groups)):
                if self.groups[i].color() == color \
                    and self.groups[i].is_adjacent(move):
                    indices.append(i)

        if not indices:
            g = group(color, move)
            self.groups.append(g)
            if DEBUG:
                self.debugfile.write('m_g: found 0 groups to which this piece belongs\n')

        elif len(indices) == 1:
            self.groups[indices[0]].add_piece(move)
            if DEBUG:
                self.debugfile.write('m_g: found 1 group to which the piece belongs\n')

        else:
            if DEBUG:
                self.debugfile.write("indices: " + str(indices) + "\n")
            self.groups[indices[0]].add_piece(move)
            for i in range(1, len(indices)):
                pieces = self.groups[indices[i]].return_pieces()
                if DEBUG:
                    self.debugfile.write("pieces: " + str(pieces) + "\n")
                self.groups[indices[0]].add_pieces(pieces)
            for i in range(1, len(indices)):
                self.groups.remove(self.groups[indices[i]-(i-1)])
                if DEBUG:
                    self.debugfile.write("removed group: " + \
                    str(indices[i]-(i-1)) + "\n")
            if DEBUG:
                self.debugfile.write('m_g: found >1 group to which the piece belongs\n')

    def count_liberties(self):
        """counts the liberties of each group on the board.
        
        for each piece in each group, it checks for open spaces adjacent
        to the group, and adds a liberty if one is found.
        """
        for i in range(len(self.groups)):
            self.groups[i].liberties = 0
            pieces = self.groups[i].return_pieces()
            for j in range(len(pieces)):
                if self.onboard((pieces[j][0]+1,pieces[j][1])) \
                    and self.board[pieces[j][0]+1][pieces[j][1]] == " ":
                    self.groups[i].add_liberty(1)
                if self.onboard((pieces[j][0]-1,pieces[j][1])) \
                    and self.board[pieces[j][0]-1][pieces[j][1]] == " ":
                    self.groups[i].add_liberty(1)
                if self.onboard((pieces[j][0],pieces[j][1]+1)) \
                    and self.board[pieces[j][0]][pieces[j][1]+1] == " ":
                    self.groups[i].add_liberty(1)
                if self.onboard((pieces[j][0],pieces[j][1]-1)) \
                    and self.board[pieces[j][0]][pieces[j][1]-1] == " ":
                    self.groups[i].add_liberty(1)

    def print_board(self):
        """prints out a formatted picture of the board"""
        self.debugfile.write("\n   ")
        if not DEBUG:
            for x in range(self.size):
                if x < 8:
                    self.debugfile.write(chr(65 + x) + " ")
                else:
                    self.debugfile.write(chr(66 + x) + " ")
            for y in range(self.size):
                self.debugfile.write("\n" + str(y+1) + " |")
                for x in range(self.size):
                    self.debugfile.write(self.board[x][y] + "|")
            self.debugfile.write("\n")
        else:
            for x in range(self.size):
                self.debugfile.write(str(x) + " ")
            for y in range(self.size):
                self.debugfile.write("\n" + str(y) + " |")
                for x in range(self.size):
                    self.debugfile.write(self.board[x][y] + "|")
            self.debugfile.write("\n")

    def onboard(self, pos):
        """returns true if pos, a tuple (x,y) is on the board"""
        if pos[0] < 0 or pos[0] > self.size - 1:
            return 0
        if pos[1] < 0 or pos[1] > self.size - 1:
            return 0
        return 1
    
    def is_adjacent_piece(self, color, move):
        """returns true if there is a piece of color adjacent to move"""
        i = 0
        if board[move[0]+1][move[1]] == color: i+=1
        if board[move[0]-1][move[1]] == color: i+=1
        if board[move[0]][move[1]+1] == color: i+=1
        if board[move[0]][move[1]-1] == color: i+=1
        return i
        
    def final_score(self):
        """score the game using Chinese rules
    
        Chinese rules say that a player's score is the number of pieces they
        have on the board plus the number of spaces surrounded only by 
        their pieces.
        TODO: make this function, y'know, work?
        """        
        for x in range(self.size):
            for y in range(self.size):
                if self.board[x][y] == "W":
                    self.white_score += 1
                elif self.board[x][y] == "B":
                    self.black_score += 1
                elif self.board[x][y] == " ":
                    if not self.makeBlankGroup((x,y)) and DEBUG:
                        self.debugfile.write("error in makeBlankGroup")
                    sbg = self.scoreBlankGroup()
                    if sbg == "W":
                        self.white_score += len(self.blank_group)
                    elif sbg == "B":
                        self.black_score += len(self.blank_group)
                    self.scanned_blanks.append(self.blank_group)
                    self.blank_group = []
        
        self.white_score += self.komi
        
        if self.white_score > self.black_score:
            score = "W+"+str(self.white_score)
        else:
            score = "B+"+str(self.black_score)
        
        return score

    def scoreBlankGroup(self):
        """score the most recently made Blank Group"""
        b_color = ""
        
        #for each piece in the blank group, for any adjacent square that 
        #is not blank, if that square doesn't match b_color (the first color
        #found), return false.
        bg = self.blank_group
        for i in range(len(bg)):
            x = bg[i][0]
            y = bg[i][1]
            if self.onboard((x+1,y)) \
                and self.board[x+1][y] != " ":
                if not b_color:
                    b_color = self.board[x+1][y]
                if self.board[x+1][y] != b_color:
                    return 0
            
            if self.onboard((x-1,y)) \
                and self.board[x-1][y] != " ":
                if not b_color:
                    b_color = self.board[x-1][y]
                if self.board[x-1][y] != b_color:
                    return 0
                
            if self.onboard((x,y+1)) \
                and self.board[x][y+1] != " ":
                if not b_color:
                    b_color = self.board[x][y+1]
                if self.board[x][y+1] != b_color:
                    return 0
                
            if self.onboard((x,y-1)) \
                and self.board[x][y-1] != " ":
                if not b_color:
                    b_color = self.board[x][y-1]
                if self.board[x][y-1] != b_color:
                    return 0

        #if all the pieces around the group are the same color, return 1
        return b_color


    def makeBlankGroup(self, pos):
        #first, do a sanity check:
        if self.onboard(pos) and self.board[pos[0]][pos[1]] == " ":
            self.blank_group.append(pos)
        else:
            return 0
        
        x = pos[0]
        y = pos[1]
        
        #now, if an adjacent square exists, is not already in blank_group, and 
        #is blank, call this function to add it to blank_group
        if self.onboard((x+1,y)) \
            and self.board[x+1][y] == " " \
            and (x+1,y) not in self.blank_group:
            self.makeBlankGroup((x+1,y))

        if self.onboard((x-1,y)) \
            and self.board[x-1][y] == " " \
            and (x-1,y) not in self.blank_group:
            self.makeBlankGroup((x-1,y))

        if self.onboard((x,y+1)) \
            and self.board[x][y+1] == " " \
            and (x,y+1) not in self.blank_group:
            self.makeBlankGroup((x,y+1))

        if self.onboard((x,y-1)) \
            and self.board[x][y-1] == " " \
            and (x,y-1) not in self.blank_group:
            self.makeBlankGroup((x,y-1))

class group:
    """
    This is a helper class to represent a group on the board.
    A group is a string of pieces adjacent to each other horizontally
    or vertically
    """
    #TODO: change all the return_* methods to get_* methods
    def __init__(self, color, pos):
        self.color = color
        self.pieces = [pos]
        self.liberties = 0
        
    def get_color(self):
        return self.color
    
    def return_pieces(self):
        return self.pieces
        
    def return_piece(self):
        return self.pieces[0]

    def add_liberty(self, libs):
        self.liberties += libs
    
    def add_piece(self, pos):
        """add a piece to the group at pos.
        
        pos is a tuple of (x_coord, y_coord) on a zero-based board
        """
        
        self.pieces.append(pos)
        
    def add_pieces(self, pieces):
        """add a set of pieces to the group.
        
        pieces is an array of (x,y) tuples
        """
        
        for i in range(len(pieces)):
            self.add_piece(pieces[i])
        
    def is_element(self, pos):
        """returns true if pos (a tuple (x,y)) is an element of the group"""
        try:
            self.pieces.index(pos)
            return 1
        except:
            return 0

    def is_adjacent(self, pos):
        """returns true if there is a piece of color adjacent to pos"""
        
        if self.is_element((pos[0]+1, pos[1])) \
            or self.is_element((pos[0]-1, pos[1])) \
            or self.is_element((pos[0], pos[1]+1)) \
            or self.is_element((pos[0], pos[1]-1)):
            return 1
        return 0
