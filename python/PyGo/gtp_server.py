#! /usr/bin/python

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU GO, a Go program.        #
#                                                               #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/ #
# for more information.                                         #
#                                                               #
# Copyright 1999, 2000, 2001 by the Free Software Foundation.   #
#                                                               #
# This program is free software; you can redistribute it and/or #
# modify it under the terms of the GNU General Public License   #
# as published by the Free Software Foundation - version 2.     #
#                                                               #
# This program is distributed in the hope that it will be       #
# useful, but WITHOUT ANY WARRANTY; without even the implied    #
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       #
# PURPOSE.  See the GNU General Public License in file COPYING  #
# for more details.                                             #
#                                                               #
# You should have received a copy of the GNU General Public     #
# License along with this program; if not, write to the Free    #
# Software Foundation, Inc., 59 Temple Place - Suite 330,       #
# Boston, MA 02111, USA.                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

from getopt import *
import popen2
import sys
import string

debug = 1

def coords_to_sgf(size, board_coords):
    global debug
    
    board_coords = string.lower(board_coords)
    if board_coords == "pass":
        return ""
    if debug:
        print "Coords: <" + board_coords + ">"
    letter = board_coords[0]
    digits = board_coords[1:]
    if letter > "i":
        sgffirst = chr(ord(letter) - 1)
    else:
        sgffirst = letter
    sgfsecond = chr(ord("a") + int(size) - int(digits))
    return sgffirst + sgfsecond



class GTP_connection:

    #
    # Class members:
    #   outfile		File to write to
    #   infile		File to read from

    def __init__(self, command):
        try:
            infile, outfile = popen2.popen2(command)
        except:
            print "popen2 failed"
            sys.exit(1)
        self.infile  = infile
        self.outfile = outfile
        
    def exec_cmd(self, cmd):
        global debug

        if debug:
            sys.stderr.write("GTP command: " + cmd + "\n")
        self.outfile.write(cmd + "\n\n")
        self.outfile.flush()
        result = ""
        line = self.infile.readline()
        while line != "\n":
            result = result + line
            line = self.infile.readline()
        if debug:
            sys.stderr.write("Reply: " + result + "\n")

        # Remove trailing newline from the result
        if result[-1] == "\n":
            result = result[:-1]

        if len(result) == 0:
            return "ERROR: len = 0"
        if (result[0] == "?"):
            return "ERROR: GTP Command failed: " + result[2:]
        if (result[0] == "="):
            return result[2:]
        return "ERROR: Unrecognized answer: " + result
        

class GTP_player:

    # Class members:
    #    connection		GTP_connection
    #	 protocolversion	int

    
    def __init__(self, command):
        self.connection = GTP_connection(command)
        self.protocolversion = 1

    def genmove(self, color):
        if color[0] in ["b", "B"]:
            if self.protocolversion == 1:
                command = "genmove_black"
            elif self.protocolversion == 2:
                command = "genmove black"
        elif color[0] in ["w", "W"]:
            if self.protocolversion == 1:
                command = "genmove_white"
            elif self.protocolversion == 2:
                command = "genmove white"
        return self.connection.exec_cmd(command)

    def black(self, move):
        if self.protocolversion == 1:
            command = "black " + move
        elif self.protocolversion == 2:
            command = "play black " + move
        self.connection.exec_cmd(command)

    def white(self, move):
        if self.protocolversion == 1:
            command = "white " + move
        elif self.protocolversion == 2:
            command = "play white " + move
        self.connection.exec_cmd(command)

    def komi(self, komi):
        self.connection.exec_cmd("komi " + komi)

    def boardsize(self, size):
        self.connection.exec_cmd("boardsize " + size)

    def clear_board(self):
        self.connection.exec_cmd("clear_board")

    def handicap(self, handi):
        result = self.connection.exec_cmd("fixed_handicap " + handi)
        return string.split(result, " ")

    def quit(self):
        return self.connection.exec_cmd("quit")
    
    def showboard(self):
        return self.connection.exec_cmd("showboard")

    def protocol_version(self):
        result = self.connection.exec_cmd("protocol_version")
        if ( result != "1" ) and ( result != "2" ) :
            return "unknown"	# unknown protocol version
        self.protocolversion = int(result)
        return result

    def get_random_seed(self):
        result = self.connection.exec_cmd("get_random_seed")
        if result[:5] == "ERROR":
            return "unknown"
        return result

    def get_program_name(self):
        return self.connection.exec_cmd("name") + " " + \
               self.connection.exec_cmd("version")

    def final_score(self):
        return self.connection.exec_cmd("final_score")


class GTP_game:

    # Class members:
    #    whiteplayer	 GTP_player
    #    blackplayer	 GTP_player
    #    size		 int
    #    komi		 float
    #    handicap        int
    #    handicap_stones int
    #    moves		 list of string
    #    resultw
    #    resultb

    def __init__(self, whitecommand, blackcommand, size, komi, handicap):
        self.whiteplayer = GTP_player(whitecommand)
        self.blackplayer = GTP_player(blackcommand)
        self.size = size
        self.komi = komi
        self.handicap = handicap

    def writesgf(self, sgffilename):
        "Write the game to an SGF file after a game"

        size = self.size
        outfile = open(sgffilename, "w")
        if not outfile:
            print "Couldn't create " + sgffilename
            return
        black_name = self.blackplayer.get_program_name()
        white_name = self.whiteplayer.get_program_name()
        black_seed = self.blackplayer.get_random_seed()
        white_seed = self.whiteplayer.get_random_seed()
        handicap = self.handicap
        komi     = self.komi
        result   = self.resultw

        outfile.write("(;GM[1]FF[4]RU[Japanese]SZ[%s]HA[%s]KM[%s]RE[%s]\n" %
                      (size, handicap, komi, result))
        outfile.write("PW[%s (random seed %s)]PB[%s (random seed %s)]\n" %
                      (white_name, white_seed, black_name, black_seed))

        if int(handicap) > 1:
            for stone in self.handicap_stones:
        	sgfstone = coords_to_sgf(size, stone)
                outfile.write("AB[%s]" % (sgfstone))
            outfile.write("\n")

        if int(handicap) < 2:
            to_play = "B"
        else:
            to_play = "W"

        for move in self.moves:
            sgfmove = coords_to_sgf(size, move)
            outfile.write(";%s[%s]\n" % (to_play, sgfmove))
            if to_play == "B":
                to_play = "W"
            else:
                to_play = "B"
        outfile.write(")\n")
        outfile.close

    def play(self, sgffile):
        "Play a game"
        global verbose

        if verbose >= 1:
            print "Setting boardsize and komi for black\n"
        self.blackplayer.boardsize(self.size)
        black_protocol_version = self.blackplayer.protocol_version()
        if black_protocol_version == "2":
            self.blackplayer.clear_board()	# must clear board in version 2
        self.blackplayer.komi(self.komi)

        if verbose >= 1:
            print "Setting boardsize and komi for white\n"
        self.whiteplayer.boardsize(self.size)
        white_protocol_version = self.whiteplayer.protocol_version()
        if white_protocol_version == "2":
            self.whiteplayer.clear_board()	# must clear board in version 2
        self.whiteplayer.komi(self.komi)

        self.handicap_stones = []
        if int(self.handicap) < 2:
            to_play = "B"
        else:
            self.handicap_stones = self.whiteplayer.handicap(self.handicap)
            for stone in self.handicap_stones:
                self.blackplayer.black(stone)
            to_play = "W"

        self.moves = []
        passes = 0
        while passes < 2:
            if to_play == "B":
                move = self.blackplayer.genmove("black")
                if move[:5] == "ERROR":
                    # FIXME: write_sgf
                    sys.exit(1)

                self.moves.append(move)
                if string.lower(move[:4]) == "pass":
                    passes = passes + 1
                else:
                    passes = 0
                    self.whiteplayer.black(move)
                    if verbose >= 1:
                        print "Black plays " + move

                to_play = "W"
            else:
                move = self.whiteplayer.genmove("white")
                if move[:5] == "ERROR":
                    # FIXME: write_sgf
                    sys.exit(1)

                self.moves.append(move)
                if string.lower(move[:4]) == "pass":
                    passes = passes + 1
                else:
                    passes = 0
                    self.blackplayer.white(move)
                    if verbose >= 1:
                        print "White plays " + move
                to_play = "B"
            if verbose >= 2:
                print self.whiteplayer.showboard()

        self.resultw = self.whiteplayer.final_score()
        self.resultb = self.blackplayer.final_score()
        if self.resultb == self.resultw:
            print "Result: ", self.resultw
        else:
            print "Result according to W: ", self.resultw
            print "Result according to B: ", self.resultb
        if sgffile != "":
            self.writesgf(sgffile)

    def result(self):
        return (self.resultw, self.resultb)
    
    def quit(self):
        self.blackplayer.quit()
        self.whiteplayer.quit()


class GTP_match:

    # Class members:
    #    black
    #    white
    #    size
    #    komi
    #    handicap

    def __init__(self, whitecommand, blackcommand, size, komi, handicap):
        self.white = whitecommand
        self.black = blackcommand
        self.size = size
        self.komi = komi
        self.handicap = handicap

    def play(self, games, sgfbase):
        game = GTP_game(self.white, self.black,
                        self.size, self.komi, self.handicap)
        results = []
        for i in range(games):
            sgffilename = "%s%03d.sgf" % (sgfbase, i + 1)
            print "Game ", i + 1
            game.play(sgffilename)
            results.append(game.result())
        game.quit()
        return results


# ================================================================
#                      Main program
#


# Default values
#

if __name__ == "__main__":
    white    = ""
    black    = ""
    komi     = "5.5"
    size     = "19"
    handicap = "0"

    games   = 1
    sgffile = "twogtp"

    verbose = 0

    helpstring = """

Run with:

twogtp --white \'<path to program 1> --mode gtp [program options]\' \\
       --black \'<path to program 2> --mode gtp [program options]\' \\
       [twogtp options]

Possible twogtp options:

  --verbose 1 (to list moves) or --verbose 2 (to draw board)
  --komi <amount>
  --handicap <amount>
  --size <board size>               (default 19)
  --games <number of games to play> (default 1)
  --sgffile <filename>              (create sgf files with sgfbase as basename)
"""

    def usage():
        print helpstring
        sys.exit(1)

    (opts, params) = getopt(sys.argv[1:], "",
                        ["black=",
                         "white=",
                         "verbose=",
                         "komi=",
                         "boardsize=", "size=",
                         "handicap=",
                         "games=",
                         "sgffile=",
                         ])

    for opt, value in opts:
        if opt == "--black":
            black = value
        elif opt == "--white":
            white = value
        elif opt == "--verbose":
            verbose = int(value)
        elif opt == "--komi":
            komi = value
        elif opt == "--boardsize" or opt == "--size":
            size = value
        elif opt == "--handicap":
            handicap = value
        elif opt == "--games":
            games = int(value)
        elif opt == "--sgffile":
            sgffile = value
        
    if params != []:
        usage()
            

    if black == "" or white == "":
        usage()

    if (int(handicap) > 1):
        komi = "0.5"

    match = GTP_match(white, black, size, komi, handicap)
    results = match.play(games, sgffile)

    i = 0
    for resw, resb in results:
        i = i + 1
        if resw == resb:
            print "Game %d: %s" % (i, resw)
        else:
            print "Game %d: %s %s" % (i, resb, resw)
