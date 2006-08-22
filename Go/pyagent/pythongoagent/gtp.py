#! /usr/bin/python
############################################################################
#                gtp.py  -  Parses Go Text Protocol commands               #
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

import sys
from go_game import go_game

DEBUG=1         #debugging constant

class GTP:
    """
    A Python class designed to parse Go Text Protocol (version 2) input and 
    pass it to a go board, specifically to my go board manager. (see go_game.py).
    The GTP is documented at
    http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html
    """

    def __init__(self,infile,outfile,debugfile):
        """
        infile is the file from which the GTP should read input (usually 
        sys.stdin).
        outfile is the file to which GTP shold write output (usually
        sys.stdout).
        debugfile is the file to which GTP should write out Debugging info,
        unless DEBUG = 0.
        
        This class relies on the symbol DEBUG, set to a true value for debugging
        information, and a false one to omit it.
        """
        self.infile = infile
        self.outfile = outfile
        self.debugfile = debugfile
        self.version = 2
        self.game = go_game(debugfile)
        self.functions = \
        [("boardsize", self.set_boardsize), \
        ("protocol_version", self.protocol_version), \
        ("clear_board", self.clear_board), \
        ("komi", self.komi), \
        ("fixed_handicap", self.fixed_handicap), \
        ("play", self.play), \
        ("genmove", self.genmove), \
        ("final_score", self.final_score), \
        ("name", self.name), \
        ("version", self.get_version), \
        ("known_command", self.known_command), \
        ("list_commands", self.list_commands), \
        ("fixed_handicap", self.fixed_handicap), \
        ("quit", None)]
    
    def parse_input(self):
        """
        Reads a line from stdin and parses it according to the GTP standard.
        
        When it finds a keyword, it calls the function GTP.keyword, which calls
        on the underlying game class to perform the action. Although not complete,
        it it is functional enough to play a game against a computer (that is,
        without undos)
        """
        
        #read a line of input
        result = ""
        line = self.infile.readline()
        while line != "\n":
            result = result + line
            line = self.infile.readline()
        if DEBUG:
            self.debugfile.write("pyagent received " + result)
        
        result.lower()
        
        #remove comments
        result = result.split('#')[0]
        result = result.strip()
        
        #first, check for a quit
        pos = result.find("quit")
        if pos > -1:
            self.respond("=\n\n")
            if DEBUG:
                self.debugfile.write("Qutting \n")
            return 0
        
        #check for an id number
        
        #iterate the function list looking for a match
        for i in range(len(self.functions)):
            pos = result.find(self.functions[i][0])
            if pos > -1:
                strlen = pos + len(self.functions[i][0])
                arg = result[strlen:].strip()
                if arg:
                    self.functions[i][1](arg)
                else:
                    self.functions[i][1]()
                return 1
        
        #found no match, return error
        return 2

    def respond(self,msg):
        """respond to stdout with string msg"""
        self.outfile.write(str(msg))
        if DEBUG:
            self.debugfile.write("pyagent responded <" + msg + ">")
        self.outfile.flush()

    def set_boardsize(self,size):
        """
        calls game.set_boardsize with parameter size. responds = (which
        means OK in the GTP) if set_boardsize returned a true value.
        """
        response = self.game.set_boardsize(size)
        if response:
            self.respond("=\n\n")
        else:
            self.debugfile.write("GTP.game.boardsize() failed")

    def protocol_version(self):
        self.respond("= " + str(self.version) + "\n\n")

    def clear_board(self):
        response = self.game.clear_board()
        if response:
            self.respond("=\n\n")
        else:
            self.debugfile.write("GTP.game.clear_board() failed\n")

    def komi(self, komi):
        """set komi. For explanation of komi, see game.set_komi"""
        response = self.game.set_komi(komi)
        if response:
            self.respond("=\n\n")
        else:
            self.debugfile.write("GTP.game.set_komi() failed\n")

    def fixed_handicap(self, handicap):
        response = self.game.set_handicap(handicap) 
        if not response:
            self.respond("? set_handicap() failed\n\n")
            self.debugfile.write("GTP.game.set_handicap() failed\n")
        else:
            self.respond("= " + response + "\n\n")

    def play(self, move):
        move = move.strip()
        response = self.game.play(move)
        if response:
            self.respond("= " + move + "\n\n")
        else:
            self.debugfile.write("GTP.game.play() failed\n")

    def genmove(self, who):
        who = who.strip()
        move = self.game.genmove(who)
        if move == 0:
            self.debugfile.write("GTP.game.genmove() failed\n")
        self.respond("= " + move + "\n\n")
    
    def final_score(self):
        score = self.game.final_score()
        if score:
            self.respond("= " + score + "\n\n")
        else:
            self.debugfile.write("game.final_score() failed")

    def name(self):
        name = self.game.get_name()
        if name:
            self.respond("= " + name + "\n\n")
        else:
            self.debugfile.write("game.name() failed")
    
    def get_version(self):
        version = self.game.get_version()
        if version:
            self.respond("= " + version + "\n\n")
        else:
            self.debugfile.write("game.version() failed")
    
    def known_command(self, command):
        """
        looks for command in the functions list. responds true if found,
        false otherwise.
        """
        for i in range(len(self.functions)):
            pos = self.functions[i][0].find(command)
            if pos > -1:
                self.respond("= true")
                return 1
        self.respond("= false")
        
    def list_commands(self):
        """responds with a list of the available commands"""
        self.respond("= ")
        for i in range(len(self.functions)):
            self.respond(self.functions[i][0] + "\n")
        self.respond("\n")

    def run_main_loop(self):
        """while there is input, read it and send it to the parsing function"""
        result = self.parse_input()
        while result:
            result = self.parse_input()
            if result == 2:
                self.respond("? unknown command\n\n")

if __name__ == '__main__':
    #read input from stdin
    input = sys.stdin
    #send output to stdout
    output = sys.stdout
    output.write("testing\n")
    #open a file "debug.dat" in the same directory as the program
    debug = file("debug.dat", "w")
    #create an instance of the GTP class
    comm = GTP(input, output, debug)
    #run the parsing loop
    comm.run_main_loop()
