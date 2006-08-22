#! /usr/bin/python
############################################################################
#                go_players.py - go player classes
#                             -------------------
#    date                 : April 18 2003
#    copyright            : (C) 2003 by Bill Mill
#    email                : llimllib@f2o.org
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
############################################################################

import gogame
import go_random
from gtp_funcs import *
from go_referee import PlayError

class GoPlayer:
    def __init__(self): pass
    def genmove(self, func): raise NotImplementedError,"GoPlayer.move()"
    def play(self, move): raise NotImplementedError,"GoPlayer.play()"
    def set_komi(self, komi): raise NotImplementedError, "GoPlayer.komi()"
    def set_boardsize(self, size): raise NotImplementedError, "GoPlayer.boardsize()"
    def clear_board(self): raise NotImplementedError, "GoPlayer.clear_board()"
    def set_handicap(self, handicap): raise NotImplementedError, "GoPlayer.handicap()"
    def quit(self): raise NotImplementedError, "GoPlayer.quit()"
    def show_board(self): raise NotImplementedError, "GoPlayer.show_board()"
    def get_name(self): raise NotImplementedError, "GoPlayer.get_name()"
    def get_version(self): raise NotImplementedError, "GoPlayer.get_version()"
    def final_score(self): raise NotImplementedError, "GoPlayer.final_score()"

class PyGoPlayer(GoPlayer):
    def __init__(self, color):
        GoPlayer.__init__(self)
        self.color = color
        self.size = None
        self.komi = None
        self.handicap = None
        
    def genmove(self, submit):
        move = self.engine.genmove()
        try:
            submit(convert_to_GTP(move, self.color, self.size))
        except PlayError, status:
            status = str(status)
            if status == '1':
                msg = "That position is occupied. Choose another place to play."
            elif status == '2':
                msg = "Suicides are not allowed."
            elif status == '3':
                msg = "This move violates the ko rule. Choose another place to play."
            else:
                msg = "Unkown play error."
            print msg
            self.genmove(submit)
        return 1
        
    def play(self, move):
        color, move = convert_from_GTP(move, self.size)
        self.engine.play(color, move[0], move[1])

    def set_komi(self, komi):
        self.komi = komi

    def set_boardsize(self, size):
        self.size = size

    def clear_board(self):
        self.engine = go_random.rand_engine(self.size, self.color)
        self.engine.clear_board()

    def set_handicap(self, handicap):
        self.handicap = handicap

    def quit(self):
        pass
        
    def show_board(self):
        pass
        
    def get_name(self):
        return self.engine.get_name()

    def get_version(self):
        return self.engine.get_version()

    def final_score(self):
        pass

class HumanGoPlayer(GoPlayer):
    def __init__(self, board):
        GoPlayer.__init__(self)
        self.board = board
        self.submit_move = None
        self.size = board.boardSize
    
    def set_komi(self, komi):
        pass
    
    def set_boardsize(self, size):
        pass
        
    def set_handicap(self, handicap):
        pass

    def clear_board(self):
        self.board.OnNewGame()

    def receive_move(self, move, color):
        """
        submits a move to the referee.
        """
        self.board.StopGetMove()
        try:
            self.submit_move(convert_to_GTP(move, color, self.size))
        except PlayError, status:
            status = str(status)
            if status == '1':
                msg = "That position is occupied. Choose another place to play."
            elif status == '2':
                msg = "Suicides are not allowed."
            elif status == '3':
                msg = "This move violates the ko rule. Choose another place to play."
            else:
                msg = "Unkown play error."
            self.board.MakeDialog('Move Error', msg)
            self.board.GetMove(self.receive_move)
            return 0
        return 1
    
    def genmove(self, func):
        self.submit_move = func
        self.board.GetMove(self.receive_move)
        
    def play(self, move):
        pass

    def quit(self):
        self.board.MakeDialog("Notice", "Your opponent has quit this game.")
    
    def show_board(self):
        pass
    
    def get_name(self):
        return "human player"
    
    def get_version(self):
        return "1.0"
    
    def final_score(self):
        pass
