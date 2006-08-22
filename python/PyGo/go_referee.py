#! /usr/bin/python
############################################################################
#                go_referee.py - go referee class
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
from gtp_funcs import *

class GoError(Exception): pass
class PlayError(GoError): pass

class GoReferee:
    def __init__(self, black, white, board):
        self.players = (black, white)
        self.curPlayer = self.players[0]
        self.curWatcher = self.players[1]
        self.isPass = 0
        self.board = board
        self.size = None
        self.handicap = None
        
    def init_game(self, size=19, komi=5.5, handicap=0):
        self.size = size
        self.handicap = handicap
        self.komi = komi
        self.game = gogame.GoGame(size, handicap)

        for player in self.players:
            player.set_komi(komi)
            player.set_boardsize(size)
            player.set_handicap(handicap)
            player.clear_board()
        
        self.call_for_move()

    def call_for_move(self):
        self.curPlayer.genmove(self.play_move)
    
    def play_move(self, move):
        """receive a move in GTP 2 format. """
        color, move_tuple = convert_from_GTP(move, self.size)
        result = self.game.move_stone(move_tuple[0], move_tuple[1], color)
        if result.status != gogame.VALID:
            raise PlayError, result.status
        self.board.PlaceMove(move_tuple, color)
        self.board.ErasePieces(result.prisoners)
        self.board.SwitchCurColor()

        self.curWatcher.play(move)

        #if there are consecutive passes, end the game, else reset pass count
        if move == "pass":
            self.isPass += 1
            if self.isPass == 2:
                self.end_game()
        else:
            self.isPass = 0

        #switch the players
        temp = self.curPlayer
        self.curPlayer = self.curWatcher
        self.curWatcher = temp
        
        if self.isPass < 2:
            self.call_for_move()
        else:
            self.end_game()
    
    def end_game(self):
        for player in self.players:
            player.final_score()
        
