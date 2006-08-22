#! /usr/bin/python
############################################################################
#                goBoard.py - creates a wxPython go board
#                             -------------------
#    copyright            : (C) 2003 by Bill Mill
#    email                : llimllib@f2o.org
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
############################################################################

import os, sys
from wxPython.wx import *
from go_players import *
from go_referee import GoReferee, GoError

## symbolic constants
ID_ABOUT=10001
ID_EXIT=10002
ID_OPEN_SGF=10003
ID_NEW_GAME=10004
ID_VIEW_DEBUG=10005
ID_HUMAN1=10006
ID_HUMAN2=10007
ID_COMP1=10008
ID_COMP2=10009
ID_PYGO1=10010
ID_PYGO2=10011

# exceptions
class GoBoardError(Exception): pass
class InvalidValueError(GoBoardError): pass

# utility function
def opj(path):
    """Convert paths to the platform-specific separator"""
    return apply(os.path.join, tuple(path.split('/')))

# main window
class GoFrame(wxFrame):
    """ Main window for a go game"""
    def __init__(self,parent,id,title):
        self.dirname="."

        wxFrame.__init__(self,parent,-4, title, \
            pos=wxPoint(0,50), \
            style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)

        self.log = MyLog(self, "PyGo Messages")
        self.log.Show(true)
        
        self.conf = Configuration(".pygo_conf", self.log)

        self.CreateStatusBar()
        
        self.MakeMenu()
        
        self.boardPanel = \
            BoardPanel(self, -1, self.conf, self.log)
            
        self.SetClientSize(self.boardPanel.GetClientSize())

        self.Show(true)
    
    def MakeMenu(self):
        gamemenu=wxMenu()
        gamemenu.Append(ID_NEW_GAME, "&New Game", "Start a new game")
        gamemenu.Append(ID_OPEN_SGF, "&Open SGF File", \
            "Opens an SGF file for viewing/editing")
        gamemenu.Append(ID_VIEW_DEBUG, "&View Debug Info", \
            "Open a window containing debugging information about the program")
        gamemenu.Append(ID_EXIT, "E&xit", "Terminate this program")
        
        helpmenu = wxMenu()
        helpmenu.Append(ID_ABOUT, "&About", "Information about this program")
        menuBar = wxMenuBar()
        menuBar.Append(gamemenu, "&File")
        menuBar.Append(helpmenu, "&Help")
        self.SetMenuBar(menuBar)

        EVT_MENU(self, ID_NEW_GAME, self.OnNewGame)
        EVT_MENU(self, ID_OPEN_SGF, self.OnOpen)
        EVT_MENU(self, ID_VIEW_DEBUG, self.OnViewDebug)
        EVT_MENU(self, ID_ABOUT, self.OnAbout)
        EVT_MENU(self, ID_EXIT, self.OnFileExit)
        EVT_CLOSE(self, self.OnExit)
    
    def OnViewDebug(self, e):
        if self.log.IsShown():
            self.log.Show(false)
        else:
            self.log.Show(true)
        return 1
    
    def MakeNewGameDialog(self):
        win = wxDialog(frame, -1, "New Game", size=wxSize(350, 200),
            style = wxCAPTION | wxSYSTEM_MENU | wxTHICK_FRAME
            )
        mainbox = wxBoxSizer(wxVERTICAL)
        playerbox = wxBoxSizer(wxHORIZONTAL)
        
        box_title = wxStaticBox(win, -1, "Black Player")
        box = wxStaticBoxSizer(box_title, wxVERTICAL)
        grid = wxFlexGridSizer(0, 1, 0, 0)

        human1 = wxRadioButton(win, ID_HUMAN1, "Human", style=wxRB_GROUP)
        pygo1 = wxRadioButton(win, ID_PYGO1, "PyGo Computer Opponent")
        comp1 = wxRadioButton(win, ID_COMP1, "Other Computer Opponent")
        comp1_cmd = wxTextCtrl(win, -1, "", size=(200,-1))

        grid.AddWindow(human1, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid.AddWindow(pygo1, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid.AddWindow(comp1, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid.AddWindow(comp1_cmd, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        box.AddSizer(grid, 0, wxALIGN_LEFT|wxALL, 5)
        
        box2_title = wxStaticBox(win, -1, "White Player")
        box2 = wxStaticBoxSizer(box2_title, wxVERTICAL)
        grid2 = wxFlexGridSizer(0, 1, 0, 0)
        
        human2 = wxRadioButton(win, ID_HUMAN2, "Human", style=wxRB_GROUP)
        pygo2 = wxRadioButton(win, ID_PYGO2, "PyGo Computer Opponent")
        comp2 = wxRadioButton(win, ID_COMP2, "Other Computer Opponent")
        comp2_cmd = wxTextCtrl(win, -1, "", size=(200,-1))
        
        grid2.AddWindow(human2, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid2.AddWindow(pygo2, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid2.AddWindow(comp2, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        grid2.AddWindow(comp2_cmd, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP, 5)
        box2.AddSizer(grid2, 0, wxALIGN_LEFT|wxALL, 5)
        
        playerbox.Add(box)
        playerbox.Add(box2)
        
        mainbox.Add(playerbox)
        
        grid3 = wxFlexGridSizer(1, 0, 0, 0)
        
        ko_label = wxStaticText(win, -1, "Ko: ")
        ko = wxTextCtrl(win, -1, "5.5", size=(30, -1))
        bs_label = wxStaticText(win, -1, "Board Size: ")
        bsizes = ['19']
        bs = wxChoice(win, 40, (80, 50), choices = bsizes)
        bs.SetStringSelection('19')
        handicap_label = wxStaticText(win, -1, "Handicap: ")
        handicap = wxTextCtrl(win, -1, "0", size=(30, -1))
        
        grid3.AddWindow(ko_label, 0, wxALL, 5)
        grid3.AddWindow(ko, 0, wxALIGN_LEFT|wxALL, 5)
        grid3.AddWindow(handicap_label, 0, wxALL, 5)
        grid3.AddWindow(handicap, 0, wxALIGN_LEFT|wxALL, 5)
        grid3.AddWindow(bs_label, 0, wxALIGN_LEFT|wxALL, 5)
        grid3.AddWindow(bs, 0, wxALIGN_LEFT|wxALL, 5)
        
        grid4 = wxGridSizer(1, 0, 0, 0)

        ok = wxButton(win, wxID_OK, " OK ")
        cancel = wxButton(win, wxID_CANCEL, " Cancel ")

        grid4.AddWindow(ok, 0, wxALIGN_RIGHT|wxALL, 5)
        grid4.AddWindow(cancel, 0, wxALIGN_RIGHT|wxALL, 5)
        
        mainbox.Add(grid3)
        mainbox.Add(grid4)
        
        win.SetSizer(mainbox)
        win.SetAutoLayout(true)
        mainbox.Fit(win)
        
        response = win.ShowModal()
        if response == wxID_OK:
            values = {
                'size': int(bs.GetStringSelection()),
                'pygo1': pygo1.GetValue(),
                'pygo2': pygo2.GetValue(),
                'human1': human1.GetValue(),
                'human2': human2.GetValue(),
                'comp1': comp1.GetValue(),
                'comp2': comp2.GetValue(),
                'ko': ko.GetValue()
            }
            win.Destroy()
            return values
        else:
            win.Destroy()
            return 0
    
    def ValidateNewGameValues(self, vals):
        bsize = vals['size']
        if bsize != 19:
            msg = 'Sorry, board sizes other than 19 are not yet allowed'
            self.boardPanel.MakeDialog('sorry', msg)
            raise InvalidValueError, "invalid board size"
        valid_lets = '01234567890.'
        for digit in vals['ko']:
            if digit not in valid_lets:
                self.boardPanel.MakeDialog('sorry', "Invalid ko value")
                raise InvalidValueError, "invalid ko value"
    
    def OnNewGame(self, e):
        # handicaps not yet implemented
        HANDICAP = 0

        vals = self.MakeNewGameDialog()
        if not vals:
            return 0
        try:
            self.ValidateNewGameValues(vals)
        except:
            self.log.write("error validating new game values")
            self.log.write(sys.exc_info()[0])
            return 0
        try:
            if vals['human1']:
                player1 = HumanGoPlayer(self.boardPanel)
            elif vals['comp1']:
                player1 = ComputerGoPlayer(self.log, "B", vals['comp1'])
            elif vals['pygo1']:
                player1 = PyGoPlayer("B")
            if vals['human2']:
                player2 = HumanGoPlayer(self.boardPanel)
            elif vals['comp2']:
                player2 = ComputerGoPlayer(self.log, "W", vals['comp2'])
            elif vals['pygo2']:
                player2 = PyGoPlayer("W")
            self.ref = GoReferee(player1, player2, self.boardPanel)
            self.ref.init_game(vals['size'], float(vals['ko']), HANDICAP)
        except GoError:
            self.log.write("error in new game creation\n")
            self.log.write(sys.exc_info()[0])
            self.log.write(sys.exc_info()[1])
        return 1
    
    def OnAbout(self, e):
        msg = "PyGo version .001\nBill Mill\nllimllib.f2o.org\nllimllib@f2o.org"
        d = wxMessageDialog(self, msg, "wxPyGo", wxOK)
        d.ShowModal()
        d.Destroy()
    
    def OnExit(self, e):
        sys.exit(1)
    
    def OnFileExit(self, e):
        self.Close()
    
    def OnOpen(self, e):
        pass
        """still not implemented
        dlg = wxFileDialog(self, "Choose a file", self.dirname, "", "*.sgf", wxOPEN)
        if dlg.ShowModal() == wxID_OK:
            self.filename=dlg.GetFilename()
            self.dirname=dlg.GetDirectory()
            f=open(os.path.join(self.dirname,self.filename),'r')
            self.control.SetValue(f.read())
            f.close()
        dlg.Destroy()"""

class BoardPanel(wxScrolledWindow):
    def __init__(self, parent, id, conf, log, boardsize=19):
        wxScrolledWindow.__init__(self, parent, id)

        self.conf = conf
        
        try:
            black_image = self.conf.get_value('black_image')
            white_image = self.conf.get_value('white_image')
            black_move_image = self.conf.get_value('black_move_image')
            white_move_image = self.conf.get_value('white_move_image')
            board_image_name = "board" + str(boardsize)
            board_image = self.conf.get_value(board_image_name)
        except:
            raise "Proper values not found in configuration file"
        
        self.boardImg = wxBitmapFromImage(wxImage(opj(board_image), wxBITMAP_TYPE_PNG))
        self.black = wxBitmapFromImage(wxImage(opj(black_image), wxBITMAP_TYPE_PNG))
        self.white = wxBitmapFromImage(wxImage(opj(white_image), wxBITMAP_TYPE_PNG))
        self.black_move = wxBitmapFromImage(wxImage(opj(black_move_image), wxBITMAP_TYPE_PNG))
        self.white_move = wxBitmapFromImage(wxImage(opj(white_move_image), wxBITMAP_TYPE_PNG))
        
        self.log = log                               # pointer to log object
        self.boardSize = boardsize                   # Number of rows & cols
        self.halfPiece = self.black.GetWidth() / 2   # Width of half a piece
        self.curPiece = self.black  # Current piece image; altern white/black
        self.curColor = "B"         # Current color; "B" or "W"
        self.movePiece = None       # Piece that's being moved around
        self.curCross = (0,0)       # cross most recently covered by the mouse
        self.firstCross = (20,20)   # pixel location of first cross on board
        self.crossDistance = 26     # pixel distance between crosses
        self.pieces = []            # array of pieces placed, 1st-last
        self.onWindow = false       # true if mouse is over window
        self.human = false          # true if pieces are controlled by human
        self.makeMove = None        # func to call when board is clicked
        self.SetBackgroundColour("WHITE")

        #initialize first piece
        self.movePiece = DragShape(self.black_move)
        self.movePiece.pos = wxPoint(self.firstCross[0] - self.halfPiece, self.firstCross[1] - self.halfPiece)
        self.movePiece.shown = false
        
        EVT_ERASE_BACKGROUND(self, self.OnEraseBackground)
        EVT_LEFT_DOWN(self, self.BoardClick)
        EVT_MOTION(self, self.MovePiece)
        EVT_LEAVE_WINDOW(self, self.OnLeaveWindow)
        EVT_ENTER_WINDOW(self, self.OnEnterWindow)
        EVT_PAINT(self, self.OnPaint)
    
    def GetMove(self, make_move):
        self.human = true
        self.makeMove = make_move
    
    def StopGetMove(self):
        self.human = false
        self.makeMove = None

    def OnLeaveWindow(self, e):
        self.onWindow = false
        self.movePiece.shown = false
        dc = wxClientDC(self)
        self.EraseShape(self.movePiece, dc)

    def OnEnterWindow(self, e):
        if self.human == false:
            return 1
        self.onWindow = true
        self.movePiece.shown = true
        dc = wxClientDC(self)
        self.DrawShapes(dc)

    def OnNewGame(self):
        self.human = false
        self.curCross = (0,0)
        self.curPiece = self.black
        self.curColor = "B"
        dc = wxClientDC(self)
        #TODO: make this faster
        piece_copy = self.pieces[:]           #copy by value
        for piece in piece_copy:
            self.pieces.remove(piece)
            self.EraseShape(piece, dc)
        self.movePiece.show = true
        self.movePiece = DragShape(self.black_move)
        self.movePiece.pos = wxPoint(self.firstCross[0] - self.halfPiece, self.firstCross[1] - self.halfPiece)
        self.movePiece.shown = false
        self.DrawBackground(dc)
        self.DrawShapes(dc)

    def OnEraseBackground(self, e):
        dc = e.GetDC()
        if not dc:
            dc = wxClientDC(self)
            rect = self.GetUpdateRegion().GetBox()
            dc.SetClippingRegion(rect.x, rect.y, rect.width, rect.height)
        self.DrawBackground(dc)

    def DrawBackground(self, dc):
        dc.DrawBitmap(self.boardImg, 0,0)

    def DrawShapes(self, dc):
        if self.movePiece.shown:
            self.movePiece.Draw(dc)
        for piece in self.pieces:
            if piece.shown:
                piece.Draw(dc)

    def EraseShape(self, shape, dc):
        r = shape.GetRect()
        dc.SetClippingRegion(r.x, r.y, r.width, r.height)
        self.DrawBackground(dc)
        self.DrawShapes(dc)
        dc.DestroyClippingRegion()

    def ErasePieces(self, pieces):
        """Erase all pieces from an array of crosses"""
        dc = wxClientDC(self)
        for piece in pieces:
            delPiece = self.FindPiece(piece)
            try:
                self.pieces.remove(delPiece)
            except ValueError:
                return 0
            self.EraseShape(delPiece, dc)

    def OnPaint(self, e):
        dc = wxClientDC(self)
        self.PrepareDC(dc)
        self.DrawShapes(dc)

    def PlaceMove(self, cross, color):
        """Place a piece on the board at cross
        
        cross should be an (x,y) tuple of ints where 0 < x,y < boardSize-1
        color should be "B" or "W"
        """
        if self.FindPiece(cross) or not self.OnBoard(cross):
            return false
        
        if color == "B":
            piece = self.black
        else:
            piece = self.white
        
        newPiece = DragShape(piece)
        pos = self.FindCoords(cross)
        newPiece.pos = wxPoint(pos[0], pos[1])
        newPiece.cross = cross
        self.pieces.append(newPiece)
        self.OnPaint(None)        
    
    def BoardClick(self, e):
        """When the board is clicked, try to place a piece"""
        #if the square is taken, the board doesn't have control, or the piece isn't on the board
        #return false
        if not self.human or not self.onWindow\
        or not self.OnBoard(self.curCross):
            return false
        
        #report the move to the controller class
        if not self.makeMove(self.curCross, self.curColor):
            return 0

        self.curCross = (-1, -1)
        
    def SwitchCurColor(self):
        if self.curPiece == self.black:
            self.curColor = "W"
            self.curPiece = self.white
        else:
            self.curColor = "B"
            self.curPiece = self.black
    
    def MovePiece(self, e):
        mpos = e.GetPosition()     #mouse position
        mouseCross = self.FindCross((mpos[0], mpos[1]))
        if self.FindPiece(mouseCross) or not self.human:
            return false

        if mouseCross != self.curCross and self.OnBoard(mouseCross):
            self.curCross = mouseCross
            dc = wxClientDC(self)
            self.movePiece.shown = false
            self.EraseShape(self.movePiece, dc)
            if self.curColor == "B":
                self.movePiece = DragShape(self.black_move)
            else:
                self.movePiece = DragShape(self.white_move)
            coords = self.FindCoords(mouseCross)
            self.movePiece.pos = wxPoint(coords[0], coords[1])
            self.OnPaint(None)

    def GetClientSize(self):
        return (self.boardImg.GetWidth(), self.boardImg.GetHeight())

    def FindCross(self, coords):
        cross = ((coords[0] - self.firstCross[0] + self.halfPiece) / self.crossDistance,
            (coords[1] - self.firstCross[0] + self.halfPiece) / self.crossDistance)
        return cross
    
    def FindCoords(self, cross):
        coords = (((cross[0] * self.crossDistance) + self.firstCross[0]) - self.halfPiece,\
                  ((cross[1] * self.crossDistance) + self.firstCross[0]) - self.halfPiece)
        return coords

    def OnBoard(self, cross):
        if cross[0] >= 0 and cross[1] >= 0 and cross[0] <= self.boardSize-1\
            and cross[1] <= self.boardSize-1:
            return true
        return false

    def FindPiece(self, cross):
        """If there is a piece at cross in self.pieces, return it"""
        for piece in self.pieces:
            if cross == piece.cross:
                return piece
        return false
    
    def MakeDialog(self, title, msg):
        dlg = wxMessageDialog(self, msg, title, 
            wxOK|wxICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()

class DragShape:
    def __init__(self, bmp):
        self.bmp = bmp
        self.pos = wxPoint(0,0)
        self.shown = true
        self.text = None
        self.fullscreen = false
        self.cross = None

    def HitTest(self, pt):
        rect = self.GetRect()        
        return rect.Inside(pt.x, pt.y)

    def GetRect(self):
        return wxRect(self.pos.x, self.pos.y,
                      self.bmp.GetWidth(), self.bmp.GetHeight())

    def Draw(self, dc, op = wxCOPY):
        if self.bmp.Ok():
            memDC = wxMemoryDC()
            memDC.SelectObject(self.bmp)

            dc.Blit(self.pos.x, self.pos.y,
                    self.bmp.GetWidth(), self.bmp.GetHeight(),
                    memDC, 0, 0, op, true)

            return true
        else:
            return false

class MyLog(wxFrame):
    def __init__(self, parent, title):
        wxFrame.__init__(self,parent,-4, title, \
            pos=wxPoint(550,50), \
            style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
        self.log = wxTextCtrl(self, -1, "", size=(400, 600), \
            style=wxTE_MULTILINE|wxTE_READONLY|wxHSCROLL|wxTE_RICH2)
        font = wxFont(12, wxMODERN, wxNORMAL, wxNORMAL)
        self.log.SetDefaultStyle(wxTextAttr("BLACK", "WHITE", font))
        
        EVT_CLOSE(self, self.OnClose)

    def OnClose(self, evt):
        self.Show(false)
    
    def write(self, message):
        if self.log:
            self.log.AppendText(str(message))
    
    def writeln(self, message):
        if self.log:
            self.log.AppendText(str(message) + "\n")

class Configuration:
    def __init__(self, file, log):
        self.log = log
        execfile(file)
        #self.conf = configuration
        
    def get_value(self, key):
        if self.conf.has_key(key):
            return self.conf[key]
        else:
            raise "key not found error"
            
if __name__ == "__main__":
    app = wxPySimpleApp()
    frame = GoFrame(None, -1, "wxPyGo")
    app.MainLoop()
