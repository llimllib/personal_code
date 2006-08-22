### GOGAME.PY

#    Copyright (C) 2002 Bill Mill
#    includes work by James Tauber
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

# Class representing the current state of a Go game with
# knowledge of capture rules and ko as well as utility
# functions for performing various calculations

### MoveResult

VALID = 0
OCCUPIED = 1
SUICIDE = 2
KO = 3
COMPLETED = 4

class MoveResult:
    def __init__(self, status, prisoners=[]):
        self.status = status
        self.prisoners = prisoners

### GoGame

EMPTY = 0
BLACK = 1
WHITE = 2

class GoGame:
    def __init__(self, size=19, handicap=0, komi=0):
        self.handicap = handicap
        self.size = size
        self.komi = komi

        self.contents = []
        for i in range(size):
            self.contents.append([EMPTY] * (size + 1))

        if handicap:
            self.to_play = WHITE
        else:
            self.to_play = BLACK

        self.white_prisoners = 0
        self.black_prisoners = 0
        self.move_number = 1
        self.last_move_pass = 0
        self.game_completed = 0
        self.last_ko_move = None
        self.last_ko_prisoner = None

    def set_komi(self, komi):
        self.komi = komi
        
    def pass_move(self):
        # uses: to_play, last_move_pass
        # changes: to_play, move_number, game_completed
        self.move_number = self.move_number + 1
        if self.to_play == BLACK:
            self.to_play = WHITE
        else:
            self.to_play = BLACK
        if self.last_move_pass:
            self.game_completed = 1
        self.last_move_pass = 1

    def is_occupied(self, x, y):
        # uses: contents
        return (self.contents[x][y] != EMPTY)

    def erase_stone(self, x, y):
        # changes: contents
        self.contents[x][y] = EMPTY
        
    def place_stone(self, x, y, color):
        """
        Place a stone without checking anything.
        
        Will not return prisoners, check if the spot is occupied, etc
        """
        # changes: contents
        self.contents[x][y] = color
        
    def move_stone(self, x, y, color=None, allow_suicide=None):
        """
        Place a stone on the board, checking that it's legal
        """
        # uses: to_play, contents, game_completed, last_ko_move, last_ko_prisoner
        # changes: last_move_pass, contents, last_ko_move, last_ko_prisoner
        #          white_prisoner, black_prisoner, black_go, move_number
        if color == None:
            color = self.to_play
        if str(color).lower() == 'w' or str(color).lower() == 'white':
            color = WHITE
        elif str(color).lower() == 'b' or str(color).lower() == 'black':
            color = BLACK

        if self.contents[x][y] != EMPTY:
            return MoveResult(OCCUPIED)

        if self.game_completed:
            return MoveResult(COMPLETED)

        self.contents[x][y] = color

        if color == BLACK:
            opposite = WHITE
        else:
            opposite = BLACK

        prisoners = []
        for (xx,yy) in self.calculate_grouped_adjacent(x,y)[opposite]:
            new_prisoners = self.calculate_prisoners(xx,yy)
            for new_prisoner in new_prisoners:
                if not new_prisoner in prisoners:
                    prisoners.append(new_prisoner)

        suicide = 0
        if len(prisoners) == 0 and len(self.calculate_prisoners(x,y)) > 0:
            if allow_suicide:
                new_prisoners = self.calculate_prisoners(x, y)
                for new_prisoner in new_prisoners:
                    if not new_prisoner in prisoners:
                        prisoners.append(new_prisoner)
                if len(prisoners) > 0:
                    suicide = 1
            else:
                self.contents[x][y] = EMPTY # reverse the move
                return MoveResult(SUICIDE)

        if len(prisoners) == 1:
            if self.last_ko_move == prisoners[0] and self.last_ko_prisoner == (x, y):
                self.contents[x][y] = EMPTY # reverse the move
                return MoveResult(KO)
            else:
                self.last_ko_move = (x, y)
                self.last_ko_prisoner = prisoners[0]
        else:
            self.last_ko_move = None
            self.last_ko_prisoner = None

        for (xx, yy) in prisoners:
            if not suicide:
                if color == BLACK:
                    self.white_prisoners = self.white_prisoners + 1
                else:
                    self.black_prisoners = self.black_prisoners + 1
            else:
                if color == BLACK:
                    self.black_prisoners = self.black_prisoners + 1
                else:
                    self.white_prisoners = self.white_prisoners + 1
                    
            self.contents[xx][yy] = EMPTY # remove the stones

        if color == BLACK:
            self.to_play = WHITE
        else:
            self.to_play = BLACK

        self.move_number = self.move_number + 1
        self.last_move_pass = 0

        return MoveResult(VALID, prisoners)

    def calculate_grouped_adjacent(self, x, y, dead_points=[]):
        # uses: contents, size
        adjacent = [[],[],[]] # @@@ assumes that EMPTY, BLACK and WHITE are 0-2
        if x != 0:
            if (x - 1, y) in dead_points:
                color = EMPTY
            else:
                color = self.contents[x - 1][y]
            adjacent[color].append((x-1,y))
        if y != 0:
            if (x, y - 1) in dead_points:
                color = EMPTY
            else:
                color = self.contents[x][y - 1]
            adjacent[color].append((x, y - 1))
        if x != self.size - 1:
            if (x + 1, y) in dead_points:
                color = EMPTY
            else:
                color = self.contents[x + 1][y]
            adjacent[color].append((x+1,y))
        if y != self.size - 1:
            if (x, y + 1) in dead_points:
                color = EMPTY
            else:
                color = self.contents[x][y + 1]
            adjacent[color].append((x,y+1))
        return adjacent
    
    def calculate_prisoners(self, x, y):
        # uses: contents
        points_looked_at = []
        points_to_look_at = []
        points_to_look_at.append((x, y))
        while 1:
            if len(points_to_look_at) == 0:
                break
            else:
                (xx, yy) = points_to_look_at.pop()
                points_looked_at.append((xx, yy))
                adjacent = self.calculate_grouped_adjacent(xx, yy)
                if len(adjacent[0]) > 0:
                    return []
                else:
                    for point in adjacent[self.contents[xx][yy]]:
                        if not point in points_looked_at and not point in points_to_look_at:
                            points_to_look_at.append(point)
        return points_looked_at

    def calculate_group(self, x, y, dead_points=[]):
        # uses: contents
        points_looked_at = []
        points_to_look_at = []
        points_to_look_at.append((x, y))
        while 1:
            if len(points_to_look_at) == 0:
                break
            else:
                (xx, yy) = points_to_look_at.pop()
                points_looked_at.append((xx, yy))
                adjacent = self.calculate_grouped_adjacent(xx, yy, dead_points)
                if (xx, yy) in dead_points:
                    color = EMPTY
                else:
                    color = self.contents[xx][yy]
                for point in adjacent[color]:
                    if not point in points_looked_at and not point in points_to_look_at:
                        points_to_look_at.append(point)
        return points_looked_at

    def calculate_territory(self, dead_points=[]):
        # uses: size, contents
        unknown_territory = []
        black_territory = []
        white_territory = []
        points_looked_at = []
        for x in range(1, self.size + 1):
            for y in range(1, self.size + 1):
                if self.contents[x][y] == EMPTY or (x, y) in dead_points:
                    if (x, y) in points_looked_at:
                        continue
                    group = self.calculate_group(x, y, dead_points)
                    black_adjacent = 0
                    white_adjacent = 0
                    for (xx, yy) in group:
                        adjacent = self.calculate_grouped_adjacent(xx, yy, dead_points)
                        black_adjacent = black_adjacent + len(adjacent[BLACK])
                        white_adjacent = white_adjacent + len(adjacent[WHITE])
                    if white_adjacent and not black_adjacent:
                        white_territory.extend(group)
                    elif black_adjacent and not white_adjacent:
                        black_territory.extend(group)
                    else:
                        unknown_territory.extend(group)
                    points_looked_at.extend(group)
        # @@@ some code might assume position of black_territory and white_territory is BLACK and WHITE
        return (unknown_territory, black_territory, white_territory)

    def calculate_score(self, dead_points=[]):
        # uses: white_prisoners, black_prisoners, contents
        territory = self.calculate_territory(dead_points)
        # @@@ assumes return tuple from territory used BLACK and WHITE
        #TODO: add komi to the right side (black, right?)
        black_score = self.white_prisoners + len(territory[BLACK])
        white_score = self.black_prisoners + len(territory[WHITE])
        for (x, y) in dead_points:
            if self.contents[x][y] == BLACK:
                white_score = white_score + 1
            elif self.contents[x][y] == WHITE:
                black_score = black_score + 1
        return (black_score, white_score)
