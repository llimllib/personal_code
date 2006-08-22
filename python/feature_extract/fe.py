import os, sys, random, sgflib, gogame

RAND = random.Random()
DEBUG = sys.stdout

class GameTreeEnded(Exception):pass
class SurroundedError(Exception): pass
class noMoveFoundError(Exception): pass
###
#fix?
class mysteriousError(Exception): pass
###
    
class FeatureExtractor:
    def __init__(self):
        self.game = gogame.GoGame()
        self.size = 19      #size of the board
        self.dSize = 3      #size of the feature diamond
        self.mDist = 2      #Max Manhattan distance of random move

    def parseData(self, data):
        """parses the SGF data given as its parameter. This function
        must be called before any other in this class is used."""
        col = sgflib.SGFParser(data).parse()
        m = col[0].mainline()
        self.curse = m.cursor()
        self.parseRoot()
    
    def parseRoot(self):
        rootNode = self.curse.current()
        if rootNode.has_key('PW') and rootNode.has_key('PB') \
        and rootNode.has_key('WR') and rootNode.has_key('BR'):
            DEBUG.write('white: %s %s   black: %s %s\n' % (rootNode['PW'][0], \
            rootNode['WR'][0], rootNode['PB'][0], rootNode['BR'][0]))

        if rootNode.has_key('AB'):              #parse handicap stones
            for move in rootNode['AB']:
                x, y = self.changeCoords(move)
                self.game.place_stone(x, y, 1)
        
    def randMove(self, eMove):
        """return a random move within a manhattan distance of 
        less than self.mDist from an expert move"""
        x = RAND.randint(-self.mDist, self.mDist)
        y = RAND.randint(-self.mDist, self.mDist)
        rMove = (eMove[0]+x, eMove[1]+y)
        loopCount = 0
        while not 0 < abs(x) + abs(y) <= self.mDist \
            or (x + y == 0 and x == 0) \
            or not self.onBoard(rMove[0], rMove[1]) \
            or self.game.is_occupied(rMove[0], rMove[1]):
            x = RAND.randint(-self.mDist, self.mDist)
            y = RAND.randint(-self.mDist, self.mDist)
            rMove = (eMove[0]+x, eMove[1]+y)
            if loopCount + 1 > 100:
                raise SurroundedError
            loopCount += 1
        return rMove

    def extractFeatures(self, expertf, randomf):
        if self.curse.current().has_key('W'):
            eMove = self.changeCoords(self.curse.current()['W'][0])
        else:
            eMove = self.changeCoords(self.curse.current()['B'][0])
        rMove = self.randMove(eMove)
        for i in range(-self.dSize, self.dSize + 1):
            for j in range(-self.dSize, self.dSize + 1):
                if 0 < abs(i) + abs(j) < 4 \
                and (i + j != 0 or i != 0):
                    ex = eMove[0] + i
                    ey = eMove[1] + j
                    rx = rMove[0] + i
                    ry = rMove[1] + j
                    if not self.onBoard(ex, ey):
                        expertf.write('-1\t')
                    else:
                        expertf.write('%s\t' % self.game.contents[ex][ey])
                    if not self.onBoard(rx, ry):
                        randomf.write('-1\t')
                    else:
                        randomf.write('%s\t' % self.game.contents[rx][ry])
        expertf.write('\n')
        randomf.write('\n')
        expertf.flush()
        randomf.flush()
    
    def onBoard(self, x, y):
        if 0 <= x < 19 and 0 <= y < 19:
            return 1
        return 0
    
    def playMoves(self, n):
        node = self.curse.next()
        for i in range(n):
            if self.curse.atEnd:
                raise GameTreeEnded
            if node.has_key('W') and len(node['W'][0]) == 2:
                x, y = self.changeCoords(node['W'][0])
                self.game.move_stone(x, y, 'W')
            elif node.has_key('B') and len(node['B'][0]) == 2:
                x, y = self.changeCoords(node['B'][0])
                self.game.move_stone(x, y, 'B')
            else: 
                raise GameTreeEnded
            node = self.curse.next()
    
    def changeCoords(self, sgfCoords):
        try:
            x = ord(sgfCoords[0]) - ord('a')
            y = ord(sgfCoords[1]) - ord('a')
        except:
            print "sgfCoords: %s " % str(sgfCoords)
            raise mysteriousError
        return (x, y)
        
if __name__ == "__main__":
    games = os.listdir('games2002')
    fe = FeatureExtractor()
    expertf = open('expert.dat', 'w')
    randomf = open('random.dat', 'w')
    i = 0
    num_examples = 0
    while i < 1000:
        filenum = RAND.randint(0, len(games) - 1)
        movenum = RAND.randint(50,200)
        data = open('games2002/%s' % games[filenum]).read()
        #data = open('games2002/2002-12-09-2.sgf').read()
        DEBUG.write('%d) processing file %s with movenum: %d\n' \
            % (i, games[filenum], movenum))
        fe.parseData(data)
        try:
            fe.playMoves(movenum)
            fe.extractFeatures(expertf, randomf)
        except GameTreeEnded, sgflib.GameTreeEndError:
            DEBUG.write("file not processed, movenum too high\n")
            i -= 1
        except SurroundedError:
            DEBUG.write("file not processed, expert move surrounded\n")
            i-= 1
        i += 1
        num_examples += 1
        DEBUG.flush()
    print "finished processing"
