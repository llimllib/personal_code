import go_game
import unittest
import sys

known_GTP_vals = (("B A1", ("B", (0,18))),
    ("W C8", ("B", (2,11))),
    ("B J15", ("B", (8,4))))

error_GTP_vals = ("B A 1",
    "BA1",
    "G A1",
    "B ZZ",
    "B 88")
    
small_legal_move_list = ("B A1",
    "W A2",
    "B B2",
    "W a3",
    "B b1")

#longer_lml = ("")
    

class GoGameTest(unittest.TestCase):
    def setUp(self):
        self.boardsize = 19
        self.g = go_game.go_game(self, sys.stdin)
        self.g.set_boardsize(self.boardsize)
        self.g.clear_board()
        self.g.set_komi("5.5")
        #test handicaps?
        
        self.testBoard = []
        for i in range(self.boardsize):
            self.testBoard.append([])
            for j in range(self.boardsize):
                self.testBoard[i].append(' ')
    
    def testGTPTranslate(self):
        for i in known_GTP_vals:
            res = self.translate_from_gtp(i[0])
            self.assertEqual(res, i[1])
    
    def testShortPlay(self):
        should_be_removed=[(0,0)]
        removed=[]
        for move in small_legal_move_list:
            pieces = self.g.play(move)
            if pieces:
                removed.append(pieces)
        
        self.assertEqual(removed, should_be_removed)
        
def suite():
    suite = unittest.TestSuite()
    suite.addTest(GoGameTest("testBoard"))
    return suite

if __name__ == "__main__":
    test_suite = suite()
    result = unittest.TestResult()
    test_suite.run(result)
