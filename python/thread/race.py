import threading
import random
import signal

accnt = {0:0.0, 1:0.0}

class Race(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.rand = random.Random()
        self.count = 0

    def run(self):
        print "thread %s started" % self.getName()
        while accnt[0] + accnt[1] == 0.0 and self.count < 100000:
            rand = self.rand.random()
            accnt[0] += rand
            accnt[1] -= rand
            self.count += 1
        print "%s exited after %d iterations" % (self.getName(), self.count)

if __name__ == "__main__":
    num_threads = 2
    for i in range(num_threads):
        thread = Race()
        thread.setName(i)
        thread.start()
