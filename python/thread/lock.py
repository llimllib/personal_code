import threading
import random
import signal

accnt = {0:0.0, 1:0.0}

class Race(threading.Thread):
    def __init__(self, lock):
        threading.Thread.__init__(self)
        self.rand = random.Random()
        self.count = 0
        self.lock = lock

    def run(self):
        print "thread %s started" % thread.getName()
        while self.count < 100000:
            self.lock.acquire()
            rand = self.rand.random()
            accnt[0] += rand
            accnt[1] -= rand
            sum = accnt[0] + accnt[1]
            self.lock.release()
            self.count += 1
            if sum != 0.0: break
        print "%s exited after %d iterations" % (self.getName(), self.count)

if __name__ == "__main__":
    num_threads = 2
    lock = threading.Lock()
    for i in range(num_threads):
        thread = Race(lock)
        thread.setName(i)
        thread.start()
