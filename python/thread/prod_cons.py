import threading
import random
import Queue
import time

class Producer(threading.Thread):
    def __init__(self, queue, rand):
        threading.Thread.__init__(self)
        self.queue = queue
        self.rand = rand
        self.count = 0

    def run(self):
        print "Thread %s started" % self.getName()
        while self.count < 100:
            self.queue.put(self.rand.randint(1,100))
            time.sleep(self.rand.randint(1,2))
            print "psize: %d" % self.queue.qsize()

class Consumer(threading.Thread):
    def __init__(self, queue, rand):
        threading.Thread.__init__(self)
        self.queue = queue
        self.rand = rand
        self.count = 0

    def run(self):
        print "Thread %s started" % self.getName()
        while self.count < 100:
            num = self.queue.get()
            time.sleep(self.rand.randint(1,3))
            print "csize: %d num: %d" % (self.queue.qsize(), num)

if __name__ == "__main__":
    rand = random.Random()
    q = Queue.Queue()
    prod = Producer(q, rand)
    prod.setName("Producer")
    cons = Consumer(q, rand)
    cons.setName("Consumer")
    prod.start()
    cons.start()
