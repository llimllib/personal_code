import threading, time, random

class HelloWorld(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.r = random.Random()

    def run(self):
        for i in range(10):
            print "Hello from thread %s" % self.getName()
            time.sleep(self.r.random())


if __name__ == "__main__":
    num_threads = 5
    for i in range(num_threads):
        thread = HelloWorld()
        thread.setName(i)
        thread.start()
