import thread, time

def HelloWorld():
    print "Hello from thread %d" % thread.get_ident()

if __name__ == "__main__":
    num_threads = 5
    for i in range(num_threads):
        x = thread.start_new_thread(HelloWorld, ())
    time.sleep(3)
