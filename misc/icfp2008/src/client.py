#!/usr/bin/python
""" The beginnings of a client for the ICFP server which is, unfortunately, not yet available """

import socket
import time
import sys
from Queue import Empty, Queue

class ControllerClient:
    def __init__(self, server, port, queue):
        self.server = (server, port)
        #message queue
        self._quit = False
        self._mq = queue

    def quit(self):
        self._quit = True

    def run(self):
        while not self._quit:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 0)
            self.sock.connect(self.server)
            self._mq.put(self.receive_msg())

    def receive_msg(self):
        b = ""
        while b[-1] != ";":
            b += self.sock.recv(4096)
        return b

from threading import Thread
def test_null:
    mq = Queue()
    c = ControllerClient('localhost', 9999, mq)
    t = Thread(target=c.run, args=())
    #won't work, will block on receive_msg. Implement nonblocking sockets or timeouts.
    t.quit()
