#!/usr/bin/python
import sys, os
from random import Random
import asynchat, socket

DEBUG = sys.stderr

class pitch_game:
    def __init__(self, server):
        self.conn = pitch_server_conn(server)

class pitch_server_conn(asynchat.async_chat):
    def __init__(self, server):
        asynchat.async_chat.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect(("localhost", 812))
        self.set_terminator("\n")
        self.buffer = ""
        self.push("This is something\n")

    def handle_connect(self):
        print >> DEBUG, "Connected"

    def collect_incoming_data(self, data):
        print "collecting"
        self.buffer = self.buffer + data

    def found_terminator(self):
        print >> DEBUG, "Received: %s" % self.buffer
        self.buffer = ""
        self.close()

class TooManyCardsError(Exception): pass
    
if __name__ == '__main__':
    game = pitch_game(("127.0.0.1", 812))
