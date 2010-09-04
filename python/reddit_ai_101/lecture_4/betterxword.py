#!/usr/local/bin/python3

def getwords(dictionary_filename):
    return {w for w in open("words.txt").read().split()}

class Node:
    def __init__(self, word, parent=None):
        self.father = parent
        self.words  = []
