import sys
from typing import List

class Tree:
    def __init__(self, parent):
        self.parent = parent
        self.left = None
        self.right = None

    def __str__(self):
        return f"[{str(self.left)},{str(self.right)}]"

    def __repr__(self):
        return self.__str__()

def make_tree(lst, parent=None):
    match lst:
        case [l, r]:
            t = Tree(parent)
            t.left = make_tree(l, t)
            t.right = make_tree(r, t)
            return t
        case value:
            return value

def explodel(node, val):
    while node.parent:
        if isinstance(node.left, int):
            node.left += val
            return
        node = node.parent

def isnode(n):
    return isinstance(n, Tree)

def isval(n):
    return isinstance(n, int)

def explode(node, lval, rval):
    # save the id of the node we're looking for
    target = id(node)

    # traverse up to the root
    root = node
    while root.parent:
        root = root.parent

    # do an inorder traversal of the tree
    queue = [root]
    leftn = None
    found = None
    while queue:
        visit = queue.pop()
        print(visit)
        # if we've already found our node, the next value we find is the one we
        # should add a number to
        if found:
            if isval(visit.left):
                visit.left += rval
                break
            elif isval(visit.right):
                visit.right += rval
                break
        # if the node is the one we're searching for, add the lval to the last
        # value we encountered
        elif id(visit) == target:
            found = True
            if leftn:
                if isval(leftn.right):
                    leftn.right += lval
                elif isval(leftn.left):
                    leftn.left += lval
                else:
                    raise Exception("leftn has no values" ,leftn)

        # if the node has any values, save it - it might be the left-most node
        if isval(visit.left) or isval(visit.right):
            leftn = visit

        if isnode(visit.right):
            queue.append(visit.right)
        if isnode(visit.left):
            queue.append(visit.left)


def find4(node, depth=0):
    if not isinstance(node.left, int):
        if depth == 3:
            explode(node.left, node.left.left, node.left.right)
            node.left = 0
        else:
            find4(node.left, depth+1)
    if not isinstance(node.right, int):
        if depth == 3:
            explode(node.right, node.right.left, node.right.right)
            node.right = 0
        else:
            find4(node.right, depth+1)
    return node

def reduce(root):
    return find4(root)

def t(lst: str) -> List[List | int]:
    return eval(repr(reduce(make_tree(lst))))

# assert t([[[[[9,8],1],2],3],4]) == [[[[0,9],2],3],4]
# assert t([7,[6,[5,[4,[3,2]]]]]) == [7,[6,[5,[7,0]]]]
root = make_tree([[6,[5,[4,[3,2]]]],1])
print(root)
print('----')
reduce(root)
print('----')
print(root)
assert t([[6,[5,[4,[3,2]]]],1]) == [[6,[5,[7,0]]],3]
