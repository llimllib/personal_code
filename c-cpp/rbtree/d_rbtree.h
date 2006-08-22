                                       /* Bill Mill and Bryan Brook
                                          CS371
                                          Due 10.13.03
                                          Assignment #6 */

/*FILE: d_rbtree.h
  DESCRIPTION: implements a  red-black tree and associated methods
               and data structures.

               Source from http://www.fordtopp.com/ftsoftds.exe. We
               only modified the rbtree::displayTree(int) function,
               where we added code to draw lines between the levels
               of the tree on the console.

               To compile, open rbtree.dsw in MSVC 6, then click 
               build->Recompile All

   By Bill Mill and Bryan Brook, 10.12.03
   Written in MSVC 6
*/


#ifndef RED_BLACK_TREE_CLASS
#define RED_BLACK_TREE_CLASS

#ifdef _MSC_VER
// disable warning messages that identifier was truncated
// to 'number' characters in the debug information
#pragma warning(disable:4786)
#endif	// _MSC_VER

#include <iostream>
#include <iomanip>
#include <queue>
#include <string>
#include <utility>
#include <strstream>
#include <vector>

#include "d_except.h"

using namespace std;

enum colorType {RED, BLACK};

// each node of a red-black tree is of type rbnode
template <typename T>
class rbnode
{
	public:
		colorType color;		// node's color
		rbnode<T> *parent;	// node's parent
		rbnode<T> *left;		// node's left child
		rbnode<T> *right;		// node's right child
		T nodeValue;

		// constructors
		rbnode(const T& item, rbnode<T> *leftPtr, rbnode<T> *rightPtr,
			    rbnode<T> *parentPtr, colorType c):
					nodeValue(item), left(leftPtr),
					right(rightPtr), parent(parentPtr),color(c)
		{}

		rbnode()
		{}
};

// objects hold a formatted label string and the level,column
// coordinates for a shadow tree node
class rbnodeShadow
{
	public:
		string nodeValueStr;	// formatted node value
		int level,column;
		rbnodeShadow *left, *right;

		rbnodeShadow ()
		{}
};

template <typename T>
class rbtree
{
	public:

#include "d_rbiter.h"
		// iterator nested classes

		rbtree();
			// constructor. create an empty tree

		rbtree(T *first, T *last);
			// constructor. initialize the tree with the data in the
			// range [first, last)

		rbtree(const rbtree<T>& obj);
			// copy constructor

		~rbtree();
			// destructor. erase the tree

		rbtree<T>& operator= (const rbtree<T>& rhs);
			// overloaded assignment

		bool empty() const;
			// is the tree empty?

		int size() const;
			// return the number of values in the tree

		iterator find(const T& item);
			// search for item. if found, return an iterator pointing
			// at it in the tree; otherwise, return end()
		const_iterator find(const T& item) const;
			// constant version

		pair<iterator, bool> insert(const T& item);
			// if item is not in the tree, insert it and
			// return a pair whose iterator component points
			// at item and whose bool component is true. if item
			// is in the tree, return a pair whose iterator
			// component points at the existing item and whose
			// bool component is false
			// Postcondition: the tree size increases by 1 if item
			// is not in the tree

		int erase(const T& item);
			// if item is in the tree, erase it and return 1;
			// otherwise, return 0
			// Postcondition: the tree size decreases by 1 if
			// item is in the tree

		void erase(iterator pos);
			// erase the item pointed to by pos.
			// Precondition: the tree is not empty and pos points
			// to an item in the tree. if the iterator is invalid, the
			// function throws the referenceError exception.
			// Postcondition: the tree size decreases by 1

		void erase(iterator first, iterator last);
			// erase all items in the range [first, last).
			// Precondition: the tree is not empty. if the tree
			// is empty, the function throws the underflowError
			// exception.
			// Postcondition: the size of the tree decreases by
			// the number of elements in the range [first, last)

		iterator begin();
			// return an iterator pointing to the first item
			// inorder
		const_iterator begin() const;
			// constant version
		iterator end();
			// return an iterator pointing just past the end of
			// the tree data
		const_iterator end() const;
			// constant version

		void displayTree(int maxCharacters) const;
			// tree display function. maxCharacters is the
			// largest number of characters required to draw
			// the value of a node

	private:

		int treeSize;		// number of nodes in the tree
		rbnode<T> *root;	// root of the tree

		static rbnode<T> *NIL;
			// used instead of NULL to represent an empty subtree. NIL
			// is shared by all rbtree<T> objects. its pointers are NULL,
			// and its color is BLACK. it participates in rotations,
			// just like any other node

		void makeEmptyTree();
			// create an empty tree. used by two constructors

		rbnode<T> *copyTree(rbnode<T> *t);
			// create a tree that is a copy of t, and return its root pointer

		void deleteTree(rbnode<T> *t);
			// erase the tree with root t

		rbnode<T> *getRBNode(const T& item, rbnode<T> *leftPtr, rbnode<T> *rightPtr,
									rbnode<T> *parentPtr, colorType c);
			// allocate a new tree node and return a pointer to it.
			// if memory allocation fails, the function throws the
			// memoryAllocationError exception

		rbnode<T> *findNode(const T& item) const;
			// search for item in the tree. if it is in the tree,
			// return a pointer to its node; otherwise, return NIL.
			// used by find() and erase()

		void split4Node(rbnode<T> *x);
			// break up a 4-node, performing a rotation, if necessary

		void rotateRight (rbnode<T> *pivot);
			// perform a single right rotation

		void rotateLeft (rbnode<T> *pivot);
			// perform a single left rotation

		void rbDeleteFixup(rbnode<T> *x);
			// fix up the tree when x is the child of
			// a BLACK node that was unlinked from its
			// position in the tree

		rbnodeShadow *buildShadowTree(rbnode<T> *t, int level, int& column) const;
			// recursive function that builds a subtree of the shadow tree
			// corresponding to node t of the tree we are drawing. level is the
			// level-coordinate for the root of the subtree, and column is the
			// changing column-coordinate of the tree nodes

		void deleteShadowTree(rbnodeShadow *t) const;
			// remove the shadow tree from memory after displayTree()
			// displays the red-black tree
};

// declare NIL
template <typename T>
rbnode<T> *rbtree<T>::NIL = 0;

template <typename T>
void rbtree<T>::makeEmptyTree()
{
	if (NIL == 0)
	{
		// first time an rbtree object created. initialize
		// NIL. all its pointers are NULL, and its color
		// is BLACK

		NIL = new rbnode<T>;

		NIL->right = NULL;
		NIL->left = NULL;
		NIL->parent = NULL;
		NIL->color = BLACK;
	}

	// root is NIL, tree size is 0
	root = NIL;
	treeSize = 0;
}

template <typename T>
rbnode<T> *rbtree<T>::copyTree(rbnode<T> *t)
{
	rbnode<T> *newLeft, *newRight, *newNode;

	// if tree branch NIL, return NIL
	if (t == NIL)
		return NIL;

	// copy the left branch of root t and assign its root to newLptr
	newLeft = copyTree(t->left);

	// copy the right branch of tree t and assign its root to newRptr
	newRight = copyTree(t->right);

	// allocate storage for the current root node and assign its
	// data, left and right pointers pointers and color. if newNode
	// is root, NIL is the correct value for its parent pointer
	newNode = getRBNode(t->nodeValue, newLeft, newRight, NIL, t->color);

	// the parent of each non-NIL child is newNode
	if (newLeft != NIL)
		newLeft->parent = newNode;

	if (newRight != NIL)
		newRight->parent = newNode;

	return newNode;
}

template <typename T>
void rbtree<T>::deleteTree(rbnode<T> *t)
{
	 // if current root node is not NIL, erase its left subtree,
	 // its right subtree and then the node itself
	 if (t != NIL)
	 {
		  deleteTree(t->left);
		  deleteTree(t->right);
		  delete t;
	 }
}

template <typename T>
rbnode<T> *rbtree<T>::getRBNode(const T& item, rbnode<T> *leftPtr, rbnode<T> *rightPtr,
		 rbnode<T> *parentPtr, colorType c)
{
	rbnode<T> *newNode;

	newNode = new rbnode<T>(item, leftPtr, rightPtr, parentPtr, c);
	if (newNode == NULL)
		throw memoryAllocationError("rbtree: memory allocation failure");

	return newNode;
}

// search for data item in the tree. if found, return its node
// address; otherwise, return NIL
template <typename T>
rbnode<T> *rbtree<T>::findNode(const T& item) const
{
	// cycle t through the tree starting with root
	rbnode<T> *t = root;

	// terminate on on empty subtree
	while(t != NIL && !(item == t->nodeValue))
		if (item < t->nodeValue)
			t = t->left;
		else
			t = t->right;

	// return pointer to node; NIL if not found
	return t;
}

template <typename T>
void rbtree<T>::split4Node(rbnode<T> *x)
{
	// perform the color flip
	x->color = RED;
	x->left->color = BLACK;
	x->right->color = BLACK;

	// if we split the root, we are done
	if (x == root)
		return;

	// to see if a rotation is required, we need the
	// parent of x. x is not root, so p != NIL
	rbnode<T> *p = x->parent;

	// a rotation is needed if the parent of x is RED
	if (p->color == RED)
	{
		// we need the grandparent of x. since the root
		// is BLACK, p cannot be root, so the grandparent
		// exists
		rbnode<T> *g = x->parent->parent;

		// the grandparent of x will be RED
		g->color = RED;

		// a double rotation is required if x is an inside
		// grandchild. check this by seeing if the orientations
		// of p to g and x to p are different
		if ( p == g->left && x == p->right )
		{
			// perform a double right rotation
			// first move x up one level and p down
			rotateLeft(x);

			// node x will be BLACK
			x->color = BLACK;
			// prepare for a right single rotation
			p = x;
		}
		else if ( p == g->right && x == p->left )
		{
			// perform a double left rotation
			// first move x up one level and p down
			rotateRight(x);

			// node x will be BLACK
			x->color = BLACK;
			// prepare for a left single rotation
			p = x;
		}
		else
			// single rotation. parent will be BLACK
			p->color = BLACK;

		// perform a single rotation
		// move p up and g down
		if (p == g->left)
			rotateRight(p);
		else
			rotateLeft(p);
	}
}

template <typename T>
void rbtree<T>::rotateRight (rbnode<T> *pivot)
{
	// need the parent and grandparent of pivot
	rbnode<T> *p = pivot->parent, *g = pivot->parent->parent;

	// adjust right and left pointers
	p->left = pivot->right;
	pivot->right = p;

	// adjust parent pointers
	pivot->parent = g;
	p->parent = pivot;
	// don't reset the parent link of the left child of p
	// if the left child is NIL. this will interfere with
	// the use of NIL in rbDeleteFixup()
	if (p->left != NIL)
		p->left->parent = p;

	if (p == root)
		// pivot is the new root
		root = pivot;
	else if (p == g->right)
		// right link of g must point at pivot now
		g->right = pivot;
	else
		// left link of g must point at pivot now
		g->left = pivot;
}

// code is the same as the single right rotation,
// with left and right interchanged
template <typename T>
void rbtree<T>::rotateLeft (rbnode<T> *pivot)
{
	rbnode<T> *p = pivot->parent, *g = pivot->parent->parent;

	p->right = pivot->left;
	pivot->left = p;

	pivot->parent = g;
	p->parent = pivot;
	if (p->right != NIL)
		p->right->parent = p;

	if (p == root)
	  root = pivot;
	else if (p == g->right)
		g->right = pivot;
	else
		g->left = pivot;
}

template <typename T>
void rbtree<T>::rbDeleteFixup(rbnode<T> *x)
{
	rbnode<T> *siblingOfx;

	while (x != root && x->color == BLACK)

		if (x == x->parent->left)
		{
			siblingOfx = x->parent->right;

			if (siblingOfx->color == RED)
			{
				// CASE 1:
				//    sibling of x is RED. perform
				//    color changes and a left rotation.
				//    this produces a configuration that
				//    corresponds to cases 2, 3, or 4
				siblingOfx->color = BLACK;
				x->parent->color = RED;
				rotateLeft(siblingOfx);
				siblingOfx = x->parent->right;
			}

			if (siblingOfx->left->color == BLACK &&
				 siblingOfx->right->color == BLACK)
			{
				// CASE 2:
				//    both the children of the siblingOfx are BLACK.
				//    take a BLACK off of x and siblingOfx. x now
				//    has only one BLACK, and siblingOfx is RED.
				//    consider parent(x) to have an extra BLACK.
				//    if we enter this case after executing case 1,
				//    x becomes RED, and the loop terminates
				siblingOfx->color = RED;
				x = x->parent;
			}
			else
			{
				if (siblingOfx->right->color == BLACK)
				{
					// CASE 3:
					//    siblingOfx is BLACK, its left child is
					//    RED, and its right child is BLACK. perform
					//    color changes and right rotation. this
					//    transforms the node configuration into
					//    case 4, which termintes the loop

					siblingOfx->left->color = BLACK;
					siblingOfx->color = RED;
					rotateRight(siblingOfx->left);
					siblingOfx = x->parent->right;
				}

				// CASE 4:
				//    siblingOfx is BLACK and siblingOfx has a
				//    RED right child. after color changes
				//    and a left rotation, the extra BLACK on
				//    x is removed. set x to the root to terminate
				//    the loop
				siblingOfx->color = x->parent->color;
				x->parent->color = BLACK;
				siblingOfx->right->color = BLACK;
				rotateLeft(siblingOfx);
				x = root;
			}
		}
		else	// same as x == x->parent->left, except that
				// "left" and "right" are interchanged
		{
			siblingOfx = x->parent->left;

			if (siblingOfx->color == RED)
			{
				siblingOfx->color = BLACK;
				x->parent->color = RED;
				rotateRight(siblingOfx);
				siblingOfx = x->parent->left;
			}

			if (siblingOfx->right->color == BLACK &&
				 siblingOfx->left->color == BLACK)
			{
				siblingOfx->color = RED;
				x = x->parent;
			}
			else
			{
				if (siblingOfx->left->color == BLACK)
				{
					siblingOfx->right->color = BLACK;
					siblingOfx->color = RED;
					rotateLeft(siblingOfx->right);
					siblingOfx = x->parent->left;
				}

				siblingOfx->color = x->parent->color;
				x->parent->color = BLACK;
				siblingOfx->left->color = BLACK;
				rotateRight(siblingOfx);
				x = root;
			}
		}

		x->color = BLACK;
}

template <typename T>
rbtree<T>::rbtree()
{
	makeEmptyTree();
}

template <typename T>
rbtree<T>::rbtree(T *first, T *last)
{
	// construct an empty tree
	makeEmptyTree();

	// insert the data into the tree
	while (first != last)
		insert(*first++);
}

template <typename T>
rbtree<T>::rbtree(const rbtree<T>& obj):
		treeSize(obj.treeSize)
{
	// call copyTree(), and assign its return value
	// to the root
	root = copyTree(obj.root);
}

template <typename T>
rbtree<T>::~rbtree()
{
	// erase the nodes in the tree
	deleteTree(root);

	// tree is emtpy
	root = NIL;
	treeSize = 0;
}

template <typename T>
rbtree<T>& rbtree<T>::operator= (const rbtree<T>& rhs)
{
	if (this != &rhs)
	{
		// erase the tree
		deleteTree(root);

		// call copyTree(), and assign its return value
		// to the root
		root = copyTree(rhs.root);
		treeSize = rhs.treeSize;
	}

	return *this;
}

template <typename T>
bool rbtree<T>::empty() const
{
	return treeSize == 0;
}

template <typename T>
int rbtree<T>::size() const
{
	return treeSize;
}

template <typename T>
rbtree<T>::iterator rbtree<T>::find (const T& item)
{
	rbnode<T> *curr;

	// search tree for item
	curr = findNode(item);

	// if item found, return iterator with value current;
	// otherwise, return end()
	if (curr != NIL)
		return iterator(curr, this);
	else
		return end();
}

template <typename T>
rbtree<T>::const_iterator rbtree<T>::find (const T& item) const
{
	rbnode<T> *curr;

	// search tree for item
	curr = findNode(item);

	// if item found, return const_iterator with value current;
	// otherwise, return end()
	if (curr != NIL)
		return const_iterator(curr, this);
	else
		return end();
}

template <typename T>
pair<rbtree<T>::iterator,bool> rbtree<T>::insert (const T& item)
{
	// TOP-DOWN INSERTION

	// declare pointers to the current node and its parent
	rbnode<T> *curr = root, *parent = NIL, *newNode;

	// loop until we find the value in the tree or locate
	// the insertion point
	while (curr != NIL)
	{
		if (curr->nodeValue == item)
		{
			// item is in the tree. return a pair whose first member
			// is an iterator pointing at item and whose second member
			// is false
			return pair<iterator,bool> (iterator(curr,this), false);
		}

		// a node split is required if both children of curr are RED
		if (curr->left->color == RED && curr->right->color == RED)
			split4Node(curr);

		// move down the tree
		parent = curr;
		if (item < curr->nodeValue)
			curr = curr->left;
		else
			curr = curr->right;
	}

	// create the new node
	newNode = getRBNode(item, NIL, NIL, parent, RED);

	// is item the first node inserted into the tree?
	if (parent == NIL)
		// item is at the root of a brand new tree.
		root = newNode;
	else
	{
		// link the new node into its parent
		if (item < parent->nodeValue)
			// insert as left child
			parent->left = newNode;
		else
			// insert as right child
			parent->right = newNode;

		// if the new node's parent is RED, we
		// must perform a rotation
		if (parent->color == RED)
			split4Node(newNode);
	}

	// the color of the root must be BLACK
	root->color = BLACK;

	// the tree has one more node
	treeSize++;

	// we did an insertion. set success to true and return
	// an iterator pointing at item
	return pair<iterator,bool> (iterator(newNode,this),true);
}

template <typename T>
int rbtree<T>::erase(const T& item)
{
	int numberErased = 1;
	// search tree for item
	rbnode<T> *p  = findNode(item);

	// if item found, delete the node
	if (p != NIL)
		erase(iterator(p,this));
	else
		numberErased = 0;

	return numberErased;
}

template <typename T>
void rbtree<T>::erase (iterator pos)
{
	// BOTTOM-UP ERASE

	// dNodePtr = pointer to node D that is deleted
	// pNodePtr = pointer to parent P of node D
	// rNodePtr = pointer to node R that replaces D
	// spliceOut = pointer to the node that is spliced out of the tree
	// childOfSpliceOut = pointer to the child of the node we splice out
	rbnode<T> *dNodePtr = pos.nodePtr, *pNodePtr, *rNodePtr,
				 *spliceOut, *childOfSpliceOut;
	// saves color of node that is spliced out
	colorType spliceoutColor;

	if (dNodePtr == NIL)
 		throw
			referenceError("rbtree erase(): invalid iterator");

	// assign pNodePtr the address of P
	pNodePtr = dNodePtr->parent;

	// If D has a NIL pointer, the
	// replacement node is the other child
	if (dNodePtr->right == NIL || dNodePtr->left == NIL)
	{
		if (dNodePtr->right == NIL)
			rNodePtr = dNodePtr->left;
		else
			rNodePtr = dNodePtr->right;

		// the parent of R is now the parent of D
		// NOTE: rNodePtr may be NIL
		rNodePtr->parent = pNodePtr;

		// we are splicing dNodePtr out of the tree
		spliceOut = dNodePtr;
		// save color of dNodePtr
		spliceoutColor = spliceOut->color;
		// record the child of our spliceout node for possible
		// balancing operations
		childOfSpliceOut = rNodePtr;
	}
	// both pointers of dNodePtr are non-NIL.
	else
	{
		// find and unlink replacement node for D.
		// starting at the right child of node D,
		// find the node whose value is the smallest of all
		// nodes whose values are greater than the value in D.
		// unlink the node from the tree.

		// pOfRNodePtr = pointer to parent of replacement node
		rbnode<T> *pOfRNodePtr = dNodePtr;

		// first possible replacement is right child of D
		rNodePtr = dNodePtr->right;

		// descend down left subtree of the right child of D,
		// keeping a record of current node and its parent.
		// when we stop, we have found the replacement
		while(rNodePtr->left != NIL)
		{
			pOfRNodePtr = rNodePtr;
			rNodePtr = rNodePtr->left;
		}

		// we are splicing rNodePtr out of the tree at its
		// current location
		spliceOut = rNodePtr;
		// save color of the node we will splice out
		spliceoutColor = spliceOut->color;
		// we will replace dNodPtr by spliceOut. make the color
		// of spliceOut the color of dNodePtr
		spliceOut->color = dNodePtr->color;
		// record the child of our spliceout node for possible
		// balancing operations
		childOfSpliceOut = rNodePtr->right;

		if (pOfRNodePtr == dNodePtr)
		{
			// right child of deleted node is the replacement.
			// assign left subtree of D to left subtree of R
			rNodePtr->left = dNodePtr->left;
			// assign the parent of D as the parent of R
			rNodePtr->parent = pNodePtr;
			// assign the left child of D to have parent R
			dNodePtr->left->parent = rNodePtr;
			// if the right child of rNodePtr is NIL,
			// make rNodrPtr the parent of NIL, since any
			// required fixup will start with the child
			if (rNodePtr->right == NIL)
				rNodePtr->right->parent = rNodePtr;
		}
		else
		{
			// we moved at least one node down a left branch
			// of the right child of D. unlink R from tree by
			// assigning its right subtree as the left child of
			// the parent of R
			pOfRNodePtr->left = rNodePtr->right;

			// the parent of the right child of R is the
			// parent of R
			// NOTE: rNodePtr may be NIL
			rNodePtr->right->parent = pOfRNodePtr;

			// put replacement node in place of dNodePtr
			// assign children of R to be those of D
			rNodePtr->left = dNodePtr->left;
			rNodePtr->right = dNodePtr->right;
			// assign the parent of R to be the parent of D
			rNodePtr->parent = pNodePtr;
			// assign the parent pointer in the children
			// of R to point at R
			rNodePtr->left->parent = rNodePtr;
			rNodePtr->right->parent = rNodePtr;
		}
	}

	// complete the link to the parent node.

	// deleting the root node. assign new root
	if (pNodePtr == NIL)
		root = rNodePtr;
	// attach R to the correct branch of P
	else if (dNodePtr->nodeValue < pNodePtr->nodeValue)
		pNodePtr->left = rNodePtr;
	else
		pNodePtr->right = rNodePtr;

	// delete the node from memory and decrement tree size
	delete dNodePtr;

	// fixup the tree if the node spliced out is BLACK
	if (spliceoutColor == BLACK)
		rbDeleteFixup(childOfSpliceOut);

	treeSize--;
}

template <typename T>
void rbtree<T>::erase (rbtree<T>::iterator first,
							  rbtree<T>::iterator last)
{
	if (treeSize == 0)
 		throw
			underflowError("rbtree erase(): tree is empty");

	if (first == begin() && last == end())
	{
		// we are asked to erase the entire tree.
		// erase the tree nodes from memory
		deleteTree(root);

		// tree is emtpy
		root = NIL;
		treeSize = 0;
	}
	else
		// erase each item in a subrange of the tree
		while (first != last)
			erase(first++);
}

template <typename T>
rbtree<T>::iterator rbtree<T>::begin()
{
	rbnode<T> *curr = root;

	// if the tree is not empty, the first node
	// inorder is the farthest node left from root
	if (curr != NIL)
		while (curr->left != NIL)
			curr = curr->left;

	// build return value using private constructor
	return iterator(curr, this);
}

template <typename T>
rbtree<T>::const_iterator rbtree<T>::begin() const
{
	const rbnode<T> *curr = root;

	// if the tree is not empty, the first node
	// inorder is the farthest node left from root
	if (curr != NIL)
		while (curr->left != NIL)
			curr = curr->left;

	// build return value using private constructor
	return const_iterator(curr, this);
}

template <typename T>
rbtree<T>::iterator rbtree<T>::end()
{
	// end indicated by an iterator with NIL stnode pointer
	return iterator(NIL, this);
}

template <typename T>
rbtree<T>::const_iterator rbtree<T>::end() const
{
	// end indicated by an iterator with NIL stnode pointer
	return const_iterator(NIL, this);
}

// recursive inorder scan used to build the shadow tree
template <typename T>
rbnodeShadow *rbtree<T>::buildShadowTree(rbnode<T> *t, int level, int& column) const
{
	// pointer to new shadow tree node
	rbnodeShadow *newNode = NULL;

	// text and ostr used to perform format conversion
	char text[80];
	ostrstream ostr(text,80);

	if (t != NIL)
	{
		// create the new shadow tree node
		newNode = new rbnodeShadow;

		// allocate node for left child at next level in tree; attach node
		rbnodeShadow *newLeft = buildShadowTree(t->left, level+1, column);
		newNode->left = newLeft;

		// initialize data members of the new node
		if (t->color == RED)
			ostr << t->nodeValue << '*' << ends;
		else
			ostr << t->nodeValue << ends;

		newNode->nodeValueStr = text;
		newNode->level = level;
		newNode->column = column;

		// update column to next cell in the table
		column++;

		// allocate node for right child at next level in tree; attach node
		rbnodeShadow *newRight = buildShadowTree(t->right, level+1, column);
		newNode->right = newRight;
	}

	return newNode;
}

template <typename T>
void rbtree<T>::displayTree(int maxCharacters) const
{
   //a Tcol is a column where a number and a line intersect
   vector<int> Tcols;
	string label;
	int level = 0, column = 0;
	int colWidth = maxCharacters + 1;
	int currLevel = 0, currCol = 0;

	if (treeSize == 0)
		return;

	// build the shadow tree
	rbnodeShadow *shadowRoot = buildShadowTree(root, level, column);

	// use during the level order scan of the shadow tree
	rbnodeShadow *currNode;

   // store siblings of each rbnodeShadow object in a queue so that
	// they are visited in order at the next level of the tree
   queue<rbnodeShadow *> q;

   // insert the root in the queue and set current level to 0
   q.push(shadowRoot);

   // continue the iterative process until the queue is empty
   while(!q.empty())
   {
      // delete front node from queue and make it the current node
      currNode = q.front();
		q.pop();

		// if level changes, output a newline
		if (currNode->level > currLevel)
		{
			currLevel = currNode->level;
			currCol = 0;
			cout << endl;
         unsigned int i, j, curTcol;
         bool outta_bounds;

         //output spaces before the first '+'
         cout << setw(Tcols[0] * colWidth + (colWidth-1)) << " ";
         //for each Tcol (see above)
         for(i=0; i < Tcols.size() - 1; i++) {
            cout << "+";
            outta_bounds = false;
            curTcol = Tcols[i];
            // if the next number is a negative one, don't put a line between
            // this cross and the next
            if(Tcols[i+1] < 0) {
               //find the next positive number
               while(Tcols[i+1] < 0 && i+2 < Tcols.size()) i++;
               //if we found another cross to be output
               if (i+2 < Tcols.size())
                  //put spaces between the current cross and the next one
                  cout << setw((Tcols[i+1] - curTcol) * colWidth - 1) << " ";
               //otherwise, we're outtabounds
               else break;
            }
            else
               for(j=0; j < (Tcols[i+1] - curTcol) * colWidth - 1 ; j++)
                  cout << "-";
         }
         //cout << "+";
         cout << endl;
         Tcols.clear();
		}

		// if a left child exists, insert the child in the queue
      if(currNode->left != NULL)
			q.push(currNode->left);

		// if a right child exists, insert the child in the queue
      if(currNode->right != NULL)
			q.push(currNode->right);

		// output formatted node label
		if (currNode->column > currCol)
		{
			cout << setw((currNode->column-currCol)*colWidth) << ' ';
         //if there is a left node, add it to the Tcols
         if(currNode->left != NULL)
            Tcols.push_back(currNode->left->column);
         //if both nodes aren't null, we'll need a Tcol underneath the middle
         if(currNode->left != NULL || currNode->right != NULL)
            Tcols.push_back(currNode->column);
         //if there is a right node, add it to the Tcols
         if(currNode->right != NULL)
            Tcols.push_back(currNode->right->column);
         //mark a space between nodes
         Tcols.push_back(-1);
         //update the current column
			currCol = currNode->column;
		}
		cout << setw(colWidth) << currNode->nodeValueStr;
		currCol++;
   }
	cout << endl;

	// delete the shadow tree
	deleteShadowTree(shadowRoot);
}

template <typename T>
void rbtree<T>::deleteShadowTree(rbnodeShadow *t) const
{
	// if current root node is not NULL, delete its left subtree,
	// its right subtree and then the node itself
	if (t != NULL)
	{
		deleteShadowTree(t->left);
		deleteShadowTree(t->right);
		delete t;
	}
}

#endif	// RED_BLACK_TREE_CLASS
