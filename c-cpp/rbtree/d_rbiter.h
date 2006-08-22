#ifdef __BORLANDC__
// suppress the warning message that functions containing for are not
// expanded inline
#pragma warn -8027
#endif	// __BORLANDC__

class  iterator;
class  const_iterator;
	// declare the iterator classes so the names are available
friend class iterator;
friend class const_iterator;
	// allow the iterator classes to access the private section
	// of rbtree

class iterator
{
	friend class rbtree<T>;
	friend class const_iterator;

	public:

		// constructor
		iterator ()
		{}

		// comparison operators. just compare node pointers
		bool operator== (const iterator& rhs) const
		{
			return nodePtr == rhs.nodePtr;
		}

		bool operator!= (const iterator& rhs) const
		{
			return nodePtr != rhs.nodePtr;
		}

		// dereference operator. return a reference to
		// the value pointed to by nodePtr
		T& operator* () const
		{
			if (nodePtr == NIL)
 				throw
					referenceError("rbtree iterator operator* (): NULL reference");

			return nodePtr->nodeValue;
		}

		// preincrement. move forward to next larger value
		iterator& operator++ ()
		{
			rbnode<T> *p;

			if (nodePtr == NIL)
			{
				// ++ from end(). get the root of the tree
				nodePtr = tree->root;

				// error! ++ requested for an empty tree
				if (nodePtr == NIL)
					throw
						underflowError("rbtree iterator operator++ (): tree empty");

				// move to the smallest value in the tree,
				// which is the first node inorder
				while (nodePtr->left != NIL)
					nodePtr = nodePtr->left;
			}
			else
			if (nodePtr->right != NIL)
			{
				// successor is the furthest left node of
				// right subtree
				nodePtr = nodePtr->right;

				while (nodePtr->left != NIL)
					nodePtr = nodePtr->left;
			}
			else
			{
				// have already processed the left subtree, and
				// there is no right subtree. move up the tree,
				// looking for a parent for which nodePtr is a left child,
				// stopping if the parent becomes NIL. a non-NIL parent
				// is the successor. if parent is NIL, the original node
				// was the last node inorder, and its successor
				// is the end of the list
				p = nodePtr->parent;

				while (p != NIL && nodePtr == p->right)
				{
					nodePtr = p;
					p = p->parent;
				}

				// if we were previously at the right-most node in
				// the tree, nodePtr = NIL, and the iterator specifies
				// the end of the list
				nodePtr = p;
			}

			return *this;
		}

		// postincrement
		iterator operator++ (int)
		{
			// save current iterator
			iterator tmp = *this;

			// move myself forward to the next tree node
			++*this;

			// return the previous value
			return tmp;
		}

		// predecrement. move backward to largest value < current value
		iterator& operator-- ()
		{
			rbnode<T> *p;

			if (nodePtr == NIL)
			{
				// -- from end(). get the root of the tree
				nodePtr = tree->root;

				// error! -- requested for an empty tree
				if (nodePtr == NIL)
					throw
						underflowError("rbtree iterator operator--: tree empty");

				// move to the largest value in the tree,
				// which is the last node inorder
				while (nodePtr->right != NIL)
					nodePtr = nodePtr->right;
			} else if (nodePtr->left != NIL)
			{
				// must have gotten here by processing all the nodes
				// on the left branch. predecessor is the farthest
				// right node of the left subtree
				nodePtr = nodePtr->left;

				while (nodePtr->right != NIL)
					nodePtr = nodePtr->right;
			}
			else
			{
				// must have gotten here by going right and then
				// far left. move up the tree, looking for a parent
				// for which nodePtr is a right child, stopping if the
				// parent becomes NIL. a non-NIL parent is the
				// predecessor. if parent is NIL, the original node
				// was the first node inorder, and its predecessor
				// is the end of the list
				p = nodePtr->parent;
				while (p != NIL && nodePtr == p->left)
				{
					nodePtr = p;
					p = p->parent;
				}

				// if we were previously at the left-most node in
				// the tree, nodePtr = NIL, and the iterator specifies
				// the end of the list
				nodePtr = p;
			}

			return *this;
		}

		// postdecrement
		iterator operator-- (int)
		{
			// save current iterator
			iterator tmp = *this;

			// move myself backward to the previous tree node
			--*this;

			// return the previous value
			return tmp;
		}

	private:

		// nodePtr is the current location in the tree. we can move
		// freely about the tree using left, right, and parent.
		// tree is the address of the rbtree object associated
		// with this iterator. it is used only to access the
		// root pointer, which is needed for ++ and --
		// when the iterator value is end()
		rbnode<T> *nodePtr;
		rbtree<T> *tree;

		// used to construct an iterator return value from
		// an rbnode pointer
		iterator (rbnode<T> *p, rbtree<T> *t) : nodePtr(p), tree(t)
		{}

};

class const_iterator
{
	friend class rbtree<T>;

	public:

		// constructor
		const_iterator ()
		{}

		// used to convert a const iterator to a const_iterator
		const_iterator (const iterator& pos): nodePtr(pos.nodePtr)
		{}

		// comparison operators. just compare node pointers
		bool operator== (const const_iterator& rhs) const
		{
			return nodePtr == rhs.nodePtr;
		}

		bool operator!= (const const_iterator& rhs) const
		{
			return nodePtr != rhs.nodePtr;
		}

		// dereference operator. return a reference to
		// the value pointed to by nodePtr
		const T& operator* () const
		{
			if (nodePtr == NIL)
 				throw
					referenceError("rbtree const_iterator operator* (): NULL reference");

			return nodePtr->nodeValue;
		}

		// preincrement. move forward to next larger value
		const_iterator& operator++ ()
		{
			rbnode<T> *p;

			if (nodePtr == NIL)
			{
				// ++ from end(). get the root of the tree
				nodePtr = tree->root;

				// error! ++ requested for an empty tree
				if (nodePtr == NIL)
					throw underflowError("rbtree const_iterator operator++ (): tree empty");

				// move to the smallest value in the tree,
				// which is the first node inorder
				while (nodePtr->left != NIL)
					nodePtr = nodePtr->left;
			}
			else
			if (nodePtr->right != NIL)
			{
				// successor is the furthest left node of
				// right subtree
				nodePtr = nodePtr->right;

				while (nodePtr->left != NIL)
					nodePtr = nodePtr->left;
			}
			else
			{
				// have already processed the left subtree, and
				// there is no right subtree. move up the tree,
				// looking for a parent for which nodePtr is a left child,
				// stopping if the parent becomes NIL. a non-NIL parent
				// is the successor. if parent is NIL, the original node
				// was the last node inorder, and its successor
				// is the end of the list
				p = nodePtr->parent;

				while (p != NIL && nodePtr == p->right)
				{
					nodePtr = p;
					p = p->parent;
				}

				// if we were previously at the right-most node in
				// the tree, nodePtr = NIL, and the iterator specifies
				// the end of the list
				nodePtr = p;
			}

			return *this;
		}

		// postincrement
		const_iterator operator++ (int)
		{
			// save current const_iterator
			const_iterator tmp = *this;

			// move myself forward to the next tree node
			++*this;

			// return the previous value
			return tmp;
		}

		// predecrement. move backward to largest value < current value
		const_iterator& operator-- ()
		{
			rbnode<T> *p;

			if (nodePtr == NIL)
			{
				// -- from end(). get the root of the tree
				nodePtr = tree->root;

				// error! -- requested for an empty tree
				if (nodePtr == NIL)
					throw
						underflowError("rbtree iterator operator--: tree empty");

				// move to the largest value in the tree,
				// which is the last node inorder
				while (nodePtr->right != NIL)
					nodePtr = nodePtr->right;
			} else if (nodePtr->left != NIL)
			{
				// must have gotten here by processing all the nodes
				// on the left branch. predecessor is the farthest
				// right node of the left subtree
				nodePtr = nodePtr->left;

				while (nodePtr->right != NIL)
					nodePtr = nodePtr->right;
			}
			else
			{
				// must have gotten here by going right and then
				// far left. move up the tree, looking for a parent
				// for which nodePtr is a right child, stopping if the
				// parent becomes NIL. a non-NIL parent is the
				// predecessor. if parent is NIL, the original node
				// was the first node inorder, and its predecessor
				// is the end of the list
				p = nodePtr->parent;
				while (p != NIL && nodePtr == p->left)
				{
					nodePtr = p;
					p = p->parent;
				}

				// if we were previously at the left-most node in
				// the tree, nodePtr = NIL, and the iterator specifies
				// the end of the list
				nodePtr = p;
			}

			return *this;
		}

		// postdecrement
		const_iterator operator-- (int)
		{
			// save current const_iterator
			const_iterator tmp = *this;

			// move myself backward to the previous tree node
			--*this;

			// return the previous value
			return tmp;
		}

	private:

		// nodePtr is the current location in the tree. we can move
		// freely about the tree using left, right, and parent.
		// tree is the address of the rbtree object associated
		// with this iterator. it is used only to access the
		// root pointer, which is needed for ++ and --
		// when the iterator value is end()
		const rbnode<T> *nodePtr;
		const rbtree<T> *tree;

		// used to construct a const_iterator return value from
		// an rbnode pointer
		const_iterator (const rbnode<T> *p, const rbtree<T> *t) : nodePtr(p), tree(t)
		{}

};
