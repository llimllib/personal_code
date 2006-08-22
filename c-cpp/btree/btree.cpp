                                              /* Bill Mill and Bryan Brook
                                                 CS 371
                                                 due 11.19.03
                                                 Assignment #8
                                              */
/* Filename:  btree.cpp

   Modified from Br. David Carlson's code

   This file contains the implementation of the functions of the
   BTTableClass as set up in btree.h.
*/

#include "btree.h"


/* Given:  msg   A message.
   Task:   To print msg and exit the program.
   Return: Nothing to the program, of course, but returns 1 to the
           operating system.
*/
void Error(char * msg)
   {
   cerr << msg << endl;
   exit(1);
   }


/* Given:   Nothing (other than the implicit BTTableClass object)
   Task:    To print out all info associated with the current table.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Nothing.
*/
void BTTableClass::Dump(void)
   {
   int k;
   long p;

   cerr << endl << "Root is node (record) number " << Root << endl;

   for (p = 0; p <= NumNodes; p++)
      {
      /*if (p % 4 == 3)
         {
         cout << " Press ENTER";
         cin.get();
         }*/

      DataFile.seekg(p * NodeSize, ios::beg);
      DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      if (p == 0)
         {
         cerr << "Node 0 is not part of tree, contains this data:" << endl;
         cerr << "   NumItems = " << CurrentNode.Branch[0] << endl;
         cerr << "   NumNodes = " << CurrentNode.Branch[1] << endl;
         cerr << "   Root = " << CurrentNode.Branch[2] << endl;
         }
      else
         {
         cerr << "Dump of node number " << p << endl;
         cerr << "   Count: " << CurrentNode.Count << endl;

         cerr << "   Keys: ";
         for (k = 0; k < CurrentNode.Count; k++)
            cerr << CurrentNode.Key[k].KeyField << " ";

         cerr << endl << "   Branches: ";
         for (k = 0; k <= CurrentNode.Count; k++)
            cerr << CurrentNode.Branch[k] << " ";
         cerr << endl << endl;
         }
      }
   }


/* Given:   Nothing (other than the implicit BTTableClass object)
   Task:    To do an inorder traversal of the B-Tree looking for out of
            order items.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Nothing.
*/
void BTTableClass::Check(void)
   {
   KeyFieldType Last;

   Last[0] = '*';
   Last[1] = NULLCHAR;
   CheckSubtree(Root, Last);
   }


/* Given:   The implicit BTTableClass object plus:
            Current   A pseudopointer to the root node of the subtree.
            Last      The Last key field value that was checked.
   Task:    To do an inorder traversal of the subtree rooted at the
            current node.  Each key field value is checked against Last
            to see if it is out of order relative to Last.  If so,
            debugging info is printed, including a complete dump of
            the B-tree.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Last      Updated to hold the last key field value checked.
*/
void BTTableClass::CheckSubtree(long Current, KeyFieldType & Last)
   {
   NodeType Node;
   int k;

   if (Current == NilPtr)
      return;

   DataFile.seekg(Current * NodeSize, ios::beg);
   DataFile.read(reinterpret_cast <char *> (&Node), NodeSize);
   for (k = 0; k < Node.Count; k++)
      {
      CheckSubtree(Node.Branch[k], Last);
      if ((Last[0] != '*') && (strcmp(Last, Node.Key[k].KeyField) >= 0))
         {
         cout << "Check has found a problem in node " << Current <<
            " index " << k << " key " << Node.Key[k].KeyField << endl;
         Dump();
         exit(1);
         }
      strcpy(Last, Node.Key[k].KeyField);
      }
   CheckSubtree(Node.Branch[Node.Count], Last);
   }


/* Given:   Mode      A char(r or w) to indicate read or write mode.
            FileName  A char string holding the external filename.
   Task:    This is the constructor for a BTTableClass object.  If mode
            r is specified, it opens the table stored in the given file
            for reading.  If w is specified, it opens a new, empty table
            for writing (to the given file).  A new empty table contains
            a "dummy" node (node zero) that will be used to hold info
            about the whole table.
   Return:  Nothing directly, but the implicit object is created.
*/
BTTableClass::BTTableClass(char Mode, char * FileName)
   {
   #ifdef DEBUG
      cout << "BTTableClass constructor called" << endl;
   #endif

   OpenMode = Mode;
   NodeSize = sizeof(NodeType);

   if (Mode == 'r')
      {
      DataFile.open(FileName, ios::in | ios::binary);
      if (DataFile.fail())
         Error("File cannot be opened");

      DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);
      if (DataFile.fail())
         {   // assume the Btree is empty if you cannot read from the file
         NumItems = 0;
         NumNodes = 0;
         Root = NilPtr;
         }
      else   // Node zero is not a normal node, it contains the following:
         {
         NumItems = CurrentNode.Branch[0];
         NumNodes = CurrentNode.Branch[1];
         Root = CurrentNode.Branch[2];
         }
      #ifdef DEBUG
         //cout << "R";
      #endif
      }
   else if (Mode == 'w')
      {
      DataFile.open(FileName, ios::in | ios::out | ios:: trunc |
         ios::binary);
      if (DataFile.fail())
         Error(strcat("File cannot be opened", FileName));

      Root = NilPtr;
      NumItems = 0;
      NumNodes = 0;   // number does not include the special node zero
      CurrentNode.Branch[0] = NumItems;
      CurrentNode.Branch[1] = NumNodes;
      CurrentNode.Branch[2] = Root;
      DataFile.seekp(0, ios::beg);
      DataFile.write(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      #ifdef DEBUG
         //cout << "W";
      #endif
      }
   else
      Error("Incorrect mode given to BTTableClass constructor");
   }


/* Given:   Nothing (other than the implicit object).
   Task:    This is the destructor for a BTTableClass object.  Its job
            is to destroy the BTTableClass object, while making sure that
            all of the table data is stored in the associated file.
   Return:  Nothing directly, but the file is updated.
*/
BTTableClass::~BTTableClass(void)
   {
   #ifdef DEBUG
      cout << endl << "BTTableClass destructor called" << endl;
   #endif

   if (OpenMode == 'w')
      {
      //  Be sure to write out the updated node zero:
      CurrentNode.Branch[0] = NumItems;
      CurrentNode.Branch[1] = NumNodes;
      CurrentNode.Branch[2] = Root;
      DataFile.seekp(0, ios::beg);
      DataFile.write(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      #ifdef DEBUG
         //cout << "W";
      #endif
      }

   #ifdef DEBUG
      Dump();
   #endif

   DataFile.close();
   }


/* Given:   Nothing (other than the implicit object).
   Task:    To decide if the implicit table object is empty.
   Return:  In the function name, true if the table object is empty,
            false otherwise.
*/
bool BTTableClass::Empty(void) const
   {   // we could read node zero, but this is faster:
   return (Root == NilPtr);
   }


/* Given:   The implicit BTTableClass object as well as:
            Target        The value to look for in the CurrentNode field.
   Task:    To look for Target as a key in CurrentNode.
   Return:  In the function name, return true if found, false otherwise.
            Location      The index of where Target was found.  If not
                          found, index and index + 1 are the indices between
                          which Target would fit.  (If Target fits to the
                          left of the first key, returns index of -1.)
*/
bool BTTableClass::SearchNode(const KeyFieldType Target,
   int & Location) const
   {
   bool Found;

   Found = false;
   if (strcmp(Target, CurrentNode.Key[0].KeyField) < 0)
      Location = -1;
   else
      { // do a sequential search, right to left:
      Location = CurrentNode.Count - 1;
      while ((strcmp(Target, CurrentNode.Key[Location].KeyField) < 0)
         && (Location > 0))
         Location--;

      if (strcmp(Target, CurrentNode.Key[Location].KeyField) == 0)
         Found = true;
      }

   return Found;
   }


/* Given:   The implicit BTTableClass object as well as:
            NewItem       Item to add to Node.
            NewRight      Pseudopointer to right subtree below NewItem.
            Node          The node to be added to.
            Location      The index at which to add newItem.
   Task:    To add Item to Node at index Location, and add NewRight
            as the branch just to the right of NewItem.  The addition is
            made by moving the needed keys and branches right by 1 in order
            to clear out index Location for NewItem.
   Return:  Node          Updated node.
*/
void BTTableClass::AddItem(const ItemType & NewItem, long NewRight,
   NodeType & Node, int Location)
   {
   int j;

   for (j = Node.Count; j > Location; j--)
      {
      Node.Key[j] = Node.Key[j - 1];
      Node.Branch[j + 1] = Node.Branch[j];
      }

   Node.Key[Location] = NewItem;
   Node.Branch[Location + 1] = NewRight;
   Node.Count++;
   }


/* Given: The implicit BTTableClass object as well as:
          CurrentItem    Item whose attempted placement into a node is
                         causing the node to be split.
          CurrentRight   Pseudopointer to the child just to the right of
                         CurrentItem.
          CurrentRoot    Pseudopointer to the node to be split.
          Location       Index of where CurrentItem should go in this node.
  Task:   To split the node that CurrentRoot points to into 2 nodes,
          pointed to by CurrentRoot and NewRight.  CurrentItem is properly
          placed in 1 of these 2 nodes (unless it is the median that gets
          moved up to the parent).  Finds Newitem, the median item that is
          to be moved up to the parent node.
  Return: NewItem        The item to be moved up into the parent node.
          NewRight       The pseudopointer to the child to the right of
                         NewItem (i.e. a pointer to the new RightNode).
*/
void BTTableClass::Split(const ItemType & CurrentItem, long CurrentRight,
   long CurrentRoot, int Location, ItemType & NewItem, long & NewRight)
   {
   int j, Median;
   NodeType RightNode;

   #ifdef DEBUG
      //cout << "S";
   #endif

   if (Location < MinKeys)
      Median = MinKeys;
   else
      Median = MinKeys + 1;

   DataFile.seekg(CurrentRoot * NodeSize, ios::beg);
   DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);

   #ifdef DEBUG
      //cout << "R";
   #endif

   for (j = Median; j < MaxKeys; j++)
      {  // move half of the items to the RightNode
      RightNode.Key[j - Median] = CurrentNode.Key[j];
      RightNode.Branch[j - Median + 1] = CurrentNode.Branch[j + 1];
      }

   RightNode.Count = MaxKeys - Median;
   CurrentNode.Count = Median;   // is then incremented by AddItem

   // put CurrentItem in place
   if (Location < MinKeys)
      AddItem(CurrentItem, CurrentRight, CurrentNode, Location + 1);
   else
      AddItem(CurrentItem, CurrentRight, RightNode,
         Location - Median + 1);

   NewItem = CurrentNode.Key[CurrentNode.Count - 1];
   RightNode.Branch[0] = CurrentNode.Branch[CurrentNode.Count];
   CurrentNode.Count--;

   DataFile.seekp(CurrentRoot * NodeSize, ios::beg);
   DataFile.write(reinterpret_cast <char *> (&CurrentNode), NodeSize);

   #ifdef DEBUG
      //cout << "W";
   #endif

   NumNodes++;
   NewRight = NumNodes;
   DataFile.seekp(NewRight * NodeSize, ios::beg);
   DataFile.write(reinterpret_cast <char *> (&RightNode), NodeSize);

   #ifdef DEBUG
      //cout << "W";
   #endif
   }


/* Given:  The implicit BTTableClass object as well as:
           CurrentItem   The item to be inserted into the B-tree table.
           CurrentRoot   Pseudopointer to root of current subtree.
   Task:   To find where to put CurrentItem in a node of the subtree with
           the given root.  CurrentItem is ordinarily inserted, though
           a duplicate item is refused.  It is also possible that
           CurrentItem might be the item moved up to be inserted into
           the parent node if a split is done.
   Return: MoveUp        True if NewItem (and associated NewRight pointer)
                         must be placed in the parent node due to
                         splitting, false otherwise.
           NewItem       Item to be placed into parent node if a split was
                         done.
           NewRight      Pseudopointer to child to the right of NewItem.
*/
void BTTableClass::PushDown(const ItemType & CurrentItem, long CurrentRoot,
   bool & MoveUp, ItemType & NewItem, long & NewRight)
   {
   int Location;

   #ifdef DEBUG
      //cout << "P";
   #endif

   if (CurrentRoot == NilPtr)   // stopping case
      {   // cannot insert into empty tree
      MoveUp = true;
      NewItem = CurrentItem;
      NewRight = NilPtr;
      }
   else   // recursive case
      {
      DataFile.seekg(CurrentRoot * NodeSize, ios::beg);
      DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      #ifdef DEBUG
         //cout << "R";
      #endif

      if (SearchNode(CurrentItem.KeyField, Location))
         Error("Error: attempt to put a duplicate into B-tree");

      PushDown(CurrentItem, CurrentNode.Branch[Location + 1], MoveUp,
         NewItem, NewRight);

      if (MoveUp)
         {
         DataFile.seekg(CurrentRoot * NodeSize, ios::beg);
         DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);

         #ifdef DEBUG
            //cout << "R";
         #endif

         if (CurrentNode.Count < MaxKeys)
            {
            MoveUp = false;
            AddItem(NewItem, NewRight, CurrentNode, Location + 1);
            DataFile.seekp(CurrentRoot * NodeSize, ios::beg);
            DataFile.write(reinterpret_cast <char *> (&CurrentNode),
               NodeSize);

            #ifdef DEBUG
               //cout << "W";
            #endif
            }
         else
            {
            MoveUp = true;
            Split(NewItem, NewRight, CurrentRoot, Location,
               NewItem, NewRight);
            }
         }
      }
   }

void BTTableClass::print_node(NodeType &node) {
   int k;
   cerr << "Count: "<< node.Count << endl;
   cerr << "   Keys: ";
   for (k = 0; k < node.Count; k++)
      cerr << node.Key[k].KeyField << " ";

   cerr << endl << "   Branches: ";
   for (k = 0; k <= node.Count; k++)
      cerr << node.Branch[k] << " ";
   cerr << endl << endl;
}

/* Given:   loc         which Branch to load next
            node        The node from which to take brach loc
   Task:    will find the logicallly next element in the tree; that is, the
            element that is next in line after some element x.
   Return:  leaves CurrentNode.Key[0] pointing at the logically next node
*/
void BTTableClass::find_logical_next(int loc, NodeType &node, long &node_num) {
   long CurrentRoot = node.Branch[++loc];
   cerr <<"loc:"<< loc <<" CR: "<< CurrentRoot<<endl;
   if(CurrentRoot == NilPtr) return;  //exit condition
   node_num = CurrentRoot;
  
   read_node(CurrentRoot, CurrentNode); 
   
   find_logical_next(-1, CurrentNode, node_num); //recurse the left subtree
}

/* Given:   A node and the location from which to shift
   Task:    To shift the nodes right, balancing them, and write them to
            the database
   Return:  True if successful
*/
bool BTTableClass::shift_right(NodeType &node, int loc) {
   print_node(node);
   return false;
}

/* Given:   A String representing the KeyField of the ItemType of the
            key to be deleted
   Task:    To delete said key
   Return:  True if successful
*/
bool BTTableClass::Delete(KeyFieldType SearchKey) {
   ItemType Item;
   NodeType node_found, parent, neighbor; //n stands for "neighbor"
   long node_num;
   long cur_node_num;
   int loc, parent_loc;

   cerr<<"================DELETE "<<SearchKey<<"===================\n";

   //return false if there is no tree, can't delete anything
   if(Root == NilPtr) { cout << "Root is nil\n"; return false; }
   loc = RetrieveNode(SearchKey, node_found, node_num);
   if(loc > -2) {
      cerr << "found Search Key..." << SearchKey << "in node: " 
            << node_num << "\n";
      print_node(node_found);

      //case 1: we have a leaf with > minimum nodes; just delete the node
      if(node_found.Branch[0] == NilPtr && node_found.Count > MinKeys) {
         cout << "got into case 1\n";
         node_found.Count--;
         //move every entry one to the left
         for(int i=loc; i < node_found.Count; i++) {
            node_found.Key[i] = node_found.Key[i+1];
         }
         //commit changes to the data file
         write_node(node_num, node_found);
         return true;
      }
      //case 2: we have a leaf with == min nodes; try swapping with neighbors,
      //else condense with parent
      else if (node_found.Branch[0] == NilPtr) {
         /*cout << "got into case 2\n";
         RetrieveParent(SearchKey, parent, parent_loc);
         if(parent_loc > 0) {                    //try swapping with neighbors
            read_node(parent.Branch[parent_loc], neighbor); //read left neighbor
            if(neighbor.Count > MinKeys + 1)
               shift_right(parent, parent_loc);
            read_node(parent.Branch[parent_loc+1], neighbor);
            if(neighbor.Count > MinKeys + 1)
               shift_left(parent, parent_loc);
         }*/
         return false;
      }
      //case 3: we don't have a leaf. Swap it with the smallest number greater
      //than it, then delete it from the leaf.
      else { 
         cout << "got into case 3\n";
         find_logical_next(loc, node_found, cur_node_num);
         
         Item = node_found.Key[0];                    //swap them
         node_found.Key[0] = CurrentNode.Key[0];
         CurrentNode.Key[0] = Item;
         write_node(cur_node_num, CurrentNode);       //write the swapped nodes
         write_node(node_num, node_found);            //to the file
         
         //now delete the switched leaf. Assumes Count > MinKeys + 1, because
         //I haven't yet implemented FixUp, ShiftRight, or ShiftLeft.
         CurrentNode.Count--;
         //move every entry one to the left
         for(int i=0; i < CurrentNode.Count; i++) {
            CurrentNode.Key[i] = CurrentNode.Key[i+1];
         }
      }
   }
   else { cout << "Search Key unfoundeded\n"; return false; }
} 

void BTTableClass::write_node(long num, NodeType node) {
   DataFile.seekp(num*NodeSize, ios::beg);
   DataFile.write(reinterpret_cast <char *> (&node), NodeSize);
}

void BTTableClass::read_node(long num, NodeType &node) {
    DataFile.seekg(num*NodeSize, ios::beg);
    DataFile.read(reinterpret_cast <char *> (&node), NodeSize);
}

/* Given:   The implicit BTTableClass object as well as:
            Item       Item to add to the table.
   Task:    To add Item to the table.
   Return:  In the function name, returns true to indicate success.
            (The implicit object is modified, of course.)
*/
bool BTTableClass::Insert(const ItemType & Item)
   {
   bool MoveUp;
   long NewRight;
   ItemType NewItem;

   #ifdef DEBUG
      //cout << "I";
   #endif

   PushDown(Item, Root, MoveUp, NewItem, NewRight);

   if (MoveUp)   // create a new root node
      {
      CurrentNode.Count = 1;
      CurrentNode.Key[0] = NewItem;
      CurrentNode.Branch[0] = Root;
      CurrentNode.Branch[1] = NewRight;
      NumNodes++;
      Root = NumNodes;
      DataFile.seekp(NumNodes * NodeSize, ios::beg);
      DataFile.write(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      #ifdef DEBUG
         //cout << "W";
      #endif
      }

   NumItems++;   // fixed 12/21/2001
   return true;   // no reason not to assume success
   }


/* Given:   The implicit BTTableClass object as well as:
            SearchKey   Key value to look for in the table.
   Task:    To look for SearchKey in the table.
   Return:  In the function name, true if SearchKey was found,
            false otherwise.
            Item        The item were SearchKey was found.
*/
bool BTTableClass::Retrieve(KeyFieldType SearchKey, ItemType & Item)
   {
   long CurrentRoot;
   int Location;
   bool Found;

   Found = false;
   CurrentRoot = Root;

   while ((CurrentRoot != NilPtr) && (! Found))
      {
      DataFile.seekg(CurrentRoot * NodeSize, ios::beg);
      DataFile.read(reinterpret_cast <char *> (&CurrentNode), NodeSize);

      #ifdef DEBUG
         //cout << "R";
      #endif

      if (SearchNode(SearchKey, Location))
         {
         Found = true;
         Item = CurrentNode.Key[Location];
         }
      else
         CurrentRoot = CurrentNode.Branch[Location + 1];
      }

   return Found;
   }

/* Given: A SearchKey to look for
   Task:  Return the node containing its parent
   Return: node = a pointer to the parent, unless node is root, in which case
           parent_loc = -1
*/
void BTTableClass::RetrieveParent(KeyFieldType SearchKey, NodeType &node, 
                                int &parent_loc) {
   long cur_root = Root;
   int loc;
   bool found;
   parent_loc = -1;

   while ((cur_root != NilPtr)) {
      read_node(cur_root, CurrentNode);
      
      if (SearchNode(SearchKey, loc))
         return;

      parent_loc = loc;                //parent's location
      read_node(cur_root, node);       //read parent into node
      cur_root = CurrentNode.Branch[loc + 1];
   }
}

/* Given: A SearchKey to look for
   Task:  Find the node containing that SearchKey
   Return: A pointer to the node and the index of the item containing
           the SearchKey. Returns -2 if not found.
*/
int BTTableClass::RetrieveNode(KeyFieldType SearchKey, NodeType &NodeFound,
                               long &node_num)
   {
   long CurrentRoot;
   int Location;
   bool Found;

   Found = false;
   CurrentRoot = Root;

   while ((CurrentRoot != NilPtr))
      {
      read_node(CurrentRoot, CurrentNode);

      if (SearchNode(SearchKey, Location))
         {
         node_num = CurrentRoot;
         NodeFound = CurrentNode;
         return Location;
         }
      else
         CurrentRoot = CurrentNode.Branch[Location + 1];
      }

   //node not found
   return -2;
   }


