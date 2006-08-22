                                              /* Bill Mill and Bryan Brook
                                                 CS 371
                                                 due 11.19.03
                                                 Assignment #8
                                              */

/* Filename:  btree.h

   Modified from Br. David Carlson's code

   This is the header file to go with btree.cpp, an implementation of
   a B-tree-based table class.
*/

#include "table.h"

// The following line is commented off to omit debugging output.
//#define DEBUG
// Remove the // at the beginning of that line and recompile if you want
// to get extensive debugging output.


const int MaxKeys = 11;   // max number of keys in a node

const int MaxKeysPlusOne = MaxKeys + 1;

const int MinKeys = 5;    // min number of keys in a node

const long NilPtr = -1L;   // the L indicates a long int


typedef struct
   {
   int Count;               // Number of keys stored in the current node
   ItemType Key[MaxKeys];   // Warning: indexing starts at 0, not 1
   long Branch[MaxKeysPlusOne];   // Fake pointers to child nodes
   } NodeType;


void Error(char * msg);


class BTTableClass: public TableBaseClass
   {
   public:
      BTTableClass(char Mode, char * FileName);
      ~BTTableClass(void);
      bool Empty(void) const;
      bool Insert(const ItemType & Item);
      bool Delete(KeyFieldType SearchKey);
      bool Retrieve(KeyFieldType SearchKey, ItemType & Item);
      void Dump(void);   // for debugging only - could be removed
      void Check(void);   // for debugging only
      void print_node(NodeType &node); // for debugging
      int  RetrieveNode(KeyFieldType SearchKey, NodeType &NodeFound, long &node_num);
   private:
      bool shift_right(NodeType &node, int loc);
      void RetrieveParent(KeyFieldType SearchKey, NodeType &node, 
            int &parent_loc);
      void read_node(long num, NodeType &node);
      void write_node(long num, NodeType node);
      void find_logical_next(int loc, NodeType &node, long &node_num);
      void CheckSubtree(long Current, KeyFieldType & Last); // for debugging
      bool SearchNode(const KeyFieldType Target, int & location) const;
      void AddItem(const ItemType & NewItem, long NewRight,
         NodeType & Node, int Location);
      void Split(const ItemType & CurrentItem, long CurrentRight,
         long CurrentRoot, int Location, ItemType & NewItem,
         long & NewRight);
      void PushDown(const ItemType & CurrentItem, long CurrentRoot,
         bool & MoveUp, ItemType & NewItem, long & NewRight);
      long Root;       // fake pointer to the root node
      long NumNodes;   // number of nodes in the B-tree
      int NodeSize;    // number of bytes per node
      NodeType CurrentNode;   // storage for current node being worked on
   };


