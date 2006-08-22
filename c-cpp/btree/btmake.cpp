                                              /* Bill Mill and Bryan Brook
                                                 CS 371
                                                 due 11.19.03
                                                 Assignment #8
                                              */

/* Modified from Br. David Carlson's code

   Filename:  btmake.cpp

   Date:  November 19 2003

   This file is not complete. What I have working is deletion in both the case
   where the search key is in a leaf with > minimum number of elements
   and the case where the node containing the smallest item greater than an 
   arbitrary element has > minimun number of elements.

   The other cases, of a leaf with == min num of elts, or an arbitrary elt
   where the node containing the smallest item greater than the elt has
   == min num of elts, is not yet implemented. To do so, I would need to
   implement the ShiftRight, ShiftLeft, and FixUp functions. However, I have
   already spent 20 hours on this program, and I estimate that writing and
   debugging those 3 functions would cost me, at the very minimum, 5 more
   hours. I do not have this time.

   I would also like to point out that the author's code is terrifically
   difficult to understand, and generally poor. I estimate that gaining 
   a fairly complete understanding of his code cost me 10 hours of my 20. 
   The author refuses to use real pointers, formats his code in a nonstandard
   way, confusingly names functions, does not seperate file reading and writing
   into functions (!), and assumes that the file's size will never have to
   decrease.

   This is a test program to create a table (B-tree-based) in a file.  It
   reads dictionary data from the source text file btree.txt.  This file
   contains on each line a word (in upper case, up to 12 characters) and
   starting in column 13 the definition (up to 36 characters).  The
   B-tree-based table will be stored in the file btree.dat.

   To compile this program under Visual C++, you will need to have the
   following files listed in the project:
   btmake.cpp   btree.cpp   itemtype.h   btree.h     table.h

   Use the associated btread program to look up data stored in this table.

   Tested with:
      Microsoft Visual C++ 6.0
      Microsoft Visual C++ .NET
      g++ under Linux
*/
#include "btree.h"
#include <string>

const int LineMax = KeyFieldMax + DFMaxPlus1;

typedef char StringType[LineMax];


/* Given:  A KeyFieldType
   Task:   To read in one word from the keyboard (or . to signal desire
           to quit).
   Return: Word   In char array form, the word read in, capitalized and
                  padded on the right with blanks so that it contains 12
                  characters.
           In the function name, return true if a word was read in, false
           if a . was read in.
*/

bool ReadKeyboard(KeyFieldType Word)
   {
   int k, ch;
   bool start;

   cin >> Word;
   cin.get();   // get the newline

   start = true;

   for (k = 0; k < KeyFieldMax; k++)
      {
      ch = Word[k];
      if (ch == '\0')
         start = false;
      if (start)
         Word[k] = toupper(ch);   // capitalize
      else
         Word[k] = ' ';   // pad with blanks
      }

   Word[KeyFieldMax] = NULLCHAR;
   return true;
   }



/* Given:  InputFile   A file stream already opened for input on a text file.
   Task:   To read in a Word and its Definition from one line of this file.
   Return: Word        In char array form, the word read in.
           Definition  In char array form, the definition read in.
*/
void ReadLine(fstream & InputFile, KeyFieldType Word,
   DataFieldType Definition)
   {
   char Line[LineMax];
   int k, ch;

   InputFile.getline(Line, LineMax);   // will read the newline char

   for (k = 0; k < KeyFieldMax; k++)
      Word[k] = Line[k];
   Word[KeyFieldMax] = NULLCHAR;

   for (k = 0; k < DataFieldMax; k++)
      {
      ch = Line[KeyFieldMax + k];
      if (ch == '\n')
         break;
      Definition[k] = ch;
      }
   Definition[k] = NULLCHAR;
  }


/* Given:  InputFile   A file stream already opened for input.
   Task:   To read the data from InputFile and load it into the Table.
   Return: Table       The B-tree table containing the data.
*/
void Load(fstream & InputFile, BTTableClass & Table)
   {
   ItemType Item;

   ReadLine(InputFile, Item.KeyField, Item.DataField);

   while (! InputFile.fail())
      {
      #ifdef DEBUG
         if (Count == 22)
            {
            Count = 0;
            cout << endl << "Press ENTER";
            cin.get();
            }
         cout << endl << "DEBUG: ready to insert " << Item.KeyField << " ";
      #endif

      Table.Insert(Item);

      #ifdef DEBUG
         Table.Check();
      #endif

      ReadLine(InputFile, Item.KeyField, Item.DataField);
      }
   }


int main(void)
   {
   KeyFieldType word;
   ItemType item;
   fstream Source;
   BTTableClass BTTable('w', "btree.dat");

   Source.open("btree.txt", ios::in);
   if (Source.fail())
      {
      cerr << "ERROR: Unable to open file btree.txt" << endl;
      exit(1);
      }

   cout << "reading data file...\n"; 
   Load(Source, BTTable);
   Source.close();

   cout << "Enter 5 words:\n";
   for(int i = 0; i < 5; i++) {
      cout << "Word " << i + 1 << ":";
      if(!ReadKeyboard(word)) { cerr << "error"; return 0; }
      strcpy(item.KeyField, word);
      strcpy(item.DataField, "test word");
      BTTable.Insert(item);
   }

   BTTable.Dump();

   cout << "Enter 10 words to be looked up:\n";
   KeyFieldType key;
   NodeType node;
   long node_n;
   int loc;
   for(int i = 0; i < 10; i++) {
      cout << "Enter a word to be looked up\n";
      if(!ReadKeyboard(word)) { cerr << "error"; return 0; }
      cout << "\""<<word<<"\"\n\"A           \"\n";
      loc = BTTable.RetrieveNode(word, node, node_n);
      if (loc > -2)
         cout << " Node #    " << node_n << endl;
      else
         cout << " Not Found \""<<word<<"\"\n";
   }

   BTTable.Dump();
   
   for(int i=0; i < 20; i++) {
      cout << "Enter a word to be deleted\n";
      if(!ReadKeyboard(word)) { cerr << "error"; return 0; }
      if (BTTable.Delete(word))
         cout << "word successfully deleted \n";
      else
         cout << "word not deleted\n";
   }

   BTTable.Dump();

   cout << "Enter 10 more words to be looked up:\n";
   for(int i = 0; i < 10; i++) {
      cout << "Enter a word to be looked up\n";
      if(!ReadKeyboard(word)) { cerr << "error"; return 0; }
      cout << "\""<<word<<"\"\n\"A           \"\n";
      loc = BTTable.RetrieveNode(word, node, node_n);
      if (loc > -2)
         cout << " Node #    " << node_n << endl;
      else
         cout << " Not Found \""<<word<<"\"\n";
   }

   BTTable.Dump();

   return 0;
}

