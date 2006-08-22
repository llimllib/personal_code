/* Filename:  btread.cpp

   Programmer:  Br. David Carlson

   Date:  November 4, 1997

   Last Modified:  December 21, 2001 to use the NULLCHAR constant.

   This is a program to read a B-tree-based table (in btree.dat file) - as
   produced by the btmake program.  This program prompts the user to
   enter a word and then displays the associated definition, if a
   match was found.  The user can do repeated lookups of this sort, until
   a . is entered to quit.
 
   In order to compile this program under Visual C++, you will need to have
   the following files listed in the project:
   btree.cpp   itemtype.h   btread.cpp   btree.h     table.h

   Tested with:
      Microsoft Visual C++ 6.0
      Microsoft Visual C++ .NET
      g++ under Linux
*/

#include <cctype>
#include "btree.h"


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

   if (Word[0] == '.')
      return false;

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


int main(void)
   {
   ItemType Item;
   KeyFieldType SearchKey;
   BTTableClass BTTable('r', "btree.dat");

   if (BTTable.Empty())
      Error("Table is empty");

   cout << "Enter the word to be looked up (or . to quit): ";

   while (ReadKeyboard(SearchKey))
      {
      if (BTTable.Retrieve(SearchKey, Item))
         cout << " Definition:   " << Item.DataField << endl;
      else
         cout << " Not found" << endl;
      cout << endl << "Enter the word to be looked up (or . to quit): ";
      }

   return 0;
   }


