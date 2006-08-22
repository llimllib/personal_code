/* Filename:  table.h

   Programmer:  Br. David Carlson

   Date:  November 10, 1997
                                                                                
   Modified:  May 23, 1999  
   
   Modifed:  July 16, 2000 to use modern headers.

   Modified:  March 4, 2001 to make Empty a const functions.

   This header file sets up TableBaseClass as an abstract base class for
   tables that are stored in a file.
*/

#include <fstream>
#include "itemtype.h"


class TableBaseClass
   {
   public:
      virtual bool Empty(void) const = 0;
      virtual bool Insert(const ItemType & Item) = 0;
      virtual bool Retrieve(KeyFieldType SearchKey, ItemType & Item) = 0;
   protected:
      fstream DataFile;   // the file stream for the table data
      long NumItems;      // number of records of type ItemType in the table
      char OpenMode;      // r or w (read or write) mode for the table
   };


