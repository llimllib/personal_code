/* Filename:  itemtype.h

   Programmer:  Br. David Carlson

   Date:  May 23, 1999                                                    

   Last Modified:  December 21, 2001 to set up the NULLCHAR constant.

   This header file sets up ItemType and associated items.
*/

#include <iostream>
#include <fstream>
using namespace std;


const int KeyFieldMax = 12;

const int KFMaxPlus1 = KeyFieldMax + 1;

const int DataFieldMax = 36;

const int DFMaxPlus1 = DataFieldMax + 1;

const int NULLCHAR = '\0';  // NULL character used to mark end of a string


typedef char KeyFieldType[KFMaxPlus1];

typedef char DataFieldType[DFMaxPlus1];

typedef struct
   {
   KeyFieldType KeyField;
   DataFieldType DataField;
   } ItemType;


