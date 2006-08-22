                                             /* Bill Mill and Bryan Brook
                                                CS 371
                                                due 10.13.03
                                                Assignment #6 */


// File: prg12_4.cpp
// the program creates a red-black tree whose elements come from
// the integer array arr. a call to writeContainer() from
// "d_util.h" displays the tree data in ascending order. the
// program calls the displayTree() function from the rbtree
// class to output the tree and then inserts the values 7 and
// 75 into the tree. after each insertion, displayTree() is
// called to display the tree structure. the program concludes
// by deleting 35 (tree root) and displaying the final tree
//
// originally from http://www.fordtopp.com/ftsoftds.exe , this
// file has been modified slightly from its original form by us.
// We have manually verified that all the insertions and deletions
// in this code are performed correctly. Also, it would be trivial
// to prove that, if this code (using ints) acts as expected in 
// all cases, any datatype with a correctly implemented < operator
// will perform correctly as well.
//
// The code is lightly documented, as the output statements 
// contained within make it very self-documenting.
//
//  By Bill Mill and Bryan Brook, 10.12.03
//  Written in MSVC 6

#include <iostream>

#include "d_rbtree.h"
#include "d_util.h"

using namespace std;

int main()
{
	// initial red-black tree data
	int arr[] = {23, 35, 55, 17, 8, 67};
	int arrSize = sizeof(arr)/sizeof(int);
	rbtree<int> t(arr, arr+arrSize);

	// display the elements in order
	cout << "Listing of elements: ";
	writeContainer(t.begin(), t.end());
	cout << endl << endl;

	// display the initial tree
	t.displayTree(3);
	cout << endl;

	// insert operations add 7 and 75
	t.insert(7);
	cout << "Insert 7" << endl;
	t.displayTree(3);
	cout << endl << endl;

	t.insert(75);
	cout << "Insert 75" << endl;
	t.displayTree(3);
	cout << endl << endl;

	//erase the root 35
	t.erase(35);
	cout << "Erase 35" << endl;
	t.displayTree(4);
	cout << endl;

	return 0;
}
