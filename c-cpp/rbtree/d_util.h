#ifndef UTILITY_FUNCTIONS
#define UTILITY_FUNCTIONS

#include <iostream>
#include <vector>
#include <string>
#include <list>
#include <map>

#include "d_stree.h"		// for the stree class

using namespace std;

#ifdef _MSC_VER
// temporary fix for Microsoft VC++ 6.0 bug
istream& getline(istream& is, string& sbuff, char term_char = '\n');
#endif // _MSC_VER

// template function to output an n element array of type T
template <typename T>
void writeArray(const T arr[], int n);

// output the elements of v
template <typename T>
void writeVector(const vector<T>& v);

// display the list. follow the output of each list element
// by separator. default value of separator = "  "
template <typename T>
void writeList(const list<T>& alist, const string& separator);

// display the search tree. follow the output of each list element
// by separator. default value of separator = "  "
template <typename T>
void writeSTree(const stree<T>& t, const string& separator);

// display the elements of a container in the iterator
// range [first, last). output separator between items.
// default value of separator = "  "
template <typename Iterator>
void writeContainer(Iterator first, Iterator last, const string& separator);

// display the key-value pairs in the map. follow the output of
// each pair by separator. default value of separator = "  "
template <typename Key, typename T>
void writeMap(const map<Key,T>& m, const string& separator);

// return an iterator pointing to the maximum container
// value in the iterator range [first, last)
template <typename Iterator>
Iterator maxElement(Iterator first, Iterator last);

// class used to implement one-argument I/O manipulators
template<typename T>
class omanip1
{
   public:
      // constructor. initialize the private data members
      omanip1(ostream& (*f)(ostream & ostr, const T& x), const T& w):
         func(f), parm(w)
      {}

		// evaluate f.func with argument f.parm.
		// this function returns an ostream&
		friend ostream& operator<< (ostream& ostr, const omanip1<T>& f)
		{
			return(*f.func)(ostr, f.parm);
		}
   private:
      // pointer to the function that does the work
      ostream& (*func)(ostream& ostr, const T& x);
      // the two manipulator parameters stored in the class
      T parm;
};

// class used to implement two-argument I/O manipulators
template<typename T>
class omanip2
{
   public:
      // constructor. initialize the private data members
      omanip2(ostream& (*f)(ostream & ostr, const T& x, const T& y),
              const T& w, const T& p ) : func(f), parm1(w), parm2(p)
      {}

		// evaluate f.func with parameters f.parm1 and f.parm2.
		// this function returns an ostream&
		friend ostream& operator<< (ostream& ostr, const omanip2<T>& f)
		{
			return(*f.func)(ostr, f.parm1, f.parm2);
		}
   private:
      // pointer to the function that does the work
      ostream& (*func)(ostream& ostr, const T& x, const T& y);
      // the two manipulator parameters stored in the class
      T parm1;
      T parm2;
};

// change ostr to fixed mode precision with p decimal places.
// output the next stream data item in a field of w positions
ostream& sr(ostream &ostr, const int& w, const int& p);

// manipulator that sets field width and precision for fixed
// mode output
omanip2<int> setreal(int w, int p);

#ifdef _MSC_VER
istream& getline(istream& is,string& sbuff, char term_char)
{
	char tc, eof = char_traits<char>::eof();
	string::size_type numchars = 0;
	sbuff.erase();


	while(true)
	{
		tc = is.get();
		if(tc == eof) // eof - set failbit and quit
		{
			is.setstate(ios::failbit);
			break;
		}
		else if(tc == term_char) // termination char extracted
			break;

		sbuff.append(1,tc);
		numchars++;

		if(numchars == sbuff.max_size()) // max chars extracted
				break;
	}

	return is;
}
#endif // _MSC_VER

template <typename T>
void writeArray(const T arr[], int n)
{
	int i;

	for(i=0;i < n;i++)
		cout << arr[i] << "  ";
	cout << endl;
}

template <typename T>
void writeVector(const vector<T>& v)
{
	// capture the size of the vector in n
	int i, n = v.size();

	for(i = 0; i < n; i++)
		cout << v[i] << "  ";
	cout << endl;
}

template <typename T>
void writeList(const list<T>& alist, const string& separator = "  ")
{
	list<T>::const_iterator  iter;

	for (iter = alist.begin(); iter != alist.end(); iter++)
		cout << *iter << separator;
	cout << endl;
}

template <typename T>
void writeSTree(const stree<T>& t, const string& separator = "  ")
{
	stree<T>::const_iterator iter = t.begin();

	while (iter != t.end())
	{
		cout << *iter << separator;
		iter++;
	}
}

template <typename Iterator>
void writeContainer(Iterator first, Iterator last,
						  const string& separator = "  ")
{
	// declare iterator of type Iterator and initialize it
	// to have value first
	Iterator iter = first;

	while (iter != last)
	{
		cout << *iter << separator;
		iter++;
	}
}

template <typename Key, typename T>
void writeMap(const map<Key,T>& m, const string& separator = "  ")
{
	map<Key, T>::const_iterator iter = m.begin();

	while(iter != m.end())
	{
		cout << (*iter).first << "  " << (*iter).second << separator;
		iter++;
	}
}

template <typename Iterator>
Iterator maxElement(Iterator first, Iterator last)
{
	// initially, assume first points to largest element
	Iterator iter = first, maxIter = first;

	// move past first element
	iter++;

	// scan range, comparing values *iter and *maxIter
	// until reach the end of the range. change maxIter
	// whenever *maxIter < *iter
	while (iter != last)
	{
		// if iter points to a new maximum element, it becomes
		// maxIter
		if (*maxIter < *iter)
			maxIter = iter;
		iter++;
	}

	// return pointer to the largest element
	return maxIter;
}

ostream& sr(ostream& ostr, const int& w, const int& p)
{
   ostr.setf(ios::fixed, ios::floatfield);
   ostr.precision(p);
   ostr.width(w);

   return ostr;
}

// manipulator with field width and precision arguments
// using sr
omanip2<int> setreal(int w, int p)
{
   return omanip2<int> (sr,w,p);
}

#endif	// UTILITY_FUNCTIONS
