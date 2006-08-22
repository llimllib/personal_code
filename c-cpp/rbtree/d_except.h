#ifndef EXCEPTION_CLASSES
#define EXCEPTION_CLASSES

#include <strstream>
#include <string>

using namespace std;

class baseException
{
	public:
		baseException(const string& str = ""):
			msgString(str)
		{
			if (msgString == "")
				msgString = "Unspecified exception";
		}

		string what() const
		{
			return msgString;
		}

	// protected allows a derived class to access msgString.
	// chapter 13 discusses protected in detail
	protected:
		string msgString;
};

// failure to allocate memory (new() returns NULL)
class memoryAllocationError: public baseException
{
	public:
		memoryAllocationError(const string& msg = ""):
			baseException(msg)
		{}
};

// function argument out of proper range
class rangeError: public baseException
{
	public:
		rangeError(const string& msg = ""):
			baseException(msg)
		{}
};

// index out of range
class indexRangeError: public baseException
{
	public:
		indexRangeError(const string& msg, int i, int size):
			baseException()
		{
			char indexString[80];
			ostrstream indexErr(indexString, 80);

			indexErr << msg << "  index " << i << "  size = " << size << ends;
			// indexRangeError can modify msgString, since it is in
			// the protected section of baseException
			msgString = indexString;
		}
};

// attempt to erase from an empty container
class underflowError: public baseException
{
	public:
		underflowError(const string& msg = ""):
			baseException(msg)
		{}
};

// attempt to insert into a full container
class overflowError: public baseException
{
	public:
		overflowError(const string& msg = ""):
			baseException(msg)
		{}
};

// error in expression evaluation
class expressionError: public baseException
{
	public:
		expressionError(const string& msg = ""):
			baseException(msg)
		{}
};

// bad object reference
class referenceError: public baseException
{
	public:
		referenceError(const string& msg = ""):
			baseException(msg)
		{}
};

// feature not implemented
class notImplementedError: public baseException
{
	public:
		notImplementedError(const string& msg = ""):
			baseException(msg)
		{}
};

// date errors
class dateError: public baseException
{
	public:
		dateError(const string& first, int v, const string& last):
			baseException()
		{
			char dateStr[80];
			ostrstream dateErr(dateStr, 80);

			dateErr << first << ' ' << v << ' ' << last << ends;
			// dateError can modify msgString, since it is in
			// the protected section of baseException
			msgString = dateStr;
		}
};

// error in graph class
class graphError: public baseException
{
	public:
		graphError(const string& msg = ""):
			baseException(msg)
		{}
};

// file open error
class fileOpenError: public baseException
{
	public:
		fileOpenError(const string& fname):
			baseException()
		{
			char errorStr[80];
			ostrstream fileErr(errorStr, 80);

			fileErr << "Cannot open \"" << fname << "\"" << ends;
			// fileOpenError can modify msgString, since it is in
			// the protected section of baseException
			msgString = errorStr;
		}
};

// error in graph class
class fileError: public baseException
{
	public:
		fileError(const string& msg = ""):
			baseException(msg)
		{}
};

#endif	// EXCEPTION_CLASSES
