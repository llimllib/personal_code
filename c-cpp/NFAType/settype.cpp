#include "settype.h" // class's header file

// class constructor
SetType::SetType(char set[])
{
    s = set;
    setSize = strlen(set);
    setSize == 0 ? empty = true : empty = false;
}

void SetType::write()
{
    if(setSize > 0) {
        cout << s[0];
        for(int i = 1; i < setSize; i++) {
            cout << ',' << s[i];
        }
        cout << endl;
    }
}

bool SetType::is_empty()
{
    if(setSize == 0) {
        return true;
    }
    return false;
}

bool SetType::is_equal(char* set)
{
    if(strlen(set) != strlen(s)) {
      return false;
    }
    for(int i = 0; i < strlen(set); i++) {
        if(!strchr(s, set[i])) {
          return false;
        }
    }
    return true;
}

bool SetType::is_member(char aChar)
{
    if(strchr(s, aChar)) {
        return true;
    }
    return false;
}

bool SetType::is_subset(char* set)
{
    for(int i = 0; i < strlen(set); i++) {
        if(!strchr(s, set[i]) && set[i] != 'e') {
            return false;
        }
    }
    return true;
}

bool SetType::is_subset(SetType* set)
{
  for(int i = 0; i < set->getSize(); i++) {
    if(!is_member(set->getCharFromSet(i))) {
      return false;
    }
  }
  return true;
}

char SetType::getCharFromSet(int index)
{
  if(index < setSize) {
    return s[index];
  }
  return ' ';
}

void SetType::setunion(char set[], char aUnion[])
{
    int unionCount = 0;
    for(int i = 0; i < setSize; i++) {
        aUnion[unionCount++] = s[i];
    }
    for(int i = 0; i < strlen(set); i++) {
        if(!strchr(aUnion, set[i])) {
            aUnion[unionCount++] = set[i];
        }
    }
    aUnion[unionCount] = '\0';
}

void SetType::intersection(char set[], char intersection[])
{
    int intersectionCount = 0;
    for(int i = 0; i < strlen(set); i++) {
        if(strchr(s, set[i])) {
            intersection[intersectionCount++] = set[i];
        }
    }
    intersection[intersectionCount] = '\0';
}

void SetType::difference(char set[], char diff[])
{
    int newSetCount = 0;
    for(int i = 0; i < strlen(set); i++) {
        if(!strchr(s, set[i])) {
            diff[newSetCount++] = set[i];
        }
    }
    for(int i = 0; i < setSize; i++) {
        if(!strchr(set, s[i])) {
            diff[newSetCount++] = s[i];
        }
    }
    diff[newSetCount] = '\0';
}

int SetType::getSize()
{
  return setSize;
}

// class destructor
SetType::~SetType()
{   
}

