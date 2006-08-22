#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#include <string>
#include "fsatype.h"
#include "settype.h"

const int LEN = 32;

int main(int argc, char *argv[])
{
  ifstream fin;
  ofstream fout;
  string word, test_strings[LEN], states, alphabet, transition, start, accept;
  int i, token, token2, string_count;
  FSAType* f;
  SetType* stateSet;
  SetType* alphaSet;
  SetType* acceptSet;
  char* c_string;
  
  fin.open("fsadata.dat");
  fout.open("results.dat");
  
  /* get the test strings */
  fin >> word >> word;
  for(i=0; word.compare("[transition-functions]"); i++) {
    test_strings[i] = word;
    fin >> word;
  }
  
  string_count = i;
  
  /* iterate through the examples. The example format is:
      <states>|<alphabet>|<transition>|<start>|<accept>
      and examples are assumed to be seperated by newlines.
      The section containing examples should be titled
      "[transition-functions]", with no surrounding spaces.*/
  fin >> word;
  while(fin) {
    cout << word << endl;
    
    /* get the states of the example */
    token = word.find('|',0);
    states = word.substr(0, token);
    c_string = (char*)states.c_str(); /* the cast is necessary because c_str()
                                          returns a "const char" type */
    stateSet = new SetType(c_string);
    cout << "states: " << states << endl;
    
    /* get the alphabet */
    token++;
    token2 = word.find('|',token);
    alphabet = word.substr(token, token2 - token);
    c_string = (char*)alphabet.c_str();
    alphaSet = new SetType(c_string);
    cout << "alphabet: " << alphabet << endl;
    
    /* get the transition function */
    token = ++token2;
    token2 = word.find('|',token);
    transition = word.substr(token, token2 - token);
    cout << "transition: " << transition << endl;
    
    /* get the start state */
    token = ++token2;
    token2 = word.find('|', token);
    start = word.substr(token, token2 - token);
    cout << "start: " << start[0] << endl;
    
    /* get the set of accept states */
    token = ++token2;
    accept = word.substr(token, word.length());
    c_string = (char*)accept.c_str();
    acceptSet = new SetType(c_string);
    cout << "accept: " << accept << endl;
    
    /* and put it all together in a machine */
    f = new FSAType(stateSet, alphaSet, transition.c_str(), 
                    start[0], acceptSet);
    
    /* for every test string, print the results to a file */
    for (i = 0; i < string_count; i++) {
      if(f->is_accepted(test_strings[i].c_str())) {
        fout << test_strings[i] << " is accepted" << endl;
      }
      else {
        fout << test_strings[i] << " is not accepted" << endl;
      }
    }
    
    fin >> word;
  }
  /* display "press any key to continue" */
  system("PAUSE");
}
