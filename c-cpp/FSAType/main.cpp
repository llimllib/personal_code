#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include "fsatype.h"
#include "settype.h"

const int MAX_BUF_SIZE = 2048;

int main(int argc, char *argv[])
{
  char buf[MAX_BUF_SIZE];
  char temp[MAX_BUF_SIZE];
  char* ftransition;
  string test_strings[32];
  char* token;
  SetType* fstates;
  SetType* falphabet;
  char fstart_state;
  SetType* faccept_states;
  FSAType* f;
  
  int test_strings_counter = -1;
  int section_num = 0;
  FILE* fp;
  FILE* fout;

  /* Primary Testing Procedures */
  SetType states("xyz");
  SetType alphabet("01");
  char trans[50] = "x-0,y,1,x;y-0,z,1,z;z-0,y,1,x";
  SetType accept("y");
  FSAType a(&states,&alphabet,trans,'x',&accept);
  
  a.write();
  char state = a.nextState('y', '0');
  cout << "Where to go from y on a 0: " << state << endl;
  a.is_accepted("10") ? cout << "10 accepted\n" : 
                        cout << "10 not accepted\n";
  a.is_accepted("100") ? cout << "100 accepted\n" :
                         cout << "100 not accepted\n";

  
  /* Thorough Testing Procedures */
  fp = fopen("fsadata.dat", "rt");
  if(fp == NULL) {
    cout << "\n*error opening file*\n";
    system("PAUSE");
    return 0;
  }
  fout = fopen("results.dat", "wt");
  if(fp == NULL) {
    cout << "\n*error opening file*\n";
    system("PAUSE");
    return 0;
  }
  
  while(!feof(fp)) {
    fgets(buf, sizeof(buf), fp);
    /* strip \n */
    buf[strlen(buf)-1] = '\0';
    if(buf[0] == '[') {
      /* section delimiter */
      section_num++;
    }
    else {   
      if(section_num == 1) {
        test_strings_counter++;
        test_strings[test_strings_counter] = (string)buf;
        /*cout << test_strings[test_strings_counter] << "<" << buf << ">"
              << test_strings[test_strings_counter].length() << " " << 
              strlen(buf);*/
      }
      if(section_num == 2) {
        token = strtok(buf, "|");
        fstates = new SetType(token);
        token = strtok(NULL, "|");
        falphabet = new SetType(token);
        ftransition = strtok(NULL, "|");
        token = strtok(NULL, "|");
        fstart_state = token[0];
        token = strtok(NULL, "|");
        /* eliminate non-printing characters that may be at the end of the 
            line */
        if(!((token[strlen(token)-1] > 'a' && token[strlen(token)-1] < 'z') ||
            (token[strlen(token)-1] > 'A' && token[strlen(token)-1] < 'A') ||
            (token[strlen(token)-1] > '0' && token[strlen(token)-1] < '9'))) {
          token[strlen(token)-1] = '\0';
        }
        faccept_states = new SetType(token);
        
        /* now put it all together */
        f = new FSAType(fstates,falphabet,ftransition,fstart_state,
                        faccept_states);
        for(int i = 0; i < test_strings_counter; i++) {
          token = (char*)test_strings[i].c_str();
          if(!f->is_accepted(test_strings[i].c_str())) {
            token = strcat(token, " ");
            token = strcat(token, "is accepted\n");
            fputs(token, fout);
          }
          else {
            token = strcat(token, " ");
            token = strcat(token, "is not accepted\n");
            fputs(token, fout);
          }
        }
        fputs("\n", fout);
      }
    }
        
  }
  
  system("PAUSE");
  return 0;
}
