#include "nfatype.h" // class's header file

// class constructor
NFAType::NFAType(SetType* Qinit, SetType* sigmainit, const char* deltainit, 
              char Sinit, SetType* Finit)
{
	Q = Qinit;
	sigma = sigmainit;
	delta = deltainit;
	S = Sinit;
	F = Finit;
	NumTransitions = 0;
	valid = true;
	
 	if(Q->is_empty()) {
    valid = false;
  }
  if(!Q->is_member(S)) {
    valid = false;
  }
  if(F->is_empty()) {
    valid = false;
  }
  if(!Q->is_subset(F)) {
    valid = false;
  }
}

void NFAType::write()
{
  cout << "The NFA ";
  valid ? cout << "is valid\n" : cout << "is not valid\n";
  cout << "States: ";
  Q->write();
  cout << "Alphabet: ";
  sigma->write();
  cout << "Transition Function: " << delta << "\nStart State: " << S;
  cout << "\nEnd State(s): ";
  F->write();
  cout << endl;
}

bool NFAType::is_valid()
{
  if(valid) {
    return true;
  }
  return false;
}

bool NFAType::is_accepted(string testString)
{
  // we discussed ways to approach this function in class; I ended up choosing
  // a method we didn't discuss after failing at the two approaches my group
  // thought best. First, I tried a depth-first search using a recursive data
  // structure, but C++ doesn't allow a class A to contain a member of class A,
  // so that approach (depth-first) would have required a really ugly solution.
  // Next, I tried to implement a constructor which created a DFA from the NFA,
  // but got stuck very fast trying to do that.
  // finally, it occurred to me that a breadth-first search wouldn't be 
  // very difficult; just systematically test every branch in the queue one 
  // after the other. If it accepted, return true, otherwise stick the result
  // into the second queue and delete that branch from the current queue. The
  // queues are implemented in C++ 'std::string's, whose powerful functions 
  // made it easier to deal with than arrays, although probably slower.


  if(!valid || testString.length() < 1) {
    return false;
  }
  
  string queue = "";           // queue and queue2 will alternate; as queue is 
                               // emptied,
  string queue2 = "";          // queue2 is filled, and then the reverse.
  string* current = &queue;    // current will be whichever queue is being 
                               // drawn from, while
  string* new_q = &queue2;     // new_q will be whichever queue is being sent to
  string cur_string, cur_state;
  string next_state_ret;
  char cur_state_char;         // nextState requires a char
  bool isQueueOne = true;      // true if current points to queue, not queue2
  
  // send the initial string to the nextState function and append it to the 
  // queue
  current->append(nextState(S, testString));
  
  // if the string was accepted, return true
  if((int)current->find("accept") > 0) {    
    return true;
  }
  
  // while the queue is not empty, fill up the next one. Check for "accept" in
  // the queue, and accept if it's found. 
  // Switch the queues if there are no more ';'s in the queue.
  while(current->find_first_of(";") > 0) { 
  
    // extract the current state and input string from the nextState return val
    cur_state = current->substr(0, current->find(":"));
    cur_state_char = cur_state[0];
    cur_string = current->substr(current->find(":") + 1, 
                                    current->find(";") - 2);

    //cout << "cur_state: " << cur_state_char << " cur_string: " << cur_string
    //     << "\ncurrent: " << *current << endl;

    // get the new val of nextState
    next_state_ret = nextState(cur_state_char, cur_string);
    
    //cout << "returned: " << next_state_ret << endl;
    
    // if string failed and queues are empty, exit in failure
    if(next_state_ret == "" && current->length() == 0 && new_q->length() == 0) {
      return false;
    }
    
    // otherwise, append the result to the new queue
    new_q->append(next_state_ret);
    
    //cout << "new_q: " << *new_q << endl;
    //system("PAUSE");
    
    // if the string was accepted, return true
    if((int)new_q->find("accept") >= 0) {
      return true;
    }
    
    // erase the state and string we just tested
    current->erase(0, current->find(";") + 1);
    
    // switch the queues
    if((int)current->find(";") < 0) {
      if(isQueueOne) {
        current = &queue2;
        new_q = &queue;
        isQueueOne = false;
      }
      else {
        current = &queue;
        new_q = &queue2;
        isQueueOne = true;
      }
      new_q->resize(0);
    }
  }
  
  return false;
}

string NFAType::nextState(char state, string input)
{
  if(!Q->is_member(state) || input.length() == 0) {
    return "";
  }
  
  string charset = "e";  
    charset += input[0];
  string semicolon = ";";
  string queue = "";
  int i = 0;
  int limit;
  int delete_me = 0;
  string state1 = (string)delta;
  
  // keep checking states until state1[0] is the correct current state
  while (state1[i] != state) {
    i = state1.find_first_of(semicolon, i);
    i++;
  }
    
  // skip past the '-'
  i++;

  // find the next semicolon;
  limit = state1.find_first_of(semicolon, i);
  
  //find the first element in the charset (e + first symbol)
  i = state1.find_first_of(charset, i);
  
  if(i < 0) {
    if(input.length() == 1 && F->is_member(state) && input == "e") {
      return "accept!!!"; //accept string
    }
    return "";
  }

  // and now we find out the possibilities:
  // If it's sigma, cat "<new_state>:<input>;" to the queue string
  // If it's the symbol, cat "<new_state>:<input - first letter>;" to the 
  // queue string
  while(i >= 0 && i < limit) {  
    
    if(state1[i] == 'e') {
      i+=2;
      queue += state1[i]; // + ":" + input + ";";
      queue += ":";
      queue += input;
      queue += ";";
    }
    else {
      i+=2;
      if(input.length() > 1) {
        queue += state1[i];
        queue += ":";
        queue += input.substr(1);
        queue += ";";
      }
      else {
        queue += state1[i];
        queue += ":e;";
      }
    }
    
    // and update the number of transitions
    NumTransitions++;
    
    //find the next e or symbol
    i = state1.find_first_of(charset, i);
  }

  return queue;
}

// class destructor
NFAType::~NFAType()
{
	
}
