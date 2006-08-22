#include <iostream>
#include <stdlib.h>
//CONSTANT DECLARATIONS
const int COLUMNS = 8;    //columns (zero based)
const int ROWS = 8;   //rows (zero based)

//FUNCTION DECLARATIONS
void placeQueen(char board[ROWS][COLUMNS]);
int count_column_attacks(char board[ROWS][COLUMNS], int col);
void printBoard(char board[ROWS][COLUMNS]);
void update_attacks(char board[ROWS][COLUMNS], int col, int row);

int main(int argc, char *argv[])
{
  char board[COLUMNS][ROWS];
  
  //fill board with *s
  for(int i = 0; i < COLUMNS; i++) {
    for(int j = 0; j < ROWS; j++) {
      board[i][j] = '*';
    }
  }
  
  //place 8 queens
  for(int i = 0; i < COLUMNS; i++) {
    placeQueen(board);
  }

  system("PAUSE");	
  return 0;
}
  
//in: two-d array representing a chess board
void placeQueen(char board[ROWS][COLUMNS]) {
  int least_attacked_col = 0;
  int first_unattacked_row = 0;
  int num_attacks_cur = ROWS + 1;
  int num_attacks_temp = ROWS + 1;
  
  //make least_attacked the column with the fewest squares attacked
  for(int i = 0; i < COLUMNS; i++) {
    num_attacks_temp = count_column_attacks(board, i);
    if(num_attacks_temp < 0) {
      cout << "solution didn't work\n";
      system("PAUSE");
    }
    if(num_attacks_temp < num_attacks_cur) { 
      least_attacked_col = i;
      num_attacks_cur = num_attacks_temp;
    }
  }
  
  //make i equal to the highest row placeable  
  for(int i = 0; board[i][least_attacked_col] == 'a'; i++) {
    first_unattacked_row = i + 1;
  }
    
  //place a queen in the board
  board[first_unattacked_row][least_attacked_col] = 'Q';
  
  update_attacks(board,first_unattacked_row,least_attacked_col);
  
  printBoard(board);
  cout << "LAC: " << least_attacked_col << " FUR: " << first_unattacked_row
    << endl;
}
  
int count_column_attacks(char board[ROWS][COLUMNS], int col) {
  int num_column_attacks = 0;
  for(int i = 0; i < ROWS; i++) {
    if(board[col][i] == 'a') {
      num_column_attacks++;
    }
  }
  if(num_column_attacks == ROWS) {
    return -1;
  }
  return num_column_attacks;
}

void update_attacks(char board[ROWS][COLUMNS], int col, int row) {
  int start_col = 0;
  int start_row = 0;
  int i =0, j = 0;
  //note the attacks on the column
  for(int i = 0; i < ROWS; i++) {
    if(i != row) {
      board[col][i] = 'a';
    }
  }
  
  //note the attacks on the row
  for(int i = 0; i < COLUMNS; i++) {
    if(i != col) {
      board[i][row] = 'a';
    }
  }
  
  if(col > row) {
    start_col = col - row;
  }
  else {
    start_row = row - col;
  }

  //now do the left-right diagonal
  for(int i = start_col; i < COLUMNS; i++) {
    if(board[i][j] != 'Q') {
      board[i][j] = 'a';
    }
    j++;
  }
  
  //now go up and right
  i = col;
  j = row;
  while(i <= COLUMNS && j <= ROWS) {
    if(board[i][j] != 'Q') {
      board[i][j] = 'a';/*
      printBoard(board);
      system("PAUSE");*/
    }
    i++;
    j++;
  }
  
  //then down and left
  i = col;
  j = row;
  while(i >= 0 && j >= 0) {
    if(board[i][j] != 'Q') {
      board[i][j] = 'a';/*
      printBoard(board);
      system("PAUSE");*/
    }
    i--;
    j--;
  }
}

void printBoard(char board[ROWS][COLUMNS]) {
  for(int i = 0; i < 8; i++) {
    for(int j = 0; j < 8; j++) {
      cout << board[i][j];
    }
    cout << endl;
  }
  cout << endl;
}
