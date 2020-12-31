#include <stdio.h>
#include <string.h>

#define MAX_LINE_LEN 500

////////////////////////////
// Tokenizer
////////////////////////////

enum token {
  LPAREN, RPAREN, QUOTE, IDEN, NUM   
};

FILE *fd;
char line[MAX_LINE_LEN] = {'\0'};

enum token next_token() { 
  
  if (isalpha(*line)) {
    
  }
}

int main(int argc, char *argv) {

  if (argc > 2) {
    fd = fopen(argv[1], "r");
  } else {
    fd = stdin;
  }

  for (;;) {
    
  }
}
