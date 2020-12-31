#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

struct token {
  int token;
  int intvalue;
};

enum {
  T_PLUS, T_MINUS, T_STAR, T_SLASH, T_INTLIT
};

struct ASTnode {
  int op;
  struct ASTnode *left;
  struct ASTnode *right;
  int intvalue;
};

enum {
  A_ADD, A_SUBTRACT, A_MULTIPLY, A_DIVIDE, A_INTLIT
};
