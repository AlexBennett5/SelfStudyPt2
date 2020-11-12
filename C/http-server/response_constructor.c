#include <stdlib>
#include "response_constructor.h"

int send_msg(int fd, char *msg) {

}

int send_header(int fd, int returncode) {

}

int send_body(int fd, char* filename) {

  FILE *file;

  // Open file with fopen() and check for error

  // Get total size of file using stat() and struct stat

  char *line;

  // Allocate space to store each line of file

  // Use getline() to get data and send_msg() the line

  // Return how big the file was
}

