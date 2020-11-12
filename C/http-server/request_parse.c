#include <stdlib.h>
#include "request_parse.h"

http_req_info parse_http_req(char* request_text) {
  http_req_info info;

  char* filename;

  // Allocate memory to filename and check it isn't NULL
  
  filename = get_filename(request_text);

  // Check if they asked for / and give them index.html

  // Check if file exists

  return info;
}

char* get_filename(char* request_text) {
  char* filename;

  // Allocate memory for filename

  // sscanf() to get filename from header

  char* filename_w_path;

  // Allocate memory for filename_w_path

  char* folder_name = "";

  // strcpy, strcat to create filename_w_path
  
  free(filename);
  return filename_w_path;
}

