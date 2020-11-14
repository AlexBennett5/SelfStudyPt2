#include <stdlib.h>
#include "request_parse.h"

http_req_info parse_http_req(char* request_text) {
  http_req_info info;

  char* filename;

  if ((filename = malloc(sizeof(char) * strlen(request_text))) == NULL)
    //ERROR: Allocation failure

  filename = get_filename(request_text);
  strcpy(info.filename, filename);

  if (strcmp(filename, "/test-site/") == 0) {
    strcat(info.filename, "index.html"); 
  }

  struct stat buffer;
  
  if (!stat(filename, &buffer)) {
    info.returncode = 404;
    info.filename = "/test-site/404.html";
  } else {
    info.returncode = 200;
  }

  return info;
}

char* get_filename(char* request_text) {
  char* filename;

  if ((filename = malloc(sizeof(char) * strlen(request_text))) == NULL)
    //ERROR: Allocation failure

  sscanf(request_text, "GET %s HTTP/1.1", filename);

  char* filename_w_path;

  if ((filename_w_path = malloc(sizeof(char) * (12 + strlen(filename)))) == NULL)
    //ERROR: Allocation failure

  char* folder_name = "/test-site";

  strcpy(filename_w_path, folder_name);
  strcat(filename_w_path, filename);
  free(filename);
  return filename_w_path;
}

