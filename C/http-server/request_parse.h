#ifndef REQUEST_PARSE_H
#define REQUEST_PARSE_H 1

typedef struct {
  int returncode;
  char *filename;
} http_req;

http_req parse_http_req(char* request);
char* get_filename(char* request);

#endif
