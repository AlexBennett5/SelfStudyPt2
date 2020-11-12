#ifndef RESPONSE_CONSTRUCT_H
#define RESPONSE_CONSTRUCT_H 1

int send_msg(int fd, char *msg);
int send_header(int fd, int returncode);
int send_body(int fd, char* filename);

#endif
