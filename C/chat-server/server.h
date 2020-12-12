#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CLIENTS 100
#define BUFFER_SIZE 4096

typedef struct {
  int sockfd;
  int index;
  char* username;
} client_info;

int compare_clients(client_info client_a, client_info client_b) {
  return (client_a.sockfd == client_b.sockfd) && (strcmp(client_a.username, client_b.username) == 0);
}

void remove_client(client_info* removedclient);
void broadcast(char* message);

void client_quits(client_info* client);
void client_loop(client_info* client);
void* handle_client(void *client_info_ptr);
void setup_server(int portno);

void read_trim(int fd, char* buffer);
void trim_whitespace(char* input);
