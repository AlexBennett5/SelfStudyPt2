#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <errno.h>
#include <pthread.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <ctype.h>
#include <signal.h>
#include "server.h"

client_info clients[MAX_CLIENTS];
static int nextclient = 0;
static pthread_mutex_t lock;

// Synchronized Functions

void remove_client(client_info* removedclient) {
  pthread_mutex_lock(&lock);
  int index_to_remove = removedclient->index;
  close(removedclient->sockfd);

  for (int i = index_to_remove; i < nextclient + 1; i++) {
    clients[i] = clients[i + 1];
  }
  nextclient--;
  pthread_mutex_unlock(&lock);
}

void broadcast(char* message) {
  pthread_mutex_lock(&lock);
  for (int i = 0; i < nextclient; i++) {
    if (write(clients[i].sockfd, message, strlen(message)) < 0) {
      perror("ERROR: sending message");
      break;
    }
  }
  printf("%s\n", message);
  pthread_mutex_unlock(&lock);
}

// Server Setup

void client_quits(client_info* client) {
  char buffer[BUFFER_SIZE];
  remove_client(client);
  sprintf(buffer, "[%s has left the chat]\n", client->username);
  broadcast(buffer);
  pthread_detach(pthread_self());
}

void client_loop(client_info* client) {
  char buffer_out[BUFFER_SIZE];
  char buffer_in[BUFFER_SIZE];

  while(1) {
    read(client->sockfd, buffer_in, sizeof(buffer_in));

    if (strcmp(buffer_in, "{quit}\n") == 0) {
      client_quits(client);
      return;
    }

    sprintf(buffer_out, "<%s> %s", client->username, buffer_in);
    broadcast(buffer_out);

  }

  client_quits(client);

}

void* handle_client(void *client_info_ptr) {
  char buffer[BUFFER_SIZE];
  
  client_info *newclient = (client_info *) client_info_ptr;
  char* username_msg = "Welcome! Please enter your username: ";
  write(newclient->sockfd, username_msg, strlen(username_msg));
  char username[BUFFER_SIZE];
  read(newclient->sockfd, username, sizeof(username));
  newclient->username = username;
  
  sprintf(buffer, "[%s has entered the chat]\n", newclient->username);
  broadcast(buffer);
  client_loop(newclient);

  return NULL;
}

void setup_server(int portno) {

  int listenfd, connfd;
  struct sockaddr_in serv_addr, cli_addr;
  pthread_t tid[MAX_CLIENTS];

  listenfd = socket(AF_INET, SOCK_STREAM, 0);
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);

  if (listenfd < 0) {
    perror("ERROR: opening socket");
    exit(1);
  }

  if (bind(listenfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
    perror("ERROR: on binding");
    exit(1);
  }

  if (listen(listenfd, 10) < 0) {
    perror("ERROR: on listen");
    exit(1);
  }

  while(1) {
    socklen_t clilen = sizeof(cli_addr);
    connfd = accept(listenfd, (struct sockaddr *) &cli_addr, &clilen);
    
    if (nextclient == MAX_CLIENTS) {
      printf("Server is currently full, cannot connect new client");
      close(connfd);
      continue;
    }

    clients[nextclient].sockfd = connfd;
    clients[nextclient].index = nextclient;

    pthread_create(&tid[nextclient], NULL, handle_client, &clients[nextclient]);

    nextclient++;

  }

  close(listenfd);

}

// Helper functions

void read_trim(int fd, char* buffer) {
  read(fd, buffer, sizeof(buffer));
  trim_whitespace(buffer);
}

void trim_whitespace(char* string) {

  size_t len = strlen(string) + 1;
  char* end = string + len;
  
  while(string < end && isspace(*end))  
    end--;

  *end = '\0';
}

int main(int argc, char *argv[]) {
  
  if (argc < 2) {
    perror("ERROR: No port provided");
    exit(1);
  }

  int portno = atoi(argv[1]);
  setup_server(portno);
}
