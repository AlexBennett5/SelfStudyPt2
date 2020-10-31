#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>

#define BUFSIZE 8096

typedef struct client_req {
  int returncode;
  char* filename;
} client_req;

char* header200_html = "HTTP/1.1 200 OK\nServer: Test Server\nContent-Type: text/html; charset=UTF-8\n\n";
char* header200_css = "HTTP/1.1 200 OK\nServer: Test Server\nContent-Type: text/css; charset=UTF-8\n\n";
char* header404 = "HTTP/1.1 404 Not Found\nServer: Test Server\n\n";

void error(char *msg) {
  perror(msg);
  exit(1);
}

int sendToClient(int clientfd, char *msg) {
  return write(clientfd, msg, strlen(msg));
}

void sendHeader(int clientfd, int returnCode) {
  switch (returnCode) {
    case 200:
      sendToClient(clientfd, header200_html);
      break;
    case 404:
      sendToClient(clientfd, header404);

  }
}

void sendBody(int clientfd, char *filename) {
  FILE *body = fopen(filename, "r");
  char *current_line;
  size_t req_size = 1;

  int end_of_file;

  while((end_of_file = getline(&current_line, &req_size, body)) > 0) {
    sendToClient(clientfd, current_line);
  } 
}

char* getClientRequest(int clientfd) {

  FILE *fstream;
  char *request;
  char *current_line;
  int previous_size = 1;
  size_t req_size = 1;

  fstream = fdopen(clientfd, "r");
  request = malloc((sizeof(char) * req_size));
  *request = '\0';
  *current_line = '\0';
  
  int end_of_line;

  while ((end_of_line = getline(&current_line, &req_size, fstream)) > 0) {
    
    if (strcmp(current_line, "\r\n") == 0)
      break;
    
    request = realloc(request, size + previous_size);
    previous_size += size;
    strcat(request, current_line);
  }

  free(current_line);

  return request;

}

client_req parseClientRequest(char *request) {
  client_req info;

  char *filename = malloc(sizeof(char) * strlen(request));

  sscanf(request, "GET %s HTTP/1.1", filename);

  char *index = "test-site";
  strcat(index, filename);

  FILE *fileExists = fopen(index, "r");

  if (exists == NULL) {
    info.returncode = 404;
    info.filename = "404.html";
  } else {
    info.returncode = 200;
    info.filename = index;
  }

  return info;
}


void sendHTTP(int sockfd) {

  int req;
  static char buffer[BUFSIZE + 1];
  
  req = read(sockfd, buffer, BUFSIZE);

  if (req == 0 || req == -1) {
    char msg_accept[] = "ERROR on accept";
    error(msg_accept);
  }

  char message[] = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: close\r\n\r\nHello, HTTP!!";

  strcpy(buffer, message);

  req = write(sockfd, &buffer, strlen(message));

  if (req < 0) {
    char msg_write[] = "ERROR writing to socket";
    error(msg_write);
  }

}

void fireman(int signum) {
  while (waitpid(-1, NULL, WNOHANG) > 0)
    printf("Child process ended\n");
}

int main(int argc, char *argv[]) {
  int sockfd, newsockfd, portno;
  pid_t pid;
  socklen_t clilen;
  struct sockaddr_in serv_addr, cli_addr;

  if (argc < 2) {
    fprintf(stderr,"ERROR, no port provided\n");
    exit(1);
  }

  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  
  if (sockfd < 0) {
    char msg_open[] = "ERROR opening socket";
    error(msg_open);
  }

  bzero((char *) &serv_addr, sizeof(serv_addr));
  portno = atoi(argv[1]);
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);

  if (bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
    char msg_bind[] = "ERROR on binding"; 
    error(msg_bind);
  }

  listen(sockfd, 32);
  clilen = sizeof(cli_addr);

  signal(SIGCHLD, fireman);
  
  while(1) {

    newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);

    if (newsockfd < 0) {
      char msg_accept[] = "ERROR on accept";
      error(msg_accept);
    }

    pid = fork();

    if (pid < 0) {
      char msg_fork[] = "ERROR on fork";
      error(msg_fork);
    }

    if (pid == 0) {
      close(sockfd);
      sendHTTP(newsockfd);
      exit(0);
    } else {
      close(newsockfd);
    }

  }
  close(sockfd);
  return 0;

}
