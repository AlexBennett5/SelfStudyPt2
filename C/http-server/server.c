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

void error(char *msg) {
  perror(msg);
  exit(1);
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
