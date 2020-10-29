#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <pthread.h>
#include <unistd.h>

#define BUFSIZE 8196
#define PORT 15715

void main() {
  int i, sockfd;
  char buffer[BUFSIZE];
  static struct sockaddr_in serv_addr;

  sockfd = socket(AF_INET, SOCK_STREAM, 0);

  if (sockfd < 0) {
    char msg_open[] = "ERROR opening socket";
    error(msg_open);
  }

  // Unfinished

}
