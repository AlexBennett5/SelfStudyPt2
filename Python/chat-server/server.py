import socket, select, threading

IP = "127.0.0.1"
PORT = 1234
ADDR = (IP, PORT)
MAX_CLIENTS = 10
RECV_BUFFER = 4096

SERVER = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
SERVER.bind(ADDR)
CONNECTION_LIST = []

def broadcast_data(client_socket, msg):
    for socket in CONNECTION_LIST:
        if socket != SERVER and socket != client_socket:
            try:
                socket.send(msg)
            except:
                socket.close()
                CONNECTION_LIST.remove(socket)

if __name__ == "__main__":
    CONNECTION_LIST.append(SERVER)
    SERVER.listen(MAX_CLIENTS)
    print("Server successfully started. Accepting clients")
    
    while True:
        read_sockets, write_sockets, error_sockets = select.select(CONNECTION_LIST, [], [])

        for sock in read_sockets:
            if sock == SERVER:
                sockfd, addr = SERVER.accept()
                CONNECTION_LIST.append(sockfd)
                print("Client [%s, %s] connected" % addr)

                broadcast_data(sockfd, "[%s:%s] enterred the chat room\n" % addr)

            else:
                try:
                    data = sock.recv(RECV_BUFFER)
                    if data:
                        broadcast_data(sock, "\r" + '<' + str(sock.getpeername()) + '>' + data)

                except:
                    broadcast_data(sock, "Client [%s, %s] logged off" % addr)
                    sock.close()
                    CONNECTION_LIST.remove(sock)
                    continue

    SERVER.close()


