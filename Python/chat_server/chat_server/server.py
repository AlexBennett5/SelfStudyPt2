from socket import socket, AF_INET, SOCK_STREAM
from threading import Thread
import sys

BUFFER_SIZE = 4096


class Server:

    def __init__(self, host, port, max_clients):
        self.host = host
        self.port = port
        self.max_clients = max_clients
        self.clients = {}

    def run(self):
        self.server_socket = socket(AF_INET, SOCK_STREAM)
        self.server_socket.bind((self.host, self.port))
        self.server_socket.listen(self.max_clients)
        accept_thread = Thread(target=self.accept_connections)
        accept_thread.start()
        accept_thread.join()
        self.server_socket.close()

    def accept_connections(self):
        while True:
            connection, address = self.server_socket.accept()
            print('%s:%s has connected' % address)
            connection.send(bytes('Welcome! Enter your name', 'utf8'))
            Thread(target=self.setup_user, args=(connection,)).start()

    def setup_user(self, connection):
        username = connection.recv(BUFFER_SIZE).decode('utf8')
        welcome_msg = 'If you ever want to exit the chat, type {quit}.'
        connection.send(bytes(welcome_msg, 'utf8'))
        self.broadcast(bytes('[%s has enterred the chat]' % username, 'utf8'))
        self.clients[connection] = username

        while True:
            message = connection.recv(BUFFER_SIZE)
            if message != bytes('{quit}', 'utf8'):
                self.broadcast(message, '<' + username + '> ')
            else:
                connection.send(bytes('{quit}', 'utf8'))
                connection.close()
                del self.clients[connection]
                self.broadcast(bytes('[%s has left the chat]' % username, 'utf8'))
                break

    def broadcast(self, message, username=''):
        for connection in self.clients:
            connection.send(bytes(username, 'utf8') + message)


if __name__ == '__main__':
    if (len(sys.argv) < 3):
        print('Format requires: python server.py hostname portno')
        sys.exit()

    server = Server(sys.argv[1], int(sys.argv[2]), 10)
    server.run()
