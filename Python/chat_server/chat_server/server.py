import socket
import select
import sys

class Server:

    def __init__(self, host, port, max_clients):
        self.host = host
        self.port = port
        self.max_clients = max_clients
        self.clients = {}

    def run(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind((self.host, self.port))
        self.server.listen(self.max_clients)
        print 'Listening on %s' % ('%s:%s' % self.server.getsockname())
        self.clients[self.server] = 'server'

        while True:
            read_sockets, write_sockets, error_sockets = select.select(self.clients.keys(), [], [])

            for connection in read_sockets:
                if connection == self.server:
                    client_connection, addr = self.server.accept()
                    self.setup_user(client_connection)
                else:
                    try:
                        message = connection.recv(4096)
                        if message != '':
                            self.broadcast(connection, '\n<' + self.clients[connection] + '>' + message)
                    except:
                        self.broadcast(connection, '\n[%s has left the chat]' % self.clients[connection])
                        connection.close()
                        del self.clients[connection]
                        continue
        self.server.close()

    def setup_user(self, connection):
        try:
            name = connection.recv(1024).strip()
        except socket.error:
            return
        if name in self.clients.keys():
            connection.send('Username is already taken\n')
        else:
            self.clients[connection] = name
            self.broadcast(connection, '\n[%s has enterred the chat]' % name)

    def broadcast(self, sender, message):
        print message,
        for connection, name in self.clients.items():
            if connection != sender:
                try:
                    connection.send(message)
                except socket.error:
                    pass


if __name__ == '__main__':
    if (len(sys.argv) < 3):
        print 'Format requires: python server.py hostname portno'
        sys.exit()

    server = Server(sys.argv[1], int(sys.argv[2]), 10)
    server.run()
