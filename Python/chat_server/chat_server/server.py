import socket
import select
import sys

class Server:
    clients = {}

    def __init__(self, host, port, max_clients):
        self.host = host
        self.port = port
        self.max_clients = max_clients
        
    def run(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind((self.host, self.port))
        self.server.listen(self.max_clients)
        print 'Listening on %s' % ('%s:%s' % self.server.getsockname())
        
        clients[self.server] = 'server'

        while True:
            read_sockets, write_sockets, error_sockets = select.select(clients.keys(), [], [])

            for connection in read_sockets:
                if connection == server:
                    client_connection, addr = server.accept()
                    setup_user(client_connection)
                else:
                    try:
                        message = connection.recv(recv_buffer)
                        if message != '':
                            broadcast(connection, '\n<' + clients[connection] + '>' + message)
                    except:
                        broadcast(connection, '\n[%s has left the chat]' % clients[connection])
                        connection.close()
                        del clients[connection]
                        continue
        self.server.close()

    def setup_user(self, connection):
        try:
            name = connection.recv(1024).strip()
        except socket.error:
            return
        if name in clients.keys():
            connection.send('Username is already taken\n')
        else:
            clients[connection] = name
            broadcast(connection, '\n[%s has enterred the chat]' % name)

    def broadcast(sender, message):
        print message,
    
        for connection, name in clients.items():
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
