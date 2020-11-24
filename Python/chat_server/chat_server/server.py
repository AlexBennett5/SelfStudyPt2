import socket
import select

clients = {}

def broadcast(sender, message):
    print(message, end = '')
    
    for connection, name in clients.items():
        if connection != sender:
            try:
                connection.send(message)
            except socket.error:
                pass

def setup_user(connection):
    connection.send('Enter your username: ')
    try:
        name = connection.recv(1024).strip()
    except socket.error:
        return
    if name in clients.keys():
        connection.send('Username is already taken\n')
    else:
        clients[connection] = name
        broadcast(name, '\n[%s has enterred the chat]' % name)

def setup_server(host, port, max_clients, recv_buffer):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind((host, port))
    server.listen(max_clients)
    print 'Listening on %s' % ('%s:%s' % server.getsockname())

    clients[server] = 'server'

    while True:
        read_sockets, write_sockets, error_sockets = select.select(clients.keys(), [], [])

        for connection in read_sockets:
            if connection == server:
                client_connection, addr = server.accept()
                setup_user(client_connection)
            else:
                try:
                    connection.send('<You>')
                    message = connection.recv(recv_buffer)
                    if message != '':
                        broadcast(connection, '\n<' + clients[connection] + '>' + message)
                except:
                    broadcast(connection, '\n[%s has left the chat]' % clients[connection])
                    connection.close()
                    del clients[connection]
                    continue
    server.close()

if __name__ == '__main__':
    setup_server('', 1234, 10, 4096)
