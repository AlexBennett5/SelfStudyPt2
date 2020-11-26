import socket
import select
import sys

class Client:

    def __init__(self, username):
        self.username = username

    def prompt(self):
        sys.stdout.write('<You> ')
        sys.stdout.flush()

    def connect_to(self, hostname, portno):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.settimeout(2)

        try:
            server_socket((hostname, portno))
        except:
            print 'Connection error'
            sys.exit()

        server_socket.send(self.username)
        print 'Connected to host'

        prompt()

        while True:
            socket_list = [sys.stdin, server_socket]
            read_sockets, write_sockets, error_sockets = select.select(socket_list, [], [])

            for chosen_socket in read_sockets:
                if chosen_socket == server_socket:
                    data = chosen_socket.recv(4096)

                    if not data:
                        print 'Connection error'
                        sys.exit()
                    else:
                        sys.stdout.write(data)
                        prompt()
                else:
                    message = sys.stdin.readline()
                    server_socket.send(message)
                    prompt()

if __name__ == '__main__':
    if (len(sys.argv) < 4):
        print 'Format requires: python client.py username hostname portno'
        sys.exit()

    client = Client(sys.argv[1])
    client.connect_to(sys.argv[2], int(sys.argv[3]))
