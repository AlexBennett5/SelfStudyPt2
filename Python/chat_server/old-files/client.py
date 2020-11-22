import socket, select, sys, string

def prompt():
    sys.stdout.write('<You> ')
    sys.stdout.flush()

if __name__ == "__main__":
    
    if (len(sys.argv) < 3):
        print "Requires: python client.py hostname portno"
        sys.exit()

    hostname = sys.argv[1]
    port = int(sys.argv[2])
    addr = (hostname, port)

    CLIENT = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    CLIENT.settimeout(2)

    try:
        CLIENT.connect(addr)
    except:
        print "Connection error"
        sys.exit()

    print "Connected to host"
    prompt()

    while True:
        socket_list = [sys.stdin, CLIENT]

        read_sockets, write_sockets, error_sockets = select.select(socket_list, [], [])

        for sock in read_sockets:

            if sock == CLIENT:
                data = sock.recv(4096)

                if not data:
                    print "\nConnection error"
                    sys.exit()
                else:
                    sys.stdout.write(data)
                    prompt()
            else:
                msg = sys.stdin.readline()
                CLIENT.send(msg)
                prompt()
