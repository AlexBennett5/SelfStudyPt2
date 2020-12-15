import socket

def main():
    
    conn = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, socket.ntohs(0x0003))

    while True:
        binarydata, address = conn.recvfrom(65536)
        etherframe = EthernetFrame(binarydata)
        etherframe.print_protocol()
        ippacket = etherframe.get_next_protocol()
        ippacket.print_protocol()
        transport_or_icmp = ippacket.get_next_protocol()
        transport_or_icmp.print_protocol()

