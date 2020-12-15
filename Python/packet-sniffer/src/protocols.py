import socket
import struct
import binascii

##TODO: IPv4, print for IPv6, TCP

### Link Layer ###

class EthernetFrame:

    def __init__(self, frame):
        macDst, macSrc, ethertype = struct.unpack(">6s6sH", frame[:14])
        self.macDst = binascii.hexlify(macDst)
        self.macSrc = binascii.hexlify(macSrc)
        ethertype = hex(ethertype)
        
        if (ethertype == '0x0800')
          self.ethertype = 'IPv4'
        elif (ethertype == '0x86DD')
          self.ethertype = 'IPv6'
        
        self.payload = frame[14:]

    def print_protocol(self):
        print("===========Ethernet Frame===========")
        print("MAC Destination: {}\nMAC Source{}".format(self.macDst, self.macSrc))
        print("EtherType: {}".format(self.ethertype))

    def get_next_protocol(self):
        if (self.ethertype = 'IPv4'):
            return IPv4(self.payload)
        elif (self.ethertype = 'IPv6'):
            return IPv6(self.payload)

### Internet Layer ###

class IPv4:
 
    def __init__(self, packet)

class IPv6:

    def __init__(self, packet):
        firstword, payload_len, next_header, hop_limit = struct.unpack(">IHBB", packet[:8])
        self.get_fields(firstword)
        self.payload_len = int(payload_len)
        self.get_next_header(next_header)
        self.hop_limit = int(hop_limit)
        self.srcIP = socket.inet_ntop(socket.AF_INET6, packet[8:24])
        self.dstIP = socket.inet_ntop(socket.AF_INET6, packet[24:40])
        self.payload = packet[40:]

    def get_fields(self, firstword):
        firstword = int(firstword)
        self.version = firstword >> 28
        self.traffic_class = (firstword >> 20) & 255
        self.flow_label = firstword & 1048575

    def get_next_header(self, next_header):
        next_header = int(next_header)
        if (next_header == 1):
            self.next_header = 'ICMP'
        elif (next_header == 6):
            self.next_header = 'TCP'
        elif (next_header == 17):
            self.next_header = 'UDP'

    def print_protocol(self):
        print("===========IPv6===========")

    def get_next_protocol(self):
        if (self.next_header == 'ICMP'):
            return ICMP(self.payload)
        elif (self.next_header == 'TCP'):
            return TCP(self.payload)
        elif (self.next_header == 'UDP'):
            return UDP(self.payload)

class ICMP:

    def __init__(self, packet):
        icmp_type, code, checksum, rest_of_header = struct.unpack(">BBHI", packet[:8])
        self.icmp_type = icmp_type
        self.code = code
        self.checksum = checksum
        self.rest_of_header = rest_of_header

    def print_protocol(self):
        print("===========ICMP===========")
        print("Type: {}\nCode: {}\nChecksum: {}".format(self.icmp_type, self.code, self.checksum))

### Transport Layer ###

class TCP:

    def __init__(self, segment):

class UDP:

    def __init__(self, datagram):
        src_port, dst_port, data_len, checksum = struct.unpack(">4H", datagram[:8])
        self.src_port = int(src_port)
        self.dst_port = int(dst_port)
        self.data_len = int(data_len)
        self.checksum = hex(checksum)

    def print_protocol(self):
        print("===========UDP===========")
        print("Source Port: {}\nDestination Port: {}".format(self.src_port, self.dst_port))
        print("Data Length (bytes): {}\nChecksum: {}".format(self.data_len, self.checksum))

