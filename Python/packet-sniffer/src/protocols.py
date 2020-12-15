
### Link Layer ###

class EthernetFrame:

    def __init__(self, packet):
        macDst, macSrc, ethertype = struct.unpack(">6s6sH", packet[:14])
        self.macDst = binascii.hexlify(macDst)
        self.macSrc = binascii.hexlify(macSrc)
        ethertype = hex(ethertype)
        
        if (ethertype == '0x0800')
          self.ethertype = 'IPv4'
        elif (ethertype == '0x86DD')
          self.ethertype = 'IPv6'
        
        self.payload = packet[14:]

    def print_protocol(self):
        print("===========Ethernet Frame===========")
        print("MAC Destination: {}\nMAC Source{}".format(self.macDst, self.macSrc))
        print("EtherType: {}".format(self.ethertype))

### Internet Layer ###

class IPv4:

class IPv6:

class ICMP:

### Transport Layer ###

class TCP:

class UDP:


