#ifndef SNIFFER_H
#define SNIFFER_H 1


void packet_handler(u_char *args, const struct pcap_pkthdr *packet_header, const u_char *packet_body);

// Link Layer //
void print_etherframe(struct ether_header *ethframe);
void print_mac(unsigned char *mac_address);
void print_ethertype(unsigned int ethertype); 

#endif
