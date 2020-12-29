#include <stdio.h>
#include <pcap.h>
#include <arpa/inet.h> 
#include <net/ethernet.h>
#include <netinet/ip.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include "sniffer.h"

int main() {
  char *device;
  char error_buffer[PCAP_ERRBUF_SIZE];
  pcap_t *handle;
  int promisc = 1;
  int timeout_limit = 10000;

  device = pcap_lookupdev(error_buffer);
  if (device == NULL) {
    printf("Error on finding device: %s\n", error_buffer);
    return 1;
  }

  handle = pcap_open_live(device, BUFSIZ, promisc, timeout_limit, error_buffer);
  if (handle == NULL) {
    printf("Error on handle: %s\n", error_buffer);
  }

  pcap_loop(handle, 0, packet_handler, NULL);
  pcap_close(handle);
  
  return 0;
}

void packet_handler(u_char *args, const struct pcap_pkthdr *packet_header, const u_char *packet_body) {

  printf("**********************************************\n");

  struct ether_header *ethframe = (struct ether_header *) packet_body;
  print_etherframe(ethframe);

  int size = sizeof(ether_header);

  switch(ntohs(ethframe->ether_type)) {

    case ETHERTYPE_IP:
      handle_ipv4(packet_body, size);
      break;

    default:
      break;
  }

  printf("**********************************************\n");
}

// Link Layer //

void print_etherframe(struct ether_header *ethframe) {

  printf("***************[Ethernet Frame]***************\n\n");
  printf("    |MAC Destination: ");
  print_mac((unsigned char *) ethframe->ether_dhost);
  printf("    |MAC Source: ");
  print_mac((unsigned char *) ethframe->ether_shost);
  printf("    |Ethertype: ");
  print_ethertype(ntohs(ethframe->ether_type));
}

void print_mac(unsigned char *mac_address) {

  for (int i = 0; i < 6; i++) {
    printf("%.2X", mac_address[i]);

    if (i != 5)
      printf("-");
  }

  printf("\n");
}

void print_ethertype(unsigned int ethertype) {

  switch(ethertype) {
    
    case ETHERTYPE_IP:
      printf("IPv4\n");
      break;

    case ETHERTYPE_IPV6:
      printf("IPv6\n");
      break;

    case ETHERTYPE_ARP:
      printf("ARP\n");
      break;

    default:
      printf("Other\n");
      break;

  }
}

// Internet Layer //

void handle_ipv4(const u_char *packet, int size) {

  struct ip *ipheader = (struct ip *) (packet + size);
  print_ipv4(ipheader);

  size += sizeof(struct ip);

  switch(ipheader->ip_p) {

    case IPPROTO_TCP:
      handle_tcp(packet, size);
      break;

    case IPPROTO_UDP:
      //handle_udp(packet, size);
      break;

    case IPPROTO_ICMP:
      //handle_icmp(packet, size);

    default:
      return;
  }

}

void print_ipv4(struct ip *ipheader) {

  char source_buf[INET_ADDRSTRLEN];
  char dest_buf[INET_ADDRSTRLEN];

  inet_ntop(AF_INET, &(ipheader->ip_src), source_buf, INET_ADDRSTRLEN);
  inet_ntop(AF_INET, &(ipheader->ip_dst), dest_buf, INET_ADDRSTRLEN);

  printf("***************[IPv4 Packet]***************\n\n");
  printf("    |Total Length: %hu bytes\n", (unsigned short) ipheader->ip_len);
  printf("    |Identification: %hu\n", (unsigned short) ipheader->ip_id);
  printf("    |Time To Live: %u sec\n", (unsigned char) ipheader->ip_ttl);
  printf("    |Next Protocol: %s\n", print_next_protocol((unsigned char) ipheader->ip_p));
  printf("    |Checksum: %hu\n\n", (unsigned short) ipheader->ip_sum);
  
  printf("    |Source IP: %s\n", source_buf);
  printf("    |Destination IP: %s\n", dest_buf);
}

char* print_next_protocol(unsigned char protocol_num) {

  switch(protocol_num) {
  
    case IPPROTO_TCP:
      return "TCP";
    
    case IPPROTO_UDP:
      return "UDP";

    case IPPROTO_ICMP:
      return "ICMP";

    default:
      return "Other";  
  }
}

// Transport Layer //

void handle_tcp(const u_char *packet, int size) {

  struct tcphdr *segment = (struct tcphdr *) (packet + size);
  print_tcp(segment);

}

void print_tcp(struct tcphdr *segment) {

  printf("***************[TCP Packet]***************\n\n");
  printf("    |Source Port: %hu\n", (unsigned short) segment->th_sport);
  printf("    |Destination Port: %hu\n", (unsigned short) segment->th_dport);
  printf("    |Sequence Number: %i\n", (unsigned int) segment->th_seq);
  printf("    |Ack Number: %i\n", (unsigned int) segment->th_ack);

}
