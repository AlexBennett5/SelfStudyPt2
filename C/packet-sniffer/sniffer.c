#include <stdio.h>
#include <pcap.h>
#include <arpa/inet.h> 
#include <net/ethernet.h>
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

  struct ether_header *ethframe = (struct ether_header *) packet_body;
  print_etherframe(ethframe);

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

    
