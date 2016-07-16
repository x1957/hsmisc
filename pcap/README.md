[TOC]

Read Pcap Data
======

# Link ([format](./src/Net/Link/Format.hs))

## ARP ([format](./src/Net/Arp/Format.hs))
* ether type 0x0806

## IP ([format](./src/Net/Ip/Format.hs))
* ether type 0x0800

### ICMP ([format](./src/Net/Icmp/Format.hs))
* ip 1

### TCP ([format](./src/Net/Tcp/Format.hs))
* ip 6

### UDP ([format](./src/Net/Udp/Format.hs))
* ip 17

#### DHCP ([format](./src/Net/Dhcp/Format.hs))
* port 67 - 68

#### DNS ([format](./src/Net/Dns/Format.hs))
* port 53

Import Pcap Data
======

Reference
======
* <https://en.wikipedia.org/wiki/EtherType>
* <https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers>
* <https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers>
