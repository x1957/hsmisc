OPTS = -W -O -hidir ../.build -odir ../.build

all: read import dump_dns

dir:
	mkdir -p ../bin

read: dir
	ghc $(OPTS) pcap-read.hs -o ../bin/pcap-read

import: dir
	ghc $(OPTS) pcap-import.hs -o ../bin/pcap-import

dump_dns: dir
	ghc $(OPTS) dump-dns-queries.hs -o ../bin/dump-dns

clean:
	-rm *.hi *.o
	-rm Net/*.hi Net/*.o
	-rm Net/Pcap/*.hi Net/Pcap/*.o
	-rm Net/Link/*.hi Net/Link/*.o
	-rm Net/Arp/*.hi Net/Arp/*.o
	-rm Net/Ip/*.hi Net/Ip/*.o
	-rm Net/Tcp/*.hi Net/Tcp/*.o

test:
	runhaskell Misc/ParseTest.hs
	runhaskell Misc/UtilsTest.hs
	runhaskell Net/AllTest.hs
