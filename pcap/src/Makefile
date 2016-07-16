all: read import
dir:
	mkdir -p ../bin
read: dir
	ghc pcap-read.hs -o ../bin/pcap-read
import: dir
	ghc pcap-import.hs -o ../bin/pcap-import
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
