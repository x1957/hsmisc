bin:
	mkdir -p ../bin
	ghc main.hs -o ../bin/pcap-read -XStandaloneDeriving
clean:
	-rm *.hi *.o
	-rm Net/*.hi Net/*.o
	-rm Net/Pcap/*.hi Net/Pcap/*.o
	-rm Net/Link/*.hi Net/Link/*.o
	-rm Net/Arp/*.hi Net/Arp/*.o
	-rm Net/Ip/*.hi Net/Ip/*.o
	-rm Net/Tcp/*.hi Net/Tcp/*.o
test:
	runhaskell ParseTest.hs
	runhaskell UtilsTest.hs
	runhaskell Net/AllTest.hs
