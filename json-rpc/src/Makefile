all: server client
server:
	mkdir -p ../bin
	ghc server.hs -o ../bin/server
client:
	mkdir -p ../bin
	ghc client.hs -o ../bin/client
clean:
	-rm *.hi *.o
	-rm Api/*.hi Api/*.o
