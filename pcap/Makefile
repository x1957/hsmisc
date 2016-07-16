main:
	cd src && make
test:
	cd src && make test
clean:
	cd src && make clean
install:
	cp bin/* ~/bin
format:
	 find src | grep ".hs" | xargs -i echo 'stylish-haskell {} > {}r && mv {}r {}' | sh
