FLAGS=-W -O -hidir ../.build -odir ../.build

default: counter-server

all: example counter-server

example:
	ghc $(FLAGS) example.hs

counter-server:
	ghc $(FLAGS) counter-server.hs

