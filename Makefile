.PHONY: build run clean

build:
	idris2 --build snake.ipkg

run: build
	./build/exec/snake.o

clean:
	idris2 --clean snake.ipkg
