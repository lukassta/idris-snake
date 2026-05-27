build:
	idris2 --build snake.ipkg

run:
	./build/exec/snake.o

clean:
	idris2 --clean snake.ipkg
