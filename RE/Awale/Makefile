default: init client server

init: 
	mkdir -p bin/client
	mkdir -p bin/server

client: bin/client/client.o
	gcc bin/client/client.o -o bin/client/client

server: bin/server/server.o bin/server/game.o
	gcc bin/server/server.o bin/server/game.o -o bin/server/server

bin/client/client.o: src/client/client.c
	gcc -c src/client/client.c -o bin/client/client.o

bin/server/server.o: src/server/server.c
	gcc -c src/server/server.c -o bin/server/server.o

bin/server/game.o: src/server/game.c
	gcc -c src/server/game.c -o bin/server/game.o

clean: 
	rm -r bin