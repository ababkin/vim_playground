all: Main.js

Main.js: Main.hs
	hastec Main.hs -main-is Main

clean:
	-rm -r main
	-rm *~
	-rm Main.hi
	-rm Main.o
	-rm Main.js

distclean: clean
	-rm Main
	-rm Main.js
