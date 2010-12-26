default:
	cd src && ghc --make -O Main.hs

dist:
	cabal check
	cabal configure
	cabal sdist

clean:
	find . -iname "*.hi" -o -iname "*.o" | xargs rm
	rm src/Main
