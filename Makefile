default:
	cd src && ghc --make -O Main.hs

dist:
	cabal check
	cabal configure
	cabal sdist

clean:
	cabal clean
	rm -rf dist/ src/Main
	find . -iname "*.hi" -o -iname "*.o" | xargs rm
