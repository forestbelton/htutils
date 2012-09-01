.PHONY: clean

htsim: htsim.hs Arch.hs
	ghc htsim.hs Arch.hs

clean:
	rm -rf htsim *.o *.hi
