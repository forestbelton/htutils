.PHONY: clean

htsim: htsim.hs Arch.hs Operation.hs
	ghc htsim.hs Arch.hs

clean:
	rm -rf htsim *.o *.hi
