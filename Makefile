.PHONY: clean

htsim: htsim.hs
	ghc htsim.hs

clean:
	rm -rf htsim htsim.o htsim.hi
