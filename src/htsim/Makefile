.PHONY: clean

htsim: htsim.hs Arch.hs Operation.hs
	ghc -O2 -fforce-recomp htsim.hs

clean:
	rm -rf htsim *.o *.hi
