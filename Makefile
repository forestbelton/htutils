.PHONY: clean

bin/htsim:
	cd src/htsim; make

clean:
	cd src/htsim; make clean
