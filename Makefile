.PHONY: clean

bin/htsim:
	cd src/htsim; make
	mkdir -p bin
	cp src/htsim/htsim bin/htsim

clean:
	cd src/htsim; make clean
	rm -rf bin
