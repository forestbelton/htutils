.PHONY: clean all

all: bin/htsim bin/htas

bin/htsim:
	cd src/htsim; make
	mkdir -p bin
	cp src/htsim/htsim bin/htsim

bin/htas:
	cd src/htas; make
	mkdir -p bin
	cp src/htas/htas bin/htas

clean:
	cd src/htsim; make clean
	cd src/htas; make clean
	rm -rf bin
