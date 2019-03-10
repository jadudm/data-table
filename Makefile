# http://aggregate.org/rfisher/Tutorials/Make/make6.html

all: 
	(cd scribblings; make all)
	raco pkg remove data-table | true
	raco pkg install
	
clean:
	rm -rf `find . -name compiled`