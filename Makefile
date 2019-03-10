SUBDIRS := scribblings

all: 
	(cd scribblings; make all)
	raco pkg remove data-table
	raco pkg install