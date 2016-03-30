# Something with spaces in directory names...
#SHELL = /bin/bash -O extglob -O globstar
SHELL = /usr/local/bin/fish

PROJECT = core

default:
	cpm $(PROJECT).prj

force:
	cpm $(PROJECT).prj --force

#FIXME not the way to go...
inline:
	cpm $(PROJECT).prj
	rm -Rvf **/*.o
	cpm $(PROJECT).prj

clean:
	rm -Rvf **/Clean\ System\ Files
	rm -Rvf $(PROJECT).exe

.PHONY: force inline clean
