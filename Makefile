# Something with spaces in directory names...
#SHELL = /bin/bash -O extglob -O globstar
SHELL = /usr/local/bin/fish

PROJECT = core

default:
	cpm $(PROJECT).prj

force:
	cpm $(PROJECT).prj --force

inline:
	cpm $(PROJECT).prj
	sleep 1
	touch **/*.dcl
	cpm $(PROJECT).prj

clean:
	rm -Rvf **/Clean\ System\ Files
	rm -Rvf $(PROJECT).exe

.PHONY: force inline clean
