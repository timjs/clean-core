# Something with spaces in directory names...
SHELL = /usr/local/bin/fish

PROJECT = core

default:
	cpm $(PROJECT).prj

force:
	cpm $(PROJECT).prj --force

clean:
	rm -Rvf (find . -name "Clean System Files")
	rm -Rvf $(PROJECT).exe

.PHONY: force clean
