# Something with spaces in directory names...
SHELL = /usr/local/bin/fish

PROJECT = core

default:
	cpm $(PROJECT).prj

force:
	cpm $(PROJECT).prj --force

clean:
	rm -R (find . -name "Clean System Files")
	rm -R $(PROJECT).exe

.PHONY: force clean
