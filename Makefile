##############################################################################
# Makefile for Apollo_7_Squale_Forth83 by Samuel Devulder
##############################################################################

STRIP=strip
WGET=wget --no-check-certificate
SED=sed -e
GIT=git
RM=rm
CP=cp
7Z=7z

# VERSION:=$(shell git describe --tags --abbrev=0)
MACHINE:=$(shell uname -m)
DATE:=$(shell date +%FT%T%Z || date)
OS:=$(shell uname -o | tr "/" "_")
EXE=

CC=gcc
CFLAGS=-O3 -Wall

ifeq ($(OS),Windows_NT)
	OS:=win
endif

ifeq ($(OS),Cygwin)
	OS:=win
endif

ifeq ($(OS),win)
	CC=i686-w64-mingw32-gcc -m32
	MACHINE=x86
	EXE=.exe
endif

LUA=LuaJIT/src/luajit$(EXE)
F9DASM=f9dasm/f9dasm$(EXE)
A09=a09/a09$(EXE)
FLEXFLOPPY=flextools/flexfloppy/flexfloppy$(EXE)

ALL=$(TOOLS) $(LUA) $(F9DASM) $(A09) FORTH.asm

##############################################################################

all: $(ALL)
	ls -l .

clean:
	-$(RM) -rf 2>/dev/null FORTH.asm FORTH.info
	
fullclean: clean
	-rm -rf FORTH.DIR
	-cd LuaJIT/ && make clean
	-cd a09/    && make clean
	-cd f9dasm/ && make clean
	
##############################################################################

%.info: %.lua %.BIN $(LUA)
	$(LUA) $< $*.BIN >$@

%.asm: %.info $(F9DASM)
	$(F9DASM) -info $< -nohex -noaddr -out $@
	
%.chk: %.asm $(A09)
	$(A09) -b$@ $<
	diff -s $*.BIN $@
	@rm -f "$@"
	
chk: FORTH.chk FORTH.info
	
FORTH.DIR: disk/OS_FORTH_imd.dsk $(FLEXFLOPPY) 	
	-mkdir $@
	$(FLEXFLOPPY) --in $< --extract $@
	
FORTH.BIN: FORTH.DIR FORTH.DIR/FORTH.BIN 
	cp FORTH.DIR/FORTH.BIN $@

%.MOT: %.CMD $(F9DASM)
	$(dir $(F9DASM))cmd2mot$(EXE) -out "$@" "$<"*

%.BIN: %.MOT $(F9DASM)
	$(dir $(F9DASM))mot2bin$(EXE) -out "$@" "$<"*
	
	
	
##############################################################################
# Build/download external tools

update:
	git submodule update --recursive --remote
	git pull
	
	# git pull --recurse-submodules


$(LUA): $(wildcard $(dir $(LUA))/*)
	cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(STRIP) "$@"

$(A09): $(wildcard $(dir $(A09))/*)
	cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(STRIP) "$@"

$(F9DASM): $(wildcard $(dir $(F9DASM))/*)
	cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(STRIP) "$@"

$(FLEXFLOPPY): $(wildcard $(dir $(FLEXFLOPPY))/*)
	cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(STRIP) "$@"
