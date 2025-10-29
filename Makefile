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

TOOLS=tools/
LUA=$(TOOLS)luajit$(EXE)
F9DASM=$(TOOLS)f9dasm$(EXE)
A09=$(TOOLS)a09$(EXE)

ALL=$(TOOLS) $(LUA) $(F9DASM) $(A09) FORTH.asm

##############################################################################

all: $(ALL)
	ls -l .

clean:
	-$(RM) -rf 2>/dev/null $(LUA) $(F9DASM) $(A09) FORTH.asm FORTH.info
	
fullclean:: clean
	-cd LuaJIT/ && make clean
	-cd A09/    && make clean
	-cd F9dasm/ && make clean
	
##############################################################################

%.info: %.lua %.BIN $(LUA)
	$(LUA) $< $*.BIN >$@

%.asm: %.info $(F9DASM)
	$(F9DASM) -info $< -out $@

##############################################################################
# Build/download external tools

$(LUA): LuaJIT/ $(wildcard LuaJIT/src/*)
	cd $< && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(CP) $</src/$(notdir $@) "$@"
	$(STRIP) "$@"

$(A09): A09/ $(wildcard A09/*)
	cd $< && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(CP) $</$(notdir $@) "$@"
	$(STRIP) "$@"


$(F9DASM): F9dasm/ $(wildcard F9dasm/*)
	cd $< && export MAKE="make -f Makefile" && $$MAKE CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	$(CP) $</$(notdir $@) "$@"
	$(STRIP) "$@"

$(TOOLS):
	mkdir -p "$@"