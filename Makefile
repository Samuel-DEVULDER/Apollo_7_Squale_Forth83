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

FORTH_DIR=forth.dir

ALL=FORTH.asm FORTH.info FORTH.dir/FORTH.CMD FORTH.zip FORTH.imd

##############################################################################

all: $(ALL) chk
	ls -l .

clean:
	-$(RM) -rf 2>/dev/null FORTH.asm FORTH.info 
	
fullclean: clean
	-@$(RM) -rf 2>/dev/null FORTH.dir FORTH.*
	-@cd LuaJIT/              && make clean
	-@cd a09/                 && make clean
	-@cd f9dasm/              && make clean
	-@cd flextools/flexfloppy && make clean
	@ls -l .

chk: FORTH.chk
	
##############################################################################

%.info: script/%.lua %.dir/FORTH.BIN $(LUA)
	@echo "Generating info file..."
	@$(LUA) $< $*.dir/FORTH.BIN >$@

%.asm: %.info %.dir/FORTH.BIN $(F9DASM)
	@echo "Disassembling $*.dir/FORTH.BIN to $@..."
	@$(F9DASM) -info $< -nohex -noaddr -out $@ $*.dir/FORTH.BIN >/dev/null
	
%.BIN: %.asm $(A09)
	@echo "Assembling $< to $@..."
	@$(A09) -b$@ $< >/dev/null
	
%.CMD: %.asm $(A09)
	@echo "Assembling $< to $@..."
	@$(A09) -f$@ $< >/dev/null
	
%.chk: %.dir/FORTH.BIN FORTH.BIN
	@echo "Comparing..."
	@echo "==============================================================="
	-@diff -s $^
	@echo "==============================================================="	
	
%.zip: 
	@echo "Retreiving $@ from system-cfg..."
	@$(WGET) -q https://forum.system-cfg.com/download/file.php?id=43469 -O "$@"

%.imd: %.zip
	@$(7Z) --help >/dev/null || apt-cyg install p7zip || sudo apt-get install p7zip
	@echo "Extracting $@ from $<..."
	@$(7Z) e "$<" "system.imd" -r -so >$@
	
%.dir/FORTH.CMD: %.imd $(FLEXFLOPPY)
	@echo "Extracting $@ from $<..."
	@$(FLEXFLOPPY) --in "$<" --extract "$(dir $@)" >/dev/null

%.dir/FORTH.MOT: %.dir/FORTH.CMD $(F9DASM)
	@echo "Converting $< to $@..."
	@$(dir $(F9DASM))cmd2mot$(EXE) -out "$@" "$<" >/dev/null

%.dir/FORTH.BIN: %.dir/FORTH.MOT $(F9DASM)
	@echo "Converting $< to $@..."
	@$(dir $(F9DASM))mot2bin$(EXE) -out "$@" "$<" >/dev/null
	
##############################################################################
# Build/download external tools

$(LUA): $(wildcard $(dir $(LUA))/*)
	@echo "Building $@..."
	@cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	@$(STRIP) "$@"
	@echo

$(A09): $(wildcard $(dir $(A09))/*)
	@echo "Building $@..."
	@cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE BUILDMODE=static CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	@$(STRIP) "$@"
	@echo

$(F9DASM): $(wildcard $(dir $(F9DASM))/*)
	@echo "Building $@..."
	@cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	@$(STRIP) "$@"
	@echo

$(FLEXFLOPPY): $(wildcard $(dir $(FLEXFLOPPY))/*.c)
	@echo "Building $@..."
	@cd $(dir $@) && export MAKE="make -f Makefile" && $$MAKE CC="$(CC) -static" CFLAGS="$(CFLAGS)"  
	@$(STRIP) "$@"
	@echo

update:
	git submodule update --recursive --remote
	git pull
	
	# git pull --recurse-submodules

