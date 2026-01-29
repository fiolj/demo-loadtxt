# Makefile, by J. FIOL 
# SHELL = /bin/bash
export PKG_CONFIG_PATH := /home/fiol/.local/lib64/pkgconfig:$(PKG_CONFIG_PATH)
#		
# Compiling with gfortran with:
#		
# gfortran  -cpp -ffree-form -march=native -std=f2008 -Wall -Werror -pedantic -Wno-trampolines -Wno-unused-function -Wno-maybe-uninitialized     -c -o new_io.o `pkg-config --cflags fortran_stdlib` new_io.f90
# gfortran  -cpp -ffree-form -march=native -std=f2008 -Wall -Werror -pedantic -Wno-trampolines -Wno-unused-function -Wno-maybe-uninitialized     -c -o demo_loadtxt.o `pkg-config --cflags fortran_stdlib` demo_loadtxt.f90
# gfortran    -o demo_loadtxt.e demo_loadtxt.o new_io.o  `pkg-config --libs fortran_stdlib`

top_dir := $(realpath $(PWD)/..)
BIND:=$(top_dir)/bin/

################################################################################
# Commands, Compiler and Flags
CP:=cp
FC:= gfortran
arch:=-march=native
FFLAGS:= -cpp -ffree-form $(arch) -std=f2008 \
	 -Wall -Werror -pedantic -Wno-trampolines -Wno-unused-function -Wno-maybe-uninitialized
FLINK = $(FC) 

STDLIB_CFLAGS := `pkg-config --cflags fortran_stdlib`
STDLIB_LIBS := `pkg-config --libs fortran_stdlib`

################################################################################
# Rules

all: demo_loadtxt.e

exec: all
	$(BIND)/demo_loadtxt.e

demo_loadtxt.e: demo_loadtxt.o new_io.o

%.o : %.f90
	$(FC)  $(FFLAGS)  $(INCLUDES) -c -o $@ ${STDLIB_CFLAGS} $<

%.e : %.o
	${FLINK} -o $@ $^ ${LDFLAGS} ${STDLIB_LIBS}
	$(CP) $@ ${BIND}

################################################################################
# Convenience

clean:
	rm -f *.o *.mod *.smod *~

################################################################################
demo_loadtxt.o: new_io.mod
new_io.mod: new_io.o
