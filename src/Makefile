# Makefile, by J. FIOL 
# SHELL = /bin/bash
export PKG_CONFIG_PATH := /home/fiol/.local/lib64/pkgconfig:$(PKG_CONFIG_PATH)
top_dir := $(realpath $(PWD)/..)
BIND:=$(top_dir)/bin/

# # El archivo principal con el código	
MNFILE= demo_loadtxt.f90
progs = $(subst .f90,.e,$(MNFILE))
#	PROGRAMS
RM:=rm -f
CP:=cp
LN:=ln -s
MKDIR:=mkdir -p

################################################################################
#	Fuentes a utilizar	
SOURCES = mod_loadtxt.f90

progs = demo_loadtxt.e test_loadtxt.e
mains = $(subst .e,.f90,$(progs))
progobj = $(subst .e,.o,$(progs))

OBJECTS = $(SOURCES:f90=o)

################################################################################
# Try to guess the architecture	
FC:= gfortran
arch:=-march=native
STDLIB_CFLAGS := `pkg-config --cflags fortran_stdlib`
FNUMFOR:=`pkg-config --cflags numfor`
FFLAGS:= -cpp -ffree-form $(arch) -std=f2008 \
	 -Wall -Werror -pedantic -Wno-trampolines -Wno-unused-function -Wno-maybe-uninitialized

STDLIB_LIBS := `pkg-config --libs fortran_stdlib`

FLINK = $(FC) $(LDFLAGS) 

# ##########################  IMPLICIT RULES  ########################
all:  $(progs)

$(progs): $(OBJECTS)  $(subst .e,.o,$@)
$(progobj): ${OBJECTS} $(OBJECTS) $(subst .o,.f90,$@)

# Rules

%.o : %.f90
	$(FC)  $(FFLAGS) $(INCLUDES) -c -o $@ ${STDLIB_CFLAGS} $<

%.e : %.o
	${FLINK} -o $@ $^ ${LDFLAGS} ${STDLIB_LIBS}
	$(CP) $@ ${BIND}

# Convenience. execute files
# To compile, build and execute you can use exec_program_name.
# To build and execute test_loadtxt.e or demo_loadtxt.e just run:
#   make exec_test_loadtxt
#   make exec_demo_loadtxt

exec_%: %.e
	${BIND}/$<

.PHONY: All clean tags Tags TAGS

tags Tags TAGS: $(SOURCES) $(mains)
	etags $(SOURCES)  $(mains)

clean:
	rm -f $(ALL_OBJECTS); rm -f *.mod

demo_loadtxt.o: mod_loadtxt.mod
test_loadtxt.o: mod_loadtxt.mod
mod_loadtxt.mod: mod_loadtxt.o
