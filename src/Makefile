.SUFFIXES: 

FC=gfortran
COMPILE.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c
MAKEMOD.f08 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -fsyntax-only -c

SOURCES=main.f90 color_mod.f90 ray_mod.f90 rtweekend_mod.f90 vec3_mod.f90 color_funs.f90 ray_funs.f90 rtweekend_funs.f90 vec3_funs.f90

ifeq ($(DEBUG),)
	FCFLAGS += -O2
else
	FCFLAGS += -O0 -g
endif

main.out: $(subst .f90,.o,$(SOURCES))
	$(FC) $(FCFLAGS) -o $@ $+

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod *.anc main.out

%.anc: %.f90
	$(MAKEMOD.f08) $<
	@touch $@

%.o: %.anc
	$(COMPILE.f08) -o $*.o $(<:.anc=.f90)
	@touch $@

main.anc: rtweekend_mod.anc vec3_mod.anc color_mod.anc ray_mod.anc

rtweekend_mod.anc: rtweekend_mod.mod rtweekend_mod.smod
vec3_mod.anc: vec3_mod.mod vec3_mod.smod
color_mod.anc: color_mod.mod color_mod.smod
ray_mod.anc: ray_mod.mod ray_mod.smod

rtweekend_funs.anc: rtweekend_mod.anc
vec3_funs.anc: vec3_mod.anc
color_funs.anc: color_mod.anc
ray_funs.anc: ray_mod.anc

vec3_mod.mod vec3_mod.smod rtweekend_mod.mod rtweekend_mod.smod color_mod.mod color_mod.smod ray_mod.mod ray_mod.smod: 

# main.anc: one.anc
# one@proc.anc: two.anc
# two@proc.anc: one.anc

# one@proc.anc: one.anc
# two@proc.anc: two.anc

# one.anc: one.mod one.smod
# two.anc: two.mod two.smod
# one.mod one.smod two.mod two.smod:
