.SUFFIXES: 

FC=gfortran
COMPILE.f08 = $(FC) $(FCFLAGS) $(OPTFLAGS) $(TARGET_ARCH) -c

SOURCES= \
main.f90 \
hittable_mod.f90 \
sphere_mod.f90 \
color_mod.f90 \
ray_mod.f90 \
rtweekend_mod.f90 \
vec3_mod.f90 \
hittable_funs.f90 \
sphere_funs.f90 \
color_funs.f90 \
ray_funs.f90 \
rtweekend_funs.f90 \
vec3_funs.f90

ifeq ($(DEBUG),)
	OPTFLAGS += -O2
else
	OPTFLAGS += -O0 -g
endif

main.out: $(subst .f90,.o,$(SOURCES))
	$(FC) $(FCFLAGS) -o $@ $+

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod *.ppm main.out

%.o: %.f90
	$(COMPILE.f08) -o $*.o $<

main.o: rtweekend_mod.o vec3_mod.o color_mod.o ray_mod.o
color_mod.o: vec3_mod.o
color_funs.o: rtweekend_funs.o color_mod.o vec3_mod.o
hittable_mod.o: vec3_mod.o ray_mod.o
hittable_funs.o: ray_mod.o vec3_mod.o hittable_mod.o
ray_mod.o: rtweekend_mod.o vec3_mod.o
ray_funs.o: ray_mod.o
rtweekend_mod.o: 
rtweekend_funs.o: rtweekend_mod.o
sphere_mod.o: hittable_mod.o vec3_mod.o ray_mod.o
sphere_funs.o: ray_mod.o sphere_mod.o
vec3_mod.o: rtweekend_mod.o 
vec3_funs.o: vec3_mod.o ray_mod.o

