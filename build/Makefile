FCC = gfortran
libroot = /usr/local/crtm_v2.3.0
FCFLAGS = -I${libroot}/include
LDFLAGS = -L${libroot}/lib
all:
	${FCC} -O0 -g  ../src/main.f90 ${FCFLAGS} ${LDFLAGS} -lcrtm -o InspCloudCoeff.x
