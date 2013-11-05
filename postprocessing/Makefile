TARGET  := tamip-cmor.x
SRCS    := cmor-auxiliary.F90 tamip-cmor.F90
OBJS    := ${SRCS:.F90=.o} 

FC      = ifort
FCFLAGS = -traceback -g -check all -I/nobackup/rossby15/sm_maeva/software/cmor-ifort/cmor-install/include -I/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/include/
LDFLAGS = -L/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib -L/nobackup/rossby15/sm_maeva/software/cmor-ifort/cmor-install/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor-ifort/libuuid/install/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor-ifort/udunits2/install/lib/
LIBS    = -lnetcdf -lnetcdff -lcmor -luuid -ludunits2 -lexpat

.PHONY: all clean distclean print_vars

all: ${TARGET}

print_vars:
	@echo TARGET=$(TARGET)
	@echo OBJS=$(OBJS)
	@echo SRCS=$(SRCS)

$(TARGET): $(OBJS) 
	$(FC) $(LDFLAGS) -o $@ $^ $(LIBS) 

$(OBJS): %.o: %.F90
	$(FC) $(FCFLAGS) -o $@ -c $< 

clean: 
	-rm -f *.o *.mod $(TARGET) *.x

test.o: test.F90
	ifort -traceback -g -check all -I/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/include/ -o test.o -c test.F90

test: test.o
	ifort -L/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib -o test.x test.o -lnetcdf -lnetcdff

read_4d2.o: read_4d2.F90
	ifort -traceback -g -check all -I/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/include/ -o read_4d2.o -c read_4d2.F90

read_4d2: read_4d2.o
	ifort -L/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib -o read_4d2.x read_4d2.o -lnetcdf -lnetcdff

distclean: clean

# LD_LIBRARY_PATH=/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib ./test.x
