TARGET  := tamip-cmor.x
SRCS    := cmor-auxiliary.F90 tamip-cmor.F90 
OBJS    := ${SRCS:.F90=.o} 

FC      = gfortran
FCFLAGS = -fbacktrace -g -Wall -I/nobackup/rossby15/sm_maeva/software/cmor/cmor-install/include -ffree-line-length-none -I/nobackup/rossby15/sm_maeva/software/netCDF/netCDF-install/include/
LDFLAGS = -L/nobackup/rossby15/sm_maeva/software/netCDF/netCDF-install/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor/cmor-install/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor/libuuid/install/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor/udunits2/install/lib/
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
	-rm -f *.o *.mod $(TARGET)

distclean: clean
