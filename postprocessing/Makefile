TARGET  := tamip-cmor.x
SRCS    := cmor-auxiliary.F90 tamip-cmor.F90 
OBJS    := ${SRCS:.F90=.o} 

FC      = ifort
CCFLAGS = -traceback -g -check all
LDFLAGS = -L/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib/ -L/nobackup/rossby15/sm_maeva/software/cmor/cmor-install/lib/
LIBS    = -lnetcdff -lcmor

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
	-rm -f *~ *.o $(TARGET)

distclean: clean
