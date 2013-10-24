TARGET  := compute_ccb.x
SRCS    := compute_ccb.f90
OBJS    := ${SRCS:.f90=.o} 

FC      = ifort
CCFLAGS = -traceback -g -check all
FCFLAGS = -I/home/sm_uflad/Projects/gribapi/1.9.9/include -traceback -g -check all
LDFLAGS = -L/home/sm_uflad/Projects/gribapi/1.9.9/lib/
LIBS    = -lgrib_api_f90 -lgrib_api

.PHONY: all clean distclean print_vars

all: ${TARGET}

print_vars:
	@echo TARGET=$(TARGET)
	@echo OBJS=$(OBJS)
	@echo SRCS=$(SRCS)

$(TARGET): $(OBJS) 
	$(FC) $(LDFLAGS) -o $@ $^ $(LIBS) 

$(OBJS): %.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $< 

clean: 
	-rm -f *.mod *.o $(TARGET) *~

distclean: clean
