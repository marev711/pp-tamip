#!/bin/ksh
#
# @ cpu_limit = 3600
# @ job_type = serial
# @ resources = ConsumableCpus(1) ConsumableMemory(700Mb)
# @ node = 1
# @ tasks_per_node   = 1
# @ shell = /usr/bin/ksh
# @ core_limit = 4096
# @ notification = error
# @ restart = no
# @ class = ns
# @ account_no = ecearth
# @ output = compile.gribtocmor.$(jobid)
# @ error  = compile.gribtocmor.$(jobid)
# @ queue

set -uxav

WORKDIR=$PERM/GRIBTOCMOR/comp
cd $WORKDIR

xlf90_r -g -O3 -qstrict -b64 -qsource $WORKDIR/gribtocmor.F90 -o $WORKDIR/gribtocmor.exe -L /home/rd/neu/emos/lib -lemos -L/home/rd/neu/CMOR2/lib -lcmor -I /home/rd/neu/CMOR2/include -I/usr/local/apps/netcdf/3.6.3/LP64/include -L/usr/local/apps/netcdf/3.6.3/LP64/lib -lnetcdf -L/home/rd/neu/udunits/udunits-2.1.8/lib -ludunits2 -lexpat -I/home/rd/neu/udunits/udunits-2.1.8/include -L/home/rd/neu/uuid/uuid-1.6.2/lib -luuid -I/home/rd/neu/uuid/uuid-1.6.2/include

