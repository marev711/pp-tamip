#! /usr/bin/env python

#########################
# 
# Name: postproc.py
#
# Purpose: Prepare T-AMIP grib files to be CMORized
#
# Usage: ./postproc.py <definition_file>
#
# Revision history: 2013-02-28  --  Script created, Martin Evaldsson, Rossby Centre
#
# Contact persons:  martin.evaldsson@smhi.se
#
########################

import pdb
import re
import subprocess
import sys
import postproc_aux


# Input file
if len(sys.argv) != 2:
    print "Usage: ./postproc.py <defintion_file>"
    sys.exit(1)
input_file = sys.argv[1]

# Read all lines
f = open(input_file, "r")
grib_entries = f.readlines()
f.close()

# Remove comments and empty lines
grib_entries = [entry for entry in grib_entries if not re.search('^#', entry) and len(entry.strip()) > 0]

# Array headers (index 0)
headers = grib_entries[0].split()

# Get string start and end indices
end_index = [m.start(0)+1 for m in re.finditer('[HGd](\s*$|\s)', grib_entries[0])]
start_index = [m+1 for m in end_index]
start_index = start_index[0:-1]
start_index.insert(0,0)
indices = zip(start_index, end_index)

# Extract entries from each row
entries = [[grib_entry[start:end].strip() for start,end in indices] for grib_entry in grib_entries[1:]]

# Couple headers to each entry
params = [dict(zip(headers, param)) for param in entries]


for param in params:
    cdo_grb2nc_parSel = "cdo -R -r -f nc -t ecmwf copy -selparam," + param['variablesGG'] + " /nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-07-20/ICMGGTM53+200907 tmp.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_grb2nc_parSel, log_handle=sys.stdout)
    cdo_3h_mean = "cdo timselmean,4 tmp.nc tmp_mean.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_3h_mean, log_handle=sys.stdout)
    sys.exit(1)
