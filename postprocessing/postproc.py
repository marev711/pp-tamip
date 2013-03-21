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

import glob
import os
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

# Get string start and end indices (from extent of headers)
end_index = [m.start(0)+1 for m in re.finditer('[HGd](\s*$|\s)', grib_entries[0])]
start_index = [m+1 for m in end_index]
start_index = start_index[0:-1]
start_index.insert(0,0)
indices = zip(start_index, end_index)

# Extract entries from each row
entries = [[grib_entry[start:end].strip() for start,end in indices] for grib_entry in grib_entries[1:]]

# Couple headers to each entry
params = [dict(zip(headers, param)) for param in entries]

f = open("../def/TAMIP_dates_and_hours.txt", "r")
dates_hours = f.readlines()
dates = [date_hh.split()[0] for date_hh in dates_hours]
hours = [date_hh.split()[1] for date_hh in dates_hours]
date_hh = dict(zip(dates, hours))

base_folder = "/nobackup/rossby15/rossby/joint_exp/tamip"
run_folders = ("TMIP_2008-10-15", "TMIP_2008-10-16", "TMIP_2008-10-17", 
               "TMIP_2008-10-18", "TMIP_2008-10-20", "TMIP_2008-10-21", 
               "TMIP_2008-10-22", "TMIP_2008-10-23", "TMIP_2008-10-25", 
               "TMIP_2008-10-26", "TMIP_2008-10-27", "TMIP_2008-10-28", 
               "TMIP_2008-10-30", "TMIP_2008-10-31", "TMIP_2008-11-01", 
               "TMIP_2008-11-02", "TMIP_2009-01-15", "TMIP_2009-01-16", 
               "TMIP_2009-01-17", "TMIP_2009-01-18", "TMIP_2009-01-20", 
               "TMIP_2009-01-21", "TMIP_2009-01-22", "TMIP_2009-01-23", 
               "TMIP_2009-01-25", "TMIP_2009-01-26", "TMIP_2009-01-27", 
               "TMIP_2009-01-28", "TMIP_2009-01-30", "TMIP_2009-01-31", 
               "TMIP_2009-02-01", "TMIP_2009-02-02", "TMIP_2009-04-15", 
               "TMIP_2009-04-16", "TMIP_2009-04-17", "TMIP_2009-04-18", 
               "TMIP_2009-04-20", "TMIP_2009-04-21", "TMIP_2009-04-22", 
               "TMIP_2009-04-23", "TMIP_2009-04-25", "TMIP_2009-04-26", 
               "TMIP_2009-04-27", "TMIP_2009-04-28", "TMIP_2009-04-30", 
               "TMIP_2009-05-01", "TMIP_2009-05-02", "TMIP_2009-05-03", 
               "TMIP_2009-07-15", "TMIP_2009-07-16", "TMIP_2009-07-17", 
               "TMIP_2009-07-18", "TMIP_2009-07-20", "TMIP_2009-07-21", 
               "TMIP_2009-07-22", "TMIP_2009-07-23", "TMIP_2009-07-25", 
               "TMIP_2009-07-26", "TMIP_2009-07-27", "TMIP_2009-07-28", 
               "TMIP_2009-07-30", "TMIP_2009-07-31", "TMIP_2009-08-01", 
               "TMIP_2009-08-02")

run_folder = run_folders[1]
curr_date = re.sub("TMIP_", "", run_folder)
os.chdir(os.path.join(base_folder, run_folder))
gg_files = glob.glob('ICMGG*200*')
TMIP_match = re.search('ICMGGTM([0-9]{2})\+(200[89])([0-9]{2})', gg_files[0])
TMIP_index = TMIP_match.group(1)
TMIP_year = TMIP_match.group(2)
TMIP_month = TMIP_match.group(3)


if len(gg_files) == 1:  #  The normal case, a singel grib file where we split the entries
    cdo_splitparam = "cdo splitparam " + gg_files[0] + " split"
    cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
    subprocess.check_call(["rename split '' split*"], shell=True)
elif len(gg_files) == 2:  # Special case if grib files are split across two months
    TMIP_match2 = re.search('ICMGGTM([0-9]{2})\+(200[89])([0-9]{2})', gg_files[1])
    TMIP_index2 = TMIP_match2.group(1)
    TMIP_year2 = TMIP_match2.group(2)
    TMIP_month2 = TMIP_match2.group(3)
    print TMIP_index2, TMIP_year2, TMIP_month2
    cdo_splitparam = "cdo splitparam " + gg_files[0] + " " + TMIP_month + "."
    cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
    cdo_splitparam = "cdo splitparam " + gg_files[1] + " " + TMIP_month2 + "."
    cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
    split_files = glob.glob(TMIP_month + '*')
    for split_file in split_files:
        other_split_file = re.sub(TMIP_month, TMIP_month2, split_file)
        merged_file = re.sub("^" + TMIP_month + "\.", "", split_file)
        cdo_mergetime = "cdo mergetime " + split_file + " " + other_split_file + " " + merged_file
        cdo_command = postproc_aux.cdo_launch(cdo_mergetime, log_handle=sys.stdout)
        os.remove(split_file)
        os.remove(other_split_file)
else:
    print "*EE*: Script can only handle two separate GRIB-files..."
    raise

for param in params:
    cdo_grb2nc_parSel = "cdo -R -r -f nc -t ecmwf copy " + param['variablesGG'] + ".grb " + param['variablesGG'] + "_tmp.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_grb2nc_parSel, log_handle=sys.stdout)

    cdo_3h_mean = "cdo timselmean,4 " + param['variablesGG'] + "_tmp.nc " + param['variablesGG'] + "_tmp_mean.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_3h_mean, log_handle=sys.stdout)

    cdo_setreftime = "cdo setreftime," + curr_date + "," + date_hh[curr_date] + ":00 " + param['variablesGG'] + "_tmp_mean.nc " + param['variablesGG'] + "_tmp_mean_reftime0.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_setreftime, log_handle=sys.stdout)

    sys.exit(1)
