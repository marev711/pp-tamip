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

# Where am I?
script_file = os.path.abspath(os.path.join(os.getcwd(), __file__))
base_dir  = re.search("(.*)/.*?", os.path.dirname(script_file)).group(1)

# Input file
if len(sys.argv) != 2:
    print "Usage: ./postproc.py <defintion_file>"
    sys.exit(1)
definition_file = sys.argv[1]

# Read all lines
f = open(definition_file, "r")
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

# Remove all but 3hrSlev-table entries
params = [entry for entry in params if re.search("TAMIP_3hrSlev", entry['table_id']) != None]

# Remove tables that happen to occure together with 3hrSlev-table entries
for param_id in range(len(params)):
    params[param_id]['table_id'] = re.sub(".*(TAMIP_3hrSlev).*", r'\1', params[param_id]['table_id'])


f = open(os.path.join(base_dir, "def/TAMIP_experiment_info.txt"), "r")
exp_info_raw = f.readlines()
exp_info = [entry for entry in exp_info_raw if not re.search('^#', entry)]
dates = [date_hh.split()[0] for date_hh in exp_info]
hours = [date_hh.split()[1] for date_hh in exp_info]
realizations = [date_hh.split()[2] for date_hh in exp_info]

date_hh = dict(zip(dates, hours))
date_rea = dict(zip(dates, realizations))

model_data_folder = "/nobackup/rossby15/rossby/joint_exp/tamip"
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
os.chdir(os.path.join(model_data_folder, run_folder))
inpath = os.path.join(model_data_folder, run_folder)
curr_file = "44.128_tmp_mean_reftime0.nc"

# Check which grib files are present in this folder
grib_files = glob.glob('ICMGG*200*')
TMIP_match = re.search('ICM..TM([0-9]{2})\+(200[89])([0-9]{2})', grib_files[0])
TMIP_index = TMIP_match.group(1)
TMIP_year = TMIP_match.group(2)
TMIP_month = TMIP_match.group(3)


# Split IFS-grib files into files named "paramId.table2Version"
## if len(grib_files) == 1:  #  The normal case, a single grib file where we split the entries
##     cdo_splitparam = "cdo splitparam " + grib_files[0] + " split"
##     cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
##     subprocess.check_call(["rename split '' split*"], shell=True)
## elif len(grib_files) == 2:  # Special case if grib files are split across two months
##     TMIP_match2 = re.search('ICM??TM([0-9]{2})\+(200[89])([0-9]{2})', grib_files[1])
##     TMIP_index2 = TMIP_match2.group(1)
##     TMIP_year2 = TMIP_match2.group(2)
##     TMIP_month2 = TMIP_match2.group(3)
##     print TMIP_index2, TMIP_year2, TMIP_month2
##     cdo_splitparam = "cdo splitparam " + grib_files[0] + " " + TMIP_month + "."
##     cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
##     cdo_splitparam = "cdo splitparam " + grib_files[1] + " " + TMIP_month2 + "."
##     cdo_command = postproc_aux.cdo_launch(cdo_splitparam, log_handle=sys.stdout)
##     split_files = glob.glob(TMIP_month + '*')
##     for split_file in split_files:
##         other_split_file = re.sub(TMIP_month, TMIP_month2, split_file)
##         merged_file = re.sub("^" + TMIP_month + "\.", "", split_file)
##         cdo_mergetime = "cdo mergetime " + split_file + " " + other_split_file + " " + merged_file
##         cdo_command = postproc_aux.cdo_launch(cdo_mergetime, log_handle=sys.stdout)
##         os.remove(split_file)
##         os.remove(other_split_file)
## else:
##     print "*EE*: Script can only handle one or two separate GRIB-files..."
##     raise

for param in params:
    param_def_file = os.path.join(base_dir, "def", param['variablesGG'] + ".def")
    if (os.path.exists(param_def_file)):
        param_file = open(param_def_file, "r")
        param_def = param_file.readlines()
        for param_def_line in param_def:
            cdo_command = param_def_line
            cdo_return = postproc_aux.cdo_launch(cdo_command, log_handle=sys.stdout)

    cdo_setreftime = "cdo setreftime," + curr_date + "," + date_hh[curr_date] + ":00 " + param['variablesGG'] + "_tmp_mean.nc " + param['variablesGG'] + "_tmp_mean_reftime0.nc"
    cdo_command = postproc_aux.cdo_launch(cdo_setreftime, log_handle=sys.stdout)

    nml_replacements = {"cmor_varname"  : param['namesGG'], 
                        "model_units"   : param['unitsGG_old'], 
                        "model_varname" : param['variablesGG'], 
                        "curr_file"     : param['variablesGG'], 
                        "inpath"        : param['variablesGG'], 
                        "experiment_id" : "tamip" + re.sub("-", "", curr_date[0:8]), 
                        "history"       : "PLACE_HOLDER", 
                        "realization"   : date_rea[curr_date], 
                        "table_id"      : param['table_id']}
    fnml = open(os.path.join(base_dir, "postprocessing/cmor.nml.tmpl"), "r")
    nml = fnml.readlines()
    fnml.close()

    os.remove(os.path.join(base_dir, "postprocessing/cmor.nml"))
    fnml = open(os.path.join(base_dir, "postprocessing/cmor.nml"), "w")
    for nml_line in nml:
        for nml_rep in nml_replacements.keys():
            if re.search(nml_rep, nml_line) != None:
                nml_line = re.sub("PLACE_HOLDER", nml_replacements[nml_rep], nml_line)
        fnml.write(nml_line)
    fnml.close()

    os.chdir(os.path.join(base_dir, "postprocessing"))
    subprocess.check_call(
             "LD_LIBRARY_PATH=/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib:/nobackup/rossby15/sm_maeva/software/cmor-ifort/libuuid/install/lib ./tamip-cmor.x",
              shell=True, stdout=subprocess.PIPE)
    sys.exit(1)
