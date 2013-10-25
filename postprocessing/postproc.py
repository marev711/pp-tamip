#! /usr/bin/env python

#########################
#
# Name: postproc.py
#
# Purpose: Prepare T-AMIP grib files to be CMORized
#
# Usage: ./postproc.py [--skip-grib-split] <definition_file>
#
# Revision history: 2013-02-28  --  Script created, Martin Evaldsson, Rossby Centre
#
# Contact persons:  martin.evaldsson@smhi.se
#
########################

import argparse
import glob
import os
import pdb
import postproc_aux
import re
import subprocess
import sys


parser = argparse.ArgumentParser(description='Postprocess and CMORize the TAMIP experiment output')
parser.add_argument('-s', '--skip-grib-split',
                    help='Skip the step where the ICMGG-files are split into separate files',
                    action='store_true')
parser.add_argument('-r', '--run-folder',
                    help='Override default TAMIP subfolders (e.g., for running a subset)',
                    nargs='*')
parser.add_argument('definition_file', help='Control file for processing (see def/* for examples)')
args = parser.parse_args()


# CMORize only from this table
current_table = "TAMIP_3hrSlev"

# Where am I?
script_file = os.path.abspath(os.path.join(os.getcwd(), __file__))
base_dir    = re.search("(.*)/.*?", os.path.dirname(script_file)).group(1)
def_dir     = os.path.join(base_dir, "def")
postpr_dir  = os.path.join(base_dir, "postprocessing")
bin_dir     = os.path.join(base_dir, "bin")

# Read defintion file
f = open(args.definition_file, "r")
grib_entries = f.readlines()
f.close()

# Remove comments and empty lines
grib_entries = [entry for entry in grib_entries if not re.search('^#', entry) and len(entry.strip()) > 0]

# Retrieve headers from defintions files
headers = grib_entries[0].split()

# Use extent of headers to figure out column width
end_index = [m.start(0)+1 for m in re.finditer('[HGd](\s*$|\s)', grib_entries[0])]
start_index = [m+1 for m in end_index]
start_index = start_index[0:-1]
start_index.insert(0,0)
indices = zip(start_index, end_index)

# Extract entries from each row
entries = [[grib_entry[start:end].strip() for start,end in indices] for grib_entry in grib_entries[1:]]

# Couple headers to each entry
params = [dict(zip(headers, param)) for param in entries]

# Remove all but 'current_table'-table entries
params = [entry for entry in params if re.search(current_table, entry['table_id']) != None]

# Remove tables that happen to occure together with 'current_table'-table entries
for param_id in range(len(params)):
    params[param_id]['table_id'] = re.sub(".*(" + current_table + ").*", r'\1', params[param_id]['table_id'])


# Couple TAMIP dates with their starting time (hours) and realisaion no
f = open(os.path.join(base_dir, "def/TAMIP_experiment_info.txt"), "r")
experiment_info_raw = f.readlines()
experiment_info = [entry for entry in experiment_info_raw if not re.search('^#', entry)]
dates = [date_hh.split()[0] for date_hh in experiment_info]
hours = [date_hh.split()[1] for date_hh in experiment_info]
realizations = [date_hh.split()[2] for date_hh in experiment_info]
experiment_ids = [date_hh.split()[3] for date_hh in experiment_info]

date_hh = dict(zip(dates, hours))
date_rea = dict(zip(dates, realizations))
date_exp = dict(zip(dates, experiment_ids))

# All experiments
experiment_folder = "/nobackup/rossby15/rossby/joint_exp/tamip"
run_folders = ["TMIP_2009-08-02", "TMIP_2009-08-01", "TMIP_2009-07-31",
               "TMIP_2009-07-30", "TMIP_2009-07-28", "TMIP_2009-07-27",
               "TMIP_2009-07-26", "TMIP_2009-07-25", "TMIP_2009-07-23",
               "TMIP_2009-07-22", "TMIP_2009-07-21", "TMIP_2009-07-20",
               "TMIP_2009-07-18", "TMIP_2009-07-17", "TMIP_2009-07-16",
               "TMIP_2009-07-15", "TMIP_2009-05-03", "TMIP_2009-05-02",
               "TMIP_2009-05-01", "TMIP_2009-04-30", "TMIP_2009-04-28",
               "TMIP_2009-04-27", "TMIP_2009-04-26", "TMIP_2009-04-25",
               "TMIP_2009-04-23", "TMIP_2009-04-22", "TMIP_2009-04-21",
               "TMIP_2009-04-20", "TMIP_2009-04-18", "TMIP_2009-04-17",
               "TMIP_2009-04-16", "TMIP_2009-04-15", "TMIP_2009-02-02",
               "TMIP_2009-02-01", "TMIP_2009-01-31", "TMIP_2009-01-30",
               "TMIP_2009-01-28", "TMIP_2009-01-27", "TMIP_2009-01-26",
               "TMIP_2009-01-25", "TMIP_2009-01-23", "TMIP_2009-01-22",
               "TMIP_2009-01-21", "TMIP_2009-01-20", "TMIP_2009-01-18",
               "TMIP_2009-01-17", "TMIP_2009-01-16", "TMIP_2009-01-15",
               "TMIP_2008-11-02", "TMIP_2008-11-01", "TMIP_2008-10-31",
               "TMIP_2008-10-30", "TMIP_2008-10-28", "TMIP_2008-10-27",
               "TMIP_2008-10-26", "TMIP_2008-10-25", "TMIP_2008-10-23",
               "TMIP_2008-10-22", "TMIP_2008-10-21", "TMIP_2008-10-20",
               "TMIP_2008-10-18", "TMIP_2008-10-17", "TMIP_2008-10-16",
               "TMIP_2008-10-15"]

if args.run_folder:
    del run_folders
    run_folders = args.run_folder

for run_folder in run_folders:
    sys.stdout.write("current run_folder=" + run_folder + "\n")
    curr_date = re.sub("TMIP_", "", run_folder)
    os.chdir(os.path.join(experiment_folder, run_folder))
    model_data_folder = os.path.join(experiment_folder, run_folder)
    sys.stdout.write("current model_data_folder=" + model_data_folder + "\n")

    # Check which grib files are present in this folder
    grib_files = glob.glob('ICMGG*200*')


    # Split IFS-grib file(s) into separate grib files named "paramId.table2Version.grb"
    if not args.skip_grib_split:
        postproc_aux.split_ICM_files(grib_files)

    for param in params:
        sys.stdout.write("current param=" + str(param) + "\n")
        os.chdir(os.path.join(experiment_folder, run_folder))
        xml_def_file = os.path.join(def_dir, param['table_id'], param['variablesGG'] + ".def")
        if (os.path.exists(xml_def_file)):
            param_def_file = postproc_aux.parse_xml(xml_def_file)

            if param_def_file.has_key("write_namelist"):
                namelist = param_def_file["write_namelist"].split('\n')

            if param_def_file.has_key("command_block"):
                command_block = param_def_file["command_block"].split('\n')
            else:
                sys.stderr.write("No command_block in " + xml_def_file + "\n")
                raise

            # Execute command block in current definition file
            for command_block_line in command_block:
                command = command_block_line
                pdb.set_trace()
                command_return = postproc_aux.command_launch(command, local_bins=bin_dir, log_handle=sys.stdout)

            # Set some attributes from xml-def file
            if param_def_file.has_key("post_cdo_units"):
                units = param_def_file["post_cdo_units"].split('\n')[0]
            else:
                units = param['unitsGG_old']

            if param_def_file.has_key("original_name"):
                original_name = param_def_file["original_name"].split('\n')[0]
            else:
                original_name = param['namesGG_old']

            if param_def_file.has_key("cmor_var_positive"):
                positive = param_def_file["cmor_var_positive"]
            else:
                positive = "Skip entry"

        # Fix reference time using the TAMIP_experiment_info.txt-file
        curr_file = param['variablesGG'] + ".nc"
        curr_temp= param['variablesGG'] + "_tmp.nc"
        cdo_setreftime = "cdo setreftime," + curr_date + "," + date_hh[curr_date] + ":00 " + curr_file + " " + curr_temp
        cdo_command = postproc_aux.command_launch(cdo_setreftime, log_handle=sys.stdout)
        os.rename(curr_temp, curr_file)

        sys.stdout.write("curr_file=" + curr_file + "\n")
        # Update the CMOR namelist file (cmor.nml)
        os.chdir(os.path.join(postpr_dir))
        nml_replacements = {"cmor_varname"  : param['namesGG'],
                            "curr_file"     : os.path.join(model_data_folder, curr_file),
                            "experiment_id" : date_exp[curr_date],
                            "history"       : "PLACE_HOLDER",
                            "inpath"        : os.path.join(def_dir, param['table_id']),
                            "model_units"   : units,  # either from "unitsGG_old" or "cdo command block"
                            "model_varname" : param['namesGG_old'],
                            "original_name" : original_name,
                            "positive"      : positive,
                            "realization"   : date_rea[curr_date],
                            "table_id"      : param['table_id']}


        postproc_aux.translate_template(nml_replacements, template="cmor.nml.tmpl", target="cmor.nml")

        # Run CMOR
        subprocess.check_call(
                 "LD_LIBRARY_PATH=/software/apps/netcdf/4.2/i1214-hdf5-1.8.9/lib:/nobackup/rossby15/sm_maeva/software/cmor-ifort/libuuid/install/lib ./tamip-cmor.x",
                  shell=True, stdout=subprocess.PIPE)
    #sys.exit(1)
