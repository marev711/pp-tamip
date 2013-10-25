import datetime
import glob
import os
import pdb
import re
import subprocess
import sys
import xml.dom.minidom

def command_launch(command, local_bins=None, log_handle=None):
    """ @brief Wrapper to execute commands from a shell
        @param command Full path to the command execute
        @param local_bins Path to the postprocessing specific binaries
        @param log_handle Handle wo which std_out/err is written
               Default 'None' indicates no logging

        This wrapper will take an commandline, executes it and writes
        std_out/std_err to a log handle. 
    """
    std_outerr = None
    if (log_handle != None):
        log_handle.write(str(datetime.datetime.now()) + ": " + command + "\n")
        if local_bins:
            command = 'PATH=${PATH}:' + local_bins + " " + command
        run_application = subprocess.check_call(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        #run_application.wait()
        #std_outerr = run_application.communicate()[0]
        #log_handle.write(std_outerr)

    return std_outerr

def parse_xml(xml_file):
    pr = {}
    parse_tree = xml.dom.minidom.parse(xml_file) 
    for node in parse_tree.getElementsByTagName("definition")[0].childNodes:
        if node.nodeType == node.ELEMENT_NODE:
            value = node.firstChild.nodeValue.strip()
            key = node.tagName
            pr[key] = value
    return pr

def translate_template(nml_replacements, template, target):
    fnml = open(template, "r")
    nml = fnml.readlines()
    fnml.close()

    if (os.path.exists(target)):
        os.remove(target)
    fnml = open(target, "w")
    for nml_line in nml:
        for nml_rep in nml_replacements.keys():
            if re.search(nml_rep, nml_line) != None:
                nml_line = re.sub("PLACE_HOLDER", nml_replacements[nml_rep], nml_line)
        if re.search("Skip entry", nml_line) == None:
            fnml.write(nml_line)
    fnml.close()

def split_ICM_files(grib_files):
    TMIP_regex = re.compile('ICM..TM([0-9]{2})\+(200[89])([0-9]{2})')
    TMIP_index = TMIP_regex.search(grib_files[0]).group(1)
    TMIP_year = TMIP_regex.search(grib_files[0]).group(2)
    TMIP_month = TMIP_regex.search(grib_files[0]).group(3)

    if len(grib_files) == 1:  #  The normal case, a single grib file where we split the entries
        cdo_splitparam = "cdo splitparam " + grib_files[0] + " split"
        cdo_command = command_launch(cdo_splitparam, log_handle=sys.stdout)
        subprocess.check_call(["rename split '' split*"], shell=True)
    elif len(grib_files) == 2:  # Special case if grib files are split across two months
        TMIP_index2 = TMIP_regex.search(grib_files[1]).group(1)
        TMIP_year2 = TMIP_regex.search(grib_files[1]).group(2)
        TMIP_month2 = TMIP_regex.search(grib_files[1]).group(3)

        cdo_splitparam = "cdo splitparam " + grib_files[0] + " " + TMIP_month + "."
        cdo_command = command_launch(cdo_splitparam, log_handle=sys.stdout)
        cdo_splitparam = "cdo splitparam " + grib_files[1] + " " + TMIP_month2 + "."
        cdo_command = command_launch(cdo_splitparam, log_handle=sys.stdout)
        split_files = glob.glob(TMIP_month + '*')
        for split_file in split_files:
            other_split_file = re.sub("^" + TMIP_month, TMIP_month2, split_file)
            merged_file = re.sub("^" + TMIP_month + "\.", "", split_file)
            cdo_mergetime = "cdo mergetime " + split_file + " " + other_split_file + " " + merged_file
            cdo_command = command_launch(cdo_mergetime, log_handle=sys.stdout)
            os.remove(split_file)
            os.remove(other_split_file)
    else:
        print "*EE*: Script can only handle one or two separate GRIB-files..."
        raise
