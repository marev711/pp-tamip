import datetime
import os
import pdb
import re
import subprocess
import xml.dom.minidom

def cdo_launch(cdo_command, log_handle=None):
    """ @brief Wrapper to execute CDO commands
        @param cdo_command Full path to the cdo command execute
        @param log_handle Handle wo which std_out/err is written
               Default 'None' indicates no logging

        This wrapper will take an cdo commandline, executes it and writes
        std_out/std_err to a log handle. 
    """
    std_outerr = None
    if (log_handle != None):
        log_handle.write(str(datetime.datetime.now()) + ": " + cdo_command + "\n")
        run_application = subprocess.check_call(cdo_command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
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
        fnml.write(nml_line)
    fnml.close()
