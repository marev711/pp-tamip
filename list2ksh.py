#! /usr/bin/env python
import shlex
import pdb

# Read all lines
f = open("gg.def", "r")
grib_entries = f.readlines()
f.close()

reformatted_entries = []
for grib_entry in grib_entries:
    curr_entry = grib_entry.split()
    # Merge last entry to single element (was probably split by above line)
    if len(curr_entry) > 3:
        curr_entry[3] = " ".join(curr_entry[3:])
        del curr_entry[4:]
        # Add double quotes to some entries
        curr_entry = [curr_entry[0], '"' + curr_entry[1] + '"', 
                                     '"' + curr_entry[2] + '"', 
                                     '"' + curr_entry[3] + '"']
    else:
        curr_entry.append('""')
    reformatted_entries.append(curr_entry)
print reformatted_entries



