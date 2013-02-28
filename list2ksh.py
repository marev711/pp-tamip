#! /usr/bin/env python
import pdb
import re
import sys

# Input file
if len(sys.argv) != 2:
    print "Usage: ./list2ksh.py <input_file>"
    sys.exit(1)
input_file = sys.argv[1]

# Read all lines
f = open(input_file, "r")
grib_entries = f.readlines()
f.close()


# Array headers (index 0) are a special case
reformatted_entries = [grib_entries[0].split()]

# Get string start and end indices
end_index = [m.start(0)+1 for m in re.finditer('[Gd](\s*$|\s)', grib_entries[0])]
start_index = [m+1 for m in end_index]
start_index = start_index[0:-1]
start_index.insert(0,0)
indices = zip(start_index, end_index)

# The remaining entries
for grib_entry in grib_entries[1:]:
    curr_entry = [grib_entry[start:end].strip() for start,end in indices]

    # Add double quotes to some entries
    curr_entry = [curr_entry[0],
                  '"' + curr_entry[1] + '"',
                  '"' + curr_entry[2] + '"',
                  '"' + curr_entry[3] + '"',
                  '"' + curr_entry[4] + '"']
    reformatted_entries.append(curr_entry)

transpose_list = map(list, zip(*reformatted_entries))

# Write transposed list back to ksh-array syntax
for array in transpose_list:
    array_def = "set -A " + array[0] + " "
    array_def_length = len(array_def)

    row_length = 0
    sys.stdout.write(array_def)
    for item in array[1:]:
        sys.stdout.write(item + " ")
        row_length = row_length + len(item)
        if row_length > 75 and array.index(item) < len(array)-1:
            sys.stdout.write(" \\" +'\n' + " " * array_def_length)
            row_length = 0
    sys.stdout.write('\n\n')

