#! /usr/bin/env python
import pdb

# Read all lines
f = open("gg.def", "r")
grib_entries = f.readlines()
f.close()

reformatted_entries = [grib_entries[0].split()]
for grib_entry in grib_entries[1:]:
    curr_entry = grib_entry.split()
    # Merge last element to single element (might be split by above line)
    if len(curr_entry) > 3:
        curr_entry[3] = " ".join(curr_entry[3:])
        del curr_entry[4:]
    # Add double quotes to some entries
        curr_entry = [curr_entry[0], '"' + curr_entry[1] + '"', 
                                     '"' + curr_entry[2] + '"', 
                                     '"' + curr_entry[3] + '"']
    else:
        curr_entry = [curr_entry[0], '"' + curr_entry[1] + '"', 
                                     '"' + curr_entry[2] + '"', 
                                     '""']
    reformatted_entries.append(curr_entry)
print(reformatted_entries)
transpose_list = map(list, zip(*reformatted_entries))

# Write transposed list back to ksh-array syntax
f = open("ksh_array_def.ksh", "w")
for array in transpose_list:
    array_def = "set -A " + array[0] + " "
    array_def_length = len(array_def)

    row_length = 0
    f.write(array_def)
    for item in array[1:]:
        f.write(item + " ")
        row_length = row_length + len(item)
        if row_length > 75:
            f.write(" \\" +'\n' + " " * array_def_length)
            row_length = 0
    f.write('\n\n')

