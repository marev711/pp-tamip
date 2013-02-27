#! /usr/bin/env python
import shlex
import pdb

padding = 5
f = open("gg_def_file.def", "r")
all_types = f.readlines()
f.close()

target_list = []
for list_type in all_types:
    target_list.append(shlex.split(list_type))

max_width = [0, 0, 0, 0, 0]
transpose_list = map(list, zip(*target_list))
for row in transpose_list:
    widths = [len(item) for item in row]
    max_width = [max(item) for item in zip(widths, max_width)]

f = open("gg.def", "w")
for row in transpose_list:
    f.write(row[0].rjust(max_width[0]+padding))
    f.write(row[1].rjust(max_width[1]+padding))
    f.write(row[2].rjust(max_width[2]+padding))
    f.write(row[3].rjust(max_width[3]+padding))
    f.write(row[4].rjust(max_width[4]+padding) + '\n')
f.close()


