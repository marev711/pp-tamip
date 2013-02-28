#! /usr/bin/env python
import shlex
import pdb
import sys

padding = 5

# Input file
if len(sys.argv) != 2:
    print "Usage: ./ksh2list.py <input_file>"
    sys.exit(1)
input_file = sys.argv[1]

# Read all lines
f = open(input_file, "r")
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

for row in transpose_list:
    sys.stdout.write(row[0].rjust(max_width[0]+padding))
    sys.stdout.write(row[1].rjust(max_width[1]+padding))
    sys.stdout.write(row[2].rjust(max_width[2]+padding))
    sys.stdout.write(row[3].rjust(max_width[3]+padding))
    sys.stdout.write(row[4].rjust(max_width[4]+padding) + '\n')


