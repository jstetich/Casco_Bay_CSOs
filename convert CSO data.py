# -*- coding: utf-8 -*-
"""
Created on Wed Sep 19 12:36:16 2018

@author: cbohlen
"""
import re

txtfile = open('CSO outfalls data copied from 2017 CSO Report.txt', 'r')

endwithnonnumeral = re.compile(r'\D$')
startswithnumeral = re.compile(r'^\d')
startswithME =      re.compile(r'^ME')

curline = ''
outlines = []
for line in txtfile.readlines():
    line = line.strip('\n')
    if endwithnonnumeral.search(line):  # last character is non-numeric, probably a city name
        #print 'Line "%s" ends with non-numeral' % line
        if startswithnumeral.search(line):  # you have a number at the start of the line, separated by spaces
            parts = line.split(' ',1)
            #print 'Had a number (%s) at the start of a Line containing characters.' % parts[0]
            curline += ', ' + parts[0]
            start = parts[1]
        else:
            start = line
        outlines.append(curline)
        curline = start
    else:
        curline += ',' + line
txtfile.close()

for line in outlines:
    print line

outfile = open('CSO outfalls2.txt', 'w')
for line in outlines:
    outfile.write(line + '\n')
outfile.close
