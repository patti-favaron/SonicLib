#!/usr/bin/python

import glob
import sys
import os
import gzip

if len(sys.argv) != 3 and len(sys.argv) != 4:
	
	print "mf3.py - Convert from Meteoflux V3.x to SonicLib standard format"
	print ""
	print "Usage:"
	print ""
	print "  ./mf3.py <Input_Path> [<Config_File>] <Output_Path>"
	print ""
	print "Copyright 2013 by Mauri Favaron"
	print "                  All rights reserved"
	print ""
	print "This is open source software, under license ..."
	print ""
	sys.exit(1)
	
if len(sys.argv) == 3:
	inputPath  = sys.argv[1]
	cfgFile    = ""
	outputPath = sys.argv[2]
else:
	inputPath  = sys.argv[1]
	cfgFile    = sys.argv[2]
	outputPath = sys.argv[3]
	
files = sorted(glob.glob(inputPath + "/*.*"))

for file in files:
	
	# Process temporary file and remove it
	out = file+".csv"
	if cfgFile == "":
		os.system("./mf3 " + file + " " + out)
	else:
		os.system("./mf3 " + file + " " + cfgFile + " " + out)
	os.system("mv " + out + " " + outputPath)
	
	# Show progress
	print "Processed: " + out
