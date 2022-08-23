#!/usr/bin/python

#------------------------------------------------------------------
# mf3.py
#
# Script, driving multi-file conversion of MeteoFlux 3.0 data to SonicLib form.
#
# Written by: Patrizia Favaron
# e-mail:     patti.favaron@gmail.com
#
# With many thanks to the Environmental Physics research group
# within the Physics Department "Aldo Pontremoli" of the
# University of Milan for their help, support, and encouragement.
#
#------------------------------------------------------------------
# Statement of Licensing Conditions
#------------------------------------------------------------------
#
# Copyright 2022 Patrizia Favaron
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
#------------------------------------------------------------------

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
	print "This is open source software, under MIT license"
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
