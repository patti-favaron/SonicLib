#!/usr/bin/python

# mfc2_usonic3.py
#
# Script convert multi-file Usonic-3 Meteoflux Core V2 data to SonicLib
#
# Written by: Patrizia Favaron
# e-mail:     patti.favaron@gmail.com
#
#------------------------------------------------------------------
# Statement of Licensing Conditions
#------------------------------------------------------------------
#
# Copyright 2022 Università degli Studi di Milano
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

if len(sys.argv) != 4:
	
	print "mfc2.py - Convert from Meteoflux Core V2 to SonicLib standard format"
	print ""
	print "Usage:"
	print ""
	print "  ./mfc2.py <Input_Path> <Config_File> <Output_Path>"
	print ""
	print "Copyright 2012 by Mauri Favaron"
	print "                  All rights reserved"
	print ""
	print "This is open source software, under license ..."
	print ""
	sys.exit(1)
	
inputPath  = sys.argv[1]
cfgFile    = sys.argv[2]
outputPath = sys.argv[3]
	
files = glob.glob(inputPath + "/*.*gz")

for file in sorted(files):
	
	# Decompress file to read to temporary copy
	tempFile = file[0:len(file)-3]
	try:
		inFile = gzip.open(file, "rb")
		outFile = open(tempFile, "wb")
		outFile.writelines(inFile)
		outFile.close()
		inFile.close()
	except:
		pass

	# Process temporary file and remove it
	out = tempFile[0:len(tempFile)-1]+".csv"
	os.system("./mfc2_usonic3 " + tempFile + " test.cfg " + out)
	os.system("mv " + out + " " + outputPath)
	os.remove(tempFile)
	
	# Show progress
	print "Processed: " + file
