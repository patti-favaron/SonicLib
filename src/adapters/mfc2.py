#!/usr/bin/python

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
	os.system("./mfc2 " + tempFile + " test.cfg " + out)
	os.system("mv " + out + " " + outputPath)
	os.remove(tempFile)
	
	# Show progress
	print "Processed: " + file
