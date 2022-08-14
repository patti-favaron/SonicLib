#!/usr/bin/env python3

#------------------------------------------------------------------
# usa1.py
#
# Script for reading USA-1 data collected using WindRecorder
# loggers to SonicLib form.
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

import os
import sys
import glob

if __name__ == "__main__":

	# Get parameters
	if len(sys.argv) != 3:
		print("usa1.py - System for converting USA-1 WindRecorder data")
		print()
		print("Usage:")
		print()
		print("  ./usa1.py <in_data_dir> <out_data_dir>")
		print()
		print("This is part of the SonicLib project, and open-source")
		print()
		print("by: Patrizia Favaron")
		print()
		sys.exit(1)
	inPath  = sys.argv[1]
	outPath = sys.argv[2]
	
	# Get list of data paths
	dataPaths = sorted(glob.glob(os.path.join(inPath, "*")))
	for path in dataPaths:
	
		files = sorted(glob.glob(os.path.join(path, "*")))
		
		for file in files:
			
			f = open(file, "r")
			lines = f.readlines()
			f.close()
				
			fileName = os.path.basename(file)
			fullName = os.path.join(outPath, fileName + ".csv")
			
			g = open(fullName, "w")
			g.write("u,v,w,t\n")
			valid = 0
			for line in lines:
				if line[0:1] == 'M' and len(line) > 3:
					sv = float(line[5:11])  / 100.0
					su = float(line[15:21]) / 100.0
					sw = float(line[25:31]) / 100.0
					st = float(line[35:41]) / 100.0
					valid += 1
					g.write("%6.2f,%6.2f,%6.2f,%6.2f\n" % (float(su), float(sv), float(sw), float(st)))
			g.close()

			print("%s: %7.3f valid",(fileName, 100.0*valid/len(lines)))
				
