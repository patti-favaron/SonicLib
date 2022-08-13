#!/usr/bin/env python3

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
		print("by: Mauri Favaron")
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
				
