#!/usr/bin/python

import glob
import os

files = glob.glob("*.RAW")
for fil in sorted(files):
	chg = fil.replace("RAW", "bin")
	cmd = "./ToUsaSim %s %s" % (fil, chg)
	print cmd
	os.system(cmd)
