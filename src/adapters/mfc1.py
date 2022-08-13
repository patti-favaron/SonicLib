#!/usr/bin/python

import glob
import sys
import os

files = glob.glob("../DataSets/MeteofluxCore_V1/Vertemate/*.*r")

for file in files:
	out = file[0:len(file)-1]+".csv"
	os.system("./mfc1 " + file + " test.cfg " + out)
