#!/usr/bin/python

import sys
import time

f = open("./20190701.12o", "r")
data = f.readlines()
f.close()

clean = []
for l in data:
	if l[1:4] == "M:x":
		clean.append(l.split('"')[1])

while True:

	for data_line in clean:
	
		print(data_line)
		time.sleep(0.1)				