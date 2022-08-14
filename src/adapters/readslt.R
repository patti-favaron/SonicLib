# readslt.R

# Author:	Werner Eugster
# Date:  	18.09.2011
# Purpose:	Provides R function read.slt() to read binary eddysol flux
#			data files
#			This version reads EddyMeas files from Olaf Kolle's software
#			The format is somewhat different from that of the SLT files from
#			eddysol that Torsten Sachs gave me
#			For reading EddyMeas files specify type="eddymeas" (this is the
#			default) whereas for EddySol files specify type="eddysol"
#
# Modified by Patrizia Favaron - for inclusion in SonicLib;
# prof.Eugster's conventions and naming have been retained. Output in
# standard raw SonicLib file.
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


library(bitops)

read.slt.header <- function (file, type="eddymeas") {

	# -----------------------------------------------------------------
	# Reading The File Header
	# -----------------------------------------------------------------

		# the very first byte contains the number of bytes per record
		# the header then has the same length
	slt.recsize <- readBin(file, "integer", n=1, size=1, signed=FALSE)
	slt.head <- as.data.frame(t(readBin(file, "integer", n=slt.recsize, size=1, signed=FALSE)))
    n <- length(slt.head)
    	# we have only programmed everything up to 7 analog channels, hence
    	# we need to quit if our header shows a longer record size than 22 bytes
    if(n > 22){
      stop("SLT files with records longer than 22 bytes are not supported (yet).")
    }
    if(type=="eddymeas"){
	  colnames(slt.head) <- c("recsize","eddymeas.version","day","month","year.XX","year.YY","hour","minute","an.0.options","an.0.number","an.1.options","an.1.number","an.2.options","an.2.number","an.3.options","an.3.number","an.4.options","an.4.number","an.5.options","an.5.number","an.6.options","an.6.number")[1:n]
    } else {
	  colnames(slt.head) <- c("recsize","start.sec","start.100s","end.hour","end.min","end.sec","end.100s")[1:n]
    }

		# number of records in that file
	slt.head$nrec <- floor(file.info(file)$size/slt.head$recsize)
	
	return(slt.head)
}

read.slt <- function (file, header=NA, type="eddymeas") {

	if(is.na(header[1])){
	  header <- read.slt.header(file, type)
	}
    n <- length(header)-1
	is.eddymeas <- as.logical(length(grep("eddymeas", names(header))))
		# this variable is FALSE in case of eddysol data files and
		# it is TRUE in case of eddymeas files
	
	# -----------------------------------------------------------------
	# Reading All Records In The File
	# -----------------------------------------------------------------

	MAXINT <- 2^15-1
	
	slt <- readBin(file, "integer", n=header$nrec*header$recsize, size=2, signed=FALSE, endian="little")
	
		# changes made on 27.05.2008 and other notes:
		# recsize must be recsize/2 everywhere, since we use 2-byte integers
		# LiT and LiP were reversed in the output of EddySol, which was an error
		# Tv in EddySol is just the value slt[i.sos] and is not yet understood
		# ch4 also has negative values in the output of EddySol
	
	i.u <- seq(header$recsize/2+1,header$nrec*header$recsize/2,header$recsize/2)
	i.v <- seq(header$recsize/2+2,header$nrec*header$recsize/2,header$recsize/2)
	i.w <- seq(header$recsize/2+3,header$nrec*header$recsize/2,header$recsize/2)
	i.sos <- seq(header$recsize/2+4,header$nrec*header$recsize/2,header$recsize/2)
	i.an0 <- seq(header$recsize/2+5,header$nrec*header$recsize/2,header$recsize/2)
	i.an1 <- seq(header$recsize/2+6,header$nrec*header$recsize/2,header$recsize/2)
	i.an2 <- seq(header$recsize/2+7,header$nrec*header$recsize/2,header$recsize/2)
	i.an3 <- seq(header$recsize/2+8,header$nrec*header$recsize/2,header$recsize/2)
	i.an4 <- seq(header$recsize/2+9,header$nrec*header$recsize/2,header$recsize/2)
	i.an5 <- seq(header$recsize/2+10,header$nrec*header$recsize/2,header$recsize/2)
	i.an6 <- seq(header$recsize/2+11,header$nrec*header$recsize/2,header$recsize/2)
	
		# convert the unsigned integers to signed integers (only u, v, w)
	u <- slt[i.u]
	v <- slt[i.v]
	w <- slt[i.w]
	u[u > MAXINT] <- -(2*(MAXINT+1)-u[u > MAXINT])
	v[v > MAXINT] <- -(2*(MAXINT+1)-v[v > MAXINT])
	w[w > MAXINT] <- -(2*(MAXINT+1)-w[w > MAXINT])
	u <- 0.01*u
	v <- 0.01*v
	w <- 0.01*w
	
	if(is.eddymeas){
	  sos <- slt[i.sos]/50.
	  Tv <- sos*sos/1.402/287.64
	} else {
	  Tv <- slt[i.sos]
	  Tv[Tv > MAXINT] <- -(2*(MAXINT+1)-Tv[Tv > MAXINT])
	  Tv <- 0.01*Tv
	}
	
		# nrec includes the header line!
	sampling.rate <- (header$nrec-1)/1800
		# we assume all files are 30-minute files
	etime <- (0:(header$nrec-2))/sampling.rate
	emin <- etime %/% 60
	esec <- etime %% 60
	timestamp <- sprintf("%02d%02d-%02d-%02d %02d:%02d:%06.3f", header$year.XX, header$year.YY, header$month, header$day, header$hour, header$minute+emin, esec)
	
	slt.data <- as.data.frame(cbind(u,v,w,Tv))
	slt.data <- cbind(timestamp, slt.data)
	slt.data$timestamp <- as.character(timestamp)
	
	if(n > 8){
	  an0 <- slt[i.an0]
	  if(bitAnd(header$an.0.options, 1) == 1){	# high resolution option active
	    an0 <- (an0+25000)/10.	# U in mV
	  } else {
	    an0[an0 > MAXINT] <- -(2*(MAXINT+1)-an0[an0 > MAXINT])
	  }
	  slt.data$an0 <- an0
	}
	if(n > 10){
	  an1 <- slt[i.an1]
	  if(bitAnd(header$an.1.options, 1) == 1){	# high resolution option active
	    an1 <- (an1+25000)/10.	# U in mV
	  } else {
	    an1[an1 > MAXINT] <- -(2*(MAXINT+1)-an1[an1 > MAXINT])
	  }
	  slt.data$an1 <- an1
	}
	if(n > 12){
	  an2 <- slt[i.an2]
	  if(bitAnd(header$an.2.options, 1) == 1){	# high resolution option active
	    an2 <- (an2+25000)/10.	# U in mV
	  } else {
	    an2[an2 > MAXINT] <- -(2*(MAXINT+1)-an2[an2 > MAXINT])
	  }
	  slt.data$an2 <- an2
	}
	if(n > 14){
	  an3 <- slt[i.an3]
	  if(bitAnd(header$an.3.options, 1) == 1){	# high resolution option active
	    an3 <- (an3+25000)/10.	# U in mV
	  } else {
	    an3[an3 > MAXINT] <- -(2*(MAXINT+1)-an3[an3 > MAXINT])
	  }
	  slt.data$an3 <- an3
	}
	if(n > 16){
	  an4 <- slt[i.an4]
	  if(bitAnd(header$an.4.options, 1) == 1){	# high resolution option active
	    an4 <- (an4+25000)/10.	# U in mV
	  } else {
	    an4[an4 > MAXINT] <- -(2*(MAXINT+1)-an4[an4 > MAXINT])
	  }
	  slt.data$an4 <- an4
	}
	if(n > 18){
	  an5 <- slt[i.an5]
	  if(bitAnd(header$an.5.options, 1) == 1){	# high resolution option active
	    an5 <- (an5+25000)/10.	# U in mV
	  } else {
	    an5[an5 > MAXINT] <- -(2*(MAXINT+1)-an5[an5 > MAXINT])
	  }
	  slt.data$an5 <- an5
	}
	if(n > 20){
	  an6 <- slt[i.an6]
	  if(bitAnd(header$an.6.options, 1) == 1){	# high resolution option active
	    an6 <- (an6+25000)/10.	# U in mV
	  } else {
	    an6[an6 > MAXINT] <- -(2*(MAXINT+1)-an6[an6 > MAXINT])
	  }
	  slt.data$an6 <- an6
	}

	return(slt.data)
}


