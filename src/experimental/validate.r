#------------------------------------------------------------------
#
# "Embryo" of de(trending/spiking) in SonicLib.
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
# Copyright 2022 by Patrizia Favaron
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

build.means <- function(dir.name=".", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.3, spike.detection.threshold=3, spike.treatment="set.na", verbose=FALSE) {
  
  d.control <- average.sonic.file.set(
    dir.name,
    averaging.time=averaging.time,
    sampling.rate=sampling.rate,
    threshold=threshold,
    delay=delay,
    trend.removal="none",
    spike.detection.threshold=spike.detection.threshold,
    spike.treatment = spike.treatment,
    verbose=verbose
  );
  
  d.detrend <- average.sonic.file.set(
    dir.name,
    averaging.time=averaging.time,
    sampling.rate=sarelative.nonmpling.rate,
    threshold=threshold,
    delay=delay,
    trend.removal="linear",
    spike.detection.threshold=spike.detection.threshold,
    spike.treatment = spike.treatment,
    verbose=verbose
  );
  
  return(list(
    control  = d.control,
    detrend  = d.detrend
  ))
  
}


max.spikes <- function(d) {
  
  max.spikes <- numeric(length(d$data$t.stamp));
  var.names  <- names(d$data);
  spikes.pos <- grep(".spike.count",var.names);
  if(length(spikes.pos) > 0) {
    spikes        <- d$data[,spikes.pos];
    max.spikes    <- apply(spikes,1,max);
    names(max.spikes) <- NULL;
  }
  else {
    max.spikes <- 0;
  }
  return(max.spikes);
  
}


max.absolute.rns <- function(d) {
  
  max.rns <- numeric(length(d$data$t.stamp));
  var.names  <- names(d$data);
  rns.pos <- grep(".rns",var.names);
  if(length(rns.pos) > 0) {
    rns        <- abs(d$data[,rns.pos]);
    max.rns    <- apply(rns,1,max);
    names(max.rns) <- NULL;
  }
  else {
    max.rns <- 0;
  }
  return(max.rns);
  
}

max.nst <- function(d) {
  
  max.val <- numeric(length(d$data$t.stamp));
  var.names  <- names(d$data);
  nst.pos <- grep(".nst",fixed=TRUE,var.names);
  if(length(nst.pos) > 0) {
    nst        <- abs(d$data[,nst.pos]);
    max.val    <- apply(nst,1,max);
    names(max.val) <- NULL;
  }
  else {
    max.val <- 0;
  }
  return(max.val);
  
}

invalidate.by.sample.size <- function(d, maximum.data.exceedance = 0.01, actual.data.threshold = 0.75, max.spike.weight = 0.025) {
  
  # The basis for any evaluation is the number of raw data expected in any mean
  expected.data <- 3600*d$sampling.rate / (60/d$averaging.time);
  
  # Check data are not too many (this is an indication that a major
  # time glitch has occurred on this hour, for example because the
  # data acquisition system clock has been adjusted)
  nominal.data      <- d$data$n.data;
  max.data          <- expected.data*(1 + maximum.data.exceedance);
  too.many.data     <- nominal.data > max.data;

  # Estimate the number of actual data as the number of data received by sonic
  # less twice the maxinum number of spikes in each row; this estimate is based
  # on the assumption that spikes are not exceedingly numerous
  spikes            <- max.spikes(d);
  spikes.weight     <- spikes/nominal.data;
  too.many.spikes   <- spikes.weight > max.spike.weight;
  actual.data       <- nominal.data - 2*max.spikes(d);  # Worst case estimate
  insufficient.data <- actual.data < actual.data.threshold*expected.data;
  
  # Return a vector containing this combined information
  return(insufficient.data | too.many.spikes | too.many.data);
  
}


invalidate.by.nonstationarity <- function(d, rns.threshold = 0.3, nst.threshold = 0.5) {
  
  # Check first order non-stationarity
  rns <- max.absolute.rns(d);
  rns.too.big <- rns > rns.threshold;
  
  # Check second order non-stationarity
  nst <- max.nst(d);
  nst.too.big <- nst > nst.threshold;
  
  # Return a vector containing this combined information
  return(data.frame(invalid.by.rns=rns.too.big, invalid.by.nst=nst.too.big));
  
}


# Compute number of data expected in a time interval
#
#   from <= current.time < to
#
# based on a data set metadata. For convenience, "from" and "to"
# are expressed as strings.
expected.data.set.size <- function(d, from, to) {
  time.from  <- as.POSIXct(from, tz="UTC");
  time.to    <- as.POSIXct(to, tz="UTC");
  avg        <- d$averaging.time*60;
  delta.t    <- as.numeric(time.to)-as.numeric(time.from);
  num.time.steps <- floor(delta.t/avg);
  return(num.time.steps);
}


# The "data yield" is the fraction of rows in data set (accepted or not)
# to the number of expected data in time interval from "from" to "to".
data.yield <- function(d, from, to) {
  expected.records <- expected.data.set.size(d, from, to);
  actual.records   <- length(d$data$t.stamp);
  return(actual.records / expected.records);
}


# The "availability" is the fraction of accepted rows in data set
# to the number of expected data; "accepted" data are those which are
# not "rejected", an information piece passed as parameter.
availability <- function(d, from, to, rejected) {
  expected.records <- expected.data.set.size(d, from, to);
  actual.records   <- length(d$data$t.stamp) - sum(rejected);
  return(actual.records / expected.records);
}

