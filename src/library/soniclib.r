library(boot);
library(plotrix);
library(deSolve);

#------------------------------------------------------------------
#
# Procedure for (testing the) direct merging of analog measurement
# sets.
#
# Written by:
#   Patrizia Favaron    (most code and, as supervisor)
#   Davide Casabianca   ("air.quality")
#   Alice Crespi        ("air.quality")
#   Manuela Dell'Acqua  ("air.quality")
#   Luca Palazzolo      ("air.quality")
#   Roberto Nava        ("air.quality")
#
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

# Read a single raw data file in SonicLib standard format, and store it to a member of
# class "sonic.raw.data"
get.raw.data <- function(file.name, sampling.rate=10, threshold=0.005, verbose=FALSE) {
  # Input values:
  #
  #    file.name      Name of file to process
  #    sampling.rate  Nominal sampling rate used when collecting data in file (Hz)
  #    threshold      Maximum fraction of invalid data to consider a file set acceptable
  #    verbose        Boolean flag indicating whether status / error messages are desired,
  #                   or not; in the latter case error conditions are reported by restituting
  #                   NULL values.
  #
  # Output:
  #
  #    Data frame, containing the data desired
  #
  # Note: This routine does *not* shift analog data to adjust delays; to accomplish
  #       this task use function "AdjustDelay", further in this source.
  #
  # Authors:
  #
  #    C.Bassi, M.Favaron, M.Garzoglio, A.Volonte'
  #

  # Get data in CSV form
  d <- read.csv(file.name, header=TRUE);

  # Check number of data is consistent with a full file, at the
  # specified sampling rate (if not, reject data file)
  l.expected <- sampling.rate*3600;
  num.cols <- length(d);
  if(num.cols <= 0) {
    if(verbose) print("get.raw.data:: Error: No columns in input data");
    return(NULL);
  }
  l.actual <- length(d[,1]);
  if(abs(abs(l.actual-l.expected)/l.expected - 1) < threshold) {
    if(verbose) print(sprintf("get.raw.data:: Error: Number of data, %d, is inconsistent with the expected number, %d", l.actual, l.expected));
    return(NULL);
  }

  # Check wind and sonic temperature columns to exist, and transfer them to temp columns
  # (sonic quadruples are required to exist for a SonicLib raw data file to be considered valid)
  n <- names(d);
  has.sonic.vars <- sum(n=="u" | n=="v" | n=="w" | n=="t") == 4;
  if(!has.sonic.vars) {
    if(verbose) print("get.raw.data:: Error: No 'u', 'v', 'w' or 't' data in file");
    return(NULL);
  }
  u <- d$u;
  v <- d$v;
  w <- d$w;
  t <- d$t;

  # Make invalid data identification easier; in SonicLib raw files invalid data
  # are identified by a value equal to -9999.9 or less (this convention, possibly
  # non-optimal from software engineering standpoint, has been adopted because of its
  # Fortran-friendliness; we remedy here by the way)
  u[u < -9999] <- NA;
  v[v < -9999] <- NA;
  w[w < -9999] <- NA;
  t[t < -9999] <- NA;

  # Attribute time stamp based on positional index, if needed
  if(!any(n=="time.stamp")) {
    time.stamp <- 0:(l.actual-1)/l.actual*3600;
  }
  else {
    delta.data <- 100.0*abs(l.expected - l.actual)/l.expected;
    if(delta.data < 1) {
      time.stamp <- 0:(l.actual-1)/l.actual*3600;
    }
    else {
      time.stamp <- d$time.stamp;
    }
  }

  # With time stamp and sonic quadruples the mandatory part of output data frame
  # is done, so we may create the output data frame.
  e <- data.frame(time.stamp, u, v, w, t);
  # Fron now on optional quantities are searched and, if present, will be added
  # to output data frame rightwards.

  # Water?
  if(any(n=="q")) {
    q <- d$q;
    q[q < 0.] <- NA;  # Negative molar concentrations are physically unrealistic
    e$q <- q;
  }

  # Carbon dioxide?
  if(any(n=="c")) {
    c <- d$c;
    c[c < 0.] <- NA;  # Negative molar concentrations are physically unrealistic
    e$c <- c;
  }

  # Ammonia?
  if(any(n=="a")) {
    a <- d$a;
    a[a < 0.] <- NA;  # Negative molar concentrations are physically unrealistic
    e$a <- a;
  }

  # Methane?
  if(any(n=="m")) {
    m <- d$m;
    m[m < 0.] <- NA;  # Negative molar concentrations are physically unrealistic
    e$m <- m;
  }

  # Conventional temperature?
  if(any(n=="temp")) {
    temp <- d$temp;
    temp[temp < -40. | temp > 60] <- NA;  # Most instruments restrict temperature range to -40..+60 interval
    e$temp <- temp;
  }

  # Relative humidity?
  if(any(n=="hrel")) {
    hrel <- d$hrel;
    hrel[hrel < 0. | hrel > 105.] <- NA;  # More than 100% humidity possible (especially under foggy conditions)
    e$hrel <- hrel;
  }

  # Leave
  q<-list(
    data          = e,
    sampling.rate = sampling.rate
  );
  class(q) <- "sonic.raw.data";
  return(q);

}


# Read consecutive data files to a unique file and aggregate data if required.
get.multi.raw.data <- function(name.first.file, n.hours=1, sampling.rate=10, threshold=0.005, average.by="none", R.in.name=FALSE, verbose=FALSE) {

  # Input values:
  #
  #    name.first.file      Name of first file to process
  #
  #    n.hours              Number of hours to read (assumed all present; default=1).
  #
  #    sampling.rate        Nominal sampling rate used when collecting data in file (Hz)
  #
  #    threshold            Maximum fraction of invalid data to consider a file set acceptable
  #
  #    average.by           String, specifying how averaging should be performed (default="none",
  #                         allowed values = "none", "seconds" and "minutes")
  #
  #    R.in.name            Boolean flag indicating whether file names conform to original
  #                         SonicLib format, yyyymmdd.hh.csv (FALSE), or MeteoFlux Core V2 derived
  #                         name yyyymmdd.hhR.csv
  #
  #    verbose              Boolean flag indicating whether status / error messages are desired,
  #                         or not; in the latter case error conditions are reported by restituting
  #                         NULL values.
  #
  # Output:
  #
  #    Data frame, containing the data desired
  #
  # Note: This routine does *not* shift analog data to adjust delays; to accomplish
  #       this task use function "AdjustDelay", further in this source.
  #
  # Authors:
  #
  #    C.Bassi, M.Favaron, M.Garzoglio, A.Volonte'
  #

  # Generate file names
  str.len <- nchar(name.first.file);
  path <- substring(name.first.file, first=1, last=str.len-15);
  file <- substring(name.first.file, first=str.len-14, last=str.len);
  year  <- substring(file, first=1, last=4);
  month <- substring(file, first=5, last=6);
  day   <- substring(file, first=7, last=8);
  hour  <- substring(file, first=10, last=11);
  time.stamp <- as.POSIXct(
    paste(year, "-", month, "-", day, " ", hour, ":00:00", sep=""),
    tz="UTC"
  );
  hours <- 3600*(0:(n.hours-1));
  time.stamp.set <- time.stamp + hours;
  if(R.in.name) {
    file.set <- strftime(time.stamp.set, format="%Y%m%d.%HR.csv", tz="UTC");
  }
  else {
    file.set <- strftime(time.stamp.set, format="%Y%m%d.%H.csv", tz="UTC");
  }
  file.set <- paste(path, file.set, sep="");

  # Read and append all data files in list
  first <- TRUE;
  i.hr <- 0;
  for(file in file.set) {
    if(first) {
      d <- get.raw.data(file, sampling.rate, threshold, verbose);
      if(is.null(d)) {
        if(verbose) print("get.multi.raw.data:: error: At least one of the data files requested does not exists or contains an insufficient number of data");
        return(NULL);
      }
      g <- d$data;
      first <- FALSE;
    }
    else {
      e <- get.raw.data(file, sampling.rate, threshold, verbose);
      e$data$time.stamp <- e$data$time.stamp + 3600*i.hr;
      g <- rbind(g,e$data);
    }
    i.hr <- i.hr+1;
  }

  # Aggregate data by seconds or minutes, if required
  if(average.by == "seconds") {
    time.index <- as.factor(as.integer(g$time.stamp));
    g <- aggregate(g, by=list(time.index), mean, na.rm=TRUE);
  }
  else if(average.by == "minutes") {
    time.index <- as.factor(as.integer(g$time.stamp) %/% 60);
    g <- aggregate(g, by=list(time.index), mean, na.rm=TRUE);
    g$time.stamp <- round(g$time.stamp - g$time.stamp[1]);
  }

  # Leave
  result <- list(
    data          = g,
    sampling.rate = 1.0/g$time.stamp[2]
  );
  class(result) <- "sonic.raw.data";
  return(result);

}


# Buid a set of spike removal sigma thresholds
set.spike.detection.threshold <- function(u=NULL, v=NULL, w=NULL, t=NULL, q=NULL, c=NULL, a=NULL, m=NULL, temp=NULL, hrel=NULL) {

  # Auxiliary function: set spike detection threshold
  spike.limit.set <- function(x) {
    if(!is.null(x)) { spike.x <- 3.0; }
    else if(class(x)=="numeric") { spike.x <- abs(x); }
    else { spike.x <- 3.0; }
    return(spike.x);
  }

  # Set spike detection thresholds
  spike.u <- spike.limit.set(u);
  spike.v <- spike.limit.set(v);
  spike.w <- spike.limit.set(w);
  spike.t <- spike.limit.set(t);
  spike.q <- spike.limit.set(q);
  spike.c <- spike.limit.set(c);
  spike.m <- spike.limit.set(m);
  spike.a <- spike.limit.set(a);
  spike.temp <- spike.limit.set(temp);
  spike.hrel <- spike.limit.set(hrel);

  # Compose object, return it and leave
  result <- list(
    u = spike.u,
    v = spike.v,
    w = spike.w,
    t = spike.t,
    q = spike.q,
    c = spike.c,
    a = spike.a,
    m = spike.m,
    temp = spike.temp,
    hrel = spike.hrel
  );
  class(result) <- "spike.detection.spec";
  return(result);

}


# Compute block means and covariances from ultrasonic anemometer data, stored as a data
# frame as produced by "get.raw.data". This routine acts on a single file: its equivalent,
# acting on multiple file, is "average.sonic.file.set".
#
# During read non-stationarity indices are also computed:
# - Relative non-stationarity for wind components
# - Steadiness index for covariances
# Both methods are outlined in M.Aubinet, T.Vesala, D.Papale (eds), "Eddy Covariance -
# A Practical Guide for Measurement and Data Analysis", Springer, 2012, pp.115-116,
# along with original papers by Vickers-Mahrt (1997) and Foken-Wichura (1996).
#
# Authors:
#
#    C.Bassi, M.Favaron, M.Garzoglio, A.Volonte'
#
# Modified by:
#
#    S.Cesco, M.Favaron
#
average.sonic.data <- function(d, initial.stamp, averaging.time=30, delay=0.3, trend.removal="none", spike.detection.threshold=3, spike.treatment="set.na", min.fraction.valid=0.75, min.valid.data=2, file.dump=NULL, R.in.name=FALSE, verbose=FALSE) {

  # Inputs:
  #
  #   d                 Object of class "sonic.raw.data", as originated by "get.raw.data"
  #                     (no default)
  #
  #   initial.stamp     POSIXct time stamp, as originated by "time.stamp.from.name"
  #                     (no default)
  #
  #   averaging.time    Averaging time (minutes, default=30, valid values 10, 15, 20, 30, 60)
  #
  #   delay             A real number specifying delay value in seconds, or an
  #                     object of class "delay.spec" (returned by "set.delay"
  #                     and internally implemented as a list
  #                     containing attributes named as one or more optional
  #                     columns, all of floating point type, each containing
  #                     column-specific delay)
  #
  #                     If object form is used, and a column exists for which no
  #                     delay has been specified, then a 0.0 delay is assumed.
  #
  #                     Default: 0.3s
  #
  #   trend.removal     String, specifying how trend removal should be accomplished.
  #                     Possible values are "none" (no trend removal), "linear"
  #                     (a linear trend is estimated by least squares fit).
  #
  #                     Default is "none".
  #
  #   spike.detection.threshold   A real positive number specifying the spike detection
  #                     threshold expressed in number of standard deviations above
  #                     or below the signal mean.
  #
  #                     Or, a value of class "spike.detection.spec" specifying the
  #                     threshold for individual channels.
  #
  #                     Default: 3.
  #
  #   spike.treatment   String, containing the name of the spike treatment method.
  #                     Possible values are "none" (nothing is done to process spikes),
  #                     or "set.na", to transform spikes to NAs
  #
  #   min.fraction.valid  A real number between 0 and 1, meaning the minimum
  #                     value acceptable for fraction of valid to total data
  #                     in a sub-block. If less than this number of valid data
  #                     is available, then the corresponding sub-block stats
  #                     are set to NA. This criterion is applied only to sonic
  #                     values (u,v,w,t). Default is 0.75 (75%)
  #
  #   min.valid.data    An integer number, stating the minimum acceptable number
  #                     of data for computing block averages. Default: 2.
  #
  #   file.dump         String, representing the name of a file to which data
  #                     are saved after spike and trend removal have been applied.
  #                     This eases debugging of trend removal and spike detection.
  #                     If NULL no file is saved.
  #
  #   verbose      Boolean flag: TRUE to print error / progress messages, FALSE to
  #                not print (default: FALSE)
  #
  # Output:
  #
  #   Object of type "sonic.avg.data" containing time stamped records containing averages, covariances,
  #   relative non stationarities and steadinesses.
  #

  # Check the object "d" is the right type
  if(class(d) != "sonic.raw.data") {
    if(verbose) print("average.sonic.data:: error: Data not of class 'sonic.raw.data'");
    return(NULL);
  }

  if(verbose & !is.null(file.dump)) {
    print(paste("About to generate manipulated file ",file.dump));
  }

  # Set optional values delay
  if(class(delay)=="numeric") {
    delay.q    <- abs(delay);
    delay.c    <- abs(delay);
    delay.a    <- abs(delay);
    delay.m    <- abs(delay);
    delay.temp <- abs(delay);
    delay.hrel <- abs(delay);
  }
  else if(class(delay)=="delay.spec") {
    delay.q    <- abs(delay$q);
    delay.c    <- abs(delay$c);
    delay.a    <- abs(delay$a);
    delay.m    <- abs(delay$m);
    delay.temp <- abs(delay$temp);
    delay.hrel <- abs(delay$hrel);
  }
  else {
    if(verbose) print("average.sonic.data:: error: Argument 'delay' is not a number of an instance of class 'delay.spec'");
    return(NULL);
  }

  # Set optional values spike detection threshold
  if(class(spike.detection.threshold)=="numeric") {
    spike.u    <- abs(spike.detection.threshold);
    spike.v    <- abs(spike.detection.threshold);
    spike.w    <- abs(spike.detection.threshold);
    spike.t    <- abs(spike.detection.threshold);
    spike.q    <- abs(spike.detection.threshold);
    spike.c    <- abs(spike.detection.threshold);
    spike.m    <- abs(spike.detection.threshold);
    spike.a    <- abs(spike.detection.threshold);
    spike.temp <- abs(spike.detection.threshold);
    spike.hrel <- abs(spike.detection.threshold);
  }
  else if(class(spike.detection.threshold)=="spike.detection.spec") {
    spike.u    <- abs(spike.detection.threshold$u);
    spike.v    <- abs(spike.detection.threshold$v);
    spike.w    <- abs(spike.detection.threshold$w);
    spike.t    <- abs(spike.detection.threshold$t);
    spike.q    <- abs(spike.detection.threshold$q);
    spike.c    <- abs(spike.detection.threshold$c);
    spike.m    <- abs(spike.detection.threshold$m);
    spike.a    <- abs(spike.detection.threshold$a);
    spike.temp <- abs(spike.detection.threshold$temp);
    spike.hrel <- abs(spike.detection.threshold$hrel);
  }
  else {
    if(verbose) print("average.sonic.data:: error: Argument 'spike.detection.threshold' is not a number of an instance of class 'spike.detection.spec'");
    return(NULL);
  }

  # Get sampling rate from input set
  sampling.rate <- d$sampling.rate;

  # Check averaging time to be an exact multiple of 5, to divide 60 exactly,
  # and to be not less than 10
  a.t <- as.integer(averaging.time);
  if(a.t %% 5 != 0) {
    if(verbose) print("average.sonic.data:: error: Averaging time is not a multiple of 5 minutes");
    return(NULL);
  }
  if(60 %% a.t != 0) {
    if(verbose) print("average.sonic.data:: error: Averaging time is not a divisor of 60 minutes");
    return(NULL);
  }
  if(a.t < 10 || a.t > 60) {
    if(verbose) print("average.sonic.data:: error: Averaging time is not between 10 and 60 minutes");
    return(NULL);
  }
  # Post: a.t is a multiple of 5 within interval [10,60] inclusive, which divides 60

  # Retrieve mandatory data
  time.stamp <- d$data$time.stamp;
  u          <- d$data$u;
  v          <- d$data$v;
  w          <- d$data$w;
  t          <- d$data$t;

  # Check whether some optional column exists, retrieve it, and apply it the proper delay
  exists.water          <- !is.null(d$data$q);
  if(exists.water) {
    q <- d$data$q;
    delay.samples <- as.integer(round(delay.q * sampling.rate));
    q <- c( q[(delay.samples+1):length(q)], rep(NA, times=delay.samples));
  }
  exists.carbon.dioxide <- !is.null(d$data$c);
  if(exists.carbon.dioxide) {
    c <- d$data$c;
    delay.samples <- as.integer(round(delay.c * sampling.rate));
    c <- c( c[(delay.samples+1):length(c)], rep(NA, times=delay.samples));
  }
  exists.ammonia        <- !is.null(d$data$a);
  if(exists.ammonia) {
    a <- d$data$a;
    delay.samples <- as.integer(round(delay.a * sampling.rate));
    a <- c( a[(delay.samples+1):length(a)], rep(NA, times=delay.samples));
  }
  exists.methane        <- !is.null(d$data$m);
  if(exists.methane) {
    m <- d$data$m;
    delay.samples <- as.integer(round(delay.m * sampling.rate));
    m <- c( m[(delay.samples+1):length(m)], rep(NA, times=delay.samples));
  }
  exists.temperature    <- !is.null(d$data$temp);
  if(exists.temperature) {
    temp <- d$data$temp;
    delay.samples <- as.integer(round(delay.temp * sampling.rate));
    temp <- c( temp[(delay.samples+1):length(temp)], rep(NA, times=delay.samples));
  }
  exists.rel.humidity   <- !is.null(d$data$hrel);
  if(exists.rel.humidity) {
    hrel <- d$data$hrel;
    delay.samples <- as.integer(round(delay.hrel * sampling.rate));
    hrel <- c( hrel[(delay.samples+1):length(hrel)], rep(NA, times=delay.samples));
  }

  # Reserve workspace
  num.blocks <- 60 %/% a.t;
  n.data  <- numeric(num.blocks);
  u.avg   <- numeric(num.blocks);
  v.avg   <- numeric(num.blocks);
  w.avg   <- numeric(num.blocks);
  t.avg   <- numeric(num.blocks);
  u.min   <- numeric(num.blocks);
  u.max   <- numeric(num.blocks);
  v.min   <- numeric(num.blocks);
  v.max   <- numeric(num.blocks);
  w.min   <- numeric(num.blocks);
  w.max   <- numeric(num.blocks);
  t.min   <- numeric(num.blocks);
  t.max   <- numeric(num.blocks);
  uu      <- numeric(num.blocks);
  uv      <- numeric(num.blocks);
  uw      <- numeric(num.blocks);
  vv      <- numeric(num.blocks);
  vw      <- numeric(num.blocks);
  ww      <- numeric(num.blocks);
  ut      <- numeric(num.blocks);
  vt      <- numeric(num.blocks);
  wt      <- numeric(num.blocks);
  vel     <- numeric(num.blocks);
  vel.max <- numeric(num.blocks);
  vel.sd  <- numeric(num.blocks);
  vel3    <- numeric(num.blocks);
  vel3.3  <- numeric(num.blocks);
  u.j     <- numeric(num.blocks);
  v.j     <- numeric(num.blocks);
  dir.sd  <- numeric(num.blocks);
  resultant.vel <- numeric(num.blocks);
  if(exists.water) {
    q.avg <- numeric(num.blocks);
    q.min <- numeric(num.blocks);
    q.max <- numeric(num.blocks);
    qq    <- numeric(num.blocks);
    uq    <- numeric(num.blocks);
    vq    <- numeric(num.blocks);
    wq    <- numeric(num.blocks);
  }
  if(exists.carbon.dioxide) {
    c.avg <- numeric(num.blocks);
    c.min <- numeric(num.blocks);
    c.max <- numeric(num.blocks);
    cc    <- numeric(num.blocks);
    uc    <- numeric(num.blocks);
    vc    <- numeric(num.blocks);
    wc    <- numeric(num.blocks);
  }
  if(exists.ammonia) {
    a.avg <- numeric(num.blocks);
    a.min <- numeric(num.blocks);
    a.max <- numeric(num.blocks);
    aa    <- numeric(num.blocks);
    ua    <- numeric(num.blocks);
    va    <- numeric(num.blocks);
    wa    <- numeric(num.blocks);
  }
  if(exists.methane) {
    m.avg <- numeric(num.blocks);
    m.min <- numeric(num.blocks);
    m.max <- numeric(num.blocks);
    mm    <- numeric(num.blocks);
    um    <- numeric(num.blocks);
    vm    <- numeric(num.blocks);
    wm    <- numeric(num.blocks);
  }
  if(exists.temperature) {
    temp.avg <- numeric(num.blocks);
  }
  if(exists.rel.humidity) {
    hrel.avg <- numeric(num.blocks);
  }

  # Reserve space for relative non-stationarity on wind components
  u.rns <- numeric(num.blocks);
  v.rns <- numeric(num.blocks);
  w.rns <- numeric(num.blocks);

  # Allocate space for non-steadinesses
  uu.nst    <- numeric(num.blocks);
  uv.nst    <- numeric(num.blocks);
  uw.nst    <- numeric(num.blocks);
  vv.nst    <- numeric(num.blocks);
  vw.nst    <- numeric(num.blocks);
  ww.nst    <- numeric(num.blocks);
  ut.nst    <- numeric(num.blocks);
  vt.nst    <- numeric(num.blocks);
  wt.nst    <- numeric(num.blocks);
  if(exists.water) {
    uq.nst  <- numeric(num.blocks);
    vq.nst  <- numeric(num.blocks);
    wq.nst  <- numeric(num.blocks);
  }
  if(exists.carbon.dioxide) {
    uc.nst  <- numeric(num.blocks);
    vc.nst  <- numeric(num.blocks);
    wc.nst  <- numeric(num.blocks);
  }
  if(exists.ammonia) {
    ua.nst  <- numeric(num.blocks);
    va.nst  <- numeric(num.blocks);
    wa.nst  <- numeric(num.blocks);
  }
  if(exists.methane) {
    um.nst  <- numeric(num.blocks);
    vm.nst  <- numeric(num.blocks);
    wm.nst  <- numeric(num.blocks);
  }

  # Reserve space for trend removal indicators
  if(trend.removal == "linear") {

    # Create all mandatory data vectors
    u.trend.slope    <- numeric(num.blocks);
    u.trend.constant <- numeric(num.blocks);
    v.trend.slope    <- numeric(num.blocks);
    v.trend.constant <- numeric(num.blocks);
    w.trend.slope    <- numeric(num.blocks);
    w.trend.constant <- numeric(num.blocks);
    t.trend.slope    <- numeric(num.blocks);
    t.trend.constant <- numeric(num.blocks);

    # Optional data related indicators
    if(exists.water) {
      q.trend.slope    <- numeric(num.blocks);
      q.trend.constant <- numeric(num.blocks);
    }
    if(exists.carbon.dioxide) {
      c.trend.slope    <- numeric(num.blocks);
      c.trend.constant <- numeric(num.blocks);
    }
    if(exists.ammonia) {
      a.trend.slope    <- numeric(num.blocks);
      a.trend.constant <- numeric(num.blocks);
    }
    if(exists.methane) {
      m.trend.slope    <- numeric(num.blocks);
      m.trend.constant <- numeric(num.blocks);
    }
    if(exists.temperature) {
      temp.trend.slope    <- numeric(num.blocks);
      temp.trend.constant <- numeric(num.blocks);
    }
    if(exists.rel.humidity) {
      hrel.trend.slope    <- numeric(num.blocks);
      hrel.trend.constant <- numeric(num.blocks);
    }

  }

  # Reserve workpace for spike removal statistics
  if(spike.treatment != "none") {

    # Create all mandatory data vectors
    u.spike.count    <- numeric(num.blocks);
    v.spike.count    <- numeric(num.blocks);
    w.spike.count    <- numeric(num.blocks);
    t.spike.count    <- numeric(num.blocks);

    # Optional data related indicators
    if(exists.water) {
      q.spike.count    <- numeric(num.blocks);
    }
    if(exists.carbon.dioxide) {
      c.spike.count    <- numeric(num.blocks);
    }
    if(exists.ammonia) {
      a.spike.count    <- numeric(num.blocks);
    }
    if(exists.methane) {
      m.spike.count    <- numeric(num.blocks);
    }
    if(exists.temperature) {
      temp.spike.count    <- numeric(num.blocks);
    }
    if(exists.rel.humidity) {
      hrel.spike.count    <- numeric(num.blocks);
    }

  }

  # Build time stamps for each data block
  t.stamp <- seq(from=initial.stamp, to=initial.stamp + (num.blocks-1)*a.t*60, by=a.t*60);

  # Main loop: iterate over all blocks, and compute means and covariances for each of them
  num.sub.blocks <- a.t %/% 5;
  for(block in 1:num.blocks) {

    # Process block data
    begin.block <- (block-1)*a.t*60;
    end.block   <- begin.block + a.t*60;
    data.block  <- d$data$time.stamp >= begin.block & d$data$time.stamp < end.block;
    n.data[block] <- sum(data.block);
    if(n.data[block] > 0) {

      if(verbose) print(sprintf("Processing data block %d",block));

      # Restrict attention to the indices really involved only
      # (not indispensable, but saves the computer a lot of work)
      this.block <- which(data.block);

      # Extract data for current block
      time.stamp.b <- d$data$time.stamp[this.block];
      u.b <- d$data$u[this.block];
      v.b <- d$data$v[this.block];
      w.b <- d$data$w[this.block];
      t.b <- d$data$t[this.block];
      if(exists.water)          q.b    <- d$data$q[this.block];
      if(exists.carbon.dioxide) c.b    <- d$data$c[this.block];
      if(exists.ammonia)        a.b    <- d$data$a[this.block];
      if(exists.methane)        m.b    <- d$data$m[this.block];
      if(exists.temperature)    temp.b <- d$data$temp[this.block];
      if(exists.rel.humidity)   hrel.b <- d$data$hrel[this.block];

      # Check the data block really contains something, and skip
      # processing if not
      u.b.valid <- sum(!is.na(u.b));
      v.b.valid <- sum(!is.na(v.b));
      w.b.valid <- sum(!is.na(w.b));
      t.b.valid <- sum(!is.na(t.b));
      n.b.valid <- min(c(u.b.valid,v.b.valid,w.b.valid,t.b.valid));
      if(n.b.valid > min.fraction.valid*n.data[block] && n.b.valid > min.valid.data) {

        # Perform trend removal, if requested
        if(trend.removal != "none") {
          u.b.d <- remove.trend(u.b, trend.removal);
          v.b.d <- remove.trend(v.b, trend.removal);
          w.b.d <- remove.trend(w.b, trend.removal);
          t.b.d <- remove.trend(t.b, trend.removal);
          if(exists.water)           q.b.d    <-remove.trend(q.b, trend.removal);
          if(exists.carbon.dioxide)  c.b.d    <-remove.trend(c.b, trend.removal);
          if(exists.ammonia)         a.b.d    <-remove.trend(a.b, trend.removal);
          if(exists.methane)         m.b.d    <-remove.trend(m.b, trend.removal);
          if(exists.temperature)     temp.b.d <-remove.trend(temp.b, trend.removal);
          if(exists.rel.humidity)    hrel.b.d <-remove.trend(hrel.b, trend.removal);
          u.b                     <- u.b.d$data;
          u.trend.slope[block]    <- u.b.d$slope;
          u.trend.constant[block] <- u.b.d$constant;
          v.b                     <- v.b.d$data;
          v.trend.slope[block]    <- v.b.d$slope;
          v.trend.constant[block] <- v.b.d$constant;
          w.b                     <- w.b.d$data;
          w.trend.slope[block]    <- w.b.d$slope;
          w.trend.constant[block] <- w.b.d$constant;
          t.b                     <- t.b.d$data;
          t.trend.slope[block]    <- t.b.d$slope;
          t.trend.constant[block] <- t.b.d$constant;
          if(exists.water) {
            q.b                     <- q.b.d$data;
            q.trend.slope[block]    <- q.b.d$slope;
            q.trend.constant[block] <- q.b.d$constant;
          }
          if(exists.carbon.dioxide) {
            c.b                     <- c.b.d$data;
            c.trend.slope[block]    <- c.b.d$slope;
            c.trend.constant[block] <- c.b.d$constant;
          }
          if(exists.ammonia) {
            a.b                     <- a.b.d$data;
            a.trend.slope[block]    <- a.b.d$slope;
            a.trend.constant[block] <- a.b.d$constant;
          }
          if(exists.methane) {
            m.b                     <- m.b.d$data;
            m.trend.slope[block]    <- m.b.d$slope;
            m.trend.constant[block] <- m.b.d$constant;
          }
          if(exists.temperature) {
            temp.b                     <- temp.b.d$data;
            temp.trend.slope[block]    <- temp.b.d$slope;
            temp.trend.constant[block] <- temp.b.d$constant;
          }
          if(exists.rel.humidity) {
            hrel.b                     <- hrel.b.d$data;
            hrel.trend.slope[block]    <- hrel.b.d$slope;
            hrel.trend.constant[block] <- hrel.b.d$constant;
          }
        }

        # Prepare the data frame holding "manipulated" data in preparation
        # to saving them
        if(!is.null(file.dump)) {
          d.block <- data.frame(time.stamp.b,u.b,v.b,w.b,t.b);
          if(exists.water) {
            d.block <- data.frame(d.block, q.b);
          }
          if(exists.carbon.dioxide) {
            d.block <- data.frame(d.block, c.b);
          }
          if(exists.ammonia) {
            d.block <- data.frame(d.block, a.b);
          }
          if(exists.methane) {
            d.block <- data.frame(d.block, m.b);
          }
          if(exists.temperature) {
            d.block <- data.frame(d.block, temp.b);
          }
          if(exists.rel.humidity) {
            d.block <- data.frame(d.block, hrel.b);
          }
          if(block == 1) {
            d.manip <- d.block;
          }
          else {
            d.manip <- rbind(d.manip,d.block);
          }
        }

        # Detect spikes and treat them according to the algorithm selected
        if(spike.treatment == "set.na") {
          mean.u    <- mean(u.b, na.rm=TRUE);
          sd.u      <- sd(u.b, na.rm=TRUE);
          mean.v    <- mean(v.b, na.rm=TRUE);
          sd.v      <- sd(v.b, na.rm=TRUE);
          mean.w    <- mean(w.b, na.rm=TRUE);
          sd.w      <- sd(w.b, na.rm=TRUE);
          mean.t    <- mean(t.b, na.rm=TRUE);
          sd.t      <- sd(t.b, na.rm=TRUE);
          invalid.before <- sum(is.na(u.b));
          u.b[abs(u.b-mean.u) > spike.u*sd.u] <- NA;
          invalid.after <- sum(is.na(u.b));
          u.spike.count[block] <- invalid.after - invalid.before;
          invalid.before <- sum(is.na(v.b));
          v.b[abs(v.b-mean.v) > spike.v*sd.v] <- NA;
          invalid.after <- sum(is.na(v.b));
          v.spike.count[block] <- invalid.after - invalid.before;
          invalid.before <- sum(is.na(w.b));
          w.b[abs(w.b-mean.w) > spike.w*sd.w] <- NA;
          invalid.after <- sum(is.na(w.b));
          w.spike.count[block] <- invalid.after - invalid.before;
          invalid.before <- sum(is.na(t.b));
          t.b[abs(t.b-mean.t) > spike.t*sd.t] <- NA;
          invalid.after <- sum(is.na(t.b));
          t.spike.count[block] <- invalid.after - invalid.before;
          if(exists.water) {
            invalid.before <- sum(is.na(q.b));
            mean.q    <- mean(q.b, na.rm=TRUE);
            sd.q      <- sd(q.b, na.rm=TRUE);
            q.b[abs(q.b-mean.q) > spike.q*sd.q] <- NA;
            invalid.after <- sum(is.na(q.b));
            q.spike.count[block] <- invalid.after - invalid.before;
          }
          if(exists.carbon.dioxide) {
            invalid.before <- sum(is.na(c.b));
            mean.c    <- mean(c.b, na.rm=TRUE);
            sd.c      <- sd(c.b, na.rm=TRUE);
            c.b[abs(c.b-mean.c) > spike.c*sd.c] <- NA;
            invalid.after <- sum(is.na(c.b));
            c.spike.count[block] <- invalid.after - invalid.before;
          }
          if(exists.ammonia) {
            invalid.before <- sum(is.na(a.b));
            mean.a    <- mean(a.b, na.rm=TRUE);
            sd.a      <- sd(a.b, na.rm=TRUE);
            a.b[abs(a.b-mean.a) > spike.a*sd.a] <- NA;
            invalid.after <- sum(is.na(a.b));
            a.spike.count[block] <- invalid.after - invalid.before;
          }
          if(exists.methane) {
            invalid.before <- sum(is.na(m.b));
            mean.m    <- mean(m.b, na.rm=TRUE);
            sd.m      <- sd(m.b, na.rm=TRUE);
            m.b[abs(m.b-mean.m) > spike.m*sd.m] <- NA;
            invalid.after <- sum(is.na(m.b));
            m.spike.count[block] <- invalid.after - invalid.before;
          }
          if(exists.temperature) {
            invalid.before <- sum(is.na(temp.b));
            mean.temp    <- mean(temp.b, na.rm=TRUE);
            sd.temp      <- sd(temp.b, na.rm=TRUE);
            temp.b[abs(temp.b-mean.temp) > spike.temp*sd.temp] <- NA;
            invalid.after <- sum(is.na(temp.b));
            temp.spike.count[block] <- invalid.after - invalid.before;
          }
          if(exists.rel.humidity) {
            invalid.before <- sum(is.na(hrel.b));
            mean.hrel    <- mean(hrel.b, na.rm=TRUE);
            sd.hrel      <- sd(hrel.b, na.rm=TRUE);
            hrel.b[abs(hrel.b-mean.hrel) > spike.hrel*sd.hrel] <- NA;
            invalid.after <- sum(is.na(hrel.b));
            hrel.spike.count[block] <- invalid.after - invalid.before;
          }
        }

        # Compute basic statistics for current block
        u.avg[block] <- mean(u.b, na.rm=TRUE);
        v.avg[block] <- mean(v.b, na.rm=TRUE);
        w.avg[block] <- mean(w.b, na.rm=TRUE);
        t.avg[block] <- mean(t.b, na.rm=TRUE);
        u.min[block] <- min(u.b, na.rm=TRUE);
        u.max[block] <- max(u.b, na.rm=TRUE);
        v.min[block] <- min(v.b, na.rm=TRUE);
        v.max[block] <- max(v.b, na.rm=TRUE);
        w.min[block] <- min(w.b, na.rm=TRUE);
        w.max[block] <- max(w.b, na.rm=TRUE);
        t.min[block] <- min(t.b, na.rm=TRUE);
        t.max[block] <- max(t.b, na.rm=TRUE);
        uu[block]    <- cov(u.b, u.b, use="pairwise.complete.obs");
        uv[block]    <- cov(u.b, v.b, use="pairwise.complete.obs");
        uw[block]    <- cov(u.b, w.b, use="pairwise.complete.obs");
        vv[block]    <- cov(v.b, v.b, use="pairwise.complete.obs");
        vw[block]    <- cov(v.b, w.b, use="pairwise.complete.obs");
        ww[block]    <- cov(w.b, w.b, use="pairwise.complete.obs");
        ut[block]    <- cov(u.b, t.b, use="pairwise.complete.obs");
        vt[block]    <- cov(v.b, t.b, use="pairwise.complete.obs");
        wt[block]    <- cov(w.b, t.b, use="pairwise.complete.obs");
        vel.instant  <- sqrt(u.b^2+v.b^2);
        vel[block]   <- mean(vel.instant,na.rm=TRUE);
        vel.max[block] <- max(vel.instant,na.rm=TRUE);
        resultant.vel[block] <- sqrt(mean(u.b, na.rm=TRUE)^2+mean(v.b, na.rm=TRUE)^2);
        vel.sd[block] <- sd(vel.instant, na.rm=TRUE);
        vel3[block]   <- mean(vel.instant^3,na.rm=TRUE);
        vel3.3[block] <- mean(sqrt(u.b^2+v.b^2+w.b^2)^3,na.rm=TRUE);
        u.versor      <- u.b/vel.instant;
        v.versor      <- v.b/vel.instant;
        u.j[block]    <- mean(u.versor, na.rm=TRUE);
        v.j[block]    <- mean(v.versor, na.rm=TRUE);
        eps           <- sqrt(1-u.j[block]^2-v.j[block]^2);
        coef          <- 2/sqrt(3) - 1;
        dir.sd[block] <- 180/pi * asin(eps) * (1 + coef*eps^3);
        if(exists.water) {
          q.avg[block] <- mean(q.b, na.rm=TRUE);
          q.min[block] <- min(q.b, na.rm=TRUE);
          q.max[block] <- max(q.b, na.rm=TRUE);
          qq[block]    <- cov(q.b, q.b, use="pairwise.complete.obs");
          uq[block]    <- cov(u.b, q.b, use="pairwise.complete.obs");
          vq[block]    <- cov(v.b, q.b, use="pairwise.complete.obs");
          wq[block]    <- cov(w.b, q.b, use="pairwise.complete.obs");
        }
        if(exists.carbon.dioxide) {
          c.avg[block] <- mean(c.b, na.rm=TRUE);
          c.min[block] <- min(c.b, na.rm=TRUE);
          c.max[block] <- max(c.b, na.rm=TRUE);
          cc[block]    <- cov(c.b, c.b, use="pairwise.complete.obs");
          uc[block]    <- cov(u.b, c.b, use="pairwise.complete.obs");
          vc[block]    <- cov(v.b, c.b, use="pairwise.complete.obs");
          wc[block]    <- cov(w.b, c.b, use="pairwise.complete.obs");
        }
        if(exists.ammonia) {
          a.avg[block] <- mean(a.b, na.rm=TRUE);
          a.min[block] <- min(a.b, na.rm=TRUE);
          a.max[block] <- max(a.b, na.rm=TRUE);
          aa[block]    <- cov(a.b, a.b, use="pairwise.complete.obs");
          ua[block]    <- cov(u.b, a.b, use="pairwise.complete.obs");
          va[block]    <- cov(v.b, a.b, use="pairwise.complete.obs");
          wa[block]    <- cov(w.b, a.b, use="pairwise.complete.obs");
        }
        if(exists.methane) {
          m.avg[block] <- mean(m.b, na.rm=TRUE);
          m.min[block] <- min(m.b, na.rm=TRUE);
          m.max[block] <- max(m.b, na.rm=TRUE);
          mm[block]    <- cov(m.b, m.b, use="pairwise.complete.obs");
          um[block]    <- cov(u.b, m.b, use="pairwise.complete.obs");
          vm[block]    <- cov(v.b, m.b, use="pairwise.complete.obs");
          wm[block]    <- cov(w.b, m.b, use="pairwise.complete.obs");
        }
        if(exists.temperature) {
          temp.avg[block] <- mean(temp.b, na.rm=TRUE);
        }
        if(exists.rel.humidity) {
          hrel.avg[block] <- mean(hrel.b, na.rm=TRUE);
        }

        # Allocate sub-block space
        uu.sub    <- numeric(num.sub.blocks);
        uv.sub    <- numeric(num.sub.blocks);
        uw.sub    <- numeric(num.sub.blocks);
        vv.sub    <- numeric(num.sub.blocks);
        vw.sub    <- numeric(num.sub.blocks);
        ww.sub    <- numeric(num.sub.blocks);
        ut.sub    <- numeric(num.sub.blocks);
        vt.sub    <- numeric(num.sub.blocks);
        wt.sub    <- numeric(num.sub.blocks);
        if(exists.water) {
          uq.sub    <- numeric(num.sub.blocks);
          vq.sub    <- numeric(num.sub.blocks);
          wq.sub    <- numeric(num.sub.blocks);
        }
        if(exists.carbon.dioxide) {
          uc.sub    <- numeric(num.sub.blocks);
          vc.sub    <- numeric(num.sub.blocks);
          wc.sub    <- numeric(num.sub.blocks);
        }
        if(exists.ammonia) {
          ua.sub    <- numeric(num.sub.blocks);
          va.sub    <- numeric(num.sub.blocks);
          wa.sub    <- numeric(num.sub.blocks);
        }
        if(exists.methane) {
          um.sub    <- numeric(num.sub.blocks);
          vm.sub    <- numeric(num.sub.blocks);
          wm.sub    <- numeric(num.sub.blocks);
        }

        # Compute relative non-stationarity for wind components
        idx <- 1:length(d$data$time.stamp);
        x   <- idx[data.block];
        u.rns[block] <- relative.nonstationarity(x, u.b);
        v.rns[block] <- relative.nonstationarity(x, v.b);
        w.rns[block] <- relative.nonstationarity(x, w.b);

        # Compute Foken non-steadiness
        for(sub.block in 1:num.sub.blocks) {

          # if(verbose)  print(sprintf("Processing data sub-block %d",sub.block));

          # Compute sub-interval covariances
          begin.sub.block <- begin.block + (sub.block-1)*a.t*60/num.sub.blocks;
          end.sub.block   <- begin.sub.block + a.t*60/num.sub.blocks;
          data.sub.block  <- d$data$time.stamp >= begin.sub.block & d$data$time.stamp < end.sub.block;
          if(any(data.sub.block)) {

            # Restrict attention to sub-block indices, to save computing effort
            idx.sub.block <- which(data.sub.block);

            # Extract data for current sub-block
            u.sb <- d$data$u[idx.sub.block];
            v.sb <- d$data$v[idx.sub.block];
            w.sb <- d$data$w[idx.sub.block];
            t.sb <- d$data$t[idx.sub.block];
            if(exists.water)          q.sb    <- d$data$q[idx.sub.block];
            if(exists.carbon.dioxide) c.sb    <- d$data$c[idx.sub.block];
            if(exists.ammonia)        a.sb    <- d$data$a[idx.sub.block];
            if(exists.methane)        m.sb    <- d$data$m[idx.sub.block];
            if(exists.temperature)    temp.sb <- d$data$temp[idx.sub.block];
            if(exists.rel.humidity)   hrel.sb <- d$data$hrel[idx.sub.block];

            uu.sub[sub.block]    <- cov(u.sb, u.sb, use="pairwise.complete.obs");
            uv.sub[sub.block]    <- cov(u.sb, v.sb, use="pairwise.complete.obs");
            uw.sub[sub.block]    <- cov(u.sb, w.sb, use="pairwise.complete.obs");
            vv.sub[sub.block]    <- cov(v.sb, v.sb, use="pairwise.complete.obs");
            vw.sub[sub.block]    <- cov(v.sb, w.sb, use="pairwise.complete.obs");
            ww.sub[sub.block]    <- cov(w.sb, w.sb, use="pairwise.complete.obs");
            ut.sub[sub.block]    <- cov(u.sb, t.sb, use="pairwise.complete.obs");
            vt.sub[sub.block]    <- cov(v.sb, t.sb, use="pairwise.complete.obs");
            wt.sub[sub.block]    <- cov(w.sb, t.sb, use="pairwise.complete.obs");
            if(exists.water) {
              uq.sub[sub.block]  <- cov(u.sb, q.sb, use="pairwise.complete.obs");
              vq.sub[sub.block]  <- cov(v.sb, q.sb, use="pairwise.complete.obs");
              wq.sub[sub.block]  <- cov(w.sb, q.sb, use="pairwise.complete.obs");
            }
            if(exists.carbon.dioxide) {
              uc.sub[sub.block]  <- cov(u.sb, c.sb, use="pairwise.complete.obs");
              vc.sub[sub.block]  <- cov(v.sb, c.sb, use="pairwise.complete.obs");
              wc.sub[sub.block]  <- cov(w.sb, c.sb, use="pairwise.complete.obs");
            }
            if(exists.ammonia) {
              ua.sub[sub.block]  <- cov(u.sb, a.sb, use="pairwise.complete.obs");
              va.sub[sub.block]  <- cov(v.sb, a.sb, use="pairwise.complete.obs");
              wa.sub[sub.block]  <- cov(w.sb, a.sb, use="pairwise.complete.obs");
            }
            if(exists.methane) {
              um.sub[sub.block]  <- cov(u.sb, m.sb, use="pairwise.complete.obs");
              vm.sub[sub.block]  <- cov(v.sb, m.sb, use="pairwise.complete.obs");
              wm.sub[sub.block]  <- cov(w.sb, m.sb, use="pairwise.complete.obs");
            }

          }

          else {

            # No data: clean reaults
            uu.sub[sub.block] <-  NA;
            uv.sub[sub.block] <-  NA;
            uw.sub[sub.block] <-  NA;
            vv.sub[sub.block] <-  NA;
            vw.sub[sub.block] <-  NA;
            ww.sub[sub.block] <-  NA;
            ut.sub[sub.block] <-  NA;
            vt.sub[sub.block] <-  NA;
            wt.sub[sub.block] <-  NA;
            if(exists.water) {
              uq.sub[sub.block] <-  NA;
              vq.sub[sub.block] <-  NA;
              wq.sub[sub.block] <-  NA;
            }
            if(exists.carbon.dioxide) {
              uc.sub[sub.block] <-  NA;
              vc.sub[sub.block] <-  NA;
              wc.sub[sub.block] <-  NA;
            }
            if(exists.ammonia) {
              ua.sub[sub.block] <-  NA;
              va.sub[sub.block] <-  NA;
              wa.sub[sub.block] <-  NA;
            }
            if(exists.methane) {
              um.sub[sub.block] <-  NA;
              vm.sub[sub.block] <-  NA;
              wm.sub[sub.block] <-  NA;
            }

          }

        }

        # Compute final non-steadinesses
        uu.nst[block] <-  abs((uu[block]-mean(uu.sub, na.rm=TRUE))/uu[block]);
        uv.nst[block] <-  abs((uv[block]-mean(uv.sub, na.rm=TRUE))/uv[block]);
        uw.nst[block] <-  abs((uw[block]-mean(uw.sub, na.rm=TRUE))/uw[block]);
        vv.nst[block] <-  abs((vv[block]-mean(vv.sub, na.rm=TRUE))/vv[block]);
        vw.nst[block] <-  abs((vw[block]-mean(vw.sub, na.rm=TRUE))/vw[block]);
        ww.nst[block] <-  abs((ww[block]-mean(ww.sub, na.rm=TRUE))/ww[block]);
        ut.nst[block] <-  abs((ut[block]-mean(ut.sub, na.rm=TRUE))/ut[block]);
        vt.nst[block] <-  abs((vt[block]-mean(vt.sub, na.rm=TRUE))/vt[block]);
        wt.nst[block] <-  abs((wt[block]-mean(wt.sub, na.rm=TRUE))/wt[block]);
        if(exists.water) {
          uq.nst[block] <-  abs((uq[block]-mean(uq.sub, na.rm=TRUE))/uq[block]);
          vq.nst[block] <-  abs((vq[block]-mean(vq.sub, na.rm=TRUE))/vq[block]);
          wq.nst[block] <-  abs((wq[block]-mean(wq.sub, na.rm=TRUE))/wq[block]);
        }
        if(exists.carbon.dioxide) {
          uc.nst[block] <-  abs((uc[block]-mean(uc.sub, na.rm=TRUE))/uc[block]);
          vc.nst[block] <-  abs((vc[block]-mean(vc.sub, na.rm=TRUE))/vc[block]);
          wc.nst[block] <-  abs((wc[block]-mean(wc.sub, na.rm=TRUE))/wc[block]);
        }
        if(exists.ammonia) {
          ua.nst[block] <-  abs((ua[block]-mean(ua.sub, na.rm=TRUE))/ua[block]);
          va.nst[block] <-  abs((va[block]-mean(va.sub, na.rm=TRUE))/va[block]);
          wa.nst[block] <-  abs((wa[block]-mean(wa.sub, na.rm=TRUE))/wa[block]);
        }
        if(exists.methane) {
          um.nst[block] <-  abs((um[block]-mean(um.sub, na.rm=TRUE))/um[block]);
          vm.nst[block] <-  abs((vm[block]-mean(vm.sub, na.rm=TRUE))/vm[block]);
          wm.nst[block] <-  abs((wm[block]-mean(wm.sub, na.rm=TRUE))/wm[block]);
        }

      }
      else {

        if(verbose) print("No sonic data in this block");

        # NA-ify trend removal
        if(trend.removal != "none") {
          u.trend.slope[block]    <- NA;
          u.trend.constant[block] <- NA;
          v.trend.slope[block]    <- NA;
          v.trend.constant[block] <- NA;
          w.trend.slope[block]    <- NA;
          w.trend.constant[block] <- NA;
          t.trend.slope[block]    <- NA;
          t.trend.constant[block] <- NA;
          if(exists.water) {
            q.trend.slope[block]    <- NA;
            q.trend.constant[block] <- NA;
          }
          if(exists.carbon.dioxide) {
            c.trend.slope[block]    <- NA;
            c.trend.constant[block] <- NA;
          }
          if(exists.ammonia) {
            a.trend.slope[block]    <- NA;
            a.trend.constant[block] <- NA;
          }
          if(exists.methane) {
            m.trend.slope[block]    <- NA;
            m.trend.constant[block] <- NA;
          }
          if(exists.temperature) {
            temp.trend.slope[block]    <- NA;
            temp.trend.constant[block] <- NA;
          }
          if(exists.rel.humidity) {
            hrel.trend.slope[block]    <- NA;
            hrel.trend.constant[block] <- NA;
          }
        }

        # NA-ify spike related quantities
        if(spike.treatment == "set.na") {
          u.spike.count[block] <- NA;
          v.spike.count[block] <- NA;
          w.spike.count[block] <- NA;
          t.spike.count[block] <- NA;
          if(exists.water)          q.spike.count[block] <- NA;
          if(exists.carbon.dioxide) c.spike.count[block] <- NA;
          if(exists.ammonia)        a.spike.count[block] <- NA;
          if(exists.methane)        m.spike.count[block] <- NA;
          if(exists.temperature)    temp.spike.count[block] <- NA;
          if(exists.rel.humidity)   hrel.spike.count[block] <- NA;
        }

        # NA-ify basic statistics for current block
        u.avg[block] <- NA;
        v.avg[block] <- NA;
        w.avg[block] <- NA;
        t.avg[block] <- NA;
        u.min[block] <- NA;
        u.max[block] <- NA;
        v.min[block] <- NA;
        v.max[block] <- NA;
        w.min[block] <- NA;
        w.max[block] <- NA;
        t.min[block] <- NA;
        t.max[block] <- NA;
        uu[block]    <- NA;
        uv[block]    <- NA;
        uw[block]    <- NA;
        vv[block]    <- NA;
        vw[block]    <- NA;
        ww[block]    <- NA;
        ut[block]    <- NA;
        vt[block]    <- NA;
        wt[block]    <- NA;
        vel.instant  <- NA;
        vel[block]   <- NA;
        vel.max[block] <- NA;
        resultant.vel[block] <- NA;
        vel.sd[block] <- NA;
        vel3[block]   <- NA;
        vel3.3[block] <- NA;
        u.j[block]    <- NA;
        v.j[block]    <- NA;
        dir.sd[block] <- NA;
        if(exists.water) {
          q.avg[block] <- NA;
          q.min[block] <- NA;
          q.max[block] <- NA;
          qq[block]    <- NA;
          uq[block]    <- NA;
          vq[block]    <- NA;
          wq[block]    <- NA;
        }
        if(exists.carbon.dioxide) {
          c.avg[block] <- NA;
          c.min[block] <- NA;
          c.max[block] <- NA;
          cc[block]    <- NA;
          uc[block]    <- NA;
          vc[block]    <- NA;
          wc[block]    <- NA;
        }
        if(exists.ammonia) {
          a.avg[block] <- NA;
          a.min[block] <- NA;
          a.max[block] <- NA;
          aa[block]    <- NA;
          ua[block]    <- NA;
          va[block]    <- NA;
          wa[block]    <- NA;
        }
        if(exists.methane) {
          m.avg[block] <- NA;
          m.min[block] <- NA;
          m.max[block] <- NA;
          mm[block]    <- NA;
          um[block]    <- NA;
          vm[block]    <- NA;
          wm[block]    <- NA;
        }
        if(exists.temperature) {
          temp.avg[block] <- NA;
        }
        if(exists.rel.humidity) {
          hrel.avg[block] <- NA;
        }

        # NA-ify relative non-stationarity for wind components
        u.rns[block] <- NA;
        v.rns[block] <- NA;
        w.rns[block] <- NA;

        # NA-ify Foken non-steadiness
        uu.nst[block] <-  NA;
        uv.nst[block] <-  NA;
        uw.nst[block] <-  NA;
        vv.nst[block] <-  NA;
        vw.nst[block] <-  NA;
        ww.nst[block] <-  NA;
        ut.nst[block] <-  NA;
        vt.nst[block] <-  NA;
        wt.nst[block] <-  NA;
        if(exists.water) {
          uq.nst[block] <-  NA;
          vq.nst[block] <-  NA;
          wq.nst[block] <-  NA;
        }
        if(exists.carbon.dioxide) {
          uc.nst[block] <-  NA;
          vc.nst[block] <-  NA;
          wc.nst[block] <-  NA;
        }
        if(exists.ammonia) {
          ua.nst[block] <-  NA;
          va.nst[block] <-  NA;
          wa.nst[block] <-  NA;
        }
        if(exists.methane) {
          um.nst[block] <-  NA;
          vm.nst[block] <-  NA;
          wm.nst[block] <-  NA;
        }

      }


    }

  }

  # Assemble output data set
  data <- data.frame(t.stamp, n.data, u.avg, u.min, u.max, v.avg, v.min, v.max, w.avg, w.min, w.max, t.avg, t.min, t.max, uu, uv, uw, vv, vw, ww, ut, vt, wt, vel, vel.max, resultant.vel, vel.sd, vel3, vel3.3, u.j, v.j, dir.sd, u.rns, v.rns, w.rns, uu.nst, uv.nst, uw.nst, vv.nst, vw.nst, ww.nst, ut.nst, vt.nst, wt.nst);
  if(exists.water) {
    data$q.avg  <- q.avg;
    data$q.min  <- q.min;
    data$q.max  <- q.max;
    data$qq     <- qq;
    data$uq     <- uq;
    data$vq     <- vq;
    data$wq     <- wq;
    data$uq.nst <- uq.nst;
    data$vq.nst <- vq.nst;
    data$wq.nst <- wq.nst;
  }
  if(exists.carbon.dioxide) {
    data$c.avg  <- c.avg;
    data$c.min  <- c.min;
    data$c.max  <- c.max;
    data$cc     <- cc;
    data$uc     <- uc;
    data$vc     <- vc;
    data$wc     <- wc;
    data$uc.nst <- uc.nst;
    data$vc.nst <- vc.nst;
    data$wc.nst <- wc.nst;
  }
  if(exists.ammonia) {
    data$a.avg  <- a.avg;
    data$a.min  <- a.min;
    data$a.max  <- a.max;
    data$aa     <- aa;
    data$ua     <- ua;
    data$va     <- va;
    data$wa     <- wa;
    data$ua.nst <- ua.nst;
    data$va.nst <- va.nst;
    data$wa.nst <- wa.nst;
  }
  if(exists.methane) {
    data$m.avg  <- m.avg;
    data$m.min  <- m.min;
    data$m.max  <- m.max;
    data$mm     <- mm;
    data$um     <- um;
    data$vm     <- vm;
    data$wm     <- wm;
    data$um.nst <- um.nst;
    data$vm.nst <- vm.nst;
    data$wm.nst <- wm.nst;
  }
  if(exists.temperature) {
    data$temp.avg  <- temp.avg;
  }
  if(exists.rel.humidity) {
    data$hrel.avg  <- hrel.avg;
  }
  if(trend.removal == "linear") {
    data$u.trend.slope    <- u.trend.slope;
    data$u.trend.constant <- u.trend.constant;
    data$v.trend.slope    <- v.trend.slope;
    data$v.trend.constant <- v.trend.constant;
    data$w.trend.slope    <- w.trend.slope;
    data$w.trend.constant <- w.trend.constant;
    data$t.trend.slope    <- t.trend.slope;
    data$t.trend.constant <- t.trend.constant;
    if(exists.water) {
      data$q.trend.slope    <- q.trend.slope;
      data$q.trend.constant <- q.trend.constant;
    }
    if(exists.carbon.dioxide) {
      data$c.trend.slope    <- c.trend.slope;
      data$c.trend.constant <- c.trend.constant;
    }
    if(exists.ammonia) {
      data$a.trend.slope    <- a.trend.slope;
      data$a.trend.constant <- a.trend.constant;
    }
    if(exists.methane) {
      data$m.trend.slope    <- m.trend.slope;
      data$m.trend.constant <- m.trend.constant;
    }
    if(exists.temperature) {
      data$temp.trend.slope    <- temp.trend.slope;
      data$temp.trend.constant <- temp.trend.constant;
    }
    if(exists.rel.humidity) {
      data$hrel.trend.slope    <- hrel.trend.slope;
      data$hrel.trend.constant <- hrel.trend.constant;
    }
  }
  if(spike.treatment != "none") {
    data$u.spike.count <- u.spike.count;
    data$v.spike.count <- v.spike.count;
    data$w.spike.count <- w.spike.count;
    data$t.spike.count <- t.spike.count;
    if(exists.water) {
      data$q.spike.count <- q.spike.count;
    }
    if(exists.carbon.dioxide) {
      data$c.spike.count <- c.spike.count;
    }
    if(exists.ammonia) {
      data$a.spike.count <- a.spike.count;
    }
    if(exists.methane) {
      data$m.spike.count <- m.spike.count;
    }
    if(exists.temperature) {
      data$temp.spike.count <- temp.spike.count;
    }
    if(exists.rel.humidity) {
      data$hrel.spike.count <- hrel.spike.count;
    }
  }

  # Save manipulated data, if requested
  if(!is.null(file.dump)) {
    # Strip the final ".b" characters so that the resulting
    # data frame complies with SonicLib raw data naming conventions
    nm <- names(d.manip);
    for(i in 1:length(nm)) {
      nm[i] <- substr(nm[i],1,nchar(nm[i])-2);
    }
    names(d.manip) <- nm;
    # Write data to file
    write.csv(d.manip,file=file.dump, row.names=FALSE);
  }

  # Leave
  result <- list(
    data            = data,
    delay           = delay,
    spike           = spike.detection.threshold,
    spike.treatment = spike.treatment,
    sampling.rate   = sampling.rate,
    averaging.time  = averaging.time
  );
  class(result) <- "sonic.avg.data";
  return(result);

}


# Build an object of type "delay.spec" to be used as value of argument "delay"
# in routine "average.sonic.data".
set.delay <- function(q=NULL, c=NULL, a=NULL, m=NULL, temp=NULL, hrel=NULL) {

  # Set delay for water
  if(!is.null(q)) { delay.q <- 0.0; }
  else if(class(q)=="numeric") { delay.q <- q; }
  else { delay.q <- 0.0; }

  # Set delay for carbon dioxide
  if(!is.null(c)) { delay.c <- 0.0; }
  else if(class(c)=="numeric") { delay.c <- c; }
  else { delay.c <- 0.0; }

  # Set delay for ammonia
  if(!is.null(a)) { delay.a <- 0.0; }
  else if(class(a)=="numeric") { delay.a <- a; }
  else { delay.a <- 0.0; }

  # Set delay for methane
  if(!is.null(m)) { delay.m <- 0.0; }
  else if(class(m)=="numeric") { delay.m <- m; }
  else { delay.m <- 0.0; }

  # Set delay for temperature
  if(!is.null(temp)) { delay.temp <- 0.0; }
  else if(class(temp)=="numeric") { delay.temp <- temp; }
  else { delay.temp <- 0.0; }

  # Set delay for relative humidity
  if(!is.null(hrel)) { delay.hrel <- 0.0; }
  else if(class(hrel)=="numeric") { delay.hrel <- hrel; }
  else { delay.hrel <- 0.0; }

  # Compose object, return it and leave
  result <- list(
    q = delay.q,
    c = delay.c,
    a = delay.a,
    m = delay.m,
    temp = delay.temp,
    hrel = delay.hrel
  );
  class(result) <- "delay.spec";
  return(result);

}


# Average a sonic file set
average.sonic.file.set <- function(
    dir.name=".",
    time.zone="UTC",
    shift=0,
    averaging.time=30,
    sampling.rate=10,
    threshold=0.05,
    delay=0.3,
    trend.removal="none",
    spike.detection.threshold=3,
    spike.treatment="set.na",
    min.fraction.valid=0.75,
    min.valid.data=2,
    save.modified.raw.data=FALSE,
    R.in.name=FALSE,
    verbose=FALSE
) {

  # Get list of data files in directory and extract base time stamps from them
  file.list <- enumerate.sonic.csv(dir.name, generate.full.path.names=TRUE);
  n <- length(file.list);
  if(n <= 0) {
    if(verbose) {
      print("average.sonic.file.set:: Error: No CSV files in directory");
      return(NULL);
    }
  }
  t.s <- rep(as.POSIXct("1900-01-01 00:00:00",tz=time.zone), times=n);
  for(idx in 1:n) {
    t.s[idx] <- time.stamp.from.name(file.list[idx], time.zone, shift, R.in.name, verbose);
  }

  # Iteratively read data, process them and append results to data frame
  first <- TRUE;
  e <- NULL;
  for(idx in 1:n) {
    if(!is.null(t.s[idx])) {
      if(verbose) print(paste("Processing file", file.list[idx], sep=" "));
      if(save.modified.raw.data) {
        s <- file.list[idx];
        out.file.name <- paste(substr(s,1,nchar(s)-4),".manip.csv",sep="");
      }
      else {
        out.file.name <- NULL;
      }
      d <- get.raw.data(file.list[idx], sampling.rate, threshold, verbose);
      class(d);
      if(!is.null(d)) {
        if(save.modified.raw.data) {
          d.avg <- average.sonic.data(
            d=d,
            initial.stamp=t.s[idx],
            averaging.time=averaging.time,
            delay=delay,
            trend.removal=trend.removal,
            spike.detection.threshold=spike.detection.threshold,
            spike.treatment=spike.treatment,
            file.dump=out.file.name,
            verbose=verbose
          );
        }
        else {
          d.avg <- average.sonic.data(
            d=d,
            initial.stamp=t.s[idx],
            averaging.time=averaging.time,
            delay=delay,
            trend.removal=trend.removal,
            spike.detection.threshold=spike.detection.threshold,
            spike.treatment=spike.treatment,
            file.dump=NULL,
            verbose=verbose
          );
        }
        if(first) {
          e <- d.avg$data;
          first <-FALSE;
        }
        else {
          e <- rbind(e, d.avg$data);
        }
      }
      else {
        if(verbose) print(paste("Data file", file.list[idx], "has been rejected because of insufficient data", sep=" "));
      }
    }

  }

  # Leave
  result <- list(
    data            = e,
    time.zone       = time.zone,
    delay           = delay,
    spike           = spike.detection.threshold,
    spike.treatment = spike.treatment,
    trend.removal   = trend.removal,
    sampling.rate   = sampling.rate,
    averaging.time  = averaging.time
  );
  class(result) <- "sonic.avg.data";
  return(result);

}


# Perform axes rotation according to classical eddy covariance or planar fit, then
# compute important derived quantities.
eddy.covariance <- function(d, station.altitude, anemometer.height=10, mode="eddy.covariance", third.rotation.angular.threshold=10, verbose=FALSE) {

  # Check input parameters
  if(class(d) != "sonic.avg.data") {
    if(verbose) print("rotate.axes:: error: Data not an object of type 'sonic.avg-data'");
    return(NULL);
  }
  # Post condition: parameters make sense, may go on

  # Check which scalar fluxes exist
  is.h2o <- !is.null(d$data$q.avg);
  is.co2 <- !is.null(d$data$c.avg);
  is.nh3 <- !is.null(d$data$a.avg);
  is.ch4 <- !is.null(d$data$m.avg);

  # Pre-allocate workspace
  n <- length(d$data$t.stamp);
  rot.theta <- numeric(n);
  rot.phi   <- numeric(n);
  rot.psi   <- numeric(n);
  rot.alpha <- numeric(n);
  rot.beta  <- numeric(n);
  rot.gamma <- numeric(n);
  u.avg.rot <- numeric(n);
  v.avg.rot <- numeric(n);
  w.avg.rot <- numeric(n);
  uu.rot    <- numeric(n);
  uv.rot    <- numeric(n);
  uw.rot    <- numeric(n);
  vv.rot    <- numeric(n);
  vw.rot    <- numeric(n);
  ww.rot    <- numeric(n);
  ut.rot    <- numeric(n);
  vt.rot    <- numeric(n);
  wt.rot    <- numeric(n);
  if(is.h2o) {
    uq.rot  <- numeric(n);
    vq.rot  <- numeric(n);
    wq.rot  <- numeric(n);
  }
  if(is.co2) {
    uc.rot  <- numeric(n);
    vc.rot  <- numeric(n);
    wc.rot  <- numeric(n);
  }
  if(is.nh3) {
    ua.rot  <- numeric(n);
    va.rot  <- numeric(n);
    wa.rot  <- numeric(n);
  }
  if(is.ch4) {
    um.rot  <- numeric(n);
    vm.rot  <- numeric(n);
    wm.rot  <- numeric(n);
  }

  if(mode == "eddy.covariance" | mode == "eddy.covariance.3") {

    # Main loop: iterate over all averages
    for(item in 1:length(d$data$t.stamp)) {

      # Form relevant matrices
      wind.avg   <- matrix(data=c(d$data$u.avg[item], d$data$v.avg[item], d$data$w.avg[item]), nrow=3, ncol=1, byrow=TRUE);
      wind.cov   <- matrix(data=c(d$data$uu[item], d$data$uv[item], d$data$uw[item], d$data$uv[item], d$data$vv[item], d$data$vw[item], d$data$uw[item], d$data$vw[item], d$data$ww[item]), nrow=3, ncol=3, byrow=TRUE);
      wind.t.cov <- matrix(data=c(d$data$ut[item], d$data$vt[item], d$data$wt[item]), nrow=3, ncol=1, byrow=TRUE);
      if(is.h2o) {
        wind.q.cov <- matrix(data=c(d$data$uq[item], d$data$vq[item], d$data$wq[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.co2) {
        wind.c.cov <- matrix(data=c(d$data$uc[item], d$data$vc[item], d$data$wc[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.nh3) {
        wind.a.cov <- matrix(data=c(d$data$ua[item], d$data$va[item], d$data$wa[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.ch4) {
        wind.m.cov <- matrix(data=c(d$data$um[item], d$data$vm[item], d$data$wm[item]), nrow=3, ncol=1, byrow=TRUE);
      }

      # Store first rotation angle, for reference
      rot.theta[item] <- atan2(wind.avg[2], wind.avg[1])*180/pi;

      # Build and apply first rotation matrix
      denom <- sqrt(d$data$u.avg[item]^2 + d$data$v.avg[item]^2);
      if(!is.null(denom) && !is.na(denom) && denom > 1.e-3) {
        sin.a <- d$data$v.avg[item] / denom;
        cos.a <- d$data$u.avg[item] / denom;
      }
      else {
        sin.a <- 0.0;
        cos.a <- 1.0;
      }
      R01 <- matrix(c(cos.a, sin.a, 0, -sin.a, cos.a, 0, 0, 0, 1), nrow=3, ncol=3, byrow=TRUE);
      wind.avg <- R01 %*% wind.avg;
      wind.cov <- R01 %*% wind.cov %*% t(R01);
      wind.t.cov <- R01 %*% wind.t.cov;
      if(is.h2o) {
        wind.q.cov <- R01 %*% wind.q.cov;
      }
      if(is.co2) {
        wind.c.cov <- R01 %*% wind.c.cov;
      }
      if(is.nh3) {
        wind.a.cov <- R01 %*% wind.a.cov;
      }
      if(is.ch4) {
        wind.m.cov <- R01 %*% wind.m.cov;
      }

      # Store second rotation angle, for further reference
      rot.phi[item] <- atan2(wind.avg[3], sqrt(wind.avg[1]^2 + wind.avg[2]^2))*180/pi;

      # Build and apply second rotation matrix
      denom <- sqrt(wind.avg[1]^2 + wind.avg[3]^2);
      if(!is.null(denom) && !is.na(denom) && denom > 1.e-3) {
        sin.b <- wind.avg[3] / denom;
        cos.b <- wind.avg[1] / denom;
      }
      else {
        sin.b <- 0.0;
        cos.b <- 1.0;
      }
      R12 <- matrix(c(cos.b, 0, sin.b, 0, 1, 0, -sin.b, 0, cos.b), nrow=3, ncol=3, byrow=TRUE);
      wind.avg <- R12 %*% wind.avg;
      wind.cov <- R12 %*% wind.cov %*% t(R12);
      wind.t.cov <- R12 %*% wind.t.cov;
      if(is.h2o) {
        wind.q.cov <- R12 %*% wind.q.cov;
      }
      if(is.co2) {
        wind.c.cov <- R12 %*% wind.c.cov;
      }
      if(is.nh3) {
        wind.a.cov <- R12 %*% wind.a.cov;
      }
      if(is.ch4) {
        wind.m.cov <- R12 %*% wind.m.cov;
      }

      # Build and apply third rotation matrix, if requested
      if(mode == "eddy.covariance.3") {

        delta <- 0.5*atan2(2.*wind.cov[2,3], wind.cov[2,2] - wind.cov[3,3]);
        delta[(abs(delta*180/pi) > third.rotation.angular.threshold)] <- 0.0; # Suppress rotation exceeding desired threshold angle
        sin.d <- sin(delta);
        cos.d <- cos(delta);
        R23 <- matrix(c(1, 0, 0, 0, cos.d, sin.d, 0, -sin.d, cos.d), nrow=3, ncol=3, byrow=TRUE);
        wind.avg <- R23 %*% wind.avg;
        wind.cov <- R23 %*% wind.cov %*% t(R23);
        wind.t.cov <- R23 %*% wind.t.cov;
        if(is.h2o) {
          wind.q.cov <- R23 %*% wind.q.cov;
        }
        if(is.co2) {
          wind.c.cov <- R23 %*% wind.c.cov;
        }
        if(is.nh3) {
          wind.a.cov <- R23 %*% wind.a.cov;
        }
        if(is.ch4) {
          wind.m.cov <- R23 %*% wind.m.cov;
        }

      }
      else {
        delta <- 0.0;
      }

      # Store third rotation angle, for future reference
      rot.psi[item] <- delta*180/pi;

      # Reassemble result
      u.avg.rot[item] <- wind.avg[1];
      v.avg.rot[item] <- wind.avg[2];
      w.avg.rot[item] <- wind.avg[3];
      uu.rot[item]    <- wind.cov[1,1];
      uv.rot[item]    <- wind.cov[1,2];
      uw.rot[item]    <- wind.cov[1,3];
      vv.rot[item]    <- wind.cov[2,2];
      vw.rot[item]    <- wind.cov[2,3];
      ww.rot[item]    <- wind.cov[3,3];
      ut.rot[item]    <- wind.t.cov[1];
      vt.rot[item]    <- wind.t.cov[2];
      wt.rot[item]    <- wind.t.cov[3];
      if(is.h2o) {
        uq.rot[item]    <- wind.q.cov[1];
        vq.rot[item]    <- wind.q.cov[2];
        wq.rot[item]    <- wind.q.cov[3];
      }
      if(is.co2) {
        uc.rot[item]    <- wind.c.cov[1];
        vc.rot[item]    <- wind.c.cov[2];
        wc.rot[item]    <- wind.c.cov[3];
      }
      if(is.nh3) {
        ua.rot[item]    <- wind.a.cov[1];
        va.rot[item]    <- wind.a.cov[2];
        wa.rot[item]    <- wind.a.cov[3];
      }
      if(is.ch4) {
        um.rot[item]    <- wind.m.cov[1];
        vm.rot[item]    <- wind.m.cov[2];
        wm.rot[item]    <- wind.m.cov[3];
      }

    }
  }

  else if(mode == "planar.fit") {

    # Get wind averages, in non rotated frame
    u <- d$data$u.avg;
    v <- d$data$v.avg;
    w <- d$data$w.avg;

    # Estimate best fitting plane by linear least squares; the resulting plane
    # has equation
    #
    #    w = b0 + b1*u + b2*v
    #
    # whose coefficients are stored as part of linear model (see R reference)
    plane <- lm(w~u+v);
    b0 <- plane$coefficients[1];
    b1 <- plane$coefficients[2];
    b2 <- plane$coefficients[3];

    # Compute plane-derived rotation matrices
    denom.beta  <- sqrt(b1^2+b2^2+1);
    denom.gamma <- sqrt(b2^2+1);
    sin.beta    <- -b1/denom.beta;
    cos.beta    <- denom.gamma/denom.beta;
    sin.gamma   <- b2/denom.gamma;
    cos.gamma   <- 1/denom.gamma;
    R12 <- matrix(c(cos.beta, 0, sin.beta, 0, 1, 0, -sin.beta, 0, cos.beta), nrow=3, ncol=3, byrow=TRUE);
    R23 <- matrix(c(1, 0, 0, 0, cos.gamma, sin.gamma, 0, -sin.gamma, cos.gamma), nrow=3, ncol=3, byrow=TRUE);
    R13 <- R23 %*% R12;

    # Main loop: iterate over all averages
    for(item in 1:length(t.stamp)) {

      # Form relevant matrices
      wind.avg   <- matrix(data=c(d$data$u.avg[item], d$data$v.avg[item], d$data$w.avg[item]), nrow=3, ncol=1, byrow=TRUE);
      wind.cov   <- matrix(data=c(d$data$uu[item], d$data$uv[item], d$data$uw[item], d$data$uv[item], d$data$vv[item], d$data$vw[item], d$data$uw[item], d$data$vw[item], d$data$ww[item]), nrow=3, ncol=3, byrow=TRUE);
      wind.t.cov <- matrix(data=c(d$data$ut[item], d$data$vt[item], d$data$wt[item]), nrow=3, ncol=1, byrow=TRUE);
      if(is.h2o) {
        wind.q.cov <- matrix(data=c(d$data$uq[item], d$data$vq[item], d$data$wq[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.co2) {
        wind.c.cov <- matrix(data=c(d$data$uc[item], d$data$vc[item], d$data$wc[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.nh3) {
        wind.a.cov <- matrix(data=c(d$data$ua[item], d$data$va[item], d$data$wa[item]), nrow=3, ncol=1, byrow=TRUE);
      }
      if(is.ch4) {
        wind.m.cov <- matrix(data=c(d$data$um[item], d$data$vm[item], d$data$wm[item]), nrow=3, ncol=1, byrow=TRUE);
      }

      # Apply plane-derived rotation matrix
      wind.avg <- R13 %*% wind.avg;
      wind.cov <- R13 %*% wind.cov %*% t(R13);
      wind.t.cov <- R13 %*% wind.t.cov;
      if(is.h2o) {
        wind.q.cov <- R13 %*% wind.q.cov;
      }
      if(is.co2) {
        wind.c.cov <- R13 %*% wind.c.cov;
      }
      if(is.nh3) {
        wind.a.cov <- R13 %*% wind.a.cov;
      }
      if(is.ch4) {
        wind.m.cov <- R13 %*% wind.m.cov;
      }

      # Build and apply data item specific matrix
      alpha <- atan2(wind.avg[2],wind.avg[1]);
      sin.alpha <- sin(alpha);
      cos.alpha <- cos(alpha);
      S <- matrix(c(cos.alpha, sin.alpha, 0, -sin.alpha, cos.alpha, 0, 0, 0, 1), nrow=3, ncol=3, byrow=TRUE);
      wind.avg <- S %*% wind.avg;
      wind.cov <- S %*% wind.cov %*% t(S);
      wind.t.cov <- S %*% wind.t.cov;
      if(is.h2o) {
        wind.q.cov <- S %*% wind.q.cov;
      }
      if(is.co2) {
        wind.c.cov <- S %*% wind.c.cov;
      }
      if(is.nh3) {
        wind.a.cov <- S %*% wind.a.cov;
      }
      if(is.ch4) {
        wind.m.cov <- S %*% wind.m.cov;
      }

      # Note: Third rotation intentionally not performed (for now)

      # Reassemble result
      u.avg.rot[item] <- wind.avg[1];
      v.avg.rot[item] <- wind.avg[2];
      w.avg.rot[item] <- wind.avg[3];
      uu.rot[item]    <- wind.cov[1,1];
      uv.rot[item]    <- wind.cov[1,2];
      uw.rot[item]    <- wind.cov[1,3];
      vv.rot[item]    <- wind.cov[2,2];
      vw.rot[item]    <- wind.cov[2,3];
      ww.rot[item]    <- wind.cov[3,3];
      ut.rot[item]    <- wind.t.cov[1];
      vt.rot[item]    <- wind.t.cov[2];
      wt.rot[item]    <- wind.t.cov[3];
      if(is.h2o) {
        uq.rot[item]    <- wind.q.cov[1];
        vq.rot[item]    <- wind.q.cov[2];
        wq.rot[item]    <- wind.q.cov[3];
      }
      if(is.co2) {
        uc.rot[item]    <- wind.c.cov[1];
        vc.rot[item]    <- wind.c.cov[2];
        wc.rot[item]    <- wind.c.cov[3];
      }
      if(is.nh3) {
        ua.rot[item]    <- wind.a.cov[1];
        va.rot[item]    <- wind.a.cov[2];
        wa.rot[item]    <- wind.a.cov[3];
      }
      if(is.ch4) {
        um.rot[item]    <- wind.m.cov[1];
        vm.rot[item]    <- wind.m.cov[2];
        wm.rot[item]    <- wind.m.cov[3];
      }

      # Store rotation angles, for further reference
      rot.alpha[item] <- alpha;
      rot.beta[item]  <- beta;
      rot.gamma[item] <- gamma;
    }

  }

  #########################
  # Compute basic indices #
  #########################

  # Compute wind provenance direction (useful to classify data)
  Dir <- atan2(-d$data$u.avg,-d$data$v.avg) * 180/pi;
  Dir.negative <- !is.na(Dir) & Dir < 0;
  Dir[Dir.negative] <- Dir[Dir.negative] + 360.0;

  # Compute air density from sonic temperature
  Ta <- d$data$t.avg + 273.15;
  Pa <- 1013.0 * exp(-0.0342*station.altitude/Ta);
  Rho.d <- 348.4*Pa/Ta;   # Density of dry air [g/m3]
  RhoCp <- 350.25*Pa/Ta;

  # Friction velocity, according to the signless definition
  u.star <- (uw.rot^2 + vw.rot^2)^0.25;

  # Sonic H0
  H0.v <- RhoCp * wt.rot;  # This is not the "sensible heat flux", but rather the "Surface Buoyancy Flux"

  # Obukhov length
  L <- -Ta / (0.4*9.81) * u.star^3/wt.rot;

  # Stability parameter
  z.over.L <- anemometer.height / L;

  # Scale temperature
  T.star <- -wt.rot / u.star;

  ####################################
  # Perform WPL correction of fluxes #
  ####################################

  # For WPL correction to be performed water must be measured
  if(is.h2o) {

    # Useful constants
    MOL.H2O <- 18.0153;  # Molar weight of water (g/mol)
    MOL.CO2 <- 44.0100;  # Molar weight of carbon dioxide (g/mol)
    MOL.NH3 <- 17.0305;  # Molar weight of ammonia (g/mol)
    MOL.CH4 <- 16.0425;  # Molar weight of methane (g/mol)
    MOL.Air <- 28.96;    # Molar weight of dry air (g/mol)

    # Air molar concentration
    c.d <- 1000.0*Rho.d/MOL.Air;   # [g/m3] / [g/mmol] = [g/m3] * [mmol/g] = [mmol/m3]

    # Water specific processing
    q.d <- d$data$q.avg / c.d;                             # Adimensional ratio
    c.dv <- c.d + d$data$q.avg;                            # Molar concentration of moist air [mmol/m3]
    t.c <- c.dv*wt.rot/Ta;                                 # [mmol/m3] [m K/s] / [K] = [mmol/(m2 s)]
    Fq.molar <- wq.rot + q.d * (t.c + wq.rot);             # [mmol/(m2 s)]
    Fq.mass  <- MOL.H2O*Fq.molar;                          # [mg/(m2 s)]

    # Thermal effect correction (Schotanus)
    mix.factor <- MOL.H2O/Rho.d/1000;
    wt.cor     <- wt.rot - 0.51*mix.factor*Ta*wq.rot;
    H0         <- RhoCp * wt.cor;

    # Latent heat
    lambda     <- 2500.8 - 2.36*d$data$t.avg + 0.0016*d$data$t.avg^2 - 0.00006*d$data$t.avg^3;  # Latent condensation heat foe water [J/g] (temperature in °C)
    He         <- lambda/1000 * Fq.mass;

    # Carbon dioxide specific processing
    if(is.co2) {
      q.c <- d$data$c.avg / c.d;                           # Adimensional ratio
      Fc.molar <- wc.rot + q.c * (t.c + wc.rot);           # [mmol/(m2 s)]
      Fc.mass  <- MOL.CO2*Fc.molar;                        # [mg/(m2 s)]
    }

    # Ammonia specific processing
    if(is.nh3) {
      q.a <- d$data$a.avg / c.d;                           # Adimensional ratio
      Fa.molar <- wa.rot + q.a * (t.c + wa.rot);           # [mmol/(m2 s)]
      Fa.mass  <- MOL.NH3*Fa.molar;                        # [mg/(m2 s)]
    }

    # Methane specific processing
    if(is.ch4) {
      q.m <- d$data$m.avg / c.d;                           # Adimensional ratio
      Fm.molar <- wm.rot + q.m * (t.c + wm.rot);           # [mmol/(m2 s)]
      Fm.mass  <- MOL.CH4*Fm.molar;                        # [mg/(m2 s)]
    }

  }

  # Assemble final data frame
  e <- d$data;
  if(mode == "eddy.covariance" | mode == "eddy.covariance.3") {
    e$theta <- rot.theta;
    e$phi   <- rot.phi;
    e$psi   <- rot.psi;
  }
  if(mode == "planar.fit") {
    e$alpha <- rot.alpha;
    e$beta  <- rot.beta;
    e$gamma <- rot.gamma;
  }
  e$u.avg.rot <- u.avg.rot;
  e$v.avg.rot <- v.avg.rot;
  e$w.avg.rot <- w.avg.rot;
  e$Dir    <- Dir;
  e$uu.rot <- uu.rot;
  e$uv.rot <- uv.rot;
  e$uw.rot <- uw.rot;
  e$vv.rot <- vv.rot;
  e$vw.rot <- vw.rot;
  e$ww.rot <- ww.rot;
  e$ut.rot <- ut.rot;
  e$vt.rot <- vt.rot;
  e$wt.rot <- wt.rot;
  e$u.star   <- u.star;
  e$H0.v     <- H0.v;
  e$L        <- L;
  e$z.over.L <- z.over.L;
  e$T.star   <- T.star;
  e$Rho.air  <- Rho.d;
  e$Rho.Cp   <- RhoCp;
  e$Pa       <- Pa;
  if(is.h2o) {
    e$wq.rot <- wq.rot;  # [mmol/(m2 s)]
    e$c.d    <- c.d;     # Molar concentration of dry air ([mmol/m3])
    e$q.d    <- q.d;     # Ratio of molar concentration of water to molar concentration of dry air (adimensional)
    e$c.dv   <- c.dv;    # Molar concentration of dry air + water (that is, moist air; [mmol/m3])
    e$t.c    <- t.c;     # Thermal component of WPL correction ([mmol/m3])
    e$Fq.molar <- Fq.molar;  # Turbulent flux of water along vertical in molar form ([mmol/(m2 s)])
    e$Fq.mass  <- Fq.mass;   # Turbulent flux of water along vertical in mass form (g/(m2 s)])
    e$mix.f    <- mix.factor;  # Conversion factor from [mmol/m3] to [g/g] for water
    e$wt.cor   <- wt.cor;      # w'T', with T real temperature and not sonic temperature
    e$H0       <- H0;          # Sensible heat flux, in strict sense [W/m2]
    e$lambda   <- lambda;      # Condensation latent heat [J/g]
    e$He       <- He;          # Latent heat flux [W/m2]
  }
  if(is.co2) {
    e$wc.rot <- wc.rot;  # [mmol/(m2 s)]
    e$q.c    <- q.c;     # Ratio of molar concentration of carbon dioxide to molar concentration of dry air (adimensional)
    e$Fc.molar <- Fc.molar;  # Turbulent flux of carbon dioxide along vertical in molar form ([mmol/(m2 s)])
    e$Fc.mass  <- Fc.mass;   # Turbulent flux of carbon dioxide along vertical in mass form (g/(m2 s)])
  }
  if(is.nh3) {
    e$wa.rot <- wa.rot;  # [mmol/(m2 s)]
  }
  if(is.ch4) {
    e$wm.rot <- wm.rot;  # [mmol/(m2 s)]
  }

  # Leave
  result <- list(
    data              = e,
    time.zone         = d$time.zone,
    delay             = d$delay,
    spike             = d$spike,
    spike.treatment   = d$spike.treatment,
    trend.removal     = d$trend.removal,
    sampling.rate     = d$sampling.rate,
    averaging.time    = d$averaging.time,
    station.altitude  = station.altitude,
    anemometer.height = anemometer.height,
    processing        = mode
  );
  class(result) <- "sonic.eddy.cov.data";
  return(result);

}

#######################
# Air quality support #
#######################

# Golder (1972) stability category estimation from similarity data,
# as from LSTAB function in CTDM+

lstab <- function(L,z0) {

  # Line separating the z0-L graph in Golder paper, as approximated in CTDM+
  XL <- function(Y,XM,B) {
    return(XM/(log(Y)-B));
  }

  # Ensure z0 to be within common range
  z0 <- min(c(z0,0.5));
  z0 <- max(c(z0,0.01));

  # Build separation lines
  unstable.set <- c(
    XL(z0,-70.0,4.35),
    XL(z0,-85.2,0.502),
    XL(z0,-245.,0.050)
  );
  stable.set <- c(
    XL(z0,-70.0,0.295),
    XL(z0,-327.,0.627)
  );

  # Classify stability depending on L
  istab <- integer(length(L));
  unstable.data <- which(L < 0);
  stable.data   <- which(L >= 0);
  unstable.L    <- L[unstable.data];
  stable.L      <- L[stable.data];
  istab.unstable <- approxfun(x=unstable.set, y=c(1,2,3), yleft=1, yright=4, method="constant");
  istab.stable   <- approxfun(x=stable.set,   y=c(5,4),   yleft=6, yright=4, method="constant");
  istab[unstable.data] <- istab.unstable(unstable.L);
  istab[stable.data]   <- istab.stable(stable.L);

  # Yield result and return
  return(istab);
}


# -- Wind speed similarity function

psi.m <- function(z.over.L) {
  n        <- length(z.over.L);
  val      <- numeric(n);
  x        <- numeric(n);
  stable   <- which(z.over.L >= 0);
  unstable <- which(z.over.L <  0);
  val[stable]   <- -5*z.over.L[stable];
  x[unstable]   <- (1 - 16*z.over.L[unstable])^0.25;
  val[unstable] <- log((1+x[unstable]^2)/2 * ((1+x[unstable])/2)^2) - 2*atan(x[unstable]) + pi/2;
  return(val);
}

# Surface roughness rough estimate

estimate.z0<-function(ed, time.stamp.from=NULL, time.stamp.to=NULL, verbose=FALSE){

  if(class(ed) != "sonic.eddy.cov.data") {
    if(verbose) print("estimate.z0:: error: Input not of type 'sonic.eddy.cov.data'");
    return(NULL);
  }

  # Ensure proper ordering of selector dates
  if(!is.null(time.stamp.from) & !is.null(time.stamp.to)) {
    if(time.stamp.from > time.stamp.to) {
      temp.time.stamp <- time.stamp.from;
      time.stamp.from <- time.stamp.to;
      time.stamp.to   <- temp.time.stamp;
    }
  }

  # Select useful data
  if(!is.null(time.stamp.from) & !is.null(time.stamp.to)) {
    ed$data <- ed$data[time.stamp.from <= ed$data$t.stamp & ed$data$t.stamp <= time.stamp.to,];
  }
  time.stamp   <- ed$data$t.stamp;
  z            <- ed$anemometer.height;
  L            <- ed$data$L;
  dir          <- ed$data$Dir;
  vel          <- ed$data$vel;
  u.star       <- ed$data$u.star;
  n.data       <- length(L);

  # Prepare estimate of surface roughness by computing its tributaries
  z.over.L     <- z / L;
  U.over.ustar <- vel / u.star;

  # Restrict z/L to the range where validity of wind similarity function has been
  # verified experimentally (Beljaars, Holtslag, "Flux parameterization over land surfaces for
  # atmospheric models", J. Appl. Meteorol., 30, pp.327-341, 1991)
  z.over.L[z.over.L < (-2)] <- NA;
  z.over.L[z.over.L >   10] <- NA;

  # Estimate aerodynamic roughness length from SL similarity
  U.over.ustar[u.star<0.03] <- NA; # Exclude unrealistic values from z0 estimation
  U.over.ustar[u.star>1   ] <- NA;
  indicator   <- 0.4*U.over.ustar+psi.m(z.over.L);
  indicator[indicator < 0] <- NA;
  z0.mean     <- z/exp(median(indicator,na.rm=TRUE));
  if(is.null(z0)) z0 <- z0.mean;

  # Build directional median of surface roughness
  indicator.d <- dir.mean(dir, indicator, confidence="none");
  z0.d        <- data.frame(
    dir   = indicator.d$c.dir,
    value = z/exp(indicator.d$median)
  );

  # Assemble and yield result
  result <- list(
    time.stamp.from   = time.stamp.from,
    time.stamp.to     = time.stamp.to,
    z0.mean           = z0.mean,
    z0.d              = z0.d
  );
  class(result) <- "z0.estimates";
  return(result);

}


# Function to perform various calculations of interest to air quality, among which mixing
# height, stability categories and Deardoff velocity.
#
# The largest and most significant part of this routine has been developed within the
# Laboratory held in 2013 at Physics Department, University of Milan. Code was
# contributed by:
#
#   Davide Casabianca
#   Alice Crespi
#   Manuela Dell'Acqua
#   Luca Palazzolo
#
# and
#
#   Patrizia Favaron (supervisor)
#
# One of the most delicate, and questionable, parts in this routine is assuming a fixed
# expression of temperature lapse rate to fit all possible cases. In his thesis,
#
#   Roberto Nava
#
# has shown this not to be the case, the lapse rate depending on site and showing
# an important seasonality. This work is ongoing, in the direction of a published
# paper, but as an immediate result we have been proud to enhance the parameter list
# by adding the three coefficient of the lapse rate formula, with default values
# as from previous version fixed values. This addition lets users specify their own
# parameters. In future a helper function may be added too - but for this we'll wait for
# the paper results.
#
air.quality<-function(ed, lat, lon, zone, time.zone="UTC", z0=0.023, h0.threshold=10, N=0.012, Gamma.A=3, Gamma.B=1.98e-3, Gamma.C=2.27e-6, GB.initialization="zero", verbose=FALSE){

  #######################
  # Auxiliary functions #
  #######################

  # -- Gryning-Batchvarova convective mixing height determination ---

  g <- function(z, Gamma.A, Gamma.B, Gamma.C) {
    out <- Gamma.A/(z+1) - Gamma.B + Gamma.C*z;
    return(out);
  }

  gry.bat <- function(t, zi, parms) {
    out <- list(
      c(
        parms$wt/g(zi, parms$Gamma.A, parms$Gamma.B, parms$Gamma.C) * 1/(zi^2/(1.4*zi-2*parms$L) + 8*parms$u.star^2*parms$T/(g(zi, parms$Gamma.A, parms$Gamma.B, parms$Gamma.C)*9.807*(1.2*zi-parms$L)))
      )
    );
    return(out);
  }

  ####################
  # Useful constants #
  ####################

  G <- 9.81;  # Gravity contant

  ###############
  # Actual code #
  ###############

  if(class(ed) != "sonic.eddy.cov.data") {
    if(verbose) print("air.quality:: error: Input not of type 'sonic.eddy.cov.data'");
    return(NULL);
  }

  # Coriolis parameter
  f <- 2*7.29e-5*sin(lat*pi/180);

  # Select useful data
  time.stamp   <- ed$data$t.stamp;
  wt           <- ed$data$wt.rot;
  L            <- ed$data$L;
  dir          <- ed$data$Dir;
  u.star       <- ed$data$u.star;
  T            <- ed$data$t.avg;
  H0           <- ed$data$H0.v;
  Ta           <- (T+273.15);
  avg.time     <- ed$averaging.time;
  z            <- ed$anemometer.height;
  date         <- substr(time.stamp,6,10);
  n.data       <- length(L);

  # Preallocate output vectors (this will spare R a lot of behind-the-scenes work)
  a              <- rep(NA, times=n.data);
  b1             <- rep(NA, times=n.data);
  b2             <- rep(NA, times=n.data);
  b3             <- rep(NA, times=n.data);
  b              <- rep(NA, times=n.data);
  zi             <- rep(NA, times=n.data);
  zi.convective  <- rep(NA, times=n.data);
  zi.stable      <- rep(NA, times=n.data);
  zi.neutral     <- rep(NA, times=n.data);
  zi.mechanical  <- rep(NA, times=n.data);
  zi.mech.stable <- rep(NA, times=n.data);

  # Classify situations based on turbulent flux of sensible heat; the "h0.threshold" value
  # is a non-negative value whose default, 0, excludes neutral cases. Sensible determination
  # of threshold should be made on a site basis.
  stable     <- which(H0 < (-h0.threshold));
  neutral    <- which(abs(H0) <= h0.threshold);
  convective <- which(H0 > h0.threshold);

  # Compute the main terms of diagnostic-only Zilitinkevich parameterization
  # and assemble them to equilibrium height equation. Once this has been found,
  # solve it and form stable part.
  a[stable]  <-(f/(0.5*u.star[stable]))^2;
  b1[stable] <-(1/(10*L[stable]));
  b2[stable] <-(N/(20*u.star[stable]));
  b3[stable] <-(sqrt(abs((9.807*wt[stable]*f)/Ta[stable]))/(1.7*u.star[stable]));
  b[stable]  <- b1[stable]+b2[stable]+b3[stable];
  for(k in stable) zi.stable[k] <- max(Re(polyroot(c(-1,b[k],a[k]))));

  # Estimate the mechanical only mixing height using "u.star" parameterization.
  # Warning: the coefficient 1330 should in reality depend on Coriolis parameter;
  # ======== in this edition we don't care. In some next version, dependence will
  #          be made explicit.
  zi.mechanical <- 1330*u.star;

  # Compose the "mechanical basis" of mixing height by assuming Zilitinkevich
  # parameterization in stable hours, and mechanical "ustar" parameterization
  # on all neutral and convective situations.
  zi.mech.stable <- zi.mechanical;
  zi.mech.stable[stable] <- zi.stable[stable];
  # Post-condition: "zi.mech.stable" is (or better, "should be") defined for all hours,
  # and not only the stable ones as pure Zilitinkevich method would.

  # Gryning-Batchvarova model, applied to all convective block of day. This
  # section is composed by two nested loops, an outer one iterating over days
  # and an inner one running GB equation on actual inner part.
  #
  # -1- Locate the beginning day in data set
  seconds.per.day <- 24*3600;
  origin    <- as.POSIXct("1970-01-01 00:00:00", tz=time.zone);
  first.day <- as.POSIXct(as.integer(min(time.stamp)) %/% seconds.per.day * seconds.per.day, tz=time.zone, origin=origin);
  # -1- Ensure first day is complete
  if(time.stamp[1] != first.day) first.day <- as.POSIXct((as.integer(min(time.stamp)) %/% seconds.per.day + 1) * seconds.per.day, tz=time.zone, origin=origin);
  # Locate the first day immediately after completion of data set, and count available days based on this.
  last.day  <- as.POSIXct(as.integer(max(time.stamp)) %/% seconds.per.day * seconds.per.day + seconds.per.day, tz=time.zone, origin=origin);
  num.days  <- (as.integer(last.day) - as.integer(first.day))%/%seconds.per.day;
  # -1- Actual loop
  for(day.index in 1:num.days) {
    # -2- Delimit current day
    this.day.begin      <- as.POSIXct(as.integer(first.day) + (day.index-1)*seconds.per.day, tz=time.zone, origin=origin);
    this.day.end        <- as.POSIXct(as.integer(this.day.begin) + seconds.per.day - 1, tz=time.zone, origin=origin);
    this.day            <- this.day.begin <= time.stamp & time.stamp <= this.day.end;
    this.day.index      <- which(this.day);
    convective.this.day <- which(this.day.begin <= time.stamp & time.stamp <= this.day.end & H0 > h0.threshold);
    # -2- Check something convective exists in this day
    if(length(convective.this.day) > 0) {
      # -3- Delimit convective part of this day
      begin.convective <- min(convective.this.day);
      if(GB.initialization == "mechanical") {
        if(begin.convective > 1) {
          initial.value <- c(zi=zi.mech.stable[begin.convective-1]);
        }
        else {
          initial.value <- c(zi=zi.mech.stable[begin.convective]);
        }
      }
      else {
        initial.value <- c(zi=0);
      }
      end.convective <- max(convective.this.day);
      # -3- Actual Gryning-Batchvarova step
      for(k in begin.convective:end.convective) {
        parms            <- list(wt=wt[k], L=L[k], u.star=u.star[k], T=Ta[k], Gamma.A=Gamma.A, Gamma.B=Gamma.B, Gamma.C=Gamma.C);
        de               <- ode(y=initial.value, func=gry.bat, parms=parms, method="rk4", times=seq(from=0,to=60*avg.time,by=60*avg.time));
        dd               <- as.data.frame(de);
        zi.convective[k] <- dd$zi[2];
        initial.value    <- c(zi=dd$zi[2]);
      }
    }
  }

  # Build final mixing height as maximum between "stable-mechanical" and convective, where the latter exists.
  zi <- zi.mech.stable;
  for(k in 1:n.data) {
    if(!is.na(zi.convective[k])) {
      zi[k] <- max(c(zi.mech.stable[k], zi.convective[k]));
    }
  }

  # Compute Deardoff velocity
  w.star <- numeric(length(n.data));
  w.star <- ifelse(!is.na(H0) & H0>0, (G/Ta * zi * ed$data$wt.rot)^(1/3), 0.)
  #w.star[H0 <= 0] <- 0.;
  #w.star[H0 > 0]  <- (G/Ta[H0 > 0] * zi[H0 > 0] * ed$data$wt.rot[H0 > 0])^(1/3);

  # Estimate stability categories using Golder (1972) method as from CtDM+ "LSTAB" function
  stability <- lstab(ed$data$L, z0);

  # Determine day-, night- and transitionary character of any time
  diy <- day.in.year(time.stamp, time.zone);
  split.time <- as.POSIXlt(time.stamp, tz=time.zone);
  hour <- split.time$hour + split.time$min/60 + split.time$sec/3600;
  solar.declination <- 0.409*cos(2*pi*(diy - 173)/365.25);
  lat.rad <- lat*pi/180;
  lon.rad <- lon*pi/180;
  sine.solar.elevation <- sin(lat.rad)*sin(solar.declination) - cos(lat.rad)*cos(solar.declination)*cos(pi*(hour - zone)/12 - lon.rad);

  # Assemble and yield result
  data.out <- data.frame(ed$data, zi, zi.convective, zi.stable, zi.mechanical, w.star, stability, solar.declination, sine.solar.elevation);
  result <- list(
    data              = data.out,
    time.zone         = ed$time.zone,
    delay             = ed$delay,
    spike             = ed$spike,
    spike.treatment   = ed$spike.treatment,
    trend.removal     = ed$trend.removal,
    sampling.rate     = ed$sampling.rate,
    averaging.time    = ed$averaging.time,
    station.altitude  = ed$station.altitude,
    anemometer.height = ed$anemometer.height,
    processing        = ed$processing,
    H0.threshold      = h0.threshold,
    lat               = lat,
    lon               = lon,
    nominal.zone      = zone,
    Brunt.Vaisala     = N,
    z0                = z0,
    GB.initialization = GB.initialization
  );
  class(result) <- "air.quality";
  return(result);

}


export.air.quality <- function(a, file, verbose=FALSE) {

  if(class(a) != "air.quality") {
    if(verbose) print("export.air.quality:: error: Input not of type 'air.quality'");
    return(NULL);
  }

  d <- a$data;
  for(i in 2:ncol(d)) {
    na.pos <- which(is.na(d[,i]));
    if(length(na.pos) > 0) {
      d[na.pos,i] <- -9999;
    }
  }

  write.csv(d, file=file, row.names=FALSE);

  return(d);

}


############################
# Visual analysis routines #
############################

# Show the "deep autocorrelation" of one of the variables in an object of type
# "sonic.raw.data" passed on input, using a "thin line" representation more helpful
# to us than the usual bargraph used for econometric time series.
show.acf <- function(d, variable="w", depth=0.5, pdf="", width=5, height=4, verbose=FALSE) {

  # Input:
  #
  #   d            An object of type "sonic.raw.data"
  #
  #   variable     The name of a variable in "d".
  #                Allowed values are "u", "v", "w", "t", "q", "c",
  #                "a", "m", "temp", "urel". Default: "w".
  #
  #   depth        Number, between 0 and 1, signifying the maximum lag used
  #                to compute autocorrelation expressed as a fraction of total data.
  #                Default: 0.5.
  #
  #   pdf          String which may be empty, if the ACF is to be plotted on
  #                display, or non-empty, and designating in case the name
  #                of a PDF file where the plot is routed; in this latter case
  #                plotting parameters "width" and "height" are considered.
  #                Default: an empty string, meaning plot is needed on display.
  #
  #   width        Page width, used to generate the PDF file (if "pdf" parameter
  #                contains a non-empty string). Inches. Default: 5.
  #                Ignored, if "pdf" contains an empty string.
  #
  #   height       Page height, used to generate the PDF file. Inches. Default: 4.
  #                Ignored, if "pdf" contains an empty string.
  #
  #   verbose      Boolean flag: TRUE to print error / progress messages, FALSE to
  #                not print (default: FALSE)
  #
  # Output:
  #
  #   A vector, containing ACF values at each lag considered. Just for reference.
  #

  # Check the type of "d" to be OK
  if(class(d) != "sonic.raw.data") {
    if(verbose) print("show.acf:: error: Input parameter 'd' is not an object of type 'sonic.raw.data'");
    return(NULL);
  }

  # How many lags to include?
  if(!is.numeric(depth) | depth < 0 | depth > 1) {
    if(verbose) print("show.acf:: error: Invalid parameter 'depth': non numeric, or not in interval [0,1]");
    return(NULL);
  }
  max.lag <- as.integer(round(length(d$data$time.stamp) * depth));
  if(max.lag < 2) {
    if(verbose) print("show.acf:: error: Maximum desired lag less than 2; check 'd' contains some data, and 'depth' to be not too small");
    return(NULL);
  }

  # Get the appropriate variable
  if(variable=="u") {
    x <- d$data$u;
  }
  else if(variable=="v") {
    x <- d$data$v;
  }
  else if(variable=="w") {
    x <- d$data$w;
  }
  else if(variable=="t") {
    x <- d$data$t;
  }
  else if(variable=="q") {
    x <- d$data$q;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'q' (water) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="c") {
    x <- d$data$c;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'c' (carbon dioxide) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="a") {
    x <- d$data$a;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'a' (ammonia) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="m") {
    x <- d$data$m;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'm' (methane) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="temp") {
    x <- d$data$temp;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'temp' (temperature) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="hrel") {
    x <- d$data$hrel;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'hrel' (relative humidity) not belonging to data set 'd'");
      return(NULL);
    }
  }
  # Post-condition: Variable name is OK (that is, belongs to data set 'd'), and
  # all its contents has been read into vector 'x'.

  # Redirect output to PDF file, if desired
  if(pdf != "") {
    pdf(file=pdf, width=width, height=height);
  }

  # Compute autocorrelation values and plot them
  acf.x <- acf(x, lag.max=max.lag, plot=FALSE, na.action=na.exclude);
  acor <- acf.x$acf;
  plot(
    x=(0:(length(acor)-1))/d$sampling.rate,
    y=acor,
    type="l",
    xlab="lag (s)",
    ylab=paste("ACF(", variable, ")", sep=""),
    ylim=c(0,1)
  );

  # Plot a reference line at 0 to help detecting zero.crosses
  abline(a=0, b=0, col="blue");

  # Disconnect from PDF and close it, if required
  if(pdf != "") {
    dev.off();
  }

  # Leave
  return(acor);

}


###########################################
# Compute and plot directional statistics #
###########################################

# Directional mean of a value.
dir.mean <- function(dir, val, width=11.25, step=1, confidence="boot", boot.size=1000, conf.level=0.95) {

  # Inputs:
  #
  #  dir:  Vector containing measured directions (degrees from N)
  #
  #  val:  Vector, of same length as 'dir', with values to average
  #
  #  width=11.25: Sector amplitude to use (degrees)
  #
  #  step=1: Separation among steps (degrees) (it should better
  #          be less than or equal to 'width')
  #
  # Output:
  #
  #  An object of type 'dir.stat'.
  #

  # Main loop: Iterate over steps
  rng <- seq(from=0, to=360-step, by=step);
  n   <- length(rng);
  center.dir <- numeric(n);
  n.data  <- numeric(n);
  m   <- numeric(n);
  s   <- numeric(n);
  val.min <- numeric(n);
  val.p25 <- numeric(n);
  val.mdn <- numeric(n);
  val.p75 <- numeric(n);
  val.max <- numeric(n);
  c95.inf <- numeric(n);
  c95.sup <- numeric(n);
  i <- 1;
  for(d in rng) {

    # Build lower and upper class limits, in real form. Once
    # defined, convert them to modular form (this, by using
    # the auxiliary boolean "splitted" which, if TRUE, indicates
    # the class being splitted among d.min:360 and 0:d.max)
    d.min <- d - width/2;
    d.max <- d + width/2;
    splitted.low  <- (d.min < 0);
    splitted.high <- (d.max > 360);
    if(splitted.low) {
      d.min <- d.min + 360;
    }
    if(splitted.high) {
      d.max <- d.max - 360;
    }

    # Select data in current class
    if(splitted.low || splitted.high) {
      idx <- dir >= d.min | dir <= d.max;
    }
    else {
      idx <- dir >= d.min & dir <= d.max;
    }
    idx <- idx[!is.na(idx)];
    val.d <- val[idx];

    # Compute numerosity, average, standard deviation, and other stats
    center.dir[i] <- d;
    if(!is.null(val.d)) {
      n.data[i]     <- length(val.d);
      m[i]          <- mean(val.d, na.rm=TRUE);
      s[i]          <- sqrt(var(val.d, na.rm=TRUE));
      q             <- quantile(val.d, p=c(0.25,0.5,0.75), na.rm=TRUE)
      val.min[i]    <- min(val.d, na.rm=TRUE);
      val.p25[i]    <- q[1];
      val.mdn[i]    <- q[2];
      val.p75[i]    <- q[3];
      val.max[i]    <- max(val.d, na.rm=TRUE);
      if(confidence == "boot" & n.data[i] > 1) {
        if(min(val.d,na.rm=TRUE) != max(val.d,na.rm=TRUE)) {
          b <- boot(data=val.d, statistic=mean.boot, R=boot.size);
          c <- boot.ci(b, conf.level, type="perc");
          c95.inf[i] <- c$percent[4];
          c95.sup[i] <- c$percent[5];
        }
        else  {
          c95.inf[i] <- NA;
          c95.sup[i] <- NA;
        }
      }
      else  {
        c95.inf[i] <- NA;
        c95.sup[i] <- NA;
      }
    }
    else {
      n.data[i] <- 0;
      m[i] <- NA;
      s[i] <- NA;
      val.min[i] <- NA;
      val.p25[i] <- NA;
      val.mdn[i] <- NA;
      val.p75[i] <- NA;
      val.max[i] <- NA;
      c95.inf[i] <- NA;
      c95.sup[i] <- NA;
    }

    i <- i+1;

  }

  # Restitute results
  rsl <- list(
    dir     = dir,
    data    = val,
    n.class = n.data,
    c.dir   = center.dir,
    conf    = confidence,
    mean    = m,
    std.dev = s,
    min     = val.min,
    p.25    = val.p25,
    median  = val.mdn,
    p.75    = val.p75,
    max     = val.max,
    conf.min = c95.inf,
    conf.max = c95.sup
  );
  class(rsl) <- "dir.stat";
  return(rsl);

}


plot.dir.mean <- function(d.m, min.val, max.val, col="black", add=FALSE, conf.limits=FALSE, verbose=FALSE) {

  # Check input parameters
  if(class(d.m) != "dir.stat") {
    if(verbose) print("plot.dir.mean:: error: Data not an object of type 'dir.mean'");
    return(NULL);
  }
  # Post condition: parameters make sense, may go on

  # Plot directional mean
  angles <- seq(from=0, to=345, by=15);
  polar.plot(
    d.m$mean,
    d.m$c.dir,
    rp.type="p",
    start=90,
    clockwise=TRUE,
    label.pos=angles,
    label=angles,
    radial.lim=c(min.val,max.val),
    line.col=col,
    lwd=3,
    add=add
  );
  if(conf.limits) {
    polar.plot(
      d.m$conf.min,
      d.m$c.dir,
      rp.type="p",
      start=90,
      clockwise=TRUE,
      label.pos=angles,
      label=angles,
      radial.lim=c(min.val,max.val),
      line.col=col,
      lwd=1,
      add=TRUE
    );
    polar.plot(
      d.m$conf.max,
      d.m$c.dir,
      rp.type="p",
      start=90,
      clockwise=TRUE,
      label.pos=angles,
      label=angles,
      radial.lim=c(min.val,max.val),
      line.col=col,
      lwd=1,
      add=TRUE
    );
  }
}


plot.dir.num <- function(d.m, col="black", add=FALSE, verbose=FALSE) {

  # Check input parameters
  if(class(d.m) != "dir.stat") {
    if(verbose) print("plot.dir.mean:: error: Data not an object of type 'dir.mean'");
    return(NULL);
  }
  # Post condition: parameters make sense, may go on

  # Plot directional numerosity
  angles <- seq(from=0, to=345, by=15);
  num <- d.m$n.class;
  freq <- num/max(num,na.rm=TRUE);
  polar.plot(
    freq,
    d.m$c.dir,
    rp.type="p",
    start=90,
    clockwise=TRUE,
    label.pos=angles,
    label=angles,
    radial.lim=c(0, max(freq,na.rm=TRUE)),
    line.col=col,
    lwd=3,
    add=add
  );

}

mean.boot <- function(data, indices) {
  r<-mean(data[indices], na.rm=TRUE);
  return(r);
}


###########################
# Seasonality elimination #
###########################

# Computes the "typical period" (defaulting to typical day) and
# residual of a signal (most typically, an output from
# eddy covariance or averaging).
typical.period.generic <- function(t.stamp, signal, avg.period=3600, length=86400) {

  sum.idx <- (as.integer(t.stamp) %/% avg.period) %% (length %/% avg.period);
  d<-data.frame(t.stamp, signal, sum.idx);
  avg <- aggregate(d, by=list(idx=sum.idx), mean, na.action=na.exclude);

  return(avg$signal);

}

##########################################
# Spectra computation and representation #
##########################################

signal.spectrum <- function(x, sampling.rate=10) {

  # Check if some data is missing. If so, use periodogram method to estimate
  # spectrum. If no missing data exist use a less expensive FFT instead, and get
  # the true discrete spectrum.
  if(any(is.na(x))) {

    # Build spectrum estimate using periodogram method
    n <- length(x); # NA values included
    x.ts <- ts(data=x, start=0, end=(n-1)/sampling.rate, frequency=sampling.rate);
    s.x <- spectrum(x.ts, na.action=na.exclude, plot=FALSE);

    # Gather values to complete output record
    f <- s.x$freq * sampling.rate;
    s <- s.x$spec;
    nyquist <- sampling.rate/2;
    time <- (0:(length(x)-1))/sampling.rate

    # Build output
    out <- list(
      type      = "estimate",
      size      = n,
      nyquist   = nyquist,
      time      = time,
      x         = x,
      frequency = f,
      s         = s
    );

  }
  else {

    # All data present: use FFT and get the true discrete spectrum

    # Compute frequencies at various indices, assuming maximum frequency to coincide with
    # Nyquist frequency of signal, itself equal to 1/2 the data sampling rate.
    n <- length(x);
    m <- n %/% 2;
    if(n %% 2 == 0) {
      f <- c(0, 1:m, (-m+1):(-1));
    }
    else {
      f <- c(0, 1:m, (-m):(-1));
    }
    nyquist <- sampling.rate / 2;
    f.step <- nyquist / m;
    f <- f.step * f;

    # Now build the complex spectral values
    s <- fft(x)/n;

    # Build output
    out <- list(
      type      = "full",
      size      = n,
      nyquist   = nyquist,
      time      = (0:(n-1))/sampling.rate,
      x         = x,
      frequency = f,
      s         = s
    );

  }

  # Transmit results and quit
  class(out) <- "data.spectrum";
  return(out);

}


signal.smooth <- function(sp, cut.in.frequency, verbose=FALSE) {

  if(class(sp) != "data.spectrum") {
    if(verbose) print("signal.smooth:: error: Input data not of type 'data.spectrum'");
    return(NULL);
  }

  if(sp$type == "full") {
    f <- sp$frequency;
    s <- sp$s;
    s[abs(f) > cut.in.frequency] <- 0+0i;
    x <- fft(s,inverse=TRUE);
    return(Re(x));
  }
  else {
    print("smooth.signal:: error: Can't filter a signal containing missing data");
    return(NULL);
  }
}


power.spectrum <- function(sp) {

  if(class(sp) != "data.spectrum") {
    if(verbose) print("power.spectrum:: error: Input data not of type 'data.spectrum'");
    return(NULL);
  }

  n <- sp$size;
  k <- n %/% 2;
  p <- numeric(k+1);
  p[1] <- abs(sp$s[1]); # Constant term
  for(i in 1:k) p[i] <- abs(sp$s[i+1]) + abs(sp$s[n-i+1]);
  f <- sp$frequency[1:(k+1)];

  # Build output
  out <- list(
    frequency = f,
    power.sp  = p
  );
  class(out) <- "data.power.spectrum";

  return(out);

}


signal.cospectrum <- function(x, y, sampling.rate=10) {

  # Ensure neither of the two signals contain missing data
  if(any(is.na(x)) | any(is.na(y))) {
    print("signal.cospectrum:: error: One or both input signals contain missing data");
    return(NULL);
  }

  # Check both data have the same length
  if(length(x) != length(y)) {
    print("signal.cospectrum:: error: Input signals have not the same length");
    return(NULL);
  }

  # Compute co-spectrum and quadrature spectrum
  n <- length(x);
  s.x <- fft(x)/n;
  s.y <- fft(y)/n;
  s.xy <- Conj(s.x) * s.y;
  cospectrum <- Re(s.xy);
  qspectrum  <- Im(s.xy);

  # Compute frequencies
  m <- n %/% 2;
  if(n %% 2 == 0) {
    f <- c(0, 1:m, (-m+1):(-1));
  }
  else {
    f <- c(0, 1:m, (-m):(-1));
  }
  nyquist <- sampling.rate / 2;
  f.step <- nyquist / m;
  f <- f.step * f;

  # Build output
  out <- list(
    nyquist = nyquist,
    frequency = f,
    co.spectrum = cospectrum,
    quadrature.spectrum = qspectrum
  );
  class(out) <- "data.cospectrum";

  # Return data and leave
  return(out);

}


plot.spectrum <- function(pspc) {

  if(class(sp) != "data.power.spectrum") {
    if(verbose) print("plot.spectrum:: error: Input data not of type 'data.power.spectrum'");
    return(NULL);
  }

  # Compute the normalized power spectrum
  f <- pspc$frequency[pspc$frequency>0 & pspc$power.sp > 0];
  p <- pspc$power.sp[pspc$frequency>0 & pspc$power.sp > 0];
  plot(f, f*p, type="l", log="xy", xlab="Frequency (Hz)", ylab="fS");

}


plot.cospectrum <- function(cspc, ogive=FALSE) {

  if(class(sp) != "data.cospectrum") {
    if(verbose) print("plot.cospectrum:: error: Input data not of type 'data.cospectrum'");
    return(NULL);
  }

  # Compute the normalized cospectrum
  f  <- cspc$frequency[cspc$frequency>0];
  co <- cspc$co.spectrum[cspc$frequency>0];
  fc <- abs(f) * co;

  # Sort data of normalized cospectrum by absolute frequency, and sum-aggregate by equal abs frequency
  n <- length(co);
  m <- n %/% 2;
  cc <- numeric(m);
  ff <- f[1:m];
  for(i in 1:m) cc[i] <- fc[i] + fc[n-i+1];

  # Plot data
  plot(ff,cc,xlab="Frequency (Hz)", ylab="Co-Spectrum", type="l", log="x", xlim=c(max(ff),min(ff)));
  if(ogive) {
    og <- sum(cc) - cumsum(cc);
    par(new=TRUE);
    plot(ff,og,log="x",type="l",xlim=c(max(ff),min(ff)),col="blue",axes=FALSE,xlab="",ylab="");
    axis(4);
  }

}


######################
# Auxiliary routines #
######################

remove.trend <- function(s, method) {
  if(method == "linear") {
    avg <- mean(s, na.rm=TRUE);
    if(!is.na(avg)) {
      idx  <- 1:length(s);
      s.lm <- lm(s~idx, na.action=na.exclude);
      constant <- s.lm$coefficients[1]; # Intercept
      slope    <- s.lm$coefficients[2];
      data     <- s - (idx*slope + constant); # Not using "residual(s.lm)", which does not deal with missing data
      data     <- data - mean(data, na.rm=TRUE) + avg;
      result<-list(data=data, constant=constant, slope=slope);
    }
    else result <- list(data=s, constant=NA, slope=NA);
  }
  else result <- NULL;
  return(result);
}


relative.nonstationarity <- function(x,y) {
  l <- lm(y~x, na.action="na.exclude");
  pos.min.x <- which.min(l$model$x);
  pos.max.x <- which.max(l$model$x);
  val.min.x <- l$fitted.values[pos.min.x];
  val.max.x <- l$fitted.values[pos.max.x];
  rel.non.stat <- abs(val.max.x - val.min.x) / mean(y, na.rm=TRUE);
  return(rel.non.stat);
}


# Get list of CSV files in a given directory (inclusive of non-SonicLib files, if any)
enumerate.sonic.csv <- function(dir.name = ".", generate.full.path.names=TRUE) {

  # Input:
  #
  #   dir.name                  Name of directory where to look for CSV files (default: ".")
  #   generate.full.path.names  Boolean flag, equal to TRUE is fully qualified
  #                             pathnames are desired; FALSE, if file names only are
  #                             desired (default: TRUE)
  #
  # Output:
  #
  #   character vector, containing the list requested; may be an empty vector

  f.list <- dir(path=dir.name, pattern="*[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].[0-2][0-9].csv", full.names=generate.full.path.names);
  if(length(f.list) <= 0) {
    f.list <- dir(path=dir.name, pattern="*[0-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9].[0-2][0-9]R.csv", full.names=generate.full.path.names);
  }
  return(f.list);

}


# Given a SonicLib raw data file name, this routine returns its corresponding
# time stamp
time.stamp.from.name <- function(file.name, time.zone="UTC", shift=0, R.in.name=FALSE, verbose=FALSE) {

  # Input:
  #
  #   file.name    File name from which to extract the time stamp (no default)
  #
  #   time.zone    Name of desired time zone (default: "UTC")
  #
  #   shift        Displacement, in seconds, to adjust time stamp, useful for correcting
  #                errors in file naming, e.g. by data acquisition system time misalignment
  #                (default: 0)
  #
  #   R.in.name    Boolean flag: TRUE if name contains an "R" (MeteoFlux-originating data)
  #                and FALSE (default) for standard SonicLib file
  #
  #   verbose      Boolean flag: TRUE to print error / progress messages, FALSE to
  #                not print (default: FALSE)
  #
  # Output:
  #
  #   The time stamp, encoded as a POSIXct value, if the conversion was possible; NULL otherwise
  #

  # Parse date and time from name
  str.len <- nchar(file.name);
  if(R.in.name) {
    year  <- as.integer(substr(file.name, start=str.len-15, stop=str.len-12));
    month <- as.integer(substr(file.name, start=str.len-11, stop=str.len-10));
    day   <- as.integer(substr(file.name, start=str.len-9, stop=str.len-8));
    hour  <- as.integer(substr(file.name, start=str.len-6, stop=str.len-5));
  }
  else {
    year  <- as.integer(substr(file.name, start=str.len-14, stop=str.len-11));
    month <- as.integer(substr(file.name, start=str.len-10, stop=str.len-9));
    day   <- as.integer(substr(file.name, start=str.len-8, stop=str.len-7));
    hour  <- as.integer(substr(file.name, start=str.len-5, stop=str.len-4));
  }
  if(is.na(year) || is.na(month) || is.na(day) || is.na(hour)) return(NULL);

  # Compose date string
  str.date <- sprintf("%4.4d-%2.2d-%2.2d %2.2d:00:00", year,month,day,hour);

  # Convert date and time from string to numeric form, then adjust according to shift
  time.stamp <- as.POSIXct(str.date, tz=time.zone);
  time.stamp <- time.stamp + shift;
  return(time.stamp);

}

###############
# Wind Energy #
###############

wind.power <- function(d, mode="2.rotations", cut.speed=TRUE, lower.cut.speed=6, upper.cut.speed=20, verbose=FALSE) {

  # Check input parameters
  if(class(d) != "sonic.avg.data") {
    if(verbose) print("wind.power:: error: Data not an object of type 'sonic.avg-data'");
    return(NULL);
  }
  if(cut.speed && lower.cut.speed >= upper.cut.speed) {
    if(verbose) print("wind.power:: error: Lower wind cut speed can not exceed upper cut speed");
    return(NULL);
  }
  # Post condition: parameters make sense, may go on

  # Select wind belonging to desired speed interval
  if(cut.speed) {
    desired.wind <- (d$data$vel >= lower.cut.speed) & (d$data$vel <= upper.cut.speed);
  }
  else {
    desired.wind <- rep(TRUE, times=length(d$data$vel));
  }

  # Exit if no wind value is in range
  n <- sum(desired.wind);
  if(n <= 0) {
    if(verbose) print("wind.power:: error: No wind in desired interval");
    return(NULL);
  }
  else if(n <= 1) {
    if(verbose) print("wind.power:: error: Not enough wind data in desired interval");
    return(NULL);
  }

  # Pre-allocate workspace
  rot.theta <- numeric(n);
  rot.phi   <- numeric(n);
  rot.psi   <- numeric(n);
  u.avg.rot <- numeric(n);
  v.avg.rot <- numeric(n);
  w.avg.rot <- numeric(n);
  uu.rot    <- numeric(n);
  uv.rot    <- numeric(n);
  uw.rot    <- numeric(n);
  vv.rot    <- numeric(n);
  vw.rot    <- numeric(n);
  ww.rot    <- numeric(n);

  if(mode == "2.rotations" | mode == "3.rotations") {

    # Main loop: iterate over all averages
    i <- 0;
    for(item in 1:length(desired.wind)) {

      if(desired.wind[item]) {
        i <- i + 1;

        # Form relevant matrices
        wind.avg   <- matrix(data=c(d$data$u.avg[item], d$data$v.avg[item], d$data$w.avg[item]), nrow=3, ncol=1, byrow=TRUE);
        wind.cov   <- matrix(data=c(d$data$uu[item], d$data$uv[item], d$data$uw[item], d$data$uv[item], d$data$vv[item], d$data$vw[item], d$data$uw[item], d$data$vw[item], d$data$ww[item]), nrow=3, ncol=3, byrow=TRUE);

        # Store first rotation angle, for reference
        rot.theta[i] <- atan2(wind.avg[2], wind.avg[1])*180/pi;

        # Build and apply first rotation matrix
        denom <- sqrt(d$data$u.avg[item]^2 + d$data$v.avg[item]^2);
        if(!is.null(denom) && !is.na(denom) && denom > 1.e-3) {
          sin.a <- d$data$v.avg[item] / denom;
          cos.a <- d$data$u.avg[item] / denom;
        }
        else {
          sin.a <- 0.0;
          cos.a <- 1.0;
        }
        R01 <- matrix(c(cos.a, sin.a, 0, -sin.a, cos.a, 0, 0, 0, 1), nrow=3, ncol=3, byrow=TRUE);
        wind.avg <- R01 %*% wind.avg;
        wind.cov <- R01 %*% wind.cov %*% t(R01);

        # Store second rotation angle, for further reference
        rot.phi[i] <- atan2(wind.avg[3], sqrt(wind.avg[1]^2 + wind.avg[2]^2))*180/pi;

        # Build and apply second rotation matrix
        denom <- sqrt(wind.avg[1]^2 + wind.avg[3]^2);
        if(!is.null(denom) && !is.na(denom) && denom > 1.e-3) {
          sin.b <- wind.avg[3] / denom;
          cos.b <- wind.avg[1] / denom;
        }
        else {
          sin.b <- 0.0;
          cos.b <- 1.0;
        }
        R12 <- matrix(c(cos.b, 0, sin.b, 0, 1, 0, -sin.b, 0, cos.b), nrow=3, ncol=3, byrow=TRUE);
        wind.avg <- R12 %*% wind.avg;
        wind.cov <- R12 %*% wind.cov %*% t(R12);

        # Build and apply third rotation matrix, if requested
        if(mode == "3.rotations") {

          delta <- 0.5*atan2(2.*wind.cov[2,3], wind.cov[2,2] - wind.cov[3,3]);
          delta[(abs(delta*180/pi) > third.rotation.angular.threshold)] <- 0.0; # Suppress rotation exceeding desired threshold angle
          sin.d <- sin(delta);
          cos.d <- cos(delta);
          R23 <- matrix(c(1, 0, 0, 0, cos.d, sin.d, 0, -sin.d, cos.d), nrow=3, ncol=3, byrow=TRUE);
          wind.avg <- R23 %*% wind.avg;
          wind.cov <- R23 %*% wind.cov %*% t(R23);

        }
        else {
          delta <- 0.0;
        }

        # Store third rotation angle, for future reference
        rot.psi[i] <- delta*180/pi;

        # Reassemble result
        u.avg.rot[i] <- wind.avg[1];
        v.avg.rot[i] <- wind.avg[2];
        w.avg.rot[i] <- wind.avg[3];
        uu.rot[i]    <- wind.cov[1,1];
        uv.rot[i]    <- wind.cov[1,2];
        uw.rot[i]    <- wind.cov[1,3];
        vv.rot[i]    <- wind.cov[2,2];
        vw.rot[i]    <- wind.cov[2,3];
        ww.rot[i]    <- wind.cov[3,3];

      }
    }
  }

  #########################
  # Compute basic indices #
  #########################

  # Compute wind provenance direction (useful to classify data)
  Dir <- atan2(-d$data$u.avg[desired.wind],-d$data$v.avg[desired.wind]) * 180/pi;
  Dir.negative <- !is.na(Dir) & Dir < 0;
  Dir[Dir.negative] <- Dir[Dir.negative] + 360.0;

  # Friction velocity, according to the signless definition
  u.star <- (uw.rot[desired.wind]^2 + vw.rot[desired.wind]^2)^0.25;

  # Angle to horizontal plane
  ang.Phi <- atan2(d$data$w.avg[desired.wind], d$data$vel[desired.wind])*180/pi;

  # Turbulence intensity, time-dependent
  T.time.series <- d$data$vel.sd[desired.wind] / d$data$vel[desired.wind];

  # Mean turbulence intensity and related quantities (Warning: not the
  # mean value of time-dependent turbulent intensity!)
  mean.vel.sd <- mean(d$data$vel.sd[desired.wind], na.rm=TRUE);
  mean.vel    <- mean(d$data$vel[desired.wind], na.rm=TRUE);
  T           <- mean.vel.sd / mean.vel;

  # Coefficient of mean power available in wind
  mean.power.coef   <- sqrt(d$data$u.avg[desired.wind]^2 + d$data$v.avg[desired.wind]^2)^3;
  mean.power.coef.3 <- sqrt(d$data$u.avg[desired.wind]^2 + d$data$v.avg[desired.wind]^2 + d$data$w.avg[desired.wind]^2)^3;

  # Total power present in wind (turbulent fluctuations included)
  tot.power.coef   <- d$data$vel3[desired.wind];
  tot.power.coef.3 <- d$data$vel3.3[desired.wind];

  # Cumulative wind speed empirical distribution
  cum.vel <- ecdf(d$data$vel[desired.wind]);

  # Function to estimate the Weibull parameters given a data series
  weibull.estimate <- function(d, verbose=FALSE) {

    # Compute mean and standard deviation
    m <- mean(d, na.rm=TRUE);
    s <- sd(d, na.rm=TRUE);
    if(is.na(s) || is.na(m)) {
      if(verbose) print("weibull.estimate:: error: No valid data in set");
      return(NULL);
    }
    if(m <= 0 || s <= 0) {
      if(verbose) print("weibull.estimate:: error: Mean and standard deviation must be positive");
      return(NULL);
    }

    # Solve the auxiliary equation to identify parameter "alpha"
    # -1- Function definition
    m.s.ratio <- function(a, m, s) sqrt(gamma(1+2/a)/gamma(1+1/a)^2-1) - s/m;
    # -1- Search an interval where the solution is
    upper.bound <- 1.;
    while(m.s.ratio(upper.bound, m, s) > 0) upper.bound <- upper.bound*2;
    lower.bound <- upper.bound/2;
    while(m.s.ratio(lower.bound, m, s) < 0) lower.bound <- lower.bound/2;
    # -1- Solve equation
    result <- uniroot(function(x,m,s) sqrt(gamma(1+2/x)/gamma(1+1/x)^2-1) - s/m, lower=lower.bound, upper=upper.bound, tol=1e-4, m=m, s=s);
    alpha <- result$root;

    # Compute the remaining parameter, beta, directly
    beta <- m/gamma(1+1/alpha);

    # Leave
    return(list(
      alpha = alpha,
      beta  = beta
    ));

  }

  # Perform actual Weibull parameters estimation
  w.est <- weibull.estimate(d$data$vel[desired.wind], verbose=verbose);
  alpha <- w.est$alpha;
  beta  <- w.est$beta;

  # Perform a test to check empirical distribution against Weibull, as estimated
  k <- ks.test(x=d$data$vel[desired.wind], y="pweibull", alpha, beta);
  weibull.statistic   <- k$statistic;
  weibull.probability <- k$p.value;

  # Estimate log-normal parameters the direct way
  std.vel <- sd(d$data$vel[desired.wind],na.rm=TRUE);
  p <- log(1+(std.vel/mean.vel)^2);
  lognormal.mean <- log(mean.vel) - p/2;
  lognormal.sd   <- sqrt(p);

  # Perform a test to check empirical distribution against lognormal, as estimated
  k <- ks.test(x=d$data$vel[desired.wind], y="plnorm", lognormal.mean, lognormal.sd);
  lognormal.statistic   <- k$statistic;
  lognormal.probability <- k$p.value;

  # Assemble final data frame
  e <- d$data[desired.wind,];
  e$theta <- rot.theta;
  e$phi   <- rot.phi;
  e$psi   <- rot.psi;
  e$Dir    <- Dir;
  e$u.star <- u.star;
  e$T      <- T.time.series;
  e$ang.Phi           <- ang.Phi;
  e$mean.power.coef   <- mean.power.coef;
  e$mean.power.coef.3 <- mean.power.coef.3;
  e$tot.power.coef    <- tot.power.coef;
  e$tot.power.coef.3  <- tot.power.coef.3;

  # Leave
  result <- list(
    data                  = e,
    time.zone             = d$time.zone,
    delay                 = d$delay,
    spike                 = d$spike.detection.threshold,
    spike.treatment       = d$spike.treatment,
    trend.removal         = d$trend.removal,
    sampling.rate         = d$sampling.rate,
    averaging.time        = d$averaging.time,
    mean.vel.stddev       = mean.vel.sd,
    turb.coefficient      = T,
    cum.vel               = cum.vel,
    weibull.alpha         = alpha,
    weibull.beta          = beta,
    weibull.statistic     = weibull.statistic,
    weibull.probability   = weibull.probability,
    lognormal.mean        = lognormal.mean,
    lognormal.sd          = lognormal.sd,
    lognormal.statistic   = lognormal.statistic,
    lognormal.probability = lognormal.probability,
    cut.speed             = cut.speed,
    lower.cut.speed       = lower.cut.speed,
    upper.cut.speed       = upper.cut.speed,
    processing            = mode
  );
  class(result) <- "sonic.wind.power.data";
  return(result);

}


hildeman.wilson.process <- function(sample.size, mu, sigma, intermittency) {

  # If intermittency is greater than or equal to 1, the Hildemann-Wilson process
  # degenerates to an identically 0 sample
  if(intermittency >= 1) return(rep(0, times=sample.size));

  # Treat negative intermittency as if zero
  if(intermittency < 0) intermittency = 0;

  # Convert true mean and standard deviation to the corresponding
  # log-normal parameters
  Q <- log(1+(sigma/mu)^2);
  m <- log(mu) - 0.5*Q;
  s <- sqrt(Q);

  # Extract primary log-normal sample
  smpl <- rlnorm(sample.size, m, s);

  # Determine the variable value corresponding to desired intermittency, then
  # subtract it from sample and clip to zero
  itrm.val <- qlnorm(intermittency, m, s);
  smpl <- smpl - itrm.val;
  smpl[smpl < 0] <- 0;

  # Leave
  return(smpl);

}

# Multiresolution analysis #

# Perform the actual multiresolution decomposition on a (time stamp, value) couple
# of equal sized vectors
multires <- function(t.stamp, val, steps) {

  # Inputs:
  #
  #   t.stamp   Time stamp vector
  #
  #   val       Value vector
  #
  #   steps     Sequence of decreasing time steps, made so that
  #             steps[i] divides steps[i-1]
  #

  # Reserve workspace
  n.steps <- length(steps);
  multires.values <- data.frame(t.stamp);
  vars            <- numeric(n.steps);

  # Main loop
  residual <- val;
  decomposition <- numeric(length(val));
  for(i in 1:n.steps) {

    # Prepare residual name based on step value
    mean.name <- sprintf("M.%d", i);
    dec.name  <- sprintf("D.%d", i);
    res.name  <- sprintf("R.%d", i);

    # Use time stamps to generate the time aggregation function, and use it
    time.block  <- as.integer(floor(t.stamp)) %/% steps[i];
    block.means <- aggregate(residual, by=list(time.block), FUN=mean, na.rm=TRUE);

    # Translate time block to 1-based index form
    block.index <- time.block - block.means$Group.1[1] + 1;

    # Construct a vector of same length as original data, and values equal
    # to block averages; deduce block residual from it
    block.means.val  <- block.means$x;
    block.means.full <- block.means.val[block.index];
    block.residual   <- residual - block.means.full;

    # Compute the total variance pertaining the full-length block averages
    block.variance <- var(block.means.full, na.rm=TRUE);

    # Update residual and decomposition
    residual <- block.residual;
    decomposition <- decomposition + block.means.full;

    # Accumulate variances, means and residuals
    multires.values[res.name]  <- residual;
    multires.values[dec.name]  <- decomposition;
    multires.values[mean.name] <- block.means.full;
    vars[i] <- block.variance;

  }

  # Build output
  l <- list(
    mres            = multires.values,
    variance.scales = vars,
    variance.res    = var(residual, na.rm=TRUE),
    variance.total  = var(val, na.rm=TRUE)
  );
  class(l) <- "multires.analysis";
  return(l);

}


# Build a vector of halving time scales until a threshold is reached
build.steps <- function(max.duration=3600, threshold=0.2) {

  # Count elements
  n <- 0;
  val <- max.duration;
  while(val > threshold) {
    n <- n+1;
    val <- val/2;
  }
  vect <- numeric(n);

  # Build vector
  i <- 0;
  val <- max.duration;
  while(val > threshold) {
    i <- i+1;
    vect[i] <- val;
    val <- val/2;
  }
  return(vect);
}


##########################################
# Auxiliary - POSIXct/POSIXlt management #
##########################################

# Return a POSIXct value (or vector) representing the date and time value(s)
# passed as input, but with time set to "00:00:00", for use in time stamping daily
# data.
floor.day <- function(date.time, time.zone="UTC") {
  int.time <- as.integer(date.time);
  base.time <- as.POSIXct("1970-01-01 00:00:00", tz="UTC");
  day.base <- as.POSIXct(int.time - (int.time %% 86400), origin=base.time, tz=time.zone);
  return(day.base);
}


# Return the "Julian day" corresponding to a POSIXct value passed as input
# with respect to a given date and time. By "Julian" day here it is meant
# the number of days from the reference time instant.
julian.day <- function(date.time, base.time) {
  int.time <- as.integer(date.time);
  int.base <- as.integer(base.time);
  return((int.time-int.base) %/% 86400 + 1);
}


# Generate one or more POSIXct day initial instants given a reference date and a number
# of days
date.shift <- function(ref.instant, num.days, time.zone="UTC") {
  ref.day <- floor.day(ref.instant, time.zone);
  shifted.day <- as.POSIXct(86400*num.days, origin=ref.day, tz=time.zone);
  return(shifted.day);
}


# Return the POSIXct value corresponding to the first instant of year of
# a date and time value passed as input.
floor.year <- function(date.time, time.zone="UTC") {
  split.time <- as.POSIXlt(date.time, tz=time.zone);
  split.time$hour <- 0;
  split.time$min  <- 0;
  split.time$sec  <- 0;
  split.time$mday <- 1;
  split.time$mon  <- 0;
  base.year <- as.POSIXct(split.time, tz=time.zone);
  return(base.year);
}


# Return an integer representing the year corresponding to a given input POSIXct
year.number <- function(date.time, time.zone="UTC") {
  split.time <- as.POSIXlt(date.time, tz=time.zone);
  return(split.time$year + 1900);
}


# Return an integer representing the month corresponding to a given input POSIXct
month.number <- function(date.time, time.zone="UTC") {
  split.time <- as.POSIXlt(date.time, tz=time.zone);
  return(split.time$mon + 1);
}


# Number of days between two time stamps
day.number <- function(date.to, date.from) {
  b.time <- as.POSIXct("1970-01-01 00:00:00", tz="UTC");
  return(julian.day(date.to,base.time=b.time) - julian.day(date.from,base.time=b.time));
}


# Special case of Julian day, referred to current year's beginning
day.in.year <- function(date.time, time.zone="UTC") {
  year <- floor.year(date.time, time.zone);
  return(day.number(date.time, date.from=year));
}

# Return the block number corresponding to a given POSIXct, respect to
# a given time step. If time step is left to 3600, counts the hour number.
step.in.day <- function(date.time, delta.time=3600) {
  return((as.integer(date.time) %% 86400) %/% delta.time);
}


# Compute basic daily statistics, and report result in vectors the same length
# as original input series, allowing for example to compute anomalies by subtracting
# the computed daily means from the input signal values.
daily.base.stats <- function(time.stamp, value, time.zone="UTC") {

  # Compute daily mean, minimum and maximum
  dd <- day.number(time.stamp, floor.day(min(time.stamp,na.rm=TRUE)), time.zone);
  u  <- data.frame(time.stamp, dd, value);
  a  <- aggregate(u, by=list(dd), FUN=mean, na.rm=TRUE);
  mi <- aggregate(u, by=list(dd), FUN=min,  na.rm=TRUE);
  mx <- aggregate(u, by=list(dd), FUN=max,  na.rm=TRUE);
  l.a <- merge(u, a, by="dd");
  l.i <- merge(u, mi, by="dd");
  l.x <- merge(u, mx, by="dd");

  # Get important columns only
  t     <- l.a$time.stamp.x;
  value <- l.a$value.x;
  mean  <- l.a$value.y;
  mi    <- l.i$value.y;
  mx    <- l.x$value.y;
  rsl   <- data.frame(Time.Stamp=t, Value=value, Mean=mean, Min=mi, Max=mx);
  return(rsl);

}


# Compute a "typical period", as specified by input variable 'aggregation.period'.
# Vectors 'aggregation.period' and 'data.vector' must be the same length. The following table
# illustrate common uses, depending on the value of 'aggregation.period'.
#
# Value of 'aggregation.period'     Meanings
#
#   year.number(time.stamp)         Sequence (possibly unlimited) of yearly means
#   month.number(time.stamp)        Typical year, in 12 month averages.
#   day.in.year(time.stamp)         Typical year, in 366 day averages.
#   hour.number(time.stamp)         Typical day, in 24 hour averages.
#
typical.period <- function(data.vector, aggregation.period) {
  typical <- aggregate(data.vector, by=list(aggregation.period), FUN=mean, na.rm=TRUE);
  return(typical);
}
