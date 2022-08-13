read.amer.file <- function(fileName) {
  
  # Gather and convert
  d <- read.csv(fileName, header=FALSE);
  w <-  d$V1;
  v <- -d$V2;
  u <-  d$V3;
  t <-  d$V4;
  q <-  d$V5*1000/5;
  c <-  d$V6*15/5 + 10;
  
  # Spike removal
  # -1- Approximate "mean" values by smoothing splines (I've used all R defaults here)
  x <- 1:length(u);
  u.s <- smooth.spline(x, u);
  v.s <- smooth.spline(x, v);
  w.s <- smooth.spline(x, w);
  t.s <- smooth.spline(x, t);
  q.s <- smooth.spline(x, q);
  c.s <- smooth.spline(x, c);
  # -1- Evaluate the splines at all index values, to provide a value estimation on all data
  u.fit <- predict(u.s, x)$y;
  v.fit <- predict(v.s, x)$y;
  w.fit <- predict(w.s, x)$y;
  t.fit <- predict(t.s, x)$y;
  q.fit <- predict(q.s, x)$y;
  c.fit <- predict(c.s, x)$y;
  # -1- Compute the fluctuation part
  u.r <- u - u.fit;
  v.r <- v - v.fit;
  w.r <- w - w.fit;
  t.r <- t - t.fit;
  q.r <- q - q.fit;
  c.r <- c - c.fit;
  # -1- Compute fluctuation standard deviation
  s.u <- sd(u, na.rm=TRUE);
  s.v <- sd(v, na.rm=TRUE);
  s.w <- sd(w, na.rm=TRUE);
  s.t <- sd(t, na.rm=TRUE);
  s.q <- sd(q, na.rm=TRUE);
  s.c <- sd(c, na.rm=TRUE);
  # -1- Identify the position of all values whose fluctuation exceeds 6 standard
  #     deviations in absolute value (as suggested by Amerflux)
  spk.pos.u <- which(abs(u.r) > 6*s.u);
  spk.pos.v <- which(abs(v.r) > 6*s.v);
  spk.pos.w <- which(abs(w.r) > 6*s.w);
  spk.pos.t <- which(abs(t.r) > 6*s.t);
  spk.pos.q <- which(abs(q.r) > 6*s.q);
  spk.pos.c <- which(abs(c.r) > 6*s.c);
  # -1- Replace more-than-6-sigmas values with their estimation
  u[spk.pos.u] <- u.fit[spk.pos.u];
  v[spk.pos.v] <- v.fit[spk.pos.v];
  w[spk.pos.w] <- w.fit[spk.pos.w];
  t[spk.pos.t] <- t.fit[spk.pos.t];
  q[spk.pos.q] <- q.fit[spk.pos.q];
  c[spk.pos.c] <- c.fit[spk.pos.c];

  # Form time stamp
  time.stamp <- numeric(length(u));
  
  # Form result, then leave. Notice time stamp has been reserved but not yet filled.
  e <-  data.frame(time.stamp, u, v, w, t, q, c);
  return(e);
  
}


read.amer.hour <- function(path, day.num, hour) {
  
  # Form filenames
  file.prefix <- sprintf("%s/G%03d%02d", path, day.num, hour);
  file.name.1 <- paste(file.prefix, "00.RAW", sep="");
  file.name.2 <- paste(file.prefix, "30.RAW", sep="");
  
  # Gather data from the two separate files and join them
  d <- read.amer.file(file.name.1);
  d <- rbind(d, read.amer.file(file.name.2));
  
  # Attribute time stamp
  n <- length(d$u);
  d$time.stamp <- (0:(n-1))*3600.0/n;
  return(d);
  
}


write.amer.hour <- function(data, path, day.num, hour, base.date=as.POSIXct("2012-01-01 00:00:00", tz="UTC"), time.zone="UTC") {
  
  # Form output file name
  day.date <- as.POSIXlt(date.shift(base.date, day.num, time.zone));
  year <- day.date$year + 1900;
  month <- day.date$mon + 1;
  day   <- day.date$mday;
  file.name <- sprintf("%s/Day_%03d/%04d%02d%02d.%02d.csv", path, day.num, year, month, day, hour);
  
  # Write data
  write.csv(data, file.name, row.names=FALSE);
  print(paste("Processed:", file.name));
  
  return(file.name);
  
}


# Return a POSIXct value (or vector) representing the date and time value(s)
# passed as input, but with time set to "00:00:00", for use in time stamping daily
# data.
floor.day <- function(date.time, time.zone="UTC") {
  int.time <- as.integer(date.time);
  base.time <- as.POSIXct("1970-01-01 00:00:00", tz="UTC");
  day.base <- as.POSIXct(int.time - (int.time %% 86400), origin=base.time, tz=time.zone);
  return(day.base);
}


# Generate one or more POSIXct day initial instants given a reference date and a number
# of days
date.shift <- function(ref.instant, num.days, time.zone="UTC") {
  ref.day <- floor.day(ref.instant, time.zone);
  shifted.day <- as.POSIXct(86400*num.days, origin=ref.day, tz=time.zone);
  return(shifted.day);
}


process.day <- function(day.num) {
  in.path  <- "../DataSets/Ameriflux_GoldStandard/Open_Path";
  out.path <- "../TestData/Ameriflux/Open_Path";
  for(hour in 0:23) {
    d <- read.amer.hour(in.path, day.num, hour);
    write.amer.hour(d, out.path, day.num, hour);
  }
}


process.data <- function() {
  process.day(104);
  process.day(181);
}


validation.run <- function() {
  
  # Data have been collected at Ranch Vaira, whose altitude is 129m according
  # to Internet-surfed location data.

  # Average data
  d.104.d300ms.trend   <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_104", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.3, trend.removal="none", verbose=TRUE);
  d.104.d300ms.notrend <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_104", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.3, trend.removal="linear", verbose=TRUE);
  d.181.d300ms.trend   <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_181", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.3, trend.removal="none", verbose=TRUE);
  d.181.d300ms.notrend <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_181", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.3, trend.removal="linear", verbose=TRUE);
  d.104.d200ms.trend   <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_104", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.2, trend.removal="none", verbose=TRUE);
  d.104.d200ms.notrend <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_104", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.2, trend.removal="linear", verbose=TRUE);
  d.181.d200ms.trend   <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_181", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.2, trend.removal="none", verbose=TRUE);
  d.181.d200ms.notrend <- average.sonic.file.set("../TestData/Ameriflux/Open_Path/Day_181", averaging.time=30, sampling.rate=10, threshold=0.05, delay=0.2, trend.removal="linear", verbose=TRUE);
  
  # Eddy covariance (few scenarios)
  ec.104.d300ms.trend   <- eddy.covariance(d.104.d300ms.trend,   station.altitude=129, anemometer.height=2-0.25, verbose=TRUE);
  ec.104.d300ms.notrend <- eddy.covariance(d.104.d300ms.notrend, station.altitude=129, anemometer.height=2-0.25, verbose=TRUE);
  ec.181.d300ms.trend   <- eddy.covariance(d.181.d300ms.trend,   station.altitude=129, anemometer.height=2-0.10, verbose=TRUE);
  ec.181.d300ms.notrend <- eddy.covariance(d.181.d300ms.notrend, station.altitude=129, anemometer.height=2-0.10, verbose=TRUE);
  ec.104.d200ms.trend   <- eddy.covariance(d.104.d200ms.trend,   station.altitude=129, anemometer.height=2-0.25, verbose=TRUE);
  ec.104.d200ms.notrend <- eddy.covariance(d.104.d200ms.notrend, station.altitude=129, anemometer.height=2-0.25, verbose=TRUE);
  ec.181.d200ms.trend   <- eddy.covariance(d.181.d200ms.trend,   station.altitude=129, anemometer.height=2-0.10, verbose=TRUE);
  ec.181.d200ms.notrend <- eddy.covariance(d.181.d200ms.notrend, station.altitude=129, anemometer.height=2-0.10, verbose=TRUE);
  
  # Write data sets in a form suitable of transmission
  write.csv(ec.104.d300ms.trend$data  , file="../ValidationSet/ec.104.d300ms.trend.csv",  row.names=F);
  write.csv(ec.104.d300ms.notrend$data, file="../ValidationSet/ec.104.d300ms.notrend.csv",row.names=F);
  write.csv(ec.181.d300ms.trend$data  , file="../ValidationSet/ec.181.d300ms.trend.csv",  row.names=F);
  write.csv(ec.181.d300ms.notrend$data, file="../ValidationSet/ec.181.d300ms.notrend.csv",row.names=F);
  write.csv(ec.104.d200ms.trend$data  , file="../ValidationSet/ec.104.d200ms.trend.csv",  row.names=F);
  write.csv(ec.104.d200ms.notrend$data, file="../ValidationSet/ec.104.d200ms.notrend.csv",row.names=F);
  write.csv(ec.181.d200ms.trend$data  , file="../ValidationSet/ec.181.d200ms.trend.csv",  row.names=F);
  write.csv(ec.181.d200ms.notrend$data, file="../ValidationSet/ec.181.d200ms.notrend.csv",row.names=F);
  
}
