get.multi.sonic.csv <- function(name.first.file, n.hours=1, sampling.rate=10, threshold=0.005, time.zone="UTC", average.by="none", verbose=FALSE) {
  
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
    tz=time.zone
  );
  hours <- 3600*(0:(n.hours-1));
  time.stamp.set <- time.stamp + hours;
  file.set <- strftime(time.stamp.set, format="%Y%m%d.%H.csv");
  file.set <- paste(path, file.set, sep="");
  
  # Read and append all data files in list
  first <- TRUE;
  i.hr <- 0;
  for(file in file.set) {
    if(first) {
      d <- get.raw.data(file, sampling.rate, threshold, verbose);
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
