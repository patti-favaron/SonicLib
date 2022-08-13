# Skeleton of the data-driven NanoPart impulsive particle model.

prepare.met <- function(file.name, delta.t=12) {
  
  # Get the SonicLib raw data file in standard form
  d<-get.raw.data(file.name);
  
  # Aggregate wind components and temperature according to the desired time frame
  t.st <- d$data$time.stamp %/% delta.t;
  u.m <- aggregate(d$data$u, by=list(t.st), FUN=mean, na.rm=TRUE)$x;
  v.m <- aggregate(d$data$v, by=list(t.st), FUN=mean, na.rm=TRUE)$x;
  w.m <- aggregate(d$data$w, by=list(t.st), FUN=mean, na.rm=TRUE)$x;
  t.m <- aggregate(d$data$t, by=list(t.st), FUN=mean, na.rm=TRUE)$x;
  u.s <- aggregate(d$data$u, by=list(t.st), FUN=sd, na.rm=TRUE)$x;
  v.s <- aggregate(d$data$v, by=list(t.st), FUN=sd, na.rm=TRUE)$x;
  w.s <- aggregate(d$data$w, by=list(t.st), FUN=sd, na.rm=TRUE)$x;
  t.s <- aggregate(d$data$t, by=list(t.st), FUN=sd, na.rm=TRUE)$x;
  
  # Compose data frame and exit
  new.t.stamp <- floor(aggregate(d$data$time.stamp, by=list(t.st), FUN=min, na.rm=TRUE)$x);
  e <- data.frame(new.t.stamp, u.m, v.m, w.m, t.m, u.s, v.s, w.s, t.s);
  names(e) <- c("time.stamp", "u", "v", "w", "t", "u.sd", "v.sd", "w.sd", "t.sd");
  return(e);
  
}