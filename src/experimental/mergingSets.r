# Generate two hourly sets, one with a nominal sampling rate of 20Hz simulating
# a sonic, and the other at 7Hz, mimicking a LI-7200.
# -1- Hourly set 1: sonic data
sampling.rate <- 20; # Hz
delta.t <- 1/sampling.rate;
t.stamp <- seq(from=0, to=(3600 - delta.t), by=delta.t);
n       <- length(t.stamp);
u       <- rnorm(n,  1.2, 0.4);
v       <- rnorm(n, -0.6, 0.35);
w       <- rnorm(n,  0.0, 0.1);
t       <- rnorm(n, 15.7, 2.0);
sonic <- data.frame(t.stamp, u, v, w, t); # This is the same format as SonicLib raw data files

# -1- Hourly set 2: LI-7200
sampling.rate <- 7; # Hz
delta.t <- 1/sampling.rate;
t.stamp <- seq(from=0, to=(3600 - delta.t), by=delta.t);
l       <- length(t.stamp);
a       <- rnorm(l,  10.0, 1.0);
m       <- rnorm(l,  30.5, 3.3);
LI7200 <- data.frame(t.stamp, a, m); # This is the same format as SonicLib raw data files


merge.sonic.nonsonic <- function(sonic, LI7200) {
  
  sonic.pointers <- data.frame(
    time.stamp = sonic$t.stamp,
    index      = 1:length(sonic$t.stamp),
    data.set   = "sonic"
  );
  
  LI7200.pointers <- data.frame(
    time.stamp = LI7200$t.stamp,
    index      = 1:length(LI7200$t.stamp),
    data.set   = "LI7200"
  );
  
  whole.pointers <- sonic.pointers;
  whole.pointers <- rbind(whole.pointers, LI7200.pointers);
  
  whole.pointers <- whole.pointers[order(whole.pointers$time.stamp),];
  whole.pointers$row.names <- NULL;
  
  n       <- length(sonic.pointers$time.stamp);
  t.stamp <- rep(NA,times=n);
  u       <- rep(NA,times=n);
  v       <- rep(NA,times=n);
  w       <- rep(NA,times=n);
  t       <- rep(NA,times=n);
  a       <- rep(NA,times=n);
  m       <- rep(NA,times=n);
  am.age  <- rep(NA,times=n);
  
  pos.next <- 0;
  
  temporary.a <- NA;
  temporary.m <- NA;
  temporary.t.stamp <- NA;
  
  for(i in 1:length(whole.pointers$time.stamp)) {
    index <- whole.pointers$index[i];
    if(whole.pointers$data.set[i] == "LI7200") {
      temporary.a       <- LI7200$a[index];
      temporary.m       <- LI7200$m[index];
      temporary.t.stamp <- LI7200$t.stamp[index];
    }
    else {
      pos.next <- pos.next + 1;
      t.stamp[pos.next] <- sonic$t.stamp[index];
      am.age[pos.next]  <- sonic$t.stamp[index] - temporary.t.stamp;
      u[pos.next]       <- sonic$u[index];
      v[pos.next]       <- sonic$v[index];
      w[pos.next]       <- sonic$w[index];
      t[pos.next]       <- sonic$t[index];
      a[pos.next]       <- temporary.a;
      m[pos.next]       <- temporary.m;
    }
  }
  
  d <- data.frame(t.stamp, am.age, u, v, w, t, a, m);
  return(d);
  
}

t0 <- system.time(d <- merge.sonic.nonsonic(sonic, LI7200));


merge.sonic.nonsonic.optimized <- function(sonic, LI7200) {
  
  type.sonic  <- as.integer(0);
  type.LI7200 <- as.integer(1);
  
  sonic.pointers <- data.frame(
    time.stamp = sonic$t.stamp,
    index      = 1:length(sonic$t.stamp),
    data.set   = type.sonic
  );
  
  LI7200.pointers <- data.frame(
    time.stamp = LI7200$t.stamp,
    index      = 1:length(LI7200$t.stamp),
    data.set   = type.LI7200
  );
  
  whole.pointers <- sonic.pointers;
  whole.pointers <- rbind(whole.pointers, LI7200.pointers);
  
  whole.pointers <- whole.pointers[order(whole.pointers$time.stamp),];
  whole.pointers$row.names <- NULL;
  
  n       <- length(sonic.pointers$time.stamp);
  t.stamp <- rep(NA,times=n);
  u       <- rep(NA,times=n);
  v       <- rep(NA,times=n);
  w       <- rep(NA,times=n);
  t       <- rep(NA,times=n);
  a       <- rep(NA,times=n);
  m       <- rep(NA,times=n);
  am.age  <- rep(NA,times=n);
  
  pos.next <- 0;
  
  temporary.a <- NA;
  temporary.m <- NA;
  temporary.t.stamp <- NA;
  
  for(i in 1:length(whole.pointers$time.stamp)) {
    index <- whole.pointers$index[i];
    if(whole.pointers$data.set[i] == type.LI7200) {
      temporary.a       <- LI7200$a[index];
      temporary.m       <- LI7200$m[index];
      temporary.t.stamp <- LI7200$t.stamp[index];
    }
    else {
      pos.next <- pos.next + 1;
      t.stamp[pos.next] <- sonic$t.stamp[index];
      am.age[pos.next]  <- sonic$t.stamp[index] - temporary.t.stamp;
      u[pos.next]       <- sonic$u[index];
      v[pos.next]       <- sonic$v[index];
      w[pos.next]       <- sonic$w[index];
      t[pos.next]       <- sonic$t[index];
      a[pos.next]       <- temporary.a;
      m[pos.next]       <- temporary.m;
    }
  }
  
  d <- data.frame(t.stamp, am.age, u, v, w, t, a, m);
  return(d);
  
}


t1 <- system.time(d <- merge.sonic.nonsonic.optimized(sonic, LI7200));
