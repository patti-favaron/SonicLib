# SonicLib example application for the analysis
# of concentration-related data
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

library(boot);    # Not used here, but referenced by SonicLib.
library(plotrix); # Loaing them here saves a step.

source("soniclib.r");
source("gb.r");

main <- function(d = NULL, run.multires=FALSE) {
  
  DIR.NAME  <- "../TestData/CRA.01";
  BASE.NAME <- "../Applications";
  OUT.NAME  <- file.path(BASE.NAME,"Odori");
  dir.create(BASE.NAME,showWarnings=FALSE);
  dir.create(OUT.NAME,showWarnings=FALSE);
  
  # Load CRA.01 data and isolate the hour with maximum variation
  # in H2O concentration. This hour is then plotted.
  if(is.null(d)) {
    d <- average.sonic.file.set(DIR.NAME,averaging.time=60, verbose=TRUE);
  }
  qq.max <- which.max(d$data$q.max - d$data$q.min);
  file.max.delta.q <- file.path(DIR.NAME,format(d$data$t.stamp[qq.max],tz="UTC",format="%Y%m%d.%H.csv"));
  day.name <- format(d$data$t.stamp[qq.max],tz="UTC",format="%Y-%m-%d %H:%M:%S");
  
  # Get actual data
  e <- get.raw.data(file.max.delta.q);
  n <- length(e$data$u);
  
  # Compute mean water concentration
  q.mean <- rep(d$data$q.avg[qq.max], times=n);
  
  # Plot water concentration in this hour
  f<-file.path(OUT.NAME, "water_conc.jpg");
  jpeg(f, width=800, height=600);
  plot((0:(n-1))/n*60, e$data$q, col="pink", type="l", xlab="Time (min)", ylab=expression(paste(H[2],O," (",mmol/m^3,")")), main=day.name);
  dev.off();
  
  # Plot water concentration in this hour
  f<-file.path(OUT.NAME, "water_conc_and_mean.jpg");
  jpeg(f, width=800, height=600);
  plot((0:(n-1))/n*60, e$data$q, col="pink", type="l", xlab="Time (min)", ylab=expression(paste(H[2],O," (",mmol/m^3,")")), main=day.name);
  lines((0:(n-1))/n*60, q.mean, col="blue", lwd=4);
  dev.off();
  
  # Plot instant wind direction
  dir <- atan2(e$data$v,e$data$u)*180/pi;
  dir[dir<0] <- dir[dir<0]+360;
  f<-file.path(OUT.NAME, "wind_direction.jpg");
  jpeg(f, width=800, height=600);
  plot((0:(n-1))/n*60, dir, col="pink", type="l", xlab="Time (min)", ylab=expression(paste("Wind direction (° from N)")), main=day.name);
  dev.off();
  
  # Convert mean and standard deviation of water concentration to the corresponding
  # log-normal parameters
  mu    <- mean(e$data$q, na.rm=TRUE);
  sigma <- sd(e$data$q, na.rm=TRUE);
  Q <- log(1+(sigma/mu)^2);
  m <- log(mu) - 0.5*Q;
  s <- sqrt(Q);
  
  # Estimate log-normal density corresponding to data
  q.vals <- seq(from=0, to=max(e$data$q,na.rm=TRUE), length.out=128);
  d.vals <- dlnorm(q.vals, m, s);
  
  # Plot water concentration in this hour
  f<-file.path(OUT.NAME, "water_conc_density.jpg");
  jpeg(f, width=800, height=600);
  hist(e$data$q, col="pink", prob=TRUE, ylab="Prob.density", xlab=expression(paste(H[2],O," (",mmol/m^3,")")), main=day.name);
  lines(q.vals, d.vals, col="red", lwd=3);
  lines(c(mu,mu), c(0,dlnorm(mu,m,s)), col="red", lwd=2, lty=3);
  points(c(mu,mu), c(0,dlnorm(mu,m,s)), col="red");
  dev.off();
  
  if(run.multires) {
  
    # Multiresolution analysis of wind components: U, V
    f<-file.path(OUT.NAME, "wind_multires_uv.jpg");
    jpeg(f, width=800, height=600);
    t.stamp <- e$data$time.stamp;
    u       <- e$data$u;
    v       <- e$data$v;
    num.int <- (1:360)*10;
    v.v     <- numeric(length(num.int));
    u.v     <- numeric(length(num.int));
    i <- 1;
    for(int in num.int) {
      u.v[i] <- sub.interval.variance(t.stamp, u, int);
      v.v[i] <- sub.interval.variance(t.stamp, v, int);
      i <- i+1;
    }
    u.var.tot <- var(u,na.rm=TRUE);
    v.var.tot <- var(v,na.rm=TRUE);
    plot(3600/num.int,u.v/u.var.tot*100,type="l",col="red",ylab="Fraction of total variance (%)",xlab="Block length (s)",main="Wind components U (red), V (blue)");
    lines(3600/num.int,v.v/v.var.tot*100,col="blue");
    dev.off();
    
    # Multiresolution analysis of wind components: W
    f<-file.path(OUT.NAME, "wind_multires_w.jpg");
    jpeg(f, width=800, height=600);
    w       <- e$data$w;
    w.v     <- numeric(length(num.int));
    i <- 1;
    for(int in num.int) {
      w.v[i] <- sub.interval.variance(t.stamp, w, int);
      i <- i+1;
    }
    w.var.tot <- var(w,na.rm=TRUE);
    plot(3600/num.int,w.v/w.var.tot*100,type="l",col="black",ylab="Fraction of total variance (%)",xlab="Block length (s)",main="Wind component W");
    dev.off();
    
    # Multiresolution analysis of water concentration
    f<-file.path(OUT.NAME, "water_multires.jpg");
    jpeg(f, width=800, height=600);
    q       <- e$data$q;
    q.v     <- numeric(length(num.int));
    i <- 1;
    for(int in num.int) {
      q.v[i] <- sub.interval.variance(t.stamp, q, int);
      i <- i+1;
    }
    q.var.tot <- var(q,na.rm=TRUE);
    plot(3600/num.int,q.v/q.var.tot*100,type="l",col="black",ylab="Fraction of total variance (%)",xlab="Block length (s)",main="Water concentration");
    dev.off();
  
  }
  
  # Eddy covariance step, with all defaults
  e <- eddy.covariance(d, station.altitude=10);
  
  # Compute mixing height (to be used later)
  zi <- gryning.batchvarova(e);
  
  # Compute Deardoff velocity
  w.star <- numeric(length(e$data$t.stamp));
  convective <- which(e$data$wt.rot > 0);
  stable     <- which(e$data$wt.rot <= 0);
  w.star[convective] <- (9.807/e$data$t.avg[convective] * zi[convective] * e$data$wt.rot[convective])^(1/3);
  
  # Estimate sigmas according to Calpuff, close to ground (as for odors)
  z <- 2;  # Wind measurement height
  an <- exp(-0.9*z/zi);
  z.over.L  <- z/e$data$L;
  z.over.zi <- z/zi;
  sigma.h <- numeric(length(zi));
  sigma.w <- numeric(length(zi));
  sigma.h[convective] <- sqrt(4*e$data$u.star[convective]^2*an[convective]^2 + 0.35*w.star[convective]^2);
  sigma.w[convective] <- sqrt(1.6*e$data$u.star[convective]^2*an[convective]^2 + 2.9*e$data$u.star[convective]*(-z.over.L[convective]));
  sigma.h[stable]     <- e$data$u.star[stable] * (1.6*(1-z.over.zi[stable])^(3/4)*z.over.L[stable] + 1.8*an[stable]) / (1 + z.over.L[stable]);
  sigma.w[stable]     <- 1.3*e$data$u.star[stable] * ((1-z.over.zi[stable])^(3/4)*z.over.L[stable] + an[stable]) / (1 + z.over.L[stable]);
  
  # Compute horizontal sigma
  measured.sigma.h <- (e$data$uu.rot + e$data$vv.rot)/2;
  
  # Plot u vs v sigma
  f<-file.path(OUT.NAME, "sigma_u_vs_v.jpg");
  jpeg(f, width=600, height=600);
  plot(e$data$uu.rot, e$data$vv.rot, xlab="Sigma u (m/s)", ylab="Sigma v (m/s)", main=day.name);
  dev.off();
  
  # Plot measured vs estimated horizontal sigma
  min.s <- min(c(measured.sigma.h,sigma.h));
  max.s <- max(c(measured.sigma.h,sigma.h));
  lin.mod <- lm(sigma.h~measured.sigma.h);
  f<-file.path(OUT.NAME, "sigma_h_measured_vs_calpuff.jpg");
  jpeg(f, width=600, height=600);
  plot(measured.sigma.h, sigma.h, xlim=c(min.s,max.s), ylim=c(min.s,max.s), xlab="Sigma h (m/s) - Measured", ylab="Sigma h (m/s) - Calpuff estimate near ground", main=day.name);
  abline(lin.mod);
  dev.off();
  
  # Plot measured vs estimated vertical sigma
  measured.sigma.w <- e$data$ww.rot;
  min.s <- min(c(measured.sigma.w,sigma.w));
  max.s <- max(c(measured.sigma.w,sigma.w));
  lin.mod <- lm(sigma.w~measured.sigma.w);
  f<-file.path(OUT.NAME, "sigma_w_measured_vs_calpuff.jpg");
  jpeg(f, width=600, height=600);
  plot(e$data$ww.rot, sigma.w, xlim=c(min.s,max.s), ylim=c(min.s,max.s), xlab="Sigma w (m/s) - Measured", ylab="Sigma w (m/s) - Calpuff estimate near ground", main=day.name);
  abline(lin.mod);
  dev.off();
  
  return(d);
  
}


sub.interval.variance <- function(t.stamp, value, num.sub.intervals.per.hour) {
  
  # Generate block allocation vector
  blk.len <- 3600/num.sub.intervals.per.hour;
  blk     <- as.integer(floor(t.stamp / blk.len));
  
  # Average data in same block
  means   <- aggregate(value, by=list(blk), FUN=mean, na.rm=TRUE);
  
  # Compute intra-block variance
  v <- var(means$x, na.rm=TRUE);
  return(v);
  
}
