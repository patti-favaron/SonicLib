library(deSolve);
library(timeSeries);

#------------------------------------------------------------------
#
# Alternate implementation of Gryning-Batchvarova model with gap
# filling and other minutiae - experimental, and just demonstrational.
#
# Written by: Patrizia Favaron
# e-mail:     patti.favaron@gmail.com
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

gryning.batchvarova <- function(e, time.zone="UTC", verbose=FALSE) {

  # Functions realizing the computation
  
  g <- function(z) {
    out <- 3/(z+1) - 1.98e-3 + 2.27e-6*z;
    return(out);
  }
  
  gry.bat <- function(t, zi, parms) {
    out <- list(
      c(
        parms$wt/g(zi) * 1/(zi^2/(1.4*zi-2*parms$L) + 8*parms$u.star^2*parms$T/(g(zi)*9.807*(1.2*zi-parms$L)))
      )
    );
    return(out);
  }
  
  fill.gaps <- function(x, typical.x=NULL) {
    
    # Fill "inside" gaps by linear interpolation
    y <- interpNA(x, mode="linear")[,1];
    
    # Fill outside gaps
    if(!is.null(typical.x)) {
      
      y[is.na(y)] <- typical.x[is.na(y)]; 
      
    }
    else {
      
      # No typical day available
      if(all(is.na(y))) return(y);
      valid.idx <- which(!is.na(y));
      min.idx   <- min(valid.idx);
      max.idx   <- max(valid.idx);
      if(min.idx > 1) {
        pre <- 1:(min.idx-1);
        y[pre] <- y[min.idx];
      }
      if(max.idx < length(y)) {
        post <- (max.idx+1):length(y);
        y[post] <- y[max.idx];
      }
    }
    
    return(y);
    
  }
  
  # Check input makes sense
  if(class(e) != "sonic.eddy.cov.data") {
    if(verbose) print("gryning.batchvarova:: error: Input not of type 'sonic.eddy.cov.data'");
    return(NULL);
  }
  
  # Get relevant data
  time.stamp <- e$data$t.stamp;
  wt         <- e$data$wt.rot;
  L          <- e$data$L;
  u.star     <- e$data$u.star;
  T          <- e$data$t.avg;
  data <- data.frame(time.stamp,wt,L,u.star,T);
  
  # Complete data in time ensuring full days are used
  avg.time  <- e$averaging.time;
  from.time <- floor.day(min(time.stamp,na.rm=TRUE),time.zone=time.zone);
  to.time   <- floor.day(max(time.stamp,na.rm=TRUE),time.zone=time.zone) + 24*3600 - 60*avg.time;
  full.time <- seq(from=from.time, to=to.time, by=avg.time*60);
  all.data  <- data.frame(time.stamp=full.time);
  full.data <- merge(data, all.data, by="time.stamp",all.y=TRUE);
  # Post-condition: Now all implicit gaps are "evident", and the set
  #                 only consists of an integer number of full days.
  
  # Prepare result vector
  zi            <- numeric(length(full.data$wt));
  zi.mechanical <- numeric(length(full.data$wt));
  zi.convective <- numeric(length(full.data$wt));
  
  # Identify days
  data.per.day <- 24*60/avg.time;
  n            <- length(zi);
  num.days     <- n %/% data.per.day;
  
  # Compute typical day for all quantities
  typical.wt     <- typical.period(wt, step.in.day(time.stamp, avg.time))$x;
  typical.L      <- typical.period(L, step.in.day(time.stamp, avg.time))$x;
  typical.u.star <- typical.period(u.star, step.in.day(time.stamp, avg.time))$x;
  typical.T      <- typical.period(T, step.in.day(time.stamp, avg.time))$x;
  
  # Fill gaps, if any, in typical periods (gaps *may* be, in case of small samples and/or
  # systematic, time-dependent gaps in original series).
  typical.wt     <- fill.gaps(typical.wt);
  typical.L      <- fill.gaps(typical.L);
  typical.u.star <- fill.gaps(typical.u.star);
  typical.T      <- fill.gaps(typical.T);
  
  # If still exist gaps in any typical days, stop: nothing can be done
  # (this typically happens with small samples)
  typical.valid <- !is.na(typical.wt) & !is.na(typical.L) & !is.na(typical.u.star) & !is.na(typical.T);
  if(any(!typical.valid)) {
    if(verbose) print("gryning.batchvarova:: error: No valid data in columns wt.rot, L, u.star. T");
    return(NULL);
  }
  
  # Process days separately
  for(day in 1:num.days) {
    
    # Print advancement message
    if(verbose) print(sprintf("Day %d of %d", day, num.days));
    
    # Get current day
    j.from <- 1 + (day-1)*data.per.day;
    j.to   <- j.from + data.per.day - 1;
    j.day.idx <- j.from:j.to;
    
    # Get this day
    time.stamp.day <- full.data$time.stamp[j.day.idx];
    wt.day         <- full.data$wt[j.day.idx];
    L.day          <- full.data$L[j.day.idx];
    u.star.day     <- full.data$u.star[j.day.idx];
    u.star.day[u.star.day <= 0] <- NA;
    T.day          <- full.data$T[j.day.idx];
    
    # Check something is to be done
    invalid <- is.na(wt.day) | is.na(L.day) | is.na(u.star.day) | is.na(T.day);
    if(all(invalid)) {
      zi[j.day.idx] <- NA;
      zi.mechanical[j.day.idx] <- NA;
      zi.convective[j.day.idx] <- NA;
    }
    else {
      
      # Fill gaps
      wt.day         <- fill.gaps(wt.day, typical.wt);
      L.day          <- fill.gaps(L.day, typical.L);
      u.star.day     <- fill.gaps(u.star.day, typical.u.star);
      T.day          <- fill.gaps(T.day, typical.T);
      
      # Build temporary output
      zi.day <- numeric(144);
  
      # Estimate mechanical turbulence and reserve workspace for convective
      zi.mech <- 1330*u.star.day;
      zi.conv <- numeric(length(zi.mech));
      zi.out  <- numeric(length(zi.mech));
      
      # Identify first and last instants of convective part
      if(!any(wt.day > 0)) {
        zi.out <- zi.mech;
      }
      else {
        positive.wt <- which(wt.day > 0); # At least one must exist because of the test
        first <- min(positive.wt);
        last  <- max(positive.wt);
        # Post-condition: The "convective" part might contain some negative
        # and missing values.
        
        # Solve Gryning-Batchvarova equation
        zini <- c(zi=0.0);
        for(i in first:last) {
          parms <- list(wt=wt.day[i], L=L.day[i], u.star=u.star.day[i], T=T.day[i]);
          de   <- ode(y=zini, func=gry.bat, parms=parms, method="rk4", times=seq(from=0,to=60*avg.time,by=60*avg.time));
          dd   <- as.data.frame(de);
          zi.conv[i] <- dd$zi[2];
          zini <- c(zi=dd$zi[2]);
        }
        for(i in 1:data.per.day) zi.out[i] <- max(c(zi.mech[i],zi.conv[i]));
        zi[j.day.idx] <- zi.out;
        zi.mechanical[j.day.idx] <- zi.mech;
        zi.convective[j.day.idx] <- zi.conv;
      }
    }
  }
  
  # Prepare output
  full.data <- data.frame(full.data, zi, zi.mechanical, zi.convective);
  
  return(full.data);
  
}
