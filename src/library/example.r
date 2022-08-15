#------------------------------------------------------------------
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

get.raw.data.ex.1 <- function() {
  
  pdf(file="get_raw_data_1.pdf", width = 4.2, height = 3.5);
  
  # Get data using 'get.raw.data', and plot its vertical wind speed density (gamma estimate)
  d <- get.raw.data("../TestData/20100516.10.csv", verbose=TRUE);
  plot(density(d$data$w),xlab="w (m/s)", ylab="Density", main="");
  
  # Compute a normal density with same first two moments as experimental "w",
  # and add it to the plot using a dashed line
  mu <- mean(d$data$w, na.rm=TRUE);
  sigma <- sqrt(var(d$data$w, na.rm=TRUE));
  w.min <- min(d$data$w, na.rm=TRUE);
  w.max <- max(d$data$w, na.rm=TRUE);
  x <- seq(from=w.min, to=w.max, length.out=128);
  lines(x,dnorm(x,mu,sigma),lty="dashed");
  
  dev.off();
  
}


test.detrending.1 <- function() {
  
  # Compute control and treatment
  d0<-average.sonic.file.set("../TestData/CRA.01", trend.removal="none", verbose=TRUE);
  d1<-average.sonic.file.set("../TestData/CRA.01", trend.removal="linear", verbose=TRUE);
  ec0 <- eddy.covariance(d0, station.altitude=50);
  ec1 <- eddy.covariance(d1, station.altitude=50);
  t.stamp <- d0$data$t.stamp;
  H0.v.trend     <- ec0$data$H0.v;
  H0.v.detrend   <- ec1$data$H0.v;
  w.rns.trend    <- abs(ec0$data$w.rns);
  wt.nst.trend   <- abs(ec0$data$wt.nst);
  w.rns.detrend  <- abs(ec1$data$w.rns);
  wt.nst.detrend <- abs(ec1$data$wt.nst);
  
  # Plot buoyancy fluxes with and without trend removal
  pdf(file="../manual/diagrams/H0vDetrend.pdf", width = 4.2, height = 3.5);
  plot(t.stamp, H0.v.trend, type="l", xlab="", ylab="Buoyancy flux (W/m2)",col="red");
  lines(t.stamp, H0.v.detrend, col="blue");
  lines(t.stamp, rep(0,times=48), col="black");
  dev.off();
  
  # Plot vertical wind speed relative non-stationarity with and without trend removal
  val.min <- min(c(min(w.rns.trend),min(w.rns.detrend),0));
  val.max <- max(c(max(w.rns.trend),max(w.rns.detrend)));
  pdf(file="../manual/diagrams/WRnsDetrend.pdf", width = 4.2, height = 3.5);
  plot(t.stamp, w.rns.trend, type="l", xlab="", ylab="Rel. non stat. for W",col="red",ylim=c(val.min,val.max));
  lines(t.stamp, w.rns.detrend, col="blue");
  lines(t.stamp, rep(0,times=48), col="black");
  dev.off();
  
  # Plot non-steadiness for w't' with and without trend removal
  pdf(file="../manual/diagrams/WTNstDetrend.pdf", width = 4.2, height = 3.5);
  plot(t.stamp, wt.nst.trend, type="l", xlab="", ylab="Rel. non stat. for W",col="red");
  lines(t.stamp, wt.nst.detrend, col="blue");
  lines(t.stamp, rep(0,times=48), col="black");
  dev.off();
  
}

plot.dir.mean.example <- function(e.dm) {
  # d<-average.sonic.file.set("../TestData/mfc1/Vertemate", verbose=TRUE);
  # e<-eddy.covariance(d, 300);
  # e.dm<-dir.mean(e$data$Dir, e$data$u.star);
  
  pdf(file="../manual/diagrams/DirMeanUstar_1.pdf", width = 4.2, height = 4.2);
  plot.dir.mean(e.dm, 0, 0.5);
  dev.off();
  
  pdf(file="../manual/diagrams/DirMeanUstar_2.pdf", width = 4.2, height = 4.2);
  plot.dir.mean(e.dm, 0, 0.5, conf.limits=TRUE);
  dev.off();
  
  pdf(file="../manual/diagrams/DirMeanUstar_3.pdf", width = 4.2, height = 4.2);
  plot.dir.num(e.dm);
  dev.off();
  
}

plot.dir.num.example <- function(e.dm) {
  
  # d<-average.sonic.file.set("../TestData/mfc1/Vertemate", verbose=TRUE);
  # e<-eddy.covariance(d, 300);
  # e.dm<-dir.mean(e$data$Dir, e$data$u.star);
  
  pdf(file="../manual/diagrams/DirNumUstar.pdf", width = 4.2, height = 4.2);
  plot.dir.num(e.dm);
  dev.off();
  
}

test.hw <- function(sample.size, mu, sigma, intermittency) {
  mean.smpl <- numeric(sample.size);
  sd.smpl   <- numeric(sample.size);
  max.smpl  <- numeric(sample.size);
  pom.smpl  <- numeric(sample.size);
  for(i in 1:sample.size) {
    smpl <- hildeman.wilson.process(300, mu, sigma, intermittency);
    mean.smpl[i] <- mean(smpl);
    max.smpl[i]  <- max(smpl);
    sd.smpl[i]   <- sd(smpl);
    pom.smpl[i]  <- max.smpl[i] / mean.smpl[i];
  }
  
  l <- list(
    avg = mean(mean.smpl),
    std = mean(sd.smpl),
    max = mean(max.smpl),
    pom = mean(pom.smpl)
  );
  return(l);
  
}


mean.hw <- function(sample.size, mu, sigma, intermittency.vector) {
  tmp <- numeric(length(intermittency.vector));
  for(i in 1:length(intermittency.vector)) {
    val <- test.hw(sample.size, mu, sigma, intermittency.vector[i]);
    tmp[i] <- val$avg;
  }
  return(tmp);
}


std.hw <- function(sample.size, mu, sigma, intermittency.vector) {
  tmp <- numeric(length(intermittency.vector));
  for(i in 1:length(intermittency.vector)) {
    val <- test.hw(sample.size, mu, sigma, intermittency.vector[i]);
    tmp[i] <- val$std;
  }
  return(tmp);
}

max.hw <- function(sample.size, mu, sigma, intermittency.vector) {
  tmp <- numeric(length(intermittency.vector));
  for(i in 1:length(intermittency.vector)) {
    val <- test.hw(sample.size, mu, sigma, intermittency.vector[i]);
    tmp[i] <- val$max;
  }
  return(tmp);
}

pom.hw <- function(sample.size, mu, sigma, intermittency.vector) {
  tmp <- numeric(length(intermittency.vector));
  for(i in 1:length(intermittency.vector)) {
    val <- test.hw(sample.size, mu, sigma, intermittency.vector[i]);
    tmp[i] <- val$pom;
  }
  return(tmp);
}


wind.power.stats <- function(w) {
  # d <- sonic.average.file.set("CRA.01",10_minutes_averaging)
  # w <- wind.power(d, cut.speed=FALSE);
  pdf("../manual/diagrams/CRA01_Weibull.pdf", height=4, width=6);
  plot((0:100)/10,dweibull((0:100)/10,w$weibull.alpha,w$weibull.beta),xlab="Wind speed (m/s)", ylab="Density", type="l")
  dev.off();
  
  pdf("../manual/diagrams/Weibull_Exceedance_Probability.pdf", height=4, width=6);
  plot((0:100)/10,100-100*pweibull((0:100)/10,1.59,3.04),xlab="Wind speed (m/s)", ylab="Frequency (%)", type="l");
  dev.off();
  
  pdf("../manual/diagrams/CRA01_Weibull_exper.pdf", height=4, width=6);
  plot((0:100)/10,dweibull((0:100)/10,w$weibull.alpha,w$weibull.beta),xlab="Wind speed (m/s)", ylab="Density", type="l");
  lines(density(w$data$vel,na.rm=TRUE),col="red",lwd=2);
  dev.off();
}


wind.power.stats.cutted <- function(w) {
  # d <- sonic.average.file.set("CRA.01",10_minutes_averaging)
  # w <- wind.power(d, lower.cut.speed=4, upper.cut.speed=25);
  pdf("../manual/diagrams/CRA01_Weibull_cut.pdf", height=4, width=6);
  plot((0:100)/10,dweibull((0:100)/10,w$weibull.alpha,w$weibull.beta),xlab="Wind speed (m/s)", ylab="Density", type="l")
  dev.off();
  
  pdf("../manual/diagrams/Weibull_Exceedance_Probability_cut.pdf", height=4, width=6);
  plot((0:100)/10,100-100*pweibull((0:100)/10,1.59,3.04),xlab="Wind speed (m/s)", ylab="Frequency (%)", type="l");
  dev.off();
  
  pdf("../manual/diagrams/CRA01_Weibull_exper_cut.pdf", height=4, width=6);
  plot((0:100)/10,dweibull((0:100)/10,w$weibull.alpha,w$weibull.beta),xlab="Wind speed (m/s)", ylab="Density", type="l");
  lines(density(w$data$vel,na.rm=TRUE),col="red",lwd=2);
  dev.off();
}
