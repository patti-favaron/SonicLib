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

# Function to access computable data
get.data.path <- function(data.set.name = NULL) {
  if(is.null(data.set.name)) {
    print("Names of available data sets:");
    print("");
    print("  CRA.01");
    print("  Ameriflux.OpenPath.Day104");
    print("  Ameriflux.OpenPath.Day181");
    print("");
  }
  else if(data.set.name == "CRA.01") {
    return("../TestData/CRA.01");
  }
  else if(data.set.name == "Ameriflux.OpenPath.Day104") {
    return("../TestData/Ameriflux/Open_Path/Day_104");
  }
  else if(data.set.name == "Ameriflux.OpenPath.Day181") {
    return("../TestData/Ameriflux/Open_Path/Day_181");
  }
}


######################################
# Wind turbine convenience functions #
######################################

# Select turbine response
turbine.pwr <- function(turbine.name = NULL, verbose=FALSE) {
  
  if(is.null(turbine.name)) {
    print("Supported turbine names:");
    print("");
    print("Siemens SWT-2.3 82");
    print("Siemens SWT-2.3 93");
    print("Siemens SWT-3.6 107");
    print("Vestas V34");
    print("Vestas V80");
    print("Vestas V90");
    print("Vestas V112");
    print("Vestas V164");
    print("Repower MM82");
    print("Repower MM92");
    print("Repower 5M");
    print("Repower 6M");
    print("Nordex N60");
    print("Nordex N80");
    print("Nordex N90");
    print("Nordex N100");
    return(NULL);
  }
  t <- read.csv("turbine_pwr.csv", header=T);
  vel <- t[,1];
  if(turbine.name == "Siemens SWT-2.3 82") {
    power <- t[,2];
  }
  else if(turbine.name == "Siemens SWT-2.3 93") {
    power <- t[,3];
  }
  else if(turbine.name == "Siemens SWT-3.6 107") {
    power <- t[,4];
  }
  else if(turbine.name == "Vestas V34") {
    power <- t[,5];
  }
  else if(turbine.name == "Vestas V80") {
    power <- t[,6];
  }
  else if(turbine.name == "Vestas V90") {
    power <- t[,7];
  }
  else if(turbine.name == "Vestas V112") {
    power <- t[,8];
  }
  else if(turbine.name == "Vestas V164") {
    power <- t[,9];
  }
  else if(turbine.name == "Repower MM82") {
    power <- t[,10];
  }
  else if(turbine.name == "Repower MM92") {
    power <- t[,11];
  }
  else if(turbine.name == "Repower 5M") {
    power <- t[,12];
  }
  else if(turbine.name == "Repower 6M") {
    power <- t[,13];
  }
  else if(turbine.name == "Nordex N60") {
    power <- t[,14];
  }
  else if(turbine.name == "Nordex N80") {
    power <- t[,15];
  }
  else if(turbine.name == "Nordex N90") {
    power <- t[,16];
  }
  else if(turbine.name == "Nordex N100") {
    power <- t[,17];
  }
  else {
    if(verbose) print("turbine.pwr:: error: Turbine not found");
    return(NULL);
  }
  turb.pwr <- data.frame(vel, power);
  return(turb.pwr);
}


# Average a data set using wind power conventions
wind.power.avg <- function(data.set.name, verbose=TRUE) {
  data.path <- get.data.path(data.set.name);
  if(is.null(data.path)) {
    if(verbose) print("wind.power.avg:: error: Invalid data path");
    return(NULL);
  }
  d <- average.sonic.file.set(dir.name=data.path, averaging.time=10, verbose=verbose);
  return(d);
}


# Compute the yield of a given turbine with respect to wind averages
turbine.yield <- function(d, turbine.name, verbose=TRUE) {
  
  # Get turbine data
  turb.pwr.curve <- turbine.pwr(turbine.name);
  if(is.null(turb.pwr.curve)) {
    if(verbose) print("turbine.yield:: error: Turbine not found");
    return(NULL);
  }
  vel <- turb.pwr.curve$vel;
  pwr <- turb.pwr.curve$power;
  
  # Compute histogram with 1m/s resolution (the same as
  # turbine power curves)
  max.vel <- ceiling(max(d$data$vel, na.rm=TRUE));
  s <- seq(from=0, to=max.vel, by=1);
  h <- hist(x=d$data$vel, breaks=s, plot=FALSE);
  dens <- h$density;
  
  # Compute expected value of wind power
  n <- length(pwr);
  m <- length(dens);
  if(n < m) {
    k <- m-n;
    pwr <- c(pwr, rep(0, k));
  }
  else if(m < n) {
    k <- n-m;
    dens <- c(dens, rep(0, k));
  }
  expected.value <- sum(dens * pwr) * d$averaging.time/60;
  return(expected.value);
  
}

