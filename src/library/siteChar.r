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

# Site characterization function, the golden standard way
#
# Input:
#
#   station.name  Name of station to characterize (must belong the list of officially
#                 known stations stored in StationList.csv file), or nothing. In this
#                 case the list of station names is printed.
#
#   d             List of type "sonic.eddy.cov.data" containing results of this station
#                 processing, or NULL if the station processing is to be done from scratch.
#                 This parameter is ignored if "station.name" is NULL.
#
# Result:
#
#   A list of type "sonic.eddy.cov.data" containing the processing results, if
#   this has been done or used; a NULL otherwise.
#

get.station.list <- function() {
  
  # Get station data from official list
  s <- read.csv("../../SHAKEUP/StationsList.csv",stringsAsFactors=FALSE);
  print(s$name);
  
}

prepare.characterization.dataset <- function(station.name) {
  
  # Get station data from official list
  s <- read.csv("../../SHAKEUP/StationsList.csv",stringsAsFactors=FALSE);
  s.idx <- which(s$name == station.name);
  if(length(s.idx) > 0) {
    d.avg <- average.sonic.file.set(s$MainDir, trend.removal="linear", verbose=TRUE);
    d <- eddy.covariance(d.avg, station.altitude=s$h);
    return(d);
  }
  else {
    print("Station not found. Current list is:");
    print(s$name);
    return(NULL);
  }
  
}

characterize <- function(station.name, d) {
  
  # Get station data from official list
  s <- read.csv("../../SHAKEUP/StationsList.csv",stringsAsFactors=FALSE);
  s.idx <- which(s$name == station.name);
  if(length(s.idx) > 0) {
    d.card <- list(
      name = s$name[s.idx],
      lat  = s$lat[s.idx],
      lon  = s$lon[s.idx],
      h    = s$h[s.idx]
    );
    save(d,d.card,file="../services.characterization/characterization.RData");
  }
  else {
    print("Station not found. Current list is:");
    print(s$name);
  }
}
