# Superordine.R - Function collection for converting USA-1 data to
#                 SonicLib form
#
# Written by: Davide Casabianca, 
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

read.data <- function(nomefile) {
  
  data <- read.csv(nomefile, header=F, stringsAsFactors = F);
  data <- data$V1;
  
  V <- numeric(length(data));
  U <- numeric(length(data));
  W <- numeric(length(data));
  Temp <- numeric(length(data));
  
  for(i in 1:(length(data))){
    
    V[i] <- as.integer(substring(data[i], 6, 11))/100 # Extract data from raw strings
    U[i] <- as.integer(substring(data[i], 16, 21))/100
    W[i] <- as.integer(substring(data[i], 26, 31))/100
    Temp[i] <- as.integer(substring(data[i], 38, 41))/100
  }
  
  tabella <- data.frame("v" = V, "u" = U, "w" = W ,"t" = Temp);
  write.csv(tabella, file = paste(nomefile, ".csv", sep = ""), row.names = F); #Put data in csv format
}

Superordine <- function(pattern){
  setwd(pattern);
  alldataset <- list.files();
  for(i in 1:(length(alldataset))){
    read.data(alldataset[i])
  }
}
