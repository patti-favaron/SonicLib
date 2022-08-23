# Ordine.R - Function collection for converting USA-1 data to
#            SonicLib form
#
# Written by: Davide Casabianca, 
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

control <- function(filename1, filename2) {
  a <- read.csv(filename1, header = F, stringsAsFactors = F); # Read the first input file
  b <- a$V1; # Extract data from the data frame
  c <- b[length(b)]; # Last string of the first input file 
  d <- read.csv(filename2, header = F, stringsAsFactors = F); # Read the second input file
  e <- d$V1;
  f <- e[1]; # First string of the second input file
  g <- nchar(c); # Count the number of characters contained in the last line of the first input file
  h <- g==41; # Compare with the number of characters of a complete string
  i <- character(41); 
  if(h == FALSE){
    i <- paste(c, f, sep = ""); # Completed line
    b <- b[-length(b)]; # Delete the last incomplete line of the first vector
    b <- c(b, i); # Append to the first vector the completed line
    e <- e[-1]; # Delete the first incomplete line of the second vector
    
  } 
  if(h == TRUE){
    b <- b 
    e <- e 
  }
  
  write(b, file = filename1, ncolumns = if(is.character(b)) 1 else 5, append = F, sep = " ");
  write(e, file = filename2, ncolumns = if(is.character(e)) 1 else 5, append = F, sep = " ");
  
}

ordine <- function(pattern){
  setwd(pattern); 
  alldataset <- list.files(pattern); # Vector containing the names of files to be rearranged
  for(i in 1:(length(alldataset) -1)){
  control(alldataset[i], alldataset[i+1])}  
}

# Function that delete the first line and the last line of the whole dataset
ordine_primo_ultimo <- function(pattern){
  setwd(pattern);
  alldataset <- list.files();
  primo <- read.csv(alldataset[1], header = F, stringsAsFactors = F);
  primo <- primo$V1;
  primo <- primo[-1];
  ultimo <- read.csv(alldataset[length(alldataset)], header = F, stringsAsFactors = F);
  ultimo <- ultimo$V1;
  ultimo <- ultimo[-length(ultimo)];
  write(primo, file = alldataset[1], ncolumns = if(is.character(primo)) 1 else 5, append = F, sep = " ");
  write(ultimo, file = alldataset[length(alldataset)], ncolumns = if(is.character(ultimo)) 1 else 5, append = F, sep = " ");
}
  
