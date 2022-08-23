#------------------------------------------------------------------
# convert.pico
#
# Function, allowing conversion of Metek USA-1 data gathered by Pico
# loggers, to SonicLib form.
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
# Copyright 2022 Patrizia Favaron
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

convert.pico <- function(dir.name, sampling.frequency=10) {
  
  expected.data <- 3600*sampling.frequency;
  
  # Main loop: process all files in directory in sequential order
  l.files <- dir(path=dir.name, pattern="*", full.names=TRUE);
  for(i in 1:length(l.files)) {
    
    file <- l.files[i];
    
    # Get data line by line, and exit if an improper number is found
    d<-read.csv(file=file, header=FALSE, stringsAsFactors = FALSE);
    n <- length(d$V1);
    if(abs(n-expected.data) < 30) {
    
      # Filter out all strings of length not 41 (namely, first and last)
      e <- d$V1[nchar(d$V1) == 41];
      
      # Perform actual data conversion taking Metek conventions into account
      # (namely: X and Y are *exchanged*!!)
      v <- as.numeric(substr(e,  6, 11))/100.0;
      u <- as.numeric(substr(e, 16, 21))/100.0;
      w <- as.numeric(substr(e, 26, 31))/100.0;
      t <- as.numeric(substr(e, 36, 41))/100.0;
      
      # Generate time stamps
      m <- length(e);
      time.stamp <- 0:(m-1)/m*3600;
      
      # Write data to file
      out.file <- paste(file, "csv", sep=".");
      out <- data.frame(
        time.stamp, u, v, w, t
      );
      write.csv(out, file=out.file, row.names=FALSE);
      print(paste("Processed",out.file,sep=" "));
    
    }
    
  }
  
}