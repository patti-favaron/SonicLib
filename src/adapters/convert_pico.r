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