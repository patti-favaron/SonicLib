# Show the "deep autocorrelation" of one of the variables in an object of type
# "sonic.raw.data" passed on input, using a "thin line" representation more helpful
# to us than the usual bargraph used for econometric time series.
show.acf <- function(d, variable="w", depth=0.5, pdf="", width=5, height=4, verbose=FALSE) {
  
  # Input:
  #
  #   d            An object of type "sonic.raw.data"
  #
  #   variable     The name of a variable in "d".
  #                Allowed values are "u", "v", "w", "t", "q", "c",
  #                "a", "m", "temp", "urel". Default: "w".
  #
  #   depth        Number, between 0 and 1, signifying the maximum lag used
  #                to compute autocorrelation expressed as a fraction of total data.
  #                Default: 0.5.
  #
  #   pdf          String which may be empty, if the ACF is to be plotted on
  #                display, or non-empty, and designating in case the name
  #                of a PDF file where the plot is routed; in this latter case
  #                plotting parameters "width" and "height" are considered.
  #                Default: an empty string, meaning plot is needed on display.
  #
  #   width        Page width, used to generate the PDF file (if "pdf" parameter
  #                contains a non-empty string). Inches. Default: 5.
  #                Ignored, if "pdf" contains an empty string.
  #
  #   height       Page height, used to generate the PDF file. Inches. Default: 4.
  #                Ignored, if "pdf" contains an empty string.
  #
  #   verbose      Boolean flag: TRUE to print error / progress messages, FALSE to
  #                not print (default: FALSE)
  #
  # Output:
  #
  #   A vector, containing ACF values at each lag considered. Just for reference.
  #
  
  # Check the type of "d" to be OK
  if(class(d) != "sonic.raw.data") {
    if(verbose) print("show.acf:: error: Input parameter 'd' is not an object of type 'sonic.raw.data'");
    return(NULL);
  }
  
  # How many lags to include?
  if(!is.numeric(depth) | depth < 0 | depth > 1) {
    if(verbose) print("show.acf:: error: Invalid parameter 'depth': non numeric, or not in interval [0,1]");
    return(NULL);
  }
  max.lag <- as.integer(round(length(d$data$time.stamp) * depth));
  if(max.lag < 2) {
    if(verbose) print("show.acf:: error: Maximum desired lag less than 2; check 'd' contains some data, and 'depth' to be not too small");
    return(NULL);
  }
  
  # Get the appropriate variable
  if(variable=="u") {
    x <- d$data$u;
  }
  else if(variable=="v") {
    x <- d$data$v;
  }
  else if(variable=="w") {
    x <- d$data$w;
  }
  else if(variable=="t") {
    x <- d$data$t;
  }
  else if(variable=="q") {
    x <- d$data$q;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'q' (water) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="c") {
    x <- d$data$c;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'c' (carbon dioxide) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="a") {
    x <- d$data$a;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'a' (ammonia) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="m") {
    x <- d$data$m;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'm' (methane) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="temp") {
    x <- d$data$temp;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'temp' (temperature) not belonging to data set 'd'");
      return(NULL);
    }
  }
  else if(variable=="hrel") {
    x <- d$data$hrel;
    if(is.null(x)) {
      if(verbose) print("show.acf:: error: Variable 'hrel' (relative humidity) not belonging to data set 'd'");
      return(NULL);
    }
  }
  # Post-condition: Variable name is OK (that is, belongs to data set 'd'), and
  # all its contents has been read into vector 'x'.
  
  # Redirect output to PDF file, if desired
  if(pdf != "") {
    pdf(file=pdf, width=width, height=height);
  }
  
  # Compute autocorrelation values and plot them
  acf.x <- acf(x, lag.max=max.lag, plot=FALSE, na.action=na.exclude);
  acor <- acf.x$acf;
  plot(
    x=(0:(length(acor)-1))/d$sampling.rate,
    y=acor,
    type="l",
    xlab="lag (s)",
    ylab=paste("ACF(", variable, ")", sep=""), 
    ylim=c(0,1)
    );
  
  # Plot a reference line at 0 to help detecting zero.crosses
  abline(a=0, b=0, col="blue");
  
  # Disconnect from PDF and close it, if required
  if(pdf != "") {
    dev.off();
  }
  
  # Leave
  return(acor);
  
}

