# Perform the actual multiresolution decomposition
vector.decompose <- function(t.stamp, val, steps) {
  
  # Reserve workspace
  n.steps <- length(steps);
  multires.values <- data.frame(t.stamp);
  vars            <- numeric(n.steps);
  
  # Main loop
  residual <- val;
  decomposition <- numeric(length(val));
  for(i in 1:n.steps) {
    
    # Prepare residual name based on step value
    mean.name <- sprintf("M.%d", i);
    dec.name  <- sprintf("D.%d", i);
    res.name  <- sprintf("R.%d", i);
    
    # Use time stamps to generate the time aggregation function, and use it
    time.block  <- as.integer(floor(t.stamp)) %/% steps[i];
    block.means <- aggregate(residual, by=list(time.block), FUN=mean, na.rm=TRUE);
    
    # Translate time block to 1-based index form
    block.index <- time.block - block.means$Group.1[1] + 1;
    
    # Construct a vector of same length as original data, and values equal
    # to block averages; deduce block residual from it
    block.means.val  <- block.means$x;
    block.means.full <- block.means.val[block.index];
    block.residual   <- residual - block.means.full;
    
    # Compute the total variance pertaining the full-length block averages
    block.variance <- var(block.means.full, na.rm=TRUE);
    
    # Update residual and decomposition
    residual <- block.residual;
    decomposition <- decomposition + block.means.full;
    
    # Accumulate variances, means and residuals
    multires.values[res.name]  <- residual;
    multires.values[dec.name]  <- decomposition;
    multires.values[mean.name] <- block.means.full;
    vars[i] <- block.variance;
    
  }
  
  # Build output
  l <- list(
    mres            = multires.values,
    variance.scales = vars,
    variance.res    = var(residual, na.rm=TRUE),
    variance.total  = var(val, na.rm=TRUE)
  );
  class(l) <- "multires.analysis";
  return(l);
  
}


# Build a vector of halving time scales until a threshold is reached
build.times.sequence <- function(total.length=3600, threshold=0.2) {
  
  # Count elements
  n <- 0;
  val <- total.length;
  while(val > threshold) {
    n <- n+1;
    val <- val/2;
  }
  vect <- numeric(n);
  
  # Build vector
  i <- 0;
  val <- total.length;
  while(val > threshold) {
    i <- i+1;
    vect[i] <- val;
    val <- val/2;
  }
  return(vect);
}

