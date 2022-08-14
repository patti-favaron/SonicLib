source("auxiliary.r");
source("soniclib.r");

w.pwr <- function(d=NULL, verbose=TRUE) {
  
  if(is.null(d)) {
    d <- wind.power.avg("CRA.01", verbose=verbose);
  }

  # Compute yield for three turbines
  
  turb.name <- "Vestas V80";
  y <- turbine.yield(d, turb.name, verbose=verbose);
  print(sprintf("Yield of turbine '%s': %f kWh", turb.name, y));
  
  turb.name <- "Siemens SWT-2.3 82";
  y <- turbine.yield(d, turb.name, verbose=verbose);
  print(sprintf("Yield of turbine '%s': %f kWh", turb.name, y));
  
  turb.name <- "Repower MM82";
  y <- turbine.yield(d, turb.name, verbose=verbose);
  print(sprintf("Yield of turbine '%s': %f kWh", turb.name, y));
  
  # Compute angle to horizontal plane
  w <- wind.power(d, cut.speed=FALSE, verbose=TRUE);
  phi <- w$data$ang.Phi;
  t.stamp <- w$data$t.stamp;
  
  # Plot angle
  #plot(t.stamp, phi, type="l", xlab="", ylab="Angle to horizontal (°)", main="Data set: CRA.01");
  
  # Plot ratio of resultant to scalar speed
  scalar.vel <- w$data$vel;
  resultant.vel <- w$data$resultant.vel;
  ratio.vel <- resultant.vel / scalar.vel;
  #plot(t.stamp, ratio.vel, type="l", xlab="", ylab="Resultant/Scalar Speed (--)", main="Data set: CRA.01");
  
  # Plot speed and direction
  #plot(t.stamp, scalar.vel, type="l", xlab="", ylab="Wind speed (m/s)", main="Dataset: CRA.01");
  #plot(x=t.stamp, y=w$data$Dir, type="p", xlab="", ylab="Wind direction (° from North)", main="Dataset: CRA.01");
  
  # Plot maximum to mean speed ratio
  max.vel <- w$data$vel.max;
  plot(x=t.stamp, y=max.vel/scalar.vel, type="l", xlab="", ylab="Maximum to mean speed ratio (--)", main="Dataset: CRA.01");
  
  return(d);
  
}

