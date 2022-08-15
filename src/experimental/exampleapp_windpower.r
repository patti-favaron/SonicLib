source("auxiliary.r");
source("soniclib.r");

#------------------------------------------------------------------
#
# Example of a SonicLib application foe assessing wind energy potential
# at a site.
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

