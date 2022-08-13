stability.classify <- function(z0.ref=0.23) {
  z0 <- c(0.002, 0.006, 0.010, 0.020, 0.040, 0.100, 0.200, 0.400);
  AB <- c(-0.153, -0.139, -0.130, -0.118, -0.108, -0.096, -0.085, -0.076);
  BC <- c(-0.076, -0.065, -0.060, -0.052, -0.044, -0.035, -0.026, -0.018);
  CD <- c(-0.028, -0.022, -0.018, -0.015, -0.011, -0.006, -0.005, -0.003);
  DE <- c(+0.030, +0.019, +0.015, +0.013, +0.010, +0.004, +0.003, +0.003);
  EF <- c(+0.102, +0.080, +0.070, +0.058, +0.048, +0.035, +0.025, +0.018);
  AB.ref <- approx(z0, AB, xout=z0.ref, rule=2);
  BC.ref <- approx(z0, BC, xout=z0.ref, rule=2);
  CD.ref <- approx(z0, CD, xout=z0.ref, rule=2);
  DE.ref <- approx(z0, DE, xout=z0.ref, rule=2);
  EF.ref <- approx(z0, EF, xout=z0.ref, rule=2);
  separation.vector <- c(-2000,AB.ref$y, BC.ref$y, CD.ref$y, DE.ref$y, EF.ref$y,+2000);
  return(separation.vector);
}
