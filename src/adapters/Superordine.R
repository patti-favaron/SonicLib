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
