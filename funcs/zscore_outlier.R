#find elements that are +/- 3 standard deviations from mean, converts elements to NA 
zscore_outlier <- function(x){
  y = which( ((x-mean(x))/sd(x)) < -3 | ((x-mean(x))/sd(x)) >3 )
  x[y] <- NA
  return(x)
  
}