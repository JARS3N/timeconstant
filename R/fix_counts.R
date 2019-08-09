#impute missing counts during injection with previous data point
fix_counts<-function(u){
  i<- which(u=="INJECTION"|is.na(u))  
  u[i]<-u[i-1]
  return(u)
}
