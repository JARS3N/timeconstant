find_closest<-function(vec,val){
  distance<- abs(vec-val) 
  vec[which(distance==min(distance))]
}
