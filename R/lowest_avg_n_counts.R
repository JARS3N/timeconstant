lowest_avg_n_counts<-function(U,n=5){
  i<-seq_along(U)
  A<-na.omit(lag(i,n-1))
  B<-na.omit(lead(i,n-1))
  mapd<-mapply(function(A,B){mean(U[A:B])},A,B)
  min(mapd)
}
