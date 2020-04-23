time_constant<-function(x){
  cumdif.counts<-diff(x$counts)
  injection<-x$Time[which(cumdif.counts==min(cumdif.counts))[1]-3]
  period<-injection + 30
  start<-x$counts[x$Time==x$Time[which(abs(x$Time-injection)==min(abs(x$Time-injection)))]]
  endP<-x$counts[x$Time==x$Time[which(abs(x$Time-period)==min(abs(x$Time-period)))]]
  endT<-min(sapply(which(abs(x$Time-period)==min(abs(x$Time-period))):(length(x$Time)-4),function(u){mean(x$counts[u:u+4])})) #avg of 5 points
  deltaP<-start-endP
  deltaT<-start-endT
  abs(period/(log(1/(1-(deltaP/deltaT)))))
}
