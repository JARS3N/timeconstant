rm_deadspace <- function(file) {
  x <-setNames(
    read.csv(file, skip = 4)[c("Time.sec.", "High.CHpH")],
   c("Time", "counts"))
  x$diff <- c(0, diff(x$counts))
  injection_at <- which(x$diff == min(x$diff)) - 3
  x <- x[-1 * (1:injection_at), ]
  x$Time <- x$Time - x$Time[1]
  x$file<-basename(file)
  x[c("Time","counts","file")]
}


TC_adj<-function(Time,counts){
# data capture@500ms, therefore
# using datapoint 61 as approx period of 30 seconds
period <- Time[61] 
deltaP <- counts[1] - counts[61]
deltaT<-counts[1]*(exp(1)^-1)
abs(period/(log(1/(1 - (deltaP/deltaT)))))
}

TC_adjx<-function(Time,counts){
  # data capture@500ms, therefore
  # using datapoint 61 as approx period of 30 seconds
  period <- Time[61] 
  deltaP <- counts[1] - counts[61]
  deltaT<-counts[1]*(1-exp(1)^-1)
  abs(period/(log(1/(1 - (deltaP/deltaT)))))
}
