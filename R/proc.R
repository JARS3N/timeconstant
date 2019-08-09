proc<-function(TIME,COUNTS){
start<-COUNTS[1]
period<-timeconstant::find_closest(TIME,30)
endP<-COUNTS[TIME==period]
endT<-timeconstant::lowest_avg_n_counts(COUNTS)
deltaP <- start - endP
deltaT <- start - endT
abs(period/(log(1/(1 - (deltaP/deltaT)))))
}
