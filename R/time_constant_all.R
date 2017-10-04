time_constant_all<-function(u){#where u is the directory of csv files to run
  require(dplyr)
  list.files(path=u,pattern="csv",full.names=TRUE) %>%
    lapply(.,time_constant::pre_process) %>%
    bind_rows(.) %>%
    group_by(.,Chan,fl) %>%
    summarize(TC=timeconstant::time_constant(data.frame(Time=Time,counts=counts)))
}
