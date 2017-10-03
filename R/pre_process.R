pre_process <-function(u){#where u is the csv file csv file
  require(dplyr)
  rio::import(u) %>%
    dplyr::select(.,-contains('LOW'),Time=contains('Time'),contains('High')) %>%
    tidyr::gather('Chan','counts',-contains("Time"))%>%
    dplyr::mutate(Chan=gsub("High ","",Chan))  %>%
    dplyr::mutate(fl=u)
}
