# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the best (i.e. 
# lowest) 30-day mortality for the specified outcome in that state
# The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
# Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.

best <- function (state, outcome) {
  outc_lib <- c("heart attack", "heart failure",  "pneumonia")
  outc_num <- c(11, 17,  23)
  num_test <- outc_num[nn<-which(outc_lib == outcome)]
  # lecture dataframe + cleaning + tri sur etat
  out_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out_df <- replace(out_df, out_df=="Not Available", NA)
  out_df <- out_df[order(out_df[,7],na.last=NA),]
  # test arguments
  num_stat <- which(unique(out_df[,7]) == state)
  if(length(num_stat) == 0 || num_stat < 1) {
    stop(" invalid state")
    return(NULL)
  }
  num_cas <- which(outc_lib == outcome)
  if(length(num_cas) == 0 || num_cas < 1) {
    stop(" invalid outcome")
    return(NULL)
  }
  #  data set pour state donné
  outdf_st <- subset(out_df, out_df[,7] == state)
  outdf_stt <- outdf_st[order(outdf_st[,num_test], outdf_st[,2],na.last=NA),]
  min_rate <- min(outdf_stt[, num_test], na.rm = TRUE)
#  identification du numero ligne ayant minimum pour outcome donné
  row_min <- subset(outdf_stt, outdf_stt[,num_test] == min_rate)
  row_min[1,]$Hospital.Name
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")



