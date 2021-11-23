# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking 
# specified by the num argument.

rankhospital <- function (state, outcome, rank) {
  outc_lib <- c("heart attack", "heart failure", "pneumonia")
  outc_num <- c(11, 17, 23)
  num_test <- outc_num[nn<-which(outc_lib == outcome)]
  # lecture dataframe + cleaning + tri sur etat
  out_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out_df <- replace(out_df, out_df=="Not Available", NA)
  out_df <- out_df[order(out_df[,7],na.last=NA),]
  # test arguments
  num_stat <- which(unique(out_df[,7]) == state)
  if(length(num_stat) == 0 || num_stat < 1) {
    stop("Error in best ", state, outcome, " : invalid state")
    return(NULL)
  }
  num_cas <- which(outc_lib == outcome)
  if(length(num_cas) == 0 || num_cas < 1) {
    stop("Error in best ", state, outcome, " : invalid outcome")
    return(NULL)
  }

  #  data set pour state donné  + tri (outcome, nom hopital) & sup NA & test argument rank
  outdf_st <- subset(out_df, out_df[,7] == state)
  outdf_stt <- outdf_st[order(outdf_st[,num_test], outdf_st[,2],na.last=NA),]
  if(is.numeric(rank) && rank > nrow(outdf_stt)) {
    return(NA)
  }
  else {
    if(rank == "best") rank <- 1
    if(rank == "worst") rank <- nrow(outdf_stt)
    if(!is.numeric(rank)) return(NA)
    }
  
  #  return row pour le rang demandé
  outdf_stt[rank,]$Hospital.Name
}


rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("TX", "heart attack", 2)
rankhospital("TX", "heart attack", 6)
rankhospital("TX", "heart attack", 9)
rankhospital("MN", "heart attack", 42)
rankhospital("MN", "heart attack", "best")
rankhospital("MN", "heart attack", 1)
rankhospital("TX", "heart attack", 200)
rankhospital("TX", "heart attack", "worst")


