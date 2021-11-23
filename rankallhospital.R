# rankall that takes two arguments: an outcome name (outcome) and a hospital
# ranking (num). The function reads the outcome-of-care-measures.csv file 
# and returns a 2-column data frame containing the hospital in each state
# that has the ranking specified in num.

rankall <- function (outcome, rank="best") {
  outc_lib <- c("heart attack", "heart failure", "pneumonia")
  outc_num <- c(11, 17, 23)
  dfx_names <- c("hospital", "state")
  num_test <- outc_num[nn<-which(outc_lib == outcome)]
  # lecture dataframe + cleaning + tri sur etat
  out_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out_df <- replace(out_df, out_df=="Not Available", NA)
  out_df <- out_df[order(out_df[,7],na.last=NA),]
  # test arguments
  num_cas <- which(outc_lib == outcome)
  if(length(num_cas) == 0 || num_cas < 1) {
    stop("Error in best ", outcome, " : invalid outcome")
    return(NULL)
  }
  if(is.numeric(rank) && rank > 30) {
    return(NA)
  }
  else {
    if(rank == "best") rankw <- 1
#    else if(rank == "worst") rankw <- nrow(outdf_stt)
    else if(!is.numeric(rank) && rank != "best" && rank != "worst") return(NA)
    else rankw <- rank
  }
  if(rank == "worst")
    dfx <- data.frame(matrix(ncol = 2, nrow = length(unique(out_df[,7]))*1)-1)
  else
    dfx <- data.frame(matrix(ncol = 2, nrow = length(unique(out_df[,7]))*rankw)-1)
  names(dfx) <- dfx_names
  
  rwst <- 0
  for (st in unique(out_df[,7])) {
    #  data set pour state donné  + tri (outcome, nom hopital) & sup NA & test argument rank
    outdf_st <- subset(out_df, out_df[,7] == st)
    outdf_stt <- outdf_st[order(outdf_st[,num_test], outdf_st[,2],na.last=NA),]
    if(rank == "worst") {
      rankw <- nrow(outdf_stt)
      dfx[rwst,1] <- outdf_stt[rankw,]$Hospital.Name
      dfx[rwst,2] <- st
      rwst <- rwst + 1
    }
    else {
      for (rw in 1:rankw) { 
        if(!is.null(outdf_stt[rw,]$Hospital.Name)) {
          dfx[rwst+rw,1] <- outdf_stt[rw,]$Hospital.Name
        }
        else {
          dfx[rwst+rw,1] <- "NA"
        }
        dfx[rwst+rw,2] <- st
      }
      rwst <- rwst + rw
    }
  }
  
  #  return dfx
  return(dfx)
}




rkal <- rankall("heart attack",2)
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
