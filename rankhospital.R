rankhospital <- function(state, outcome, num="best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                   na.strings="Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  if (!is.element(state, data$State)){
    stop("invalid state")
  }
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  state_data <- subset(data,State==state)
  if (outcome == "heart attack"){
    OC <- 11
  }
  else if (outcome == "heart failure"){
    OC <- 17
  }
  else{
    OC <- 23
  }
  outcome_data <- as.numeric(state_data[,OC])
  required_data <- state_data[!is.na(outcome_data),]
  
  required_data[,OC] <- as.numeric(required_data[,OC])
  sorted_fr_best <- required_data[order(required_data[,OC],required_data[,2]),]
  
  if (num=="worst"){
    return(sorted_fr_best[nrow(sorted_fr_best),2])
  }
  else if (num=="best"){
    return(sorted_fr_best[1,2])
  }
  else{
    return(sorted_fr_best[num,2])
  }
  
}