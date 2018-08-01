best <- function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                   na.strings="Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  if (!is.element(state, state.abb)){
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
  
  deathrate <- as.numeric(required_data[,OC])
  best_rows <- which(deathrate==min(deathrate))
  best_hospital <- required_data[best_rows,2]
  
  if (length(best_hospital) > 1) {
    best_hospital <- sort(best_hospital)
  }
  return(best_hospital[1])
}