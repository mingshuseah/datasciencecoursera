rankall <- function(outcome, num="best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <- levels(factor(data$State))
  hospital <- vector(mode="character") 
  
  for (i in seq(state)) {
    hospital[i] <- rankhospital(state[i], outcome, num)
  }
  data.frame(hospital, state)
  
}