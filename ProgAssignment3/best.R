best <- function(stateabb, outcome) {
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv")
  
  
  ## Check that state and outcome are valid
  
  if (!stateabb %in% state.abb){
    
    
    stop("invalid state")
    
  }
  else {s <- stateabb}
  
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  else if (outcome == "heart attack"){  
  
  lowest_by_state <- sapply(split(as.numeric(data[, 11]), data$State ), min, na.rm = TRUE)
  
  lowest <- lowest_by_state[s]
  
  state_data <- subset(data, State == s)
  
  hospital <- subset(state_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == lowest)
  
  name <- hospital$Hospital.Name
  
  }
  
  else if (outcome == "heart failure") {  
  
  
  lowest_by_state <- sapply(split(as.numeric(data[, 17]), data$State ), min, na.rm = TRUE)
  
  lowest <- lowest_by_state[s]
  
  state_data <- subset(data, State == s)
  
  hospital <- subset(state_data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == lowest)
  
  name <- hospital$Hospital.Name
  
  
  }
  else if (outcome == "pneumonia") { 
  
  lowest_by_state <- sapply(split(as.numeric(data[, 23]), data$State ), min, na.rm = TRUE)
  
  lowest <- lowest_by_state[s]
  
  state_data <- subset(data, State == s)
  
  hospital <- subset(state_data, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == lowest)
  
  name <- hospital$Hospital.Name
  
  }
  
  
  name
  
}