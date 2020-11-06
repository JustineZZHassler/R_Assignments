

rankhospital <- function(state, outcome, num = "best"){
  
  ## read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  
  if (!state %in% state.abb){
    
    
    stop("invalid state")
    
  }
  
  else {s <- state}
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    
    stop("invalid outcome")
  }
  
  else if (outcome == "heart attack") { i <- 4 }
  else if (outcome == "heart failure") { i <- 5 }
  else if (outcome == "pneumonia") { i <- 6 }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  new_data <- subset(data, select = c(1,2,7,11,17,23))
  
  colnames(new_data) <- c("hospital_no", "hospital_name", "state", "heart_attack", "heart_failure", "pneumonia")
  
  state_data <- subset(new_data, state == s)
  
  state_data$heart_attack <- as.numeric(state_data$heart_attack)
  state_data$heart_failure <- as.numeric(state_data$heart_failure)
  state_data$pneumonia <- as.numeric(state_data$pneumonia)
  
  
  
  if (i == 4){
    
    
    state_rank <- order(state_data$heart_attack, state_data$hospital_name)
    state_data$rank <- NA
    state_data$rank[state_rank] <- 1 : nrow(state_data)
    
    
  }
  
  else if (i == 5){
    
    state_rank <- order(state_data$heart_failure, state_data$hospital_name)
    state_data$rank <- NA
    state_data$rank[state_rank] <- 1 : nrow(state_data)
    
    
  }
  
  else if (i == 6){
    
    state_rank <- order(state_data$pneumonia, state_data$hospital_name)
    state_data$rank <- NA
    state_data$rank[state_rank] <- 1 : nrow(state_data)
    
    
  }
  

  if (num == "best"){ final <- subset(state_data, rank == 1, select = hospital_name)
  
    name <- final$hospital_name}
  
  else if (num == "worst"){ 
    
    num <- nrow(state_data[complete.cases(state_data[,i]),]) 
    
    final <- subset(state_data, rank == num, select = hospital_name)
    name <- final$hospital_name
  
  }
  
  else if (num > nrow(state_data[complete.cases(state_data[,i]),])) { name <- NA}
  
  
  else {final <- subset(state_data, rank == num, select = hospital_name)
  name <- final$hospital_name}
  
  
  name
  
}