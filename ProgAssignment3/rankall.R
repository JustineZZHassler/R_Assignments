



rankall <- function(outcome, num = "best"){
  
  source("rankhospital.R")
  
  state <- c()
  hospital <- c()
  
  for (s in state.abb) {
    
    
    name <-rankhospital(s, outcome, num) 
    
    state <- append(state, s)
    hospital <- append(hospital, name)
  }
  
  df <- data.frame(hospital, state)
  df.order <- order(df$state)
  
  result <- df[df.order,]
  
  result
  
}