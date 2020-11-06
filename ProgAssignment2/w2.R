pollutantmean <- function(directory, pollutant, id = 1:332){

  data <- c()
  
  
  for (i in id) {
    
    if (i < 10){
      file_name <- paste("00",toString(i), ".csv", sep="")
    }
    else if (i<100){
      file_name <- paste("0",toString(i), ".csv",sep = "")
    }
    else {file_name <- paste(toString(i), ".csv", sep="")}
    
    d <- read.csv(file.path(directory, file_name))
    n <- d[pollutant][!is.na(d[pollutant])]
    
    data <- append(data, n)
    
  } 
  
  sum(data)/length(data)
  
}
  
complete <- function(directory, id = 1:332){
  
  ID <- c()
  nobs <- c()
  
  for (i in id) {
    
    if (i < 10){
      file_name <- paste("00",toString(i), ".csv", sep="")
    }
    else if (i<100){
      file_name <- paste("0",toString(i), ".csv",sep = "")
    }
    else {file_name <- paste(toString(i), ".csv", sep="")}
    
    d <- read.csv(file.path(directory, file_name))
    
    full_obs <- subset(d, !is.na(d$sulfate)&!is.na(d$nitrate))
    
    nobs <- append(nobs, nrow(full_obs))
    ID <- append(ID, i)
    
  }

  final <- data.frame(id = ID, nobs)
  final
  
}


corr <- function(directory, threshold = 0){
  
  results <- c()
  id = 1:332
  
  for (i in id) {
    
    if (i < 10){
      file_name <- paste("00",toString(i), ".csv", sep="")
    }
    else if (i<100){
      file_name <- paste("0",toString(i), ".csv",sep = "")
    }
    else {file_name <- paste(toString(i), ".csv", sep="")}
    
    d <- read.csv(file.path(directory, file_name))
    full_obs <- subset(d, !is.na(d$sulfate)&!is.na(d$nitrate))
    
    if (nrow(full_obs) > threshold){
      
      result <- cor(full_obs["sulfate"], full_obs["nitrate"])
      results <- append(results, result)
      
    }
  }
  results
  
}
