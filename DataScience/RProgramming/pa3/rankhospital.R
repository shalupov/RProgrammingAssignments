## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.
rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check outcome
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")     
  }
  
  # Filter by state
  data <- data[data$State==state,]  
  if (nrow(data) == 0) { 
    stop("invalid state") 
  }
  
  ## Return hospital name in that state with lowest 30-day death
  colmap = list(
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",    
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  data[, 1] <- as.numeric(data[[colmap[[outcome]]]])
  sorted <- data[ order(data[, 1], data[,2]), ]
  sorted <- sorted[complete.cases(sorted),]
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(sorted)
  }
  
  sorted[num, 2]
}
