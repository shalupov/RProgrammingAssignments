## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.
rankall <- function(outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check outcome
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")     
  }
  
  ## Return hospital name in that state with lowest 30-day death
  colmap = list(
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",    
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  data[, 1] <- as.numeric(data[[colmap[[outcome]]]])
  
  by_state <- split(data, data$State)
  sorted <- lapply(by_state, function(d) d[ order(d[, 1], d[,2]), c(1, 2)])
  sorted <- lapply(sorted, function(d) d[complete.cases(d),])
  
  # print(sorted[["AL"]])
  
  result <- lapply(sorted, function(d) {
    if (num == "best") {
      d[1, 2]
    } else if (num == "worst") {
      d[nrow(d), 2]
    } else {
      d[num, 2]
    }
  })
  
  data.frame(hospital=unlist(result, use.names=FALSE), state=names(result))
}
