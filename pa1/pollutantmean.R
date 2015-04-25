library(data.table)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  data_by_file <- list()
  for (i in id) {
    data_by_file[[i]] = read.csv(sprintf("%s/%03d.csv", directory, i))
  }
  
  data <- rbindlist(data_by_file)

  #data <- raw_data[!is.na(raw_data[[pollutant]])]
  
  #print(data_filtered_id[1:20,])
  #print(nrow(data))
  #print(nrow(data_filtered_id))
  
  mean(data[[pollutant]], na.rm=TRUE)
}

# print(pollutantmean("specdata", "nitrate", 23))
# print(pollutantmean("specdata", "sulfate", 1:10))

