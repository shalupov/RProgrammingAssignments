corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  all_data <- lapply(
    sort(list.files(directory, full.names=TRUE)), 
    function(fn) read.csv(fn))
  data <- Filter(function(x) sum(complete.cases(x)) > threshold, all_data)
  
  unlist(sapply(data, function(x) cor(x$sulfate, x$nitrate, use="complete.obs")))
  #length(all_data)
  
  
  #data_by_file <- list()
  #for (i in id) {
  #  data_by_file[[i]] = read.csv(sprintf("%s/%03d.csv", directory, i))
  #}
  
}

# print(head(corr("specdata", 150)))