createFilePattern <- function(ids){
  pattern <- NULL
  for (id in ids){
    if (!is.null(pattern)){
      pattern <- paste(pattern, sprintf("|%03s\\.csv", id))
    } else {
      pattern <- sprintf("%03s\\.csv", id)
    }
  }
  pattern <- paste("(", pattern, ")")
  return(pattern)
}

loadData <- function(path, pattern) { 
  files <- dir(path, pattern=pattern, full.names = TRUE)
  #print(files)
  tables <- lapply(files, function(file){read.csv(file, header=TRUE, sep=",")})
  return(do.call(rbind, tables))
}

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
  ## NOTE: Do not round the result!
  
  #pattern <- createFilePattern(id)
  pattern="\\.csv"
  df <- loadData(directory, pattern)
  #print(names(df))
  #df <- df[df$ID %in% id & is.na(df[[pollutant]])==FALSE, ]
  df <- mean(df[df$ID %in% id, pollutant], na.rm=TRUE)
  return(df)
}
