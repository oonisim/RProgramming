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
  print(pattern)
  return(pattern)
}

loadData <- function(path, pattern) { 
  files <- dir(path, pattern=pattern, full.names = TRUE)
  #print(files)
  tables <- lapply(files, function(file){read.csv(file, header=TRUE, sep=",")})
  return(do.call(rbind, tables))
}

casestat <- function(df, ids){
  i = 0;
  nobs <- matrix(NA, length(ids), 2)
  colnames(nobs) <- c("id", "nobs")
  for (id in ids){
    cases <- df[df$ID %in% id & !is.na(df[colnames(df) == "sulfate"]) & !is.na(df[colnames(df) == "nitrate"]), ]
    nobs[(i <- i + 1),] <- c(id, nrow(cases))
  }  
  return(nobs)
}
  
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  pattern="\\.csv"
  df <- loadData(directory, pattern)
  return(casestat(df, id))
}
