loadData <- function(path, pattern) { 
  files <- dir(path, pattern=pattern, full.names = TRUE)
  #print(files)
  tables <- lapply(files, function(file){read.csv(file, header=TRUE, sep=",")})
  return(do.call(rbind, tables))
}

nobstat <- function(df, ids) {
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  i = 0;
  nobs <- data.frame("id"=numeric(0), "cnt"=integer(0))
  for (id in ids){
    cases <- df[df[['ID']] == id & !is.na(df[colnames(df) == "sulfate"]) & !is.na(df[colnames(df) == "nitrate"]), ]
    cor(cases[,colnames(df)=="sulfate"], cases[,colnames(df)=="nitrate"])
    nobs[(i <- i + 1),] <- c(id, nrow(cases))
  }  
  print(nobs);
  return(nobs)
}

correlate <- function(df, threshold, ids) {
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  result <- numeric()
  for (id in ids){
    cases <- df[df[['ID']] == id & !is.na(df[colnames(df) == "sulfate"]) & !is.na(df[colnames(df) == "nitrate"]), ]
    if(nrow(cases) > threshold){
      #result <- append(result, cor(cases[,colnames(df)=="sulfate"], cases[,colnames(df)=="nitrate"]))
      result <- append(result, round(cor(cases[,colnames(df)=="sulfate"], cases[,colnames(df)=="nitrate"]), digits=5))
      #print(result);
    }
  }  
  return(result)
}

correlate_bak <- function(df, nobs, threshold){
  ids <- nobs[nobs$cnt > threshold, colnames(nobs)=="id"]
  print(ids)
  
  sulfates <- df[(df[['ID']] %in% ids) & !is.na(df[colnames(df) == "sulfate"]) & !is.na(df[colnames(df) == "nitrate"]), colnames(df)=="sulfate"]
  nitrates <- df[(df[['ID']] %in% ids) & !is.na(df[colnames(df) == "sulfate"]) & !is.na(df[colnames(df) == "nitrate"]), colnames(df)=="nitrate"]
  print(sulfates)
  return(cor(sulfates, nitrates))
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  pattern="\\.csv"
  df <- loadData(directory, pattern)
#  nobs <- nobstat(df, 1:322)
#  return(correlate(df, nobs, threshold))
  return(correlate(df, threshold, 1:332))
}
