rankall <- function(outcome, num = "best") {
  if(!is.numeric(num) & num != "best" & num != "worst"){
    stop("invalid num")
  }
  
  #--------------------------------------------------------------------------------
  # Read outcome data and lower the column names to match without cases.
  #--------------------------------------------------------------------------------
  all <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "NA", ""), colClasses = "character")
  #csv <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "NA", ""))
  names(all) <- tolower(names(all))
  
  #--------------------------------------------------------------------------------
  # Sort hospitals in that state based on the rate
  #--------------------------------------------------------------------------------
  column=tolower(gsub("[ ()-]", ".", paste("Hospital 30-Day Death (Mortality) Rates from", outcome, sep=".")))
  if(!is.element(column, names(all))){
    stop(paste("invalid outcome"))
  }

  #--------------------------------------------------------------------------------
  # Loop states 
  #--------------------------------------------------------------------------------
  result <- data.frame("hospital"=character(0), "state"=character(0), stringsAsFactors = FALSE)
  for( state in sort(unique(all$state)) ){
    csv <- NULL
    # Filter out (hospital, outcome) for the target state only.
    # Change the type of the column into number from character.
    csv <- all[all$state==state & !is.na(all[[column]]), c("hospital.name", column)]
    # Change the type of the column into number from character.
    csv[,column] <- sapply(csv[,column], as.numeric)
    # Sort the data on (column, hospital.name)
    csv <- csv[order(csv[,2], csv[,1]), ]

    if(num == "best"){
      result[state, ] <- c(csv[1, "hospital.name"], state)
    } else if(num == "worst"){
      result[state, ] <- c(csv[nrow(csv), "hospital.name"], state)
    } else {
      if(num > nrow(csv)){
        result[state, ] <- c("<NA>", state)
      } else{
        result[state, ] <- c(csv[num, "hospital.name"], state)
      }
    }
  }
  return(result)
}