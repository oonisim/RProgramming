best <- function(state, outcome) {
  #--------------------------------------------------------------------------------
  # Read outcome data and lower the column names to match without cases.
  #--------------------------------------------------------------------------------
  csv <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "NA", ""), colClasses = "character")
  #csv <- read.csv("outcome-of-care-measures.csv", na.strings = c("Not Available", "NA", ""))
  names(csv) <- tolower(names(csv))
  
  #--------------------------------------------------------------------------------
  # Check that state and outcome are valid
  #--------------------------------------------------------------------------------
  states <- unique(csv[, "state"])
  if(!is.element(state, states)){
    stop(paste(" invalid state"))
  }
  
  #--------------------------------------------------------------------------------
  # Return hospital name in that state with lowest 30-day death rate
  #--------------------------------------------------------------------------------
  column=tolower(gsub("[ ()-]", ".", paste("Hospital 30-Day Death (Mortality) Rates from", outcome, sep=".")))
  if(!is.element(column, names(csv))){
    stop(paste("invalid outcome"))
  }
  
  # Filter out (hospital, outcome) for the target state only.
  # Change the type of the column into number from character.
  csv <- csv[csv$state==state & !is.na(csv[[column]]) & !is.na(csv$hospital.name), c("hospital.name", column)]
  # Change the type of the column into number from character.
  csv[,column] <- sapply(csv[,column], as.numeric)
  # Get the minimum rate and filter out those hospital names that match with the rate.
  csv <- csv[csv[[column]]==min(csv[, column], na.rm=TRUE), "hospital.name"]
  return(csv)
}