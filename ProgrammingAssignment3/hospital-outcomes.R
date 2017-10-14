best <- function (state = "CA", outcome = "pneumonia") {
  
  outcomes <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  outcomes <- outcomes[,c(2,7,11,17,23)]
  cols <- c("Hospital.Name", "State","heart attack","heart failure","pneumonia")
  colnames(outcomes) <- cols
  states <- unique(outcomes$State)
  
  if ( !outcome %in% cols) stop("invalid outcome")
  if ( !state %in% states) stop("invalid state")
  
  hospitals <- outcomes[outcomes$State == state 
                        & !is.na(outcomes[outcome]),
                        c(outcome,"Hospital.Name")]
  
  hospitals[outcome] <- sapply(hospitals[outcome], as.numeric)
  best_hosps <- sort(as.vector(hospitals[hospitals[outcome] == min(hospitals[outcome]),2]))
  best_hosps[1]
  
}


