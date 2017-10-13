best <- function (state = "CA", type = "pneumonia") {
  
  outcomes <- read.csv("outcome-of-care-measures.csv")
  outcomes <- outcomes[,c(2,7,11,17,23)]
  colnames(outcomes) <- c("Hospital.Name",
                         "State",
                         "heart attack",
                         "heart failure",
                         "pneumonia")
  hospitals <- outcomes[outcomes$State == state 
                        & outcomes[type] != "Not Available",
                        c(type,"Hospital.Name")]
  
  hospitals[type] <- lapply(hospitals[type], as.numeric)
  
  best_hosps <- sort(as.vector(hospitals[hospitals[type] == min(hospitals[type]),2]))
  best_hosps[1]
  
}
