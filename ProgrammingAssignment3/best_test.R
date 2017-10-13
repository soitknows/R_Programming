
best <- function(state = "CA", type = "pneumonia") {
    
    outcomes <- read.csv("outcome-of-care-measures.csv")
    outcomes <- outcomes[,c(2,7,11,17,23)]
    colnames(outcomes) <- c("Hospital.Name",
                           "State",
                           "heart attack",
                           "heart failure", 
                           "pneumonia")
    hospitals <- outcomes[outcomes$State == state &
                          outcomes[type] != "Not Available",
                          c(type, "Hospital.Name")]
    
    hospitals[type] <- lapply(hospitals[type], as.numeric)
    hospitals
    top_hosp <- as.character(sort(hospitals[hospitals[type] == min(hospitals[type])
                          ,"Hospital.Name"]))
    top_hosp[1]
}




