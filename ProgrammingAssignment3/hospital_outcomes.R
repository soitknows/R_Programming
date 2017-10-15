input_file <- "outcome-of-care-measures.csv"

Best <- function (state = "CA", outcome = "pneumonia") {
  # Finds the hospital with best (lowest) 30-day mortality rate.
  # 
  # Args:
  #   state: A two character alphabetic code for the state where hospitals 
  #       reside. Deafult is "CA".
  #   outcome: One of three possible clinical conditions (heart attack, 
  #       heart failure, pneumonia). Deafult is "pneumonia".
  #
  # Returns:
  #   The hospital with the best (e.g. lowest) 30-day mortality rate for the 
  #   state and clinical conditon specified. If an invalid state or condition 
  #   is spectifed an error will be thrown. 
  #
  # Example:
  #   > best("MD", "heart attack")
  #   [1] "JOHNS HOPKINS HOSPITAL, THE"
  
  outcomes <- read.csv(input_file, na.strings="Not Available")
  outcomes <- outcomes[,c(2, 7, 11, 17, 23)]
  cols <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
  colnames(outcomes) <- cols
  states <- unique(outcomes$State)
  
  if ( !outcome %in% cols) stop("invalid outcome")
  if ( !state %in% states) stop("invalid state")
  
  hospitals <- outcomes[outcomes$State == state & !is.na(outcomes[outcome]),
                        c(outcome,"Hospital.Name")]
  
  hospitals[outcome] <- sapply(hospitals[outcome], as.numeric)
  best.hosps <- sort(as.vector(hospitals[hospitals[outcome] == min(hospitals[outcome]), 2]))
  best.hosps[1]
}




