# R Programming - Johns Hopkins University
# Programming Assignment 3 - Hospital Quality
# Author: Andrew Tarver


# Load up source data and prepare for function use.
input_file <- "data/outcome-of-care-measures.csv"
data <- read.csv(input_file, na.strings="Not Available")
data <- data[,c(2, 7, 11, 17, 23)]  # these indexes are for columns below.
# Rename columns for easier sub-setting and error checking.
cols <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
colnames(data) <- cols
# State list used for error checking
states <- unique(data$State)


GetHosps <- function (state = "AL", outcome = "heart attack") {
  # Gets a subset of data for use in Best and RankHospital functions.
  #
  # Args:
  #   state: A two character alphabetic code for the state where hospitals 
  #       reside. Deafult is "AL".
  #   outcome: One of three possible clinical conditions (heart attack, 
  #       heart failure, pneumonia). Default is "heart attack".
  #
  # Returns:
  #   Data frame of 30-day mortality rates for hospitals in the state specified.
  
  if ( !state %in% states) stop("invalid state")
  if ( !outcome %in% cols) stop("invalid outcome")
  hospitals <- data[data$State == state & !is.na(data[outcome]),
       c(outcome,"Hospital.Name")]
  hospitals[outcome] <- sapply(hospitals[outcome], as.numeric)
  hospitals
}

Best <- function (state = "AL", outcome = "heart attack") {
  # Finds the hospital with best (lowest) 30-day mortality rate.
  # 
  # Args:
  #   state: A two character alphabetic code for the state where hospitals 
  #       reside. Deafult is "AL".
  #   outcome: One of three possible clinical conditions (heart attack, 
  #       heart failure, pneumonia). Default is "heart attack".
  #
  # Returns:
  #   Character vector for name of hospital with the best (e.g. lowest) 
  #   30-day mortality rate for the state and clinical conditon specified. 
  
  hospitals <- GetHosps(state, outcome)
  best.hosps <- sort(as.vector(hospitals[hospitals[outcome] == min(hospitals[outcome]), 2]))
  best.hosps[1]
}

RankHospital <- function (state = "AL", outcome = "heart attack", num = "best") {
  # Finds the hospital for the rank and outcome specifed
  #
  # Args:
  #   state: A two character alphabetic code for the state where hospitals 
  #       reside. Deafult is "AL".
  #   outcome: One of three possible clinical conditions (heart attack, 
  #       heart failure, pneumonia). Default is "heart attack".
  #   num: hospital rank that is either a "best", "worst", or a numeric.
  #
  # Returns:
  #   Character vector that is the name of the hospital for the state, outcome,
  #   and rank specified.
  
  hospitals <- GetHosps(state, outcome)
  hospitals <- hospitals[order(hospitals[outcome],hospitals$Hospital.Name),]
  num_results <- dim(hospitals)[1]
  if (num == "best") { num <- 1 }
  if (num == "worst") { num <- num_results}
  ifelse (num > num_results, NA, as.vector(hospitals[num,"Hospital.Name"])) 
}

RankAll <- function (outcome = "heart attack", num = "best") {
  # Finds the hospitals for a particular rank and health condition in all states
  #
  # Args:
  #   outcome: One of three possible clinical conditions (heart attack, 
  #       heart failure, pneumonia). Default is "heart attack".
  #   num: hospital rank that is either a "best", "worst", or a numeric.
  #
  # Returns:
  #   A data frame containing the ranking hospital in each sate for the rank 
  #   and clinical condition specified.
  
  ranks <- data.frame(hospital=rep(NA,54),state=states,row.names = states)
  for (state in states) {
    ranks[state,"hospital"] <-RankHospital(state=state,outcome=outcome,num=num)
  }
  ranks
}
