pollutantmean <- function(directory, pollutant, id = 1:332){
    result <- c()
    monitors <- list.files(directory)
    # Make sure id vector does not exceed the file count in directory
    if (length(id) > length(monitors)){
        print("Id paramter exceeds number of files in directory.")
    } else {
        # Function to combine vectors for later use in "for" loop.
        append_vects <- function(v1,v2){
            v1_end <-length(v1) + 1
            i <- 1
            while (i <= length(v2)){
                v1[v1_end] <- v2[i]
                i = i + 1
                v1_end = v1_end + 1
            }
            v1
        }
        
        for (monitor in monitors[id]){
            # Build path to data file and load it
            path <- paste(directory,monitor, sep="/")
            data <- read.csv(path)
            if (pollutant == "nitrate") {
                # Remove all NA nitrate readings and append to result
                readings <- data$nitrate[!is.na(data$nitrate)]
                result <- append_vects(result,readings)
            } else if (pollutant == "sulfate") {
                # Remove all NA sulfate readings and append to result
                readings <- data$sulfate[!is.na(data$sulfate)]
                result <- append_vects(result,readings)
            } else {
                print("Pollutant must be nitrate or sulfate.")
            }
        }
        mean(result)
    }
}


complete <- function(directory, id = 1:332){
    result <- data.frame(id=numeric(0),nobs=numeric(0))
    monitors <- list.files(directory)
    # Make sure id vector does not exceed the file count in directory
    if (length(id) > length(monitors)){
        print("Id paramter exceeds number of files in directory.")
    } else {
        i <- 1
        for (monitor in monitors[id]){
            # Build path to data file and load it
            path <- paste(directory,monitor, sep="/")
            data <- read.csv(path)
            # Count complete osbservations in each data set
            nobs <- length(data$Date[which(!is.na(data$sulfate) & !is.na(data$nitrate))])
            # Assign each id and observation count to a row in result
            result[i,] <- c(id[i],nobs)
            i = i + 1
        }
    }
    result
}

corr <- function (directory, threshold = 0){
    result <- c()
    i <- 1
    # Load df of complete observations for each dataset
    cobs <- complete("specdata")
    # Filter complete observations df to only include data sets 
    # with complete observation counts above the threshold 
    tobs <- cobs[cobs$nobs > threshold,]
    ids <- tobs$id
    monitors <- list.files(directory)
    for (monitor in monitors[ids]){
        # Build path to data file and load it
        path <- paste(directory,monitor, sep="/")
        data <- read.csv(path)
        # Select only complete observations
        x <- data[(!is.na(data$sulfate) & !is.na(data$nitrate)),]
        # Calculate correlation and append to result
        result[i] <- cor(x$sulfate,x$nitrate)
        i = i +1
    }
    result
}