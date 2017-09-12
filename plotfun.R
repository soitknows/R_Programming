# Plotting fun!
data <- read.csv("specdata/003.csv")
data <- data[(!is.na(data$sulfate) & !is.na(data$nitrate)),]
data$Date2 <- as.Date(data$Date)
data$Year <- substr(data$Date,1,4)



ggplot(data, aes(x=Date2,y=sulfate,group=Year, colour=sulfate)) +
    geom_point(size=2) +
    scale_x_date() +
    scale_colour_gradientn(colours=rainbow(4)) 


aggregate(data[,2], list(data$Year),mean)

