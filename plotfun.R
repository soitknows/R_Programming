# Plotting fun!
library(ggplot2)
data <- read.csv("specdata/003.csv")
data <- data[(!is.na(data$sulfate) & !is.na(data$nitrate)),]
data$Date2 <- as.Date(data$Date)
data$Year <- substr(data$Date,1,4)

ggplot(data, aes(x=Date2,y=sulfate,group=Year, colour=sulfate)) +
    geom_point(size=2) +
    scale_x_date() +
    scale_colour_gradientn(colours=rainbow(4)) 

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
yr_mean <- aggregate(data[,2], list(data$Year),mean)
ggplot(yr_mean, aes(x = Group.1, y = x, fill = Group.1)) +
    geom_col() + 
    scale_fill_manual(values=cbbPalette)


data <- read.csv("specdata/001.csv")
data <- data[(!is.na(data$sulfate) & !is.na(data$nitrate)),]
ggplot(data,aes(Date, group=1)) + 
    scale_x_date() + 
    geom_ribbon(aes(ymin=sulfate-1,ymax=sulfate+1), fill="grey70") +
    geom_line(aes(y = sulfate))
    

<<<<<<< HEAD


library(RODBC)
library(ggplot2)
cred <- read.csv("../odbc.csv", stringsAsFactors = FALSE)
con <-odbcConnect("hdsqldb1", uid=cred$user, pwd=cred$pw, rows_at_time = 1 )
res <- sqlQuery(con, "SELECT wf_model_id, 
                COUNT(wf_model_id) as icount 
                FROM crosby_finance.dbo.wf_instance 
                GROUP BY wf_Model_id")
x <- res[res$icount > 500,]
ggplot(x,aes(wf_model_id,icount,fill=as.factor(icount))) +
    geom_bar(stat="identity")


=======
>>>>>>> 1fd5aee6d510374b27f9c6c17bd4cce32b22d492
res <- sqlQuery(con, "SELECT wf_creator,wf_start
                FROM crosby_finance.dbo.wf_instance
                WHERE wf_model_id = 'ANDY_AP'")
res$Date <- as.Date(substr(res$wf_start,1,10))
ggplot(res,aes(Date)) + geom_density()

ggplot(res,aes(Date)) + 
    geom_freqpoly(color="blue") 
close(myconn)



library(plotly)
library(dplyr)
data <- read.csv("case_closures.csv")
data <- read.csv("case_closures_YTD.csv")
data$Resolved.Date <- as.Date(data$Resolved.On,"%m/%d/%Y")
data$Resolved.Month <- factor(months(data$Resolved.Date))
ndata <- group_by(data,Resolved.Month,Reg.Product) %>% summarize( count = n())

ggplot(ndata) +
    geom_bar(aes(x=Resolved.Month,y=count, fill=(Resolved.Month)),stat="identity") +
    facet_wrap(~Reg.Product)

p <-ggplot(data) +
    geom_bar(aes(x=Resolved.Month, fill=(Reg.Product))) 
ggplotly(p)
<<<<<<< HEAD
=======




p <- ggplot(d, aes(x=class,fill=product)) +
    geom_bar() +
    facet_wrap(~facet, ncol=3)
ggplotly(p)


    

>>>>>>> 1fd5aee6d510374b27f9c6c17bd4cce32b22d492
