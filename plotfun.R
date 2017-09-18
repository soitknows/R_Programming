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


res <- sqlQuery(con, "SELECT wf_creator,wf_start
                FROM crosby_finance.dbo.wf_instance
                WHERE wf_model_id = 'ANDY_AP'")
res$Date <- as.Date(substr(res$wf_start,1,10))
ggplot(res,aes(Date)) + geom_density()

ggplot(res,aes(Date)) + 
    geom_freqpoly(color="blue") 

close(myconn)


library(plotly)
p <- ggplot(data, aes(Month, fill=Reg.Product)) +
    stat_count() 
ggplotly(p)
