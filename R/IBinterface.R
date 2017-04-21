library(IBrokers)
library(quantmod)
library(sqldf)
#install.packages("rPython")
#python.exec( "import math" )
#python.get( "math.pi" )
workingDirectory <- "~/Programming/tradeMaster";
setwd(workingDirectory)

tws <- twsConnect()
#US0378331005

contract <- twsEquity('AAPL','SMART','ISLAND')
AAPL <- reqHistoricalData(tws,contract,barSize="1 hour",duration="1 Y") # request historical data, ,file=paste0(workingDirectory,"/data/AAPL.csv")
contract <- twsEquity('MMM','SMART','ISLAND')
MMM <- reqHistoricalData(tws,contract,barSize="1 hour",duration="1 Y") # request historical data, ,file=paste0(workingDirectory,"/data/AAPL.csv")

left_join
dat <-


 read.csv(paste0(workingDirectory,"/data/AAPL.csv"),header=FALSE)
names() <- c("","Open", "High", "Low",  "Close", "Volume", "WAP", "hasGaps", "Count")

myWrapper <- eWrapper()
myWrapper$tickPrice <- function(msg, timestamp, file, ...) { cat("tickPrice",msg) }
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")
reqMktData(tws, twsSTK("AAPL"), eventWrapper=myWrapper)
twsDisconnect(tws)
