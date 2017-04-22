library(tidyr)
library(dplyr)
library(readr)
library(IBrokers)
library(quantmod)
library(RSQLite)
#install.packages("rPython")
#python.exec( "import math" )
#python.get( "math.pi" )
#indices     <- read_csv("./data/index.csv")
#dbWriteTable(con, "Indices", indices)
#rm(indices)
#dbListTables(con)
workingDirectory <- "~/Programming/tradeMaster";
setwd(workingDirectory)

con         <- dbConnect(RSQLite::SQLite(),"./data/database_file.sqlite")
indices     <- dbGetQuery(con, "SELECT * FROM Indices")
dbDisconnect(con)



#US0378331005

indices <- rbind(indices,c("DIA","DOW"))

tws <- twsConnect()
#Get historical data
for (i in 1:nrow(indices)){
  #Get ticker
  ticker    <- indices[i,"ticker"]
  #Create contract
  contract  <- twsEquity(paste(ticker),'SMART')
  #Assign it to the environment
  assign(ticker,reqHistoricalData(tws,contract,barSize="1 hour",duration="1 Y"))
  #build the data frame

  Sys.sleep(10)
  print(paste(ticker))
}

ticks <-indices$ticker[order(indices$ticker)]# c("AAPL","BA","CAT","CVX","GS","MMM","DIS","DIA")
  for (i in 1:length(ticks)){
    #Get ticker
    ticker    <- ticks[i]
    d <- get(ticker)
    if(i == 1) dat <<- d
    dat <<- merge(dat,d)
}

tsDat <- dat[,which(grepl(".Close$",names(dat)) == 1 )]
tsDat <- diff(log(tsDat))
tsDat <- tsDat[complete.cases(tsDat),]
cor(tsDat)

weightModel <- lm(DIA.Close~-1+.,data=tsDat)
prices <- apply(dat[,which(grepl(".Close$",names(dat)) == 1 )],2,mean,na.rm=TRUE)
prices <- prices[-8]
weights <-  prices / sum(prices)
weights <- weights[order(weights)]
imputedWeights <- coef(weightModel)[order(coef(weightModel))]

comparison <- data.frame(trueWeights=weights,imputedWeights=imputedWeights)
comparison$delta <- comparison[,2] - comparison[,1]
comparison <- round(comparison,4)
weightSums <- apply(comparison,2,sum)
names(weightSums) <- weightSums;
comparison <- rbind(comparison,weightSums)


fn <- function(t,Y,X){
  grf <- 0*matrix(1,nrow=nrow(as.matrix(X)),ncol=1)
  g <- matrix(1,nrow=nrow(X),ncol=1)
  amat <- matrix(Y*g-grf,ncol=1)
  return(amat)
}


constrained <- glmc(DIA.WAP~-1+.,Amat=fn(nrow(tsDat),Y=tsDat[,"DIA.WAP"],X=tsDat[,-31]),data=tsDat)


 read.csv(paste0(workingDirectory,"/data/AAPL.csv"),header=FALSE)
names() <- c("","Open", "High", "Low",  "Close", "Volume", "WAP", "hasGaps", "Count")

myWrapper <- eWrapper()
myWrapper$tickPrice <- function(msg, timestamp, file, ...) { cat("tickPrice",msg) }
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")
reqMktData(tws, twsSTK("AAPL"), eventWrapper=myWrapper)
twsDisconnect(tws)
