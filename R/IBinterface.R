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
#workingDirectory <- "~/Programming/tradeMaster";
#setwd(workingDirectory)

con         <- dbConnect(RSQLite::SQLite(),"./data/database_file.sqlite")
indices     <- dbGetQuery(con, "SELECT * FROM Indices")
dbDisconnect(con)

tblES <- as.tbl(cbind(data.frame(id=index(ES)),data.frame(ES[,1:ncol(ES)])))

my_db <- src_mysql(host = "patstat.cq2v32gzlgqv.us-west-2.rds.amazonaws.com", user = "mark",
                   password = "june11octo17",dbname="AWS")
copy_to(my_db,tblES,"IB_FUTURES",temporary=FALSE)
tbl(my_db,"IB_FUTURES")

db_insert_to(con = my_db$con, table = "IB_FUTURES", )
write_to(name="IB_FUTURES",schema="AWS")





field <- "TRADES" # "OPTION_IMPLIED_VOLATILITY"
tws <- twsConnect()

twsFuture()
future <- twsFuture("ES","GLOBEX","201706")
#assign("ES",reqHistoricalData(tws,future,barSize="1 min",duration="5 D",whatToShow=field))


myWrapper <- eWrapper()
myWrapper$tickPrice <- function(msg, timestamp, file, ...) {}

# add new tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) {
  print(paste("Call back called."))
  #print(paste(msg))
  print(paste(cat("tickPrice",msg))
  #cancelMktData(tws,0)
}
reqMktData(tws,future,tickGenerics="106",eventWrapper = myWrapper)



#Get historical data
for (i in 1:nrow(indices)){
  #Get ticker
  ticker    <- indices[i,"ticker"]
  #Create contract
  contract  <- twsEquity(paste(ticker),'SMART')
  if(ticker %in% c("CSCO","MSFT","AAPL","INTC")) contract <-  twsEquity(paste(ticker),'SMART','ISLAND')
  #Assign it to the environment
  assign(ticker,reqHistoricalData(tws,contract,barSize="1 sec",duration="5 min",whatToShow=field))
  #build the data frame

  Sys.sleep(10)
  print(paste(ticker))
}

ticks <-indices$ticker[order(indices$ticker)]#
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
names(tsDat) <- indices$ticker
cor(tsDat)

weights <- tsDat[nrow(tsDat),-which(names(tsDatPX) %in% c("DIA","SPY","QQQ"))]
weights <- weights/sum(weights)


indexWeights <- cov(tsDatPX[,-which(names(tsDatPX) %in% c("DIA","SPY","QQQ"))])
rM <- tsDatPX[,-which(names(tsDatPX) %in% c("","SPY","QQQ"))]
get_index_weights("DIA",returnMatrix = rM)
compute_index_volatility <- compute_index_volatility(weights,SIGMA)




constituentIV <- tsDatVol[,-which(names(tsDatVol) %in% c("DIA","SPY","QQQ"))]


timeScale <- 252*6.5*60
computeIndexIVfromConstituentIV(weights,optionCovariance,constituentIV,timeScale)


weights <- sum()

tsDat[1:2,-c(31:33)]

lm(QQQ ~ -1 + AAPL + MMM + AXP  + BA + CAT + CVX + CSCO + KO + DD +
  XOM + GE + GS + HD + IBM + INTC + JNJ + JPM + MCD + MRK +
  MSFT + NKE + PFE + PG + TRV + UNH + UTX + VZ + V + WMT +
  DIS, tsDatPX[1:100,])




tws <- twsConnect()
contract  <- twsEquity("AAPL",'SMART','ISLAND')
reqMktData(tws, contract,snapshot = FALSE, tickGenerics = "100,101,104,106,165,221,225,236")

# remove tickPrice action
myWrapper <- eWrapper()

myWrapper$tickPrice <- function(msg, timestamp, file, ...) {}

# add new tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) {
  print(paste("Venus is a planet."))
  cancelMktData(tws,0)
}

myWrapper$tickGeneric <- function(msg, timestamp, file, ...) {}
myWrapper$tickGeneric <- function(msg, timestamp, file, ...) { cancelMktData(tws,0) }

# add new data into the object, and retrieve
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")

## Not run:
tws <- twsConnect()
reqMktData(tws, twsSTK("SBUX"), eventWrapper=myWrapper)
twsDisconnect(tws)

reqMktData(tws, snapshot = TRUE, Contract=twsSTK("SBUX"),tickGenerics="106,165,221,225,236")
reqMktData(tws, snapshot = TRUE, Contract=twsSTK("SBUX"))



















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



 read.csv(paste0(workingDirectory,"/data/AAPL.csv"),header=FALSE)
names() <- c("","Open", "High", "Low",  "Close", "Volume", "WAP", "hasGaps", "Count")

myWrapper <- eWrapper()
myWrapper$tickPrice <- function(msg, timestamp, file, ...) { cat("tickPrice",msg) }
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")
reqMktData(tws, twsSTK("AAPL"), eventWrapper=myWrapper)
twsDisconnect(tws)
