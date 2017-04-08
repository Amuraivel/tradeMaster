SLI <- read.csv(file="https://www.six-swiss-exchange.com/downloads/indexdata/hsli.csv",sep=";",row.names=NULL,skip = 5,as.is=TRUE)
values <- c("Date","Close")
SMI <- read.csv(file="https://www.six-swiss-exchange.com/downloads/indexdata/hsmi.csv",sep=";",skip = 6)
SMI$Date <- as.character(rownames(SMI))
SMI$Date <- as.numeric(as.Date(SMI$Date,"%d.%m.%Y"))
SMI <- SMI[,c("Date","Close")]

library(Quandl)
#---STOCKS---
Quandl.api_key("tyNA1apCEZmn5L6_F2bh")
ABBN	<-  Quandl("GOOG/VTX_ABBN");
ABBN$Date <- as.numeric(as.Date(ABBN$Date,"%d-%m-%y"));
ADEN 	<-  Quandl("GOOG/VTX_ADEN");
ADEN$Date <- as.numeric(as.Date(ADEN$Date,"%d-%m-%y"));
ATLN 	<-  Quandl("GOOG/VTX_ATLN");
ATLN$Date <- as.numeric(as.Date(ATLN$Date,"%d-%m-%y"));
#ARYN 	<-  Quandl("GOOG/SWX_ARYN");
#ARYN$Date <- as.numeric(as.Date(ARYN$Date,"%d-%m-%y"));
BAER	<-  Quandl("GOOG/VTX_BAER");
BAER$Date <- as.numeric(as.Date(BAER$Date,"%d-%m-%y"));
BALN	<-  Quandl("GOOG/VTX_BALN");
BALN$Date <- as.numeric(as.Date(BALN$Date,"%d-%m-%y"));
CFR 	<-  Quandl("GOOG/VTX_CFR");
CFR$Date <- as.numeric(as.Date(CFR$Date,"%d-%m-%y"));
CLN 	<-  Quandl("GOOG/VTX_CLN");
CLN$Date <- as.numeric(as.Date(CLN$Date,"%d-%m-%y"));
CSGN	<-  Quandl("GOOG/VTX_CSGN");
CSGN$Date <- as.numeric(as.Date(CSGN$Date,"%d-%m-%y"));
DUFN	<-  Quandl("GOOG/VTX_DUFN");
DUFN$Date <- as.numeric(as.Date(DUFN$Date,"%d-%m-%y"));
#GALN 	<-  Quandl("GOOG/SWX_GALN");
#GALN$Date <- as.numeric(as.Date(GALN$Date,"%d-%m-%y"));
GEBN 	<-  Quandl("GOOG/VTX_GEBN");
GEBN$Date <- as.numeric(as.Date(GEBN$Date,"%d-%m-%y"));
GIVN 	<-  Quandl("GOOG/VTX_GIVN");
GIVN$Date <- as.numeric(as.Date(GIVN$Date,"%d-%m-%y"));
HOLN 	<-  Quandl("GOOG/VTX_HOLN");
HOLN$Date <- as.numeric(as.Date(HOLN$Date,"%d-%m-%y"));
KNIN 	<-  Quandl("GOOG/VTX_KNIN");
KNIN$Date <- as.numeric(as.Date(KNIN$Date,"%d-%m-%y"));
LONN 	<-  Quandl("GOOG/VTX_LONN");
LONN$Date <- as.numeric(as.Date(LONN$Date,"%d-%m-%y"));
NESN 	<-  Quandl("GOOG/VTX_NESN");
NESN$Date <- as.numeric(as.Date(NESN$Date,"%d-%m-%y"));
NOVN 	<-  Quandl("GOOG/VTX_NOVN");
NOVN$Date <- as.numeric(as.Date(NOVN$Date,"%d-%m-%y"));
ROG 	<-  Quandl("GOOG/VTX_ROG");
ROG$Date <- as.numeric(as.Date(ROG$Date,"%d-%m-%y"));
SCHP	<-  Quandl("GOOG/VTX_SCHP");
SCHP$Date <- as.numeric(as.Date(SCHP$Date,"%d-%m-%y"));
SCMN 	<-  Quandl("GOOG/VTX_SCMN");
SCMN$Date <- as.numeric(as.Date(SCMN$Date,"%d-%m-%y"));
SGSN	<-  Quandl("GOOG/VTX_SGSN");
SGSN$Date <- as.numeric(as.Date(SGSN$Date,"%d-%m-%y"));
SIK	<-  Quandl("GOOG/VTX_SIK");
SIK$Date <- as.numeric(as.Date(SIK$Date,"%d-%m-%y"));
SLHN	<-  Quandl("GOOG/VTX_SLHN");
SLHN$Date <- as.numeric(as.Date(SLHN$Date,"%d-%m-%y"));
SOON	<-  Quandl("GOOG/VTX_SOON");
SOON$Date <- as.numeric(as.Date(SOON$Date,"%d-%m-%y"));
SREN 	<-  Quandl("GOOG/VTX_SREN");
SREN$Date <- as.numeric(as.Date(SREN$Date,"%d-%m-%y"));
SYNN 	<-  Quandl("GOOG/VTX_SYNN");
SYNN$Date <- as.numeric(as.Date(SYNN$Date,"%d-%m-%y"));
UBSN 	<-  Quandl("YAHOO/UBS");
UBSN$Date <- as.numeric(as.Date(UBSN$Date,"%d-%m-%y"));
UHRN 	<-  Quandl("GOOG/SWX_UHRN");
UHRN$Date <- as.numeric(as.Date(UHRN$Date,"%d-%m-%y"));
ZURN 	<-  Quandl("GOOG/VTX_ZURN");
ZURN$Date <- as.numeric(as.Date(ZURN$Date,"%d-%m-%y"));


dataSet <- merge(SMI,ABBN[,values],by="Date");
dataSet <- merge(dataSet,ADEN[,values],by="Date");
dataSet <- merge(dataSet,ATLN[,values],by="Date");
#dataSet <- merge(dataSet,ARYN[,values],by="Date");
dataSet <- merge(dataSet,BAER[,values],by="Date");
dataSet <- merge(dataSet,BALN[,values],by="Date");
dataSet <- merge(dataSet,CFR[,values],by="Date");
dataSet <- merge(dataSet,CLN[,values],by="Date");
dataSet <- merge(dataSet,CSGN[,values],by="Date");
dataSet <- merge(dataSet,DUFN[,values],by="Date");
#dataSet <- merge(dataSet,GALN[,values],by="Date");
dataSet <- merge(dataSet,GEBN[,values],by="Date");
dataSet <- merge(dataSet,GIVN[,values],by="Date");
dataSet <- merge(dataSet,HOLN[,values],by="Date");
dataSet <- merge(dataSet,KNIN[,values],by="Date");
dataSet <- merge(dataSet,LONN[,values],by="Date");
dataSet <- merge(dataSet,NESN[,values],by="Date");
dataSet <- merge(dataSet,NOVN[,values],by="Date");
dataSet <- merge(dataSet,ROG[,values],by="Date");
dataSet <- merge(dataSet,SCHP[,values],by="Date");
dataSet <- merge(dataSet,SCMN[,values],by="Date");
dataSet <- merge(dataSet,SIK[,values],by="Date");
dataSet <- merge(dataSet,SLHN[,values],by="Date");
dataSet <- merge(dataSet,SGSN[,values],by="Date");
dataSet <- merge(dataSet,SOON[,values],by="Date");
dataSet <- merge(dataSet,SREN[,values],by="Date");
dataSet <- merge(dataSet,SYNN[,values],by="Date");
dataSet <- merge(dataSet,UBSN[,values],by="Date");
dataSet <- merge(dataSet,UHRN[,values],by="Date");
dataSet <- merge(dataSet,ZURN[,values],by="Date");

colnames(dataSet) <- c("Date","SMI","ABBN","ADEN","ATLN","BAER","BALN","CFR","CLN","CSGN","DUFN","GEBN","GIVN","HOLN","KNIN","LONN","NESN","NOVN","ROG","SCHP","SCMN","SIK","SLHN","SGSN","SOON","SREN","SYNN","UBSN","UHRN","ZURN");#"ARYN","GALN"
N <- length(colnames(dataSet))-1
dataSet 			<- as.ts(dataSet);
rownames(dataSet) 	<- dataSet[,"Date"]
dataSet <- dataSet[,-1];
R <- diff(log(dataSet))
#Empirical corr, shrunken correlation? vs. bootstrapped?
sliCor 	 <- round(cor(R),3);
sliCorV  <- c(sliCor)
matrix(sliCorV,nrow=N,ncol=N,byrow=TRUE);
smiSIGMA <- cov(R);
nrow(cor(R))

vols <- sqrt(diag(smiSIGMA))*sqrt(252)

#Compute 
 GBSOption(TypeFlag = c("p"), S = dataSet[1,], X = (dataSet[1,]*1.05), Time = rep(45/365,N), r = c(0.00),
     b = c(0.00), sigma = vols )

sli.boot.cor.fun <- function(X) {
	t(c(round(cor(X),3)));
}
bootedCor <- tsboot(R,sli.boot.cor.fun, R = 1000, l = 45, sim = "geom")
muCor <- apply(bootedCor$t,MARGIN=2,FUN=mean)
sdCor <- apply(bootedCor$t,MARGIN=2,FUN=sd)
muCor <- matrix(muCor,ncol=N,nrow=N)
sdCor <- matrix(sdCor,ncol=N,nrow=N)
colnames(muCor) <- colnames(R);
rownames(muCor) <- colnames(R);
colnames(sdCor) <- colnames(R);
rownames(sdCor) <- colnames(R);
tCor <- round(muCor/(sdCor),3);
round(muCor,3);



##RISK LIMITS
maxDeltaPortfolio				<- 0.1;#Allow some directional risk
maxAbsExposurePortfolio			<- 1000;#Value of the controlled underlying
maxDeltaAdjustedExposurePortfolio 	<- maxDeltaPortfolio*maxExposurePortfolio;#
maxAbsExposureUnderlying 			<- 0.02*maxExposurePortfolio;#% of total exposure
maxDeltaUnderlying 				<- 0.5;#% of total value
maxDeltaAdjustedExposureUnderlying 	<- maxExposureUnderlying*maxDeltaUnderlying


###CURRENT PORTFOLIO
portfolioDeltas		<- ;#Gets the sum of all SMI portfolio deltas
portfolioUnderlying	<- ;#SuM of all portfolio value in underlying


library(IBrokers)
conn <- twsConnect(clientId = 1, host = 'localhost', 
           port = 7496, verbose = TRUE, timeout = 5,
           filename = NULL)
zurnSTK <- twsSTK(symbol="ZURN",exch="VIRTX",currency="CHF");
reqMktData(conn, zurnSTK)
smiContract <- twsContract(conId=215526211,"SMI")










