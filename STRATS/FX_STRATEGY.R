library(quantmod)
library(Quandl)
###############
## DATA 
##############################################################################
##############################################################################
Quandl.api_key("tyNA1apCEZmn5L6_F2bh")
##############################################################################
#Equities
R3KUSD  <- Quandl("YAHOO/INDEX_RUA");#Russell 3K
E50EUR	<- Quandl("YAHOO/INDEX_STOXX50E");##Eurostoxx 50
NIKJPY	<- Quandl("NIKKEI/INDEX");##NiKkei 225
ASXAUD	<- Quandl("YAHOO/INDEX_AXJO")#ASX 200

###############################FX#############################################
EURUSD <- Quandl("ECB/EURUSD");#remember quoted reverse! ; #"EURUSD" 
USDCHF <- Quandl("BUNDESBANK/BBEX3_D_CHF_USD_CA_AA_000"); #"USDCHF"
GBPUSD <- Quandl("BUNDESBANK/BBEX3_D_GBP_USD_CM_AC_000");
USDCAD <- Quandl("BUNDESBANK/BBEX3_D_CAD_USD_CA_AC_000"); #"USDCAD" 
USDAUD <- Quandl("BOE/XUDLADD"); #"USDAUD" 
USDJPY <- Quandl("BUNDESBANK/BBEX3_D_JPY_USD_CA_AC_000"); #"USDJPY" 
GLDUSD <- Quandl("LBMA/GOLD")[,c("Date","USD (PM)")];#Treating GLD as a currency

###############################10Y Rates#######################################
AUDYLD	<- Quandl("RBA/F17_0")[,c("Date","Zero-coupon yield - 10 yrs - Per cent per annum (FZCY1000D)")];
CADYLD	<- Quandl("BOC/V39055");
CHFYLD 	<- Quandl("SNB/RENDOBLID")[,c("Date","Spot interest rates with different maturities for Confederation bond issues; euro-denominated bond issues and US treasury bond issues - CHF Swiss Confederation bond issues - 10 years")];
GBPYLD	<- Quandl("BOE/IUDMRZC");
EURYLD	<- Quandl("BUNDESBANK/BBK01_WT1010");
USDYLD	<- Quandl("YAHOO/INDEX_TNX")[,c("Date","Close")];

ZBUSD <- Quandl("CHRIS/CME_US1")[,c("Date","Last")]


##########################Commodities#########################################
OILUSD <- Quandl("FRED/DCOILBRENTEU"); #Oil
CRNUSD <- Quandl("TFGRAIN/CORN");#Corn
SOYUSD <- Quandl("TFGRAIN/SOYBEANS");#Corn
COPUSD <- Quandl("COM/COPPER");#Copper
WTIUSD <- Quandl("CHRIS/ICE_T1")[,c("Date","Settle")]#WTI

#########################FUNDAMENTALS##########################################
USAPPI <- Quandl("FRED/PPIACO");



##############################################################################
dayGrid <- data.frame(Date=as.Date(seq(from=-2000,to=17061,by=1)));
##############################################################################
dataSet <- merge(dayGrid,R3KUSD[,c("Date","Close")],all.x=TRUE,by="Date");
dataSet <- merge(dataSet, E50EUR[,c("Date","Close")],by="Date");
dataSet <- merge(dataSet, NIKJPY[,c("Date","Close Price")],by="Date");
dataSet <- merge(dataSet, ASXAUD[,c("Date","Close")],by="Date");
eqN <- c("Date","R3KUSD","E50EUR","NIKJPY","ASXAUD");
names(dataSet) <- c(eqN);
##############################################################################
###############################FX#############################################
dataSet <- merge(dataSet, EURUSD,by="Date");
dataSet <- merge(dataSet, USDCHF,by="Date");
dataSet <- merge(dataSet, GBPUSD,by="Date");
dataSet <- merge(dataSet, USDCAD,by="Date");
dataSet <- merge(dataSet, USDAUD,by="Date");
dataSet <- merge(dataSet, USDJPY,by="Date");
dataSet <- merge(dataSet, GLDUSD,by="Date");
fxN <- c("EURUSD","USDCHF","GBPUSD","USDCAD","USDAUD","USDJPY","GLDUSD")
names(dataSet) <- c(eqN,fxN);
##############################################################################
#############################Yields###########################################
dataSet <- merge(dataSet, AUDYLD,by="Date");
dataSet <- merge(dataSet, CADYLD,by="Date");
dataSet <- merge(dataSet, CHFYLD,by="Date");
dataSet <- merge(dataSet, GBPYLD,by="Date");
dataSet <- merge(dataSet, EURYLD,by="Date");
dataSet <- merge(dataSet, USDYLD,by="Date");
yldN <- c("AUDYLD","CADYLD","CHFYLD","GBPYLD","EURYLD","USDYLD")
names(dataSet) <- c(eqN,fxN,yldN);
##############################################################################
##########################Commodities#########################################
dataSet <- merge(dataSet, OILUSD,by.x="Date",by.y="DATE");
dataSet <- merge(dataSet, CRNUSD[,c("Date","Cash Price")],by="Date");
dataSet <- merge(dataSet, SOYUSD[,c("Date","Cash Price")],by="Date");
dataSet <- merge(dataSet, COPUSD,by="Date");
cmN 	<- c("OILUSD","CRNUSD","SOYUSD","COPUSD");
names(dataSet) <- c(eqN,fxN,yldN,cmN);
##############################################################################
################################CURENCY CROSSES###############################
dataSet$EURCHF <- dataSet[,"EURUSD"]*(dataSet[,"USDCHF"]); # "EURCHF"
dataSet$EURGBP <- dataSet[,"EURUSD"]*(1/(dataSet[,"GBPUSD"])); # "EURGBP"
dataSet$EURCAD <- dataSet[,"EURUSD"]*(dataSet[,"USDCAD"]); # "EURCAD" 
dataSet$EURAUD <- dataSet[,"EURUSD"]*(dataSet[,"USDAUD"]); #"EURAUD" 
dataSet$EURJPY <- dataSet[,"EURUSD"]*(dataSet[,"USDJPY"]); #"EURJPY"
dataSet$USDGBP <- 1/dataSet$GBPUSD #USDGBP
dataSet$CHFGBP <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDGBP"]);  #"CHFGBP" 
dataSet$CHFCAD <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDCAD"]);      #"CHFCAD" 
dataSet$CHFAUD <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDAUD"]); 	#"CHFAUD"
dataSet$CHFJPY <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDJPY"]); #"CHFJPY"
dataSet$GBPCAD <- dataSet[,"GBPUSD"]*dataSet[,"USDCAD"]; #"GBPCAD"
dataSet$GBPAUD <- dataSet[,"GBPUSD"]*dataSet[,"USDAUD"]; #"GBPCAD"
dataSet$GBPJPY <- dataSet[,"GBPUSD"]*dataSet[,"USDJPY"]; #"GBPJPY"
dataSet$CADAUD <- (1/dataSet[,"USDCAD"])*dataSet[,"USDAUD"]; #"CADAUD"
dataSet$CADJPY <- (1/dataSet[,"USDCAD"])*dataSet[,"USDJPY"]; #"CADJPY"
dataSet$AUDJPY <- (1/dataSet[,"USDAUD"])*dataSet[,"USDJPY"]; #"AUDJPY"
##############################################################################

##############################################################################
##############################################################################
#
##############################################################################




rownames(dataSet) <- dataSet$Date
dataSet <- dataSet[,-1];
#dataSet <- dataSet[,which(names(dataSet) %in% crossed$cross)];

tsDataSet <- as.xts(dataSet);
tsDataSet <- diff(log(tsDataSet));
tmp <- tsDataSet;
tmp[is.nan(tmp)] <- 0.00
tmp <- tmp[-1,]
tmp$E50USD <- tmp$E50EUR*(tmp$EURUSD);
tmp$NIKJPY <- tmp$NIKJPY*(1/tmp$USD);
model <- prcomp(tmp[,c("R3KUSD","E50USD","NIKJPY","OILUSD","CRNUSD","COPUSD","USDYLD","EURUSD","JPYUSD")])


x <- abs(cor(tsDataSet))
write.csv(as.matrix(x),"/Users/mark/Desktop/pairCor.csv");

cor(tsDataSet,use="pairwise.complete")

sqrt(diag(cov(tsDataSet,use="pairwise.complete")))*sqrt(252)


FXmodel <- VAR(tsDataSet[,c("AUDJPY","CHFAUD","CADJPY","GBPJPY","CHFCAD","USDAUD","CHFJPY")],type="both",ic="AIC",lag.max=30)
print(predict((FXmodel)))


xy <- abs(cor(tsDataSet))

tmp <- subset(chData[,c("USDCHF","CADJPY","EURCHF","Liabilities - Foreign currency liabilities","Assets - Foreign currency investments")],!is.na(chData$"Liabilities - Foreign currency liabilities"))
tmp <- as.ts(tmp)
tmp <- diff(log(tmp))
tmp[is.nan(tmp)] <- 0.00
FXmodel <- VAR(tmp,type="both",ic="AIC",lag.max=30)
predict(FXmodel)
IRFfxModel <- irf(FXmodel)
plot(IRFfxModel)





##VARIABLES
SNB		<- Quandl("SNB/SNBBIPO");
###QUANDLE

chData <- merge(dataSet,SNB[,c("Date","Liabilities - Foreign currency liabilities","Assets - Foreign currency investments")],all.x=TRUE,by="Date")
chData$"Liabilities - Foreign currency liabilities" <- na.locf(chData$"Liabilities - Foreign currency liabilities",na.rm = FALSE);
chData$"Assets - Foreign currency investments" 		<- na.locf(chData$"Assets - Foreign currency investments",na.rm = FALSE);

crosses <- combn(c("EUR","USD","CHF","GBP","CAD","AUD","JPY",""), 2, FUN = NULL, simplify = TRUE)
crossed <- data.frame(t(crosses))
crossed$cross <- paste( crossed[,1],crossed[,2],sep="")



####EQUITY ANALISIS#######
eqUSD <- data.frame(R3K=dataSet$R3KUSD,E50=(dataSet$E50EUR*dataSet$EURUSD),NIK=(dataSet$NIKJPY/dataSet$USDJPY),ASX=(dataSet$ASXAUD/dataSet$USDAUD))
eqUSD$R3K <- eqUSD$R3K/eqUSD$R3K[1]
eqUSD$E50 <- eqUSD$E50/eqUSD$E50[1]
eqUSD$NIK <- eqUSD$NIK/eqUSD$NIK[1]
eqUSD$ASX <- eqUSD$ASX/eqUSD$ASX[1]
ts.plot(as.ts(eqUSD),ty="l",col=c("green","blue","red","violet"))#/dataSet$GLDUSD





############################################################################
#Portfolio
############################################################################


#EQUITY

#FOREIGN EXCHANGE

#COMMODITY

#FIXED INCOME





############################################################################
###INTERACTIVE BROKER
############################################################################
library(IBrokers)
tws <- twsConnect()
twsDisconnect(tws)


portVal 	<- reqAccountUpdates(tws)
nPositions 	<- length(portVal[[2]])
symbol 		<- NA;
value		<- NA;
currency	<- NA;
sectype		<- NA;
positionNr	<- 1:nPositions;
portfolio <- data.frame(positionNr,symbol,currency,sectype,value);
for (p in 1:nPositions) {

	portfolio[p,"symbol"] 	<- portVal[[2]][[p]][["contract"]][["symbol"]];
	portfolio[p,"currency"] <- portVal[[2]][[p]][["contract"]][["currency"]];
	portfolio[p,"sectype"] 	<- portVal[[2]][[p]][["contract"]][["sectype"]];
	portfolio[p,"value"] 	<- portVal[[2]][[p]][["portfolioValue"]]["marketValue"];
	if(!is.null(FXrates)){
		portfolio[p,"value"]		<- FXrates[portfolio[p,"currency"],"rate"]*portfolio[p,"value"];
		portfolio[p,"currency"] 	<- "USD";
	}
	
}

#####################GET FX RATES#############################
FX 		<- unique(portfolio[,"currency"]);
rate 	<- NA
FXrates <- data.frame(FX,rate);

for(fx in 1:length(FX)){
	pair <- paste(FXrates[fx,"FX"],"USD",sep="/");
	FXrates[fx,"rate"] <- getSymbols(pair,src="oanda",auto.assign=FALSE,from=as.character(as.Date(Sys.time())-1))
}
tmp 		<- subset(portfolio,sectype=="STK");
tmp$share 	<- tmp[,"value"]/sum(abs(tmp[,"value"]))
tmp 		<- tmp[order(tmp$share,decreasing=TRUE),]
##############################################################




function (twsconn) 
{
    .reqAllOpenOrders(twsconn)
    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}








#######################################################
library(quantmod)
library(Quandl)
W5K			<- getSymbols("WILL5000IND",src="FRED",auto.assign=FALSE);#
EDV			<- Quandl("GOOG/AMEX_EDV")[,c("Date","Close")];
EDV			<- zoo(x=EDV$Close,order.by=as.Date(EDV$Date));
GLD			<- Quandl("GOOG/AMEX_GLD")[,c("Date","Close")];
GLD			<- zoo(x=GLD$Close,order.by=as.Date(GLD$Date));
###
min_vol_px 	<- read.csv("~/Desktop/MIN-VOL/min_vol_prices.csv");
key			<- read.csv("~/Desktop/MIN-VOL/names.csv");
tickers		<- c(colnames(min_vol_px[1,2:ncol(min_vol_px)]),"EDV");
mu 			<- as.matrix(min_vol_px[1,2:ncol(min_vol_px)]);
mu			<- c(mu,0.0285,0.022);
min_vol_px	<- min_vol_px[-1,]
min_vol_px	<- zoo(x=min_vol_px[,2:ncol(min_vol_px)],order.by=as.Date(min_vol_px[,1]));
min_vol_px <- merge(min_vol_px,EDV);#,by.x=0,by.y=0
min_vol_px <- merge(min_vol_px,GLD);#,by.x=0,by.y=0
min_vol_px <- merge(min_vol_px,W5K);#,all.x=TRUE,by=0
min_vol_px <- window(min_vol_px,start=as.Date("2007-12-13"),end=as.Date("2016-02-18"))
#Remove by, on
min_vol_px <- diff(log(as.zoo(min_vol_px)));
min_vol_px <- min_vol_px[complete.cases(min_vol_px),]
market 	<- min_vol_px[,which(names(min_vol_px)%in%c("WILL5000IND"))]
stocks	<- min_vol_px[,-which(names(min_vol_px)%in%c("WILL5000IND"))];

library(PortfolioAnalytics)
initialWeights <- rep(1/ncol(stocks),ncol(stocks));

names(initialWeights) <- tickers;
portf.dn <- portfolio.spec(assets=initialWeights);
#Add constraint
portf.dn <- add.constraint(portf.dn,type="weight_sum",min_sum=.99,max_sum=1.01);
#Add box constraint 20% -20%
portf.dn <- add.constraint(portf.dn,type="box",min=0,max=0.1);
#Limit assets
portf.dn <- add.constraint(portf.dn,type="position_limit",max_pos=20);
#Limit assets
#portf.dn <- add.constraint(portf.dn,type="position_limit",min_pos=10);
#Add beta constraint, market = W5K, 0 risk free rait
betas <- t(CAPM.beta(stocks,market,0))
portf.dn <- add.constraint(portf.dn,type="factor_exposure",B=betas,lower=-0.1,upper=1)
#Add objective maximize return with target of 0.0015
portf.dn <- add.objective(portf.dn,type="return",name="mean",target=0.20);
portf.dn <- add.objective(portf.dn,type="return",name="mean",target=0.20);

#Add objective to minimize portfolio StdDev with target of 0.02
portf.dn <- add.objective(portf.dn,type="risk",name="StdDev",target=0.1)
rp <- random_portfolios(portf.dn);
moments <- function(R){
	out <- list()
	
	out$mu <- rep(0,ncol(R));mu;
	return(out)
}
opt.dn <- optimize.portfolio(R=stocks,portf.dn,optimization_method="ROI",momentFUN="moments",trace=TRUE)

port <- opt.dn[[1]][which(opt.dn[[1]]>0)];
port <- port[order(port,decreasing=TRUE)]
names(port)	<- as.character(key[which(key[,1]%in%names(port)),2]);
port












