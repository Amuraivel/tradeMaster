library(quantmod)
library(Quandl)
library(plotly)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(zoo)
library(Amelia)
library(memisc)
library(dplyr)
library(readr)
library(parallel)
library(tawny)
require(memisc)
##############################################################################
#Tradeable
##############################################################################
#' 
#' 
#' 
#' 
#' 
#' 
#' @export
getOptimalPortfolio <- function(portfolioAssets=NA){
  if (is.na(portfolioAssets)){
    #Default portfolio assets
    portfolioAssets <- c("WTI","GOLD","SILVER","CORN","WHEAT","SOY","CATTLE","HOGS","COCOA","COFFEE","ORANGE","SUGAR","COTTON","BONDS","BUNDS","JGBS","SP500","N225","ESTX50","FTSE100","AP200","VIX","EEM","AUD","CAD","CHF","EUR","GBP","JPY","MXN")
  } else if (portfolioAssets == "EQUTIES") {
    #EQUITIES
    portfolioAssets <- c("SP500","N225","ESTX50","AP200","FTSE100","EEM")
  } else if (portfolioAssets == "FX") {
    #FOREIGN EXCHANGE
    portfolioAssets <- c("AUD","CAD","CHF","EUR","GBP","JPY","MXN")
  } else if (portfolioAssets == "COMMODITIES") {
    #COMMODITIES
    portfolioAssets <- c("WTI","GOLD","SILVER","CORN","WHEAT","SOY","CATTLE","HOGS","COCOA","COFFEE","ORANGE","SUGAR","COTTON")
  } else if (portfolioAssets == "FIXED_INCOME"){
    #FIXED INCOME
    portfolioAssets <- c("BONDS","BUNDS","JGBS")
  }
  
  ###################START PREPARING THE DATA BASED ON THE ASSET SELECTION##############################
  tmp         <- tbl(marketDB,"imputedReturnsMatrix") %>% dplyr::select(one_of(c("Date",portfolioAssets))) %>% as.data.frame()
  dates       <- tmp[,1] #get dates from first column
  R           <- tmp[,-1] #strip date strings to leave naked return matrix
  R           <- as.xts(R,as.Date(dates))#cast to time series
  asset.names <- colnames(R) #get the names
  N           <- length(asset.names)#Number of assets
  ##############################################################################

  
  #These generate more rubust estimates of the underlying covariance matrix
  #We could possibly use intra-day tick data to improve it even more 
  Sigma_shrinkage <- cov_shrink(R, prior.fun = cov.prior.cc)# Ledoit and Wolf
  
  
  #This method will override the default of using the empirical returns and empirical covariance matrix
  myDefinedMoments <<- function(R,Sigma=Sigma_shrinkage, ...) {
    #Initialize a list
    out       <- list()
    #To-do : penalize the empirical returns assymetrically so than downward deviation counts by as much as would be necessary to return to 
    #The initial value, i.e. 1*(1-.r)*(1+.r) = 1
    ###Sets the expected value of the asset equal to the standard deviation in that volatility risk premium is proportional to variance
    out$mu    <- apply(R, 2, sd,na.rm=TRUE) #sets the means equal to the sd : 1st moment
    #To-do : Eventually we would scale returns to match the VR premium for each asset -- here we assume they are proportional to variance
    out$sigma <- Sigma
    out$m3    <- PerformanceAnalytics:::M3.MM(R) #skewness : 3rd moment 
    #To-do :  skewness implications for gamma?
    out$m4    <- PerformanceAnalytics:::M4.MM(R) #kurtosis : 4th moment
    #To-do : kurtotic implications for profit taking?
    out
  }
  
  # Creates a new portfolio object with the assets in the list
  pspec <- portfolio.spec(assets=asset.names)
  #WHAT WE ARE TRYING TO MAXIMIZE -- the first moment
  pspec <- add.objective(pspec, type="return", name="mean")
  
  ################OPERATIONAL LIMITS####################
  #Limits solution to short or long
  pspec <- add.constraint(pspec, type="weight_sum",min_sum=-1, max_sum=1)
  
  #Limit leverage to capital
  pspec <- add.constraint(portfolio=pspec, type="leverage_exposure", leverage=1)

  #Equitization portfolio : we'll use 50% in options and 25% in equitization
  
  ###############RISK CONSTRAINTS########################
  ###RISK FACTOR CONSTRAINTS#############################
  my_moments <- myDefinedMoments(R,Sigma=Sigma_shrinkage)
  #Now incorporate the PCA to determine the risk factors with shrunken covariance matrix
  PC <- princomp(covmat = Sigma_shrinkage)
  #Now get the Eigenvalues of the PCA to scale them to be equal
  pcSD <- summary(PC)[1]
  #Now scale all the factors to have the same amount of weight as the first
  scaledPCsd <- 1/(pcSD$sdev/pcSD$sdev[1])
  #Now constrain the portfolio by using the PC loadings such that the all of the factors are equally weighted to the primary risk vector
  pspec <- add.constraint(portfolio=pspec, factor_exposure_constraint(type = "factor_exposure", my_moments$mu,B=PC$loadings[,], -scaledPCsd, scaledPCsd,
                                             enabled = TRUE, message = FALSE)
  
  ##########TRANSLATE INTO HUMAN BEHAVIOR PERCEPTION OF RISK##########
  #We assume that the person is perfectly rational and thus risk-neutral 
  pspec <- add.objective(portfolio=pspec, type="quadratic_utility",risk_aversion=0)
  
  ##########REDUCE ABSOLUTE DAILY CHANGE###############################
  #WE ALSO DON"T WANT TOO MUCH DAILY P&L CHANGE -- 1% ~ sd of SP500
  pspec <- add.objective(pspec, type="risk", name="StdDev", target=0.01)
  #Minimize the tail loss over the daily returns
  pspec <- add.objective(portfolio=pspec,type="risk",name="ETL",arguments=list(p=0.99))
  
  #########LIMIT ABSOLUTE DRAW-DOWNS and SHORT-FALL#############################
  #We care about big draw downs risk budget so as to minimize short-fall
  #We are very sensitive to the tail risk because idiosyncratic risk can be spread off, but stil one asset cannot be more than 10%
  #We also don't want more than a 5% down-day
  pspec <- add.objective(pspec, type="risk_budget", name="ES",
                         arguments=list(p=0.999, clean="boudt"),
                         min_concentration=TRUE, min_prisk=0, max_prisk=0.1,target=0.05)
  

  
  ######CONCENTRATION RISK REDUCTION ##########################################
  #Limits solution reasonable position weightings, may not be binding
  #To-do : change this to historical vol scaled weightings
  pspec           <- add.constraint(portfolio=pspec, type="box", min=rep(-1/N*4,N), max=rep(1/N*4,N))
  
  ############NOW SOLVE THE PORTFOLIO PROBLEM USING VARIOUS TECHNIQUES##############
  #Now we find the constrained portfolio using global optimization by differential evolution
  DEoptimPort     <- optimize.portfolio(R, portfolio = pspec, optimize_method="DEoptim",trace=TRUE, traceDE=0,momentFUN="myDefinedMoments")#
  
  #Now we find the constrained portfolio using global optimization by random portfolio theory, this is for diagnosing stability
  #randomOptimPort <- optimize.portfolio(R, portfolio = pspec, optimize_method="random",trace=TRUE,momentFUN="myDefinedMoments")#
  
  #Now we find the constrained portfolio using global optimization by particle swarm optimization
  particleOptimPort <- optimize.portfolio(R, portfolio = pspec, optimize_method="pso",trace=TRUE,momentFUN="myDefinedMoments")#
  
  #Now we find the constrained portfolio using global optimization by genetic algorithm, very slow
  geneticOptimPort <- optimize.portfolio(R, portfolio = pspec, optimize_method="GenSA",trace=TRUE,momentFUN="myDefinedMoments")#
  
  ###Strange error
  #Now we find the constrained portfolio using global optimization by quadratic programming
  #roiOptimPort    <- optimize.portfolio(R, portfolio = pspec, optimize_method="ROI",trace=TRUE,momentFUN="myDefinedMoments")#
  portfolioSolutions <- combine.optimizations(list(DEoptimPort=DEoptimPort,randomOptimPort=randomOptimPort,particleOptimPort=particleOptimPort,geneticOptimPort=geneticOptimPort))
  data.frame(DEoptimPort=DEoptimPort[[1]],particleOptimPort=particleOptimPort[[1]],geneticOptimPort=geneticOptimPort[[1]])
}


port <- getOptimalPortfolio()
#momentFUN="crra.moments"
chart.Concentration(port)
chart.EfficientFrontier(port, risk.col = "StdDev")



#Create a cluster with 24 cores to do parallel processing on server
registerDoParallel(cores=24)
bt.opt.crra <- optimize.portfolio.rebalancing(R, pspec, 
                                              optimize_method="DEoptim",
                                              search_size=5000, trace=TRUE,
                                              traceDE=0,
                                              momentFUN="crra.moments",
                                              rebalance_on="months", 
                                              training_period=1, 
                                              trailing_periods=1)
#GET THE RETURNS
ret.crra <- summary(bt.opt.crra)$portfolio_returns
colnames(ret.crra) <- "CRRA"

charts.PerformanceSummary(opt.pspec, main="Optimization Performance")


#PLOT A BASIC CORRELATION TABLE
assetCorrelations <- cor(R,use="pairwise.complete")



ts.plot(x=d$imputations$imp1[,1],y=log(d$imputations$imp1[,-1]))
D <- as.xts(dataSet[,-1])
rownames(D) <- as.character(dataSet$Date)

tmp <- timeSeriesData
tsTemp <- as.data.frame(tmp)
tmp <- gather(as.data.frame(D),asset,price)  %>% group_by(asset) %>% mutate(dLnPrice = log(price) - lag(log(price))) %>% filter(!is.na(dLnPrice)) %>% mutate(cumulativeDeviation_t1 = lag(cumsum(dLnPrice))) %>% filter(asset %in% c("SP500","N225","ESTX50","GOLD","WTI","SOY","BUNDS","BONDS","JGBS","EUR","GBP","AUD"))


deviationModel <- lm(dLnPrice ~-1+as.factor(asset)*cumulativeDeviation_t1,tmp)
summary(deviationModel)
pander(mtable('Deviation Model'= deviationModel,summary.stats = c('R-squared','F','p','N')))
kable(
	data.frame(
		Estimate=round(
		coef(deviationModel),3),
		tStat = round(as.vector(coef(deviationModel)/sqrt(diag(vcov(deviationModel)))),3)
		)
	)


row.names(dataSet) <- as.character(dataSet$Date)
D <- dataSet %>%  ts()
%>% filter()

timeSeriesData		 		<- as.ts(dataSet[complete.cases(dataSet),-1])
timeSeriesData 				<- diff(log(timeSeriesData))
rownames(timeSeriesData)	<- as.character(as.Date(dataSet$Date[(nrow(dataSet)-1):nrow(dataSet)]))
model <- prcomp(timeSeriesData, center=TRUE, scale.=TRUE)
summary(model)
model
modelNonNormal <- prcomp(timeSeriesData)
modelNonNormal
summary(modelNonNormal)

plot(x=1:ncol(timeSeriesData),y=summary(modelNonNormal)$importance[2,])



#########################FUNDAMENTALS##########################################
USAPPI <- Quandl("FRED/PPIACO");

##############################################################################

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

names(dataSet) <- c(eqN,fxN,yldN,cmN);
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

