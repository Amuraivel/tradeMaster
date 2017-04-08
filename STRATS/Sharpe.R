##Read in total returns
#SPX,TSX,

RETURNS <- read.csv("~/Desktop/Sharpe_TR.csv");
names(RETURNS)[2:ncol(RETURNS)] <- paste(names(RETURNS)[2:ncol(RETURNS)],"_R",sep='');
EARNINGS <- read.csv("~/Desktop/Sharpe_earn.csv");
names(EARNINGS )[2:ncol(EARNINGS)] <- paste(names(EARNINGS )[2:ncol(EARNINGS )],"_E",sep='');

X <- merge(RETURNS,EARNINGS);
#Parameters
#####################################CURRENCIES######################################
#Currency effects
library(Quandl)
Quandl.api_key('tyNA1apCEZmn5L6_F2bh');
AUDUSD <- Quandl("RBA/FXRUSD");
CADUSD <- Quandl("BUNDESBANK/BBEX3_D_CAD_USD_CA_AC_000");
CADUSD[,2]<- 1/CADUSD[,2];
CHFUSD <- Quandl("BUNDESBANK/BBEX3_D_CHF_USD_CA_AA_000");
CHFUSD[,2]<- 1/CHFUSD[,2];
SEKUSD <- Quandl("BUNDESBANK/BBEX3_D_SEK_USD_CA_AC_000");
SEKUSD[,2]<- 1/SEKUSD[,2];
NOKUSD <- Quandl("BUNDESBANK/BBEX3_D_NOK_USD_CA_AA_000");
NOKUSD[,2]<- 1/NOKUSD[,2];
EURUSD <- Quandl("ECB/EURUSD");
GBPUSD <- Quandl("BOE/XUDLUSS");
JPYUSD <- Quandl("BOE/XUDLJYD");
JPYUSD[,2] <- 1/JPYUSD[,2];
HKDUSD <- Quandl("BOE/XUDLHDD");
HKDUSD[,2] <- 1/HKDUSD[,2];
AUDTWD <- Quandl("RBA/FXRNTD");
tmp <- merge(AUDTWD,AUDUSD,by="Date");
tmp$TWD <- tmp[,2]/tmp[,3];
TWDUSD	<- tmp[,-c(2,3)];
TWDUSD[,2] <- 1/TWDUSD[,2];


##MERGE
FXrates <- merge(AUDUSD,CADUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD");
FXrates <- merge(FXrates,CHFUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF");
FXrates <- merge(FXrates,EURUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF","EUR");
FXrates <- merge(FXrates,GBPUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF","EUR","GBP");
FXrates <- merge(FXrates,JPYUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF","EUR","GBP","JPY");
FXrates <- merge(FXrates,HKDUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF","EUR","GBP","JPY","HKD");
FXrates <- merge(FXrates,TWDUSD,by="Date");
names(FXrates) <- c("Date","AUD","CAD","CHF","EUR","GBP","JPY","HKD","TWD");
FXrates$USD <- 1;
##Swiss franc
FXrates <- FXrates[order(FXrates$Date,decreasing="TRUE"),];
FXrates_R <- log(FXrates[1:nrow(FXrates)-1,2:ncol(FXrates)]) - log(FXrates[2:nrow(FXrates),2:ncol(FXrates)]); 
rownames(FXrates_R) <- FXrates[2:nrow(FXrates),"Date"];
FXrates_R$Date <- rownames(FXrates_R);
aggregate(cbind(EUR,JPY,HKD,CHF,TWD,AUD,CAD,GBP)~0,data=FXrates_R[1:4000,],sum);
###########################################################

X <- merge(X,FXrates_R);
X[,"FTSE_TR"] <- X[,"FTSE_R"] + X$GBP;
X[,"DAX_TR"] <- X[,"DAX_R"] + X$EUR;
X[,"NIKKEI_TR"] <- X[,"NIKKEI_R"] + X$JPY;
X[,"TSE_TR"] 	<- X[,"TSE_R"] + X$CAD;
X[,"HKHS_TR"] 	<- X[,"HKHS_R"] + X$HKD;
X[,"TWX_TR"]	<- X[,"TWX_R"] + X$TWD;
X[,"SMI_TR"]	<- X[,"SMI_R"] + X$CHF;
X[,"SPX_TR"]	<- X[,"SPX_R"] + X$USD;
X[,"ASX_TR"]	<- X[,"ASX_R"] + X$AUD;

TOTAL_RETURNS <- X[,c("FTSE_TR","DAX_TR","NIKKEI_TR","TSE_TR","HKHS_TR","TWX_TR","SMI_TR","SPX_TR","ASX_TR")];

tradingDays <- 3500;
E <- aggregate(cbind(DAX_E,FTSE_E,NIKKEI_E,TSE_E,HKHS_E,TWX_E,SMI_E,SPX_E,ASX_E)~0,mean,data=X[1:tradingDays,]);
TR <- aggregate(cbind(DAX_TR,FTSE_TR,NIKKEI_TR,TSE_TR,HKHS_TR,TWX_TR,SMI_TR,SPX_TR,ASX_TR)~0,sum,data=X[1:tradingDays,]);
SD <- aggregate(cbind(DAX_TR,FTSE_TR,NIKKEI_TR,TSE_TR,HKHS_TR,TWX_TR,SMI_TR,SPX_TR,ASX_TR)~0,sd,data=X[1:tradingDays,])*sqrt(tradingDays);
TR/SD;

model <- princomp(X[,c("FTSE_TR","DAX_TR","NIKKEI_TR","TSE_TR","HKHS_TR","TWX_TR","SMI_TR","SPX_TR","ASX_TR")])
summary(model)
###Optimizer
volLag <- 126
earnLag <- 32

findVolLag <- function(volLag,maxReturns){

obs <- tradingDays - volLag;

Sharpes <- matrix(NA,nrow=obs,ncol=ncol(TOTAL_RETURNS));
for(t in 1:obs){
	Sharpes[t,] <-  t(as.matrix(aggregate(cbind(DAX_E,FTSE_E,NIKKEI_E,TSE_E,HKHS_E,TWX_E,SMI_E,SPX_E,ASX_E)~0,median,data=EARNINGS[t:t+earnLag,],na.rm=TRUE))/as.matrix(aggregate(cbind(DAX_TR,FTSE_TR,NIKKEI_TR,TSE_TR,HKHS_TR,TWX_TR,SMI_TR,SPX_TR,ASX_TR)~0,sd,data=TOTAL_RETURNS[t:(t+volLag),],na.rm=TRUE)));
}

longReturns <- data.frame(time=1:obs,r=NA);
shortReturns <- data.frame(time=1:obs,r=NA);
#Get the returns of the max sharpe
for (t in 1:obs){
	#Buy the index with the current highest Sharpe
	longReturns[t,"r"] <- TOTAL_RETURNS[t,which.max(Sharpes[t,])];
	#Get the index with the current lowest Sharpe to short
	shortReturns[t,"r"] <- TOTAL_RETURNS[t,which.min(Sharpes[t,])]
}

#portfolio annualized returns
longReturn 			<- (mean(longReturns[,"r"],na.rm=TRUE)+1)^(252)-1;
longVolatility 		<- sd(longReturns[,"r"],na.rm=TRUE)*sqrt(252);
longSharpe			<- longReturn/longVolatility;
#longTotalReturns 	<- sum(portReturns[1:tradingDays,"r"],na.rm=TRUE);

shortReturn 		<- (mean(shortReturns[,"r"],na.rm=TRUE)+1)^(252)-1;
shortVolatility 	<- sd(shortReturns[,"r"],na.rm=TRUE)*sqrt(252);
shortSharpe			<- shortReturn/shortVolatility;
#shortTotalReturns 	<- sum(shortReturns[1:tradingDays,"r"],na.rm=TRUE);

lsReturn 			<- longReturn - shortReturn;
lsVolatility 		<- sd(longReturns[,"r"]-shortReturns[,"r"],na.rm=TRUE)*sqrt(252);
lsSharpe			<- lsReturn/lsVolatility;

data.frame(longReturn,longVolatility,longSharpe,shortReturn,shortVolatility,shortSharpe,lsReturn,lsVolatility,lsSharpe)

}
findVolLag(10)
portMoments$t <- 1:252;

#FIND SOLUTION WITH BRUTE FORCE
library(parallel)
cl <- makeCluster(8)
library(doParallel)
registerDoParallel(cl)
library(foreach)
portMoments <- foreach(lag = 1:252, .combine=rbind) %dopar% findVolLag(lag); 
stopCluster(cl)


plot(portMoments[,c("t","longReturn")],main="Long Index with Highest Sharpe\n (DAX,FTSE,NIKKEI,TSE,HS,TWX,SMI,SPX,ASX)",xlab="Window for calculating SD (trading days)",ylab="Annualized return")
legend("topright",legend=c("N=3500, Sharpe=earningsYield_median30d/SD_tDays"))



portMoments$window <- 1:nrow(portMoments);
lm(strategySharpe~t+I(t^2),data=portMoments);
plot(portMoments[,c("window","strategySharpe")])

optimize(findVolLag,15:252,maximum=TRUE)
