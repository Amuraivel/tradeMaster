##############################################################################
################ DATA FOR MULTI-ASSET CORE ALLOCATION STRATEGY ###############
##############################################################################
marketDB <- src_mysql("MARKET_DATA", host = "thompson.internet-box.ch", port = 3306, user = "", password = "")
Quandl.api_key("tyNA1apCEZmn5L6_F2bh")
##########################Commodities#########################################
recache <- FALSE
if (recache == TRUE ) {}
#ENERGY
WTI <- Quandl("CHRIS/CME_CL1")[,c("Date","Settle")]
GAS <- Quandl("CHRIS/CME_NG1")[,c("Date","Settle")]

#METALS
GOLD  <- Quandl("CHRIS/CME_GC1")[,c("Date","Settle")]
SILVER	<- Quandl("CHRIS/CME_SI1")[,c("Date","Settle")]
#COPUSD <- <- Quandl("CHRIS/CME_HG1")[,c("Date","Settle")]#Options are marginal
#GRAINS
CORN	<- Quandl("CHRIS/CME_C1")[,c("Date","Settle")]
SOY		<- Quandl("CHRIS/CME_S1")[,c("Date","Settle")]
WHEAT	<- Quandl("CHRIS/CME_W1")[,c("Date","Settle")]
#Meat
CATTLE	<- Quandl("CHRIS/CME_LC1")[,c("Date","Settle")]
HOGS	<- Quandl("CHRIS/CME_LN1")[,c("Date","Settle")]
#Softs
COCOA	<- Quandl("CHRIS/ICE_CC1")[,c("Date","Settle")]
COFFEE	<- Quandl("CHRIS/ICE_KC1")[,c("Date","Settle")]
ORANGE	<- Quandl("CHRIS/ICE_OJ1")[,c("Date","Settle")]
SUGAR 	<- Quandl("CHRIS/ICE_SB1")[,c("Date","Settle")]
COTTON	<- Quandl("CHRIS/ICE_CT1")[,c("Date","Settle")]

##############################################################################
#Equities
SP500	  <- Quandl("CHRIS/CME_ES1")[,c("Date","Settle")]
#Nikkei would need to 
getSymbols.FRED("NIKKEI225",env=globalenv()) #Quandl("SCF/CME_NK1_OB")[,c("Date","Settle")]#
N225    <- data.frame(Date=as.Date(index(get("NIKKEI225"))),N225=get("NIKKEI225"))
ESTX50	<- Quandl("CHRIS/EUREX_FESX1")[,c("Date","Settle")]
AP200	  <- Quandl("CHRIS/ASX_AP1")[,c("Date","Previous Settlement")]#ASX 200
#R3KUSD  <- Quandl("YAHOO/INDEX_RUA");#Russell 3K
#HSI		<- Quandl("BCIW/_HSI")
#HSCE	<- Quandl("BCIW/_HSCE")
FTSE100	<- Quandl("CHRIS/LIFFE_Z1")[,c("Date","Settle")]#FTSE100
EEM		<- Quandl("GOOG/NYSE_EEM")[,c("Date","Close")]#Emerging markets


###############################FIXED INCOME#######################################
BONDS	<- Quandl("CHRIS/CME_US1")[,c("Date","Settle")]
BUNDS	<- Quandl("CHRIS/EUREX_FGBL1")[,c("Date","Settle")]
JGBS	<- Quandl("CHRIS/SGX_JB1")[,c("Date","Settle")]
#10Y Rates
#AUDYLD  <- Quandl("RBA/F17_0")[,c("Date","Zero-coupon yield - 10 yrs - Per cent per annum (FZCY1000D)")];
#CADYLD  <- Quandl("BOC/V39055");
#CHFYLD 	<- Quandl("SNB/RENDOBLID")[,c("Date","Spot interest rates with different maturities for Confederation bond issues; euro-denominated bond issues and US treasury bond issues - CHF Swiss Confederation bond issues - 10 years")];
#GBPYLD	<- Quandl("BOE/IUDMRZC");
#EURYLD	<- Quandl("BUNDESBANK/BBK01_WT1010");
#USDYLD	<- Quandl("YAHOO/INDEX_TNX")[,c("Date","Close")];
###############################FX#############################################
#FX FUTURES WITH OPTIONS
AUDUSD	<- Quandl("CHRIS/CME_AD1")[,c("Date","Settle")]
CADUSD	<- Quandl("CHRIS/CME_CD1")[,c("Date","Settle")]
CHFUSD	<- Quandl("CHRIS/CME_SF1")[,c("Date","Settle")]
EURUSD	<- Quandl("CHRIS/CME_EC1")[,c("Date","Settle")]
GBPUSD	<- Quandl("CHRIS/CME_BP1")[,c("Date","Settle")]
JPYUSD	<- Quandl("CHRIS/CME_JY1")[,c("Date","Settle")]
MXNUSD	<- Quandl("CHRIS/CME_MP1")[,c("Date","Settle")]
###############################Volatility#####################################
VIX     <- Quandl("CHRIS/CBOE_VX1")[,c("Trade Date","Settle")]
names(VIX)[1] <- "Date"

###############################Merge Data#######################################
#dayGrid <- data.frame(Date=as.Date(seq(from=-2000,to=17061,by=1)));
#####Energy
dataSet <- left_join(WTI,GAS,by="Date")

#Metals
dataSet <- left_join(dataSet, GOLD,by="Date")
dataSet <- left_join(dataSet, SILVER,by="Date")
#Grains
dataSet <- left_join(dataSet,CORN,by="Date")
dataSet <- left_join(dataSet,WHEAT,by="Date")
dataSet <- left_join(dataSet,SOY,by="Date")
#Meats
dataSet <- left_join(dataSet, CATTLE,by="Date")
dataSet <- left_join(dataSet, HOGS,by="Date")
#Softs
dataSet <- left_join(dataSet, COCOA,by="Date")
dataSet <- left_join(dataSet, COFFEE,by="Date")
dataSet <- left_join(dataSet, ORANGE,by="Date")
dataSet <- left_join(dataSet, SUGAR,by="Date")
dataSet <- left_join(dataSet, COTTON,by="Date")

##
dataSet <- left_join(dataSet, SP500,by="Date")
dataSet <- left_join(dataSet, N225,by="Date")
dataSet <- left_join(dataSet, ESTX50,by="Date")
dataSet <- left_join(dataSet, FTSE100,by="Date")
dataSet <- left_join(dataSet, AP200,by="Date")
dataSet <- left_join(dataSet, EEM,by="Date")

##
dataSet <- left_join(dataSet, BONDS,by="Date")
dataSet <- left_join(dataSet, BUNDS,by="Date")
dataSet <- left_join(dataSet, JGBS,by="Date")


##
dataSet <- left_join(dataSet, VIX,by="Date")

##
dataSet <- left_join(dataSet, AUDUSD,by="Date")
dataSet <- left_join(dataSet, CADUSD,by="Date")
dataSet <- left_join(dataSet, CHFUSD,by="Date")
dataSet <- left_join(dataSet, EURUSD,by="Date")
dataSet <- left_join(dataSet, GBPUSD,by="Date")
dataSet <- left_join(dataSet, JPYUSD,by="Date")
dataSet <- left_join(dataSet, MXNUSD,by="Date")

names(dataSet) <- c("Date","WTI","GAS","GOLD","SILVER","CORN","WHEAT","SOY","CATTLE","HOGS","COCOA","COFFEE","ORANGE","SUGAR","COTTON","SP500","N225","ESTX50","FTSE100","AP200","EEM","BONDS","BUNDS","JGBS","VIX","AUD","CAD","CHF","EUR","GBP","JPY","MXN")

################################CURENCY CROSSES###############################
#dataSet$EURCHF <- dataSet[,"EURUSD"]*(dataSet[,"USDCHF"]); # "EURCHF"
#dataSet$EURGBP <- dataSet[,"EURUSD"]*(1/(dataSet[,"GBPUSD"])); # "EURGBP"
#dataSet$EURCAD <- dataSet[,"EURUSD"]*(dataSet[,"USDCAD"]); # "EURCAD"
#dataSet$EURAUD <- dataSet[,"EURUSD"]*(dataSet[,"USDAUD"]); #"EURAUD"
#dataSet$EURJPY <- dataSet[,"EURUSD"]*(dataSet[,"USDJPY"]); #"EURJPY"
#dataSet$USDGBP <- 1/dataSet$GBPUSD #USDGBP
#dataSet$CHFGBP <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDGBP"]);  #"CHFGBP"
#dataSet$CHFCAD <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDCAD"]);      #"CHFCAD"
#dataSet$CHFAUD <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDAUD"]);   #"CHFAUD"
#dataSet$CHFJPY <- (1/dataSet[,"USDCHF"])*(dataSet[,"USDJPY"]); #"CHFJPY"
#dataSet$GBPCAD <- dataSet[,"GBPUSD"]*dataSet[,"USDCAD"]; #"GBPCAD"
#dataSet$GBPAUD <- dataSet[,"GBPUSD"]*dataSet[,"USDAUD"]; #"GBPCAD"
#dataSet$GBPJPY <- dataSet[,"GBPUSD"]*dataSet[,"USDJPY"]; #"GBPJPY"
#dataSet$CADAUD <- (1/dataSet[,"USDCAD"])*dataSet[,"USDAUD"]; #"CADAUD"
#dataSet$CADJPY <- (1/dataSet[,"USDCAD"])*dataSet[,"USDJPY"]; #"CADJPY"
#dataSet$AUDJPY <- (1/dataSet[,"USDAUD"])*dataSet[,"USDJPY"]; #"AUDJPY"
##############################################################################
################WRITE DATA TO DATABASE########################################
#First drop existing table
marketDB %>% db_drop_table(table='genericFutures')
#Create and rebuild table
copy_to(marketDB, dataSet, "genericFutures", temporary = FALSE)                 

##################MUTLIPLE IMPUTATION#####################
clust <- makeCluster(1)#Make cluster with 1 core
#Get the generic data
d <- tbl(marketDB,"genericFutures") %>% transform(Date = as.numeric(as.Date(Date))) %>% mutate_at(vars(matches("chr")),funs(as.numeric)) %>% transform(Date=as.Date(Date))
#Now impute the data
d <- amelia(d, ts="Date",m = 1,logs=names(dataSet)[-1],lags=names(dataSet)[-1],ncpus=1,p2s=2,cl=clust,parallel=c("snow"))
#Log difference it
tmp   <-  as.tbl(d[[1]][[1]]) %>% group_by(Date) %>% gather(asset, price,-Date) %>% group_by(Date,asset) %>% 
  transform(lnDprice=log(price) - log(lag(price)))  %>% select(Date,asset,lnDprice) %>% 
  spread(asset, lnDprice) %>% 
  filter(complete.cases(.))
#Drop existing returns matrix and write to it back to the DB
marketDB$con %>% db_drop_table(table='imputedReturnsMatrix')
copy_to(marketDB, tmp, "imputedReturnsMatrix", temporary = FALSE)  



##############################################################################
################ OPTION DATA LOADERS FOR VARIOUS OPTION PROVIDERS ############
##############################################################################
## JAN  FEB  MAR	APR	MAY	JUN	JUL	AUG	SEP	OCT	NOV	DEC
# Calls	A	B	C	D	E	F	G	H	I	J	K	L
# Puts	M	N	O	P	Q	R	S	T	U	V	W	X

#' @name
#' @return tidyDataFrame
#' @import readr
#' @export
setwd("/Users/mark/OneDrive - Arêté Group SA/Option Trading/data/")
filePath <- "IVolatilty_ROG.csv"
readIVolatityFormat <- function(filePath){
  dat <- read_csv(filePath)
  dat
}
tmp <- readIVolatityFormat(filePath)
optionDB <- dbConnect(RMySQL::MySQL(),"OPTION_DATA",host = "thompson.internet-box.ch", user = "mark",password = "june11")
#copy_to( optionDB, tmp, "ivolatility", temporary = FALSE)                 # create table
db_insert_into( con = optionDB, table = "ivolatility", values = tmp) # insert into


filePath <- "IVolatilty_ROG.csv"
readICEFormat <- function(filePath){
  dat <- read_csv(filePath)
  dat
}
tmp <- readICEFormat(filePath)
optionDB <- dbConnect(RMySQL::MySQL(),"OPTION_DATA",host = "thompson.internet-box.ch", user = "mark",password = "june11")
copy_to( optionDB, tmp, "ivolatility", temporary = FALSE)                 # create table
#db_insert_into( con = optionDB, table = "ivolatility", values = tmp) # insert into





# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
blackscholes <- function(S, X, rf, T, sigma) {
  values <- c(2)

  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)

  values
}

blackscholes(170.00,264.3,0,0,.18)

mean(c( 97.160,92.160))
