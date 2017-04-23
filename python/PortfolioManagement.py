#PortfolioManager
#Copyleft 2016-08-02 Mark James Thompson mark@thompson.ch
###THIS MODULE MONITORS RISK AND CONTROLS STRATEGY LIMITS
#
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects.vectors import StrVector



class PortfolioManagement: 
    def __init__(self,riskAversion):
        self.riskAversion = 1
        '''
        WE USE QUANTLIB to price options
        This is a work around for MAC OS, but it also allows sophisticated portfolio management in R
        instead of using QuantLib Directly which would be best
        '''
        self.QuantLib = importr("RQuantLib")

        #print("Stock price is: {}\nOption price is = {}\n Delta={}".format(px,round(BSoption.callPrice,2),round(BSoption._delta()[1],2)));\ '
    def callPortfolio(self):
        self.QuantLib
        print("Portfolio Called")
    

###Manages AND DETERMINES CURRENT PORTFOLIO RISK
#Exposure

#Deltas

#Gamma

#Vega


####


###SENDS RISK BUDGET TO STRATEGIES###
#--> SEND LIMITS TO SM
###



#optionValue = QuantLib.EuropeanOption(type="call", underlying=100, strike=100, dividendYield=0.01,
 #   riskFreeRate=0.03, maturity=0.5, volatility=0.2)
#print(optionValue)


'''
#Grab the historical series
#HFC = Quandl.get("YAHOO/HFC");
#HFCreturns = HFC['Close'].pct_change()
standardDeviation = .44#np.std(HFCreturns)*sqrt(252);
print(standardDeviation);
#[underlyingPrice, strikePrice, interestRate, daysToExpiration]

smiData = Quandl.get("WFE/INDEXES_SIXSWISSEXCHANGESMI");#Gets the SMI

'''

