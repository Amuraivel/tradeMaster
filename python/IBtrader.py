import time
from ezibpy import ezibpy
from IBtrader import *

####CONNECTION######
# initialize ezIBpy
# Add your host and ID here
ibConn = ezibpy.ezIBpy();
ibConn.connect(clientId=9999, host="127.0.0.1", port=7496)
# subscribe to account/position updates
ibConn.requestPositionUpdates(subscribe=True)
ibConn.requestAccountUpdates(subscribe=True)
# wait 30 seconds until data comes in
time.sleep(15)
# available variables (auto-updating)
print("Account Information")
print(ibConn.account)


####
# --> ASK PORTFOLIO WHICH MKT DATA


# --> ASK STRATEGY WHICH DATA

# --> RETURN PM
pm = PortfolioManagement(1)
pm.callPortfolio()
# --> RETURN SM
###

###CREATE CONTRACTS TO POLL
# fut_contract = ibConn.createFutureContract("ES", expiry="201606")
# csh_contract = ibConn.createCashContract("EUR", currency="USD")
#
contract = ibConn.createStockContract("SPY", "USD", "SMART")

# opt_contract = ibConn.createOptionContract("AAPL", expiry="20160425", strike=105.0, otype="PUT")
# nasdaq = ibConn.createContract(("NQ", "FUT", "GLOBEX", "USD", "201609", 0.0,""));
# ticker = "AAPL"
# stk_contract = ibConn.createStockContract(ticker);
###


####POLL MARKET DATA#########
print("Now polling market data")
# Crude method to poll market data for testing
t = 0;
while t < 0:
    # p
    ibConn.requestMarketData()

    #####HAND OFF MARKET DATA TO PORTFOLIO####
    # --> SEND MARKET DATA TO PM
    # --> SEND MARKET DATA TO STRATEGY
    ##

    # IF price less than last price, then fill

    a = ibConn.marketData
    mktDataHFC = a[2];
    formatter = "%r"
    ask = mktDataHFC['ask'];
    bid = mktDataHFC['bid'];

    time.sleep(10)
    t = t + 1;
print("Updating orders")
#######RECEIVES A DICTIONARY OF ORDERS AND ADJUSTMENTS
#
# --> SEND ORDERS TO IB
###DO XYZ IN IB########

order = ibConn.createOrder(quantity=1)
orderId = ibConn.placeOrder(contract, order)

# let order fill
time.sleep(1)
print("Waiting for IB to acknowledge...")
####OBSERVE CHANGES#####
# --> WAIT FOR STATUS
# --> DO SOMETHING DEPENDING ON STATUS
####

# see the positions
print("Positions")
print(ibConn.positions)

# disconnect
ibConn.disconnect()
print("IB Disconnected")


