import QuantLib as ql # version 1.5
import matplotlib.pyplot as plt
# from IBtrader import Asset

#class OptionPricer(object):
 #   def __init__(self, riskAversion):
  #      self.riskFreeRate = 0.005
   #     return self

def calculateGreeksFromRawValue(spot,strike,date,expiration,putCall,bid,ask):
    # option data
    American = False
    spot_price = spot  # 127.62
    strike_price = strike  # 130
    calculation_date = ql.Date(int('{:%d}'.format(date)), int('{:%m}'.format(date)), int('{:%Y}'.format(date)))
    maturity_date = ql.Date(int('{:%d}'.format(expiration)), int('{:%m}'.format(expiration)), int('{:%Y}'.format(expiration)))

    #print("Date is ",calculation_date)
    #print("Expiration is ", maturity_date)


    volatility = 0.20 # the historical vols for a year
    dividend_rate =  0.0163
    option_type = ql.Option.Call
    if putCall != "C":
        option_type = ql.Option.Put
    risk_free_rate = 0.001
    day_count = ql.Actual365Fixed()
    calendar = ql.UnitedStates()


    ql.Settings.instance().evaluationDate = calculation_date

    # construct the European Option
    payoff = ql.PlainVanillaPayoff(option_type, strike_price)

    spot_handle = ql.QuoteHandle(ql.SimpleQuote(spot_price))
    flat_ts = ql.YieldTermStructureHandle(ql.FlatForward(calculation_date, risk_free_rate, day_count))
    dividend_yield = ql.YieldTermStructureHandle(ql.FlatForward(calculation_date, dividend_rate, day_count))
    flat_vol_ts = ql.BlackVolTermStructureHandle(ql.BlackConstantVol(calculation_date, calendar, volatility, day_count))

    exercise = ql.EuropeanExercise(maturity_date) #AmericanExercise(calculation_date,maturity_date) #(,maturity_date)
    process = ql.BlackScholesMertonProcess(spot_handle,
                                               dividend_yield,
                                               flat_ts,
                                               flat_vol_ts)
    engine = ql.AnalyticEuropeanEngine(process)

    if (American == True):
        exercise = ql.AmericanExercise(calculation_date, maturity_date)
        time_steps = 100
        grid_points = 100
        engine = ql.FDAmericanEngine(process) # (process, time_steps, grid_points)


    option = ql.VanillaOption(payoff, exercise)

    option.setPricingEngine(engine)


    dict = {'IV':0.0,'DTE':day_count.dayCount(calculation_date,maturity_date), 'theoreticalPrice':option.NPV(),'delta':option.delta(),'theta':option.theta() }


    try:
        mid = (ask+bid)/2

        if ((putCall == "P") and ((strike - spot) > mid)):
            print("More extrinsic than price, setting mid to extrinsic + 0.01")
            extrinsic = max([(strike - spot)+dict['DTE']*.01,bid,ask])
            mid = extrinsic


        #Error Catching for odd prices
        #if ((mid/option.NPV() > 2) or (option.NPV()/mid) > 2):
         #   iv = 0.0
        #else:
        iv = option.impliedVolatility(mid,process,minVol=1.0e-10,accuracy=1.0e-2,maxVol=1.0e+10)
        dict['IV'] = iv
    except ValueError:
        print("Oops! the option price is out of reasonable bounds -- check the value{}".format(bid))


    return dict








