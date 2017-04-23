    ###
#THIS MODULE DUMPS:
#1. DUMPS DATA INTO A DATABASE or FILE
#2. PULLS DATA FROM ANY SOURCE FOR PM or SM
#
from quandl import *
from numpy import *
from pandas import *
import mysql.connector
import optionpricer
from pony.orm import *

class DataManagement:


cnx = mysql.connector.connect(user='mark',password='june11',host='server.local',db='OPTION_DATA')
cny = mysql.connector.connect(user='mark', password='june11', host='server.local', db='OPTION_DATA')
cursor = cnx.cursor()
updateCursor = cny.cursor()

#, `unadjusted stock price` AS price, STR_TO_DATE(date, '%m/%d/%y') AS date
query = ("SELECT `option symbol` AS ticker, strike, `unadjusted stock price` AS price, STR_TO_DATE(date, '%m/%d/%y') AS date, STR_TO_DATE(expiration, '%m/%d/%y') AS expiration,`call/put` AS callPut, bid, ask FROM ivolatility WHERE volume > 0 AND date='03/21/16' AND `open interest` > 0 AND (( strike/`unadjusted stock price` >= .9 AND `call/put` = 'C' ) OR (strike/`unadjusted stock price` <= 1.1 AND `call/put` = 'P' ))")
cursor.execute(query)
for (ticker, strike, price,date,expiration,callPut,bid,ask) in cursor:
    print("{} with k={} and s={} date={} expiration={} {} bid={} ask={}".format(ticker, strike,  price, date,expiration, callPut,bid,ask))#bid={} ask={} , bid, ask
    greeks = optionpricer.calculateGreeksFromRawValue(float(price),float(strike),date,expiration,callPut,float(bid),float(ask))
    dt = "{:%m}/{:%d}/{:%y}".format(date,date,date)
    print(greeks)
    print(dt)
    updateCursor.execute("UPDATE ivolatility SET delta = %s WHERE `option symbol` = %s AND date = %s ", (greeks['delta'], ticker, dt))
    cny.commit()
cursor.close()
cny.close()
cnx.close()

"""
import pymysql.cursors
# Connect to the database
connection = pymysql.connect(host='server.local',
                             user='mark',
                             password='june11',
                             db='OPTION_DATA',
                             charset='utf8',
                             cursorclass=pymysql.cursors.DictCursor)
try:
    with connection.cursor() as cursor:
        sql = "SELECT `option symbol` AS ticker, `unadjusted stock price` AS price, STR_TO_DATE(date, '%m/%d/%y') AS date FROM ivolatility WHERE date='03/21/16'"
        cursor.execute(sql)
        result = cursor.fetch()
        print(result)
finally:
    connection.close()





try:
    with connection.cursor() as cursor:
        # Create a new record
        sql = "INSERT INTO `users` (`email`, `password`) VALUES (%s, %s)"
        cursor.execute(sql, ('webmaster@python.org', 'very-secret'))

    # connection is not autocommit by default. So you must commit to save
    # your changes.
    connection.commit()

    with connection.cursor() as cursor:
        # Read a single record
        sql = "SELECT `id`, `password` FROM `users` WHERE `email`=%s"
        cursor.execute(sql, ('webmaster@python.org',))
        result = cursor.fetchone()
        print(result)
finally:
    connection.close()
"""




# Create Various Manipulator Objects
class DataBaseManipulator:
    def __init__(self,data_directory):
        self.__data_directory = data_directory
        print(self.__data_directory)

    def build_index_db(self,index_dictionary):
        # Construct an empty database
        db = Database()
        # BIND
        file = self.__data_directory
        file += 'database_file.sqlite'
        print(file)
        db.bind('sqlite', file, create_db=True)

        # Constructs an equity index table
        class EquityIndex(db.Entity):
            ticker = Required(str)
            index = Required(str)

        show(EquityIndex)
        db.generate_mapping(create_tables=True)

        with db_session:
            c1 = EquityIndex(ticker=list(index_dictionary.keys())[0], index=list(index_dictionary.values())[0])
            # Done automagically
            commit()
            constituents = select(p for p in EquityIndex)

dbm = DataBaseManipulator(data_directory="/Users/mark/Programming/tradeMaster/data/")
print(dbm)
d = {'AAPL': 'DOW'}
print(list(d.keys())[0])
dbm.build_index_db({'AAPL': 'DOW'})

