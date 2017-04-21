from pony.orm import *


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

