class Asset:
    """Holds an asset class structure"""

    def __init__(self):
        self.assetClass = "FX"


    def f(self):
        return print(self.assetClass)

class Derivative(Asset):

# Option Class
class Option(Derivative):

class Future(Derivative):

class Equity(Asset):
    def __init__(self):



class FX(Asset):

class Bond(Asset):