context("Index Option Volatility")


T <- 10000000
y <- exp(rnorm(T))
x <- y*.9 + exp(rnorm(T))
z <- y*.3 + x*.6 + exp(rnorm(T))
SIGMA <- cov(cbind(x,y,z))
weights <- c(.1,.3,.6)
vol <- sqrt(weights%*%SIGMA%*%weights)

test_that("Volworks",{

  expect_equal(vol,compute_index_volatility(indexWeights=weights,indexCovariance=SIGMA),info="Vol calculation correct.")

})

#prices <- apply(dat[,which(grepl(".Close$",names(dat)) == 1 )],2,mean,na.rm=TRUE)
#prices <- prices[-8]
#weights <-  prices / sum(prices)
#weights <- weights[order(weights)]

#test_that("Weights ",{}
