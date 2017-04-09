#' Compute Market Capitalization
#'
#' This function returns market capitalizations for a vector of stocks
#' @param shareCount : count of shares
#'
#' @param sharePrice : price per share
#'
#' @return Returns a market capitalization or a vector of capitalizations
#'
#' @export
compute_market_capitalization <- function(shareCount,sharePrice){
  marketCapitalization <- shareCount*sharePrice
}

#' Print "Compute Index Weights"
#'
#' This function computes the weights of the index constituents based on the market capitalization based on the floats
#'
#'@param marketCapitalizations : a vector of market capitalizations of the index's constituents
#'
#'@return weights : a vector of weights for the index
#'
#'@export
compute_index_weights <- function(marketCapitalizations){
  weights <- marketCapitalizations/sum(marketCapitalizations)
  weights
}


#' Print "Compute Index Weights"
#'
#' This function computes the weights of the index constituents based on the market capitalization based on the floats
#'
#'@param indexWeights : a vector of market capitalizations of the index's constituents
#'
#'@param constituentVolatilities
#'
#'@param indexCovariance
#'
#'@return indexVolatility : a volatility for the index
#'
#'@export

compute_index_volatility <- function(indexWeights,constituentVolatilities,indexCovariance){
    indexVolatility <- sqrt(t(indexWeights)%*%indexCovariance%*%indexWeights)
    indexVolatility
}


#compute_constituent_volatilities <- function(indexWeights,indexVolatility,indexCovariance){
#}


#temp <- cbind(c(1,2,3,4),rnorm(4,0,1))
