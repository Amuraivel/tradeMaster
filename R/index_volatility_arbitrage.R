#' Compute Index Weights
#' This function returns the weights of the index constituents by empirically estimating them using a linear model.
#' It presumes that the index is perfectly arbitraged. It presumes there are enough observations since the last change
#' in the index constituents or weightings.
#'
#' @param returnMatrix : A well-formed matrix of returns.
#'
#' @param indexTicker : A named column for the index returns
#'
#' @return weights : a vector of weights empirically estimated from the return matrix, or be returned using
#'
#' @notes Future implementations could incorporate an API call to an external data-source to compute the weights using market cap
#'
#' @export
get_index_weights <- function(indexTicker,returnMatrix){
  #Linear regression
  f <- paste(paste(indexTicker), paste("-1",paste(names(tsDat)[which(names(tsDat)!=indexTicker)],collapse="+"),sep="+"),sep="~")
  f <- as.formula(f)
  weightsModel <- lm(f,data=returnMatrix)
  #Return the coefficients
  coef(weightsModel)
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

compute_index_volatility <- function(indexWeights,indexCovariance){
    indexVolatility <- sqrt(t(indexWeights)%*%indexCovariance%*%indexWeights)
    indexVolatility
}

#' Print "Compute Index Weights"
#'
#' This function computes the weights of the index constituents based on the market capitalization based on the floats
#'
#'@param indexWeights : a vector of market capitalizations of the index's constituents
#'
#'@param indexCovariance : a covariance matrix for the index
#'
#'@param asset : a number or label
#'
#'@return indexVolatility : returns the vol
#'
#'@export
compute_constituent_volatilities <- function(indexWeights,indexCovariance,asset){

}


#temp <- cbind(c(1,2,3,4),rnorm(4,0,1))
