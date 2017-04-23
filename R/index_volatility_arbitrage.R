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
  f <- paste(paste(indexTicker), paste("-1",paste(names(returnMatrix)[which(names(returnMatrix)!=indexTicker)],collapse="+"),sep="+"),sep="~")
  f <- as.formula(f)
  weightsModel <- lm(f,data=returnMatrix)
  #Return the coefficients
  coef(weightsModel)
}

#' print computeIndexIVfromConstituentIV returns the IV for the index were all the constituents be taken into account
#' 
#' @param weights
#' 
#' @param optionCovariance
#' 
#' @param constituentIV
#' 
#' @param timeScale : an integer comensurate with yearly IV
#' 
#' @export 
computeIndexIVfromConstituentIV <- function(weights,optionCovariance,constituentIV,timeScale){
  oC <- optionCovariance
  #Replaces the realized volatility with the actual IV of the instruments
  diag(oC) <- sqrt(sqrt(diag(optionCovariance))*sqrt(timeScale))
  IndexIV  <- sqrt(weights%*%optionCovariance%*%t(weights))*sqrt(timeScale)
  IndexIV
}


#' Print "Compute Index Weights"
#'
#' This function calculates the theorical volatility of the index based on constituent covariance
#'
#'@param indexWeights : a vector of market capitalizations of the index's constituents
#'
#'@param constituentVolatilities
#'
#'@param indexCovariance
#'
#'@param timeScale : an integer so the values can be scaled to an annualized basis. For the typical trading calendary in the US, this would be 12 for months, days, 252, hours, 1638 for hours, 98280 for minutes
#'
#'@return indexVolatility : a volatility for the index
#'
#'@export
computeConstituentIVfromIndexIV <- function(indexWeights,indexIV,indexCovariance,timeScale){
    indexIV = j
  
    indexVolatility <- sqrt(indexWeights%*%indexCovariance%*%t(indexWeights))
    indexVolatility <- sqrt(scale)*indexVolatility 
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
