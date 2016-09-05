#' Implementation of WASPAS Method for Multi-Criteria Decision Making Problems.
#'
#' @description The \code{WASPAS} function implements the Weighted Aggregated Sum Product ASsessment (WASPAS) Method.
#' @param decision The decision matrix (\emph{m} x \emph{n}) with the values of the \emph{m} alternatives, for the \emph{n} criteria.
#' @param weights A vector of length \emph{n}, containing the weights for the criteria. The sum of the weights has to be 1.
#' @param cb A vector of length \emph{n}. Each component is either \code{cb(i)='max'} if the \emph{i-th} criterion is benefit or \code{cb(i)='min'} if the \emph{i-th} criterion is a cost.
#' @param lambda A value in [0,1]. It is used in the calculation of the W index.
#' @return \code{WASPAS} returns a data frame which contains the score of the WSM, WPM and the Q index and the ranking of the alternatives.
#' @references Zavadskas, E. K.; Turskis, Z.; Antucheviciene, J.; Zakarevicius, A. Optimization of Weighted Aggregated Sum Product Assessment. Electronics and Electrical Engineering, 122(6), 3-6, 2012.
#' @examples
#'
#'  d <- matrix(c(370,314,480,850,11,7,10,16,2.69,2.37,3.09,3.17,2.75,3.27,3.67,4.10,
#'  5,35,30,50,1.63,1.72,1.87,1.91,1.47,2.07,1.38,2.22,7.11,5.60,7.82,8.25,88,12.60,94,
#'  23,410,100,410,65,2.93,2.13,2.87,1.10,1.98,3.21,2.94,4.37),nrow = 4,ncol = 12)
#'  w <- c(0.0626,0.0508,0.1114,0.0874,0.0625,0.1183,0.0784,0.0984,0.053,0.1417,
#'  0.0798,0.0557)
#'  cb <- c('min','min','max','max','max','max','max','max','min','min','max','max')
#'  lambda <- 0.5
#'  WASPAS(d,w,cb,lambda)

WASPAS <- function(decision, #matrix with all the alternatives
                   weights,  #vector with the numeric values of the weights
                   cb,       #vector with the "type" of the criteria (benefit = "max", cost = "min")
                   lambda    #value with the real number of the 'lambda' parameter to calculate W
)
{
  #Checking the arguments
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  if(sum(weights) != 1)
    stop("The sum of 'weights' is not equal to 1")
  if(! is.character(cb))
    stop("'cb' must be a character vector with the type of the criteria")
  if(! all(cb == "max" | cb == "min"))
    stop("'cb' should contain only 'max' or 'min'")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(length(cb) != ncol(decision))
    stop("length of 'cb' does not match the number of the criteria")
  if(missing(lambda))
    stop("a value for 'lambda' in [0,1] should be provided")


  #WASPAS method

  #1. Normalization
  N <- matrix(nrow = nrow(decision), ncol = ncol(decision))

  Norm <- as.integer(cb == "max") * apply(decision, 2, max) +
    as.integer(cb == "min") * apply(decision, 2, min)

  N <- matrix(nrow = nrow(decision), ncol = ncol(decision))
  for(j in 1:ncol(decision)){
    if (cb[j] == 'max'){
      N[,j] <- decision[,j] / Norm[j]
    }
    else{
      N[,j] <- Norm[j] / decision[,j]
    }
  }


  #2. WSM
  W <- diag(weights)
  NW <- N%*%W
  WSM <- apply(NW, 1, sum)

  #3. WPM
  WPM <- matrix(nrow = nrow(decision), ncol = ncol(decision))
  for(j in 1:ncol(decision)){
    WPM[,j] <- N[,j]^weights[j]
  }
  WPM <- apply(WPM, 1, prod)

  #4. Q index
  Q <- (WSM*lambda) + ((1-lambda)*WPM)

  #5. Ranking the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), WSM = WSM, WPM = WPM, Q = Q, Ranking = rank(-Q, ties.method= "first")))

}
