#' Implementation of RIM Method for Multi-Criteria Decision Making Problems.
#'
#' @description The \code{RIM} function implements the Reference Ideal Method (RIM).
#' @param decision The decision matrix (\emph{m} x \emph{n}) with the values of the \emph{m} alternatives, for the \emph{n} criteria.
#' @param weights A vector of length \emph{n}, containing the weights for the criteria. The sum of the weights has to be 1.
#' @param AB A matrix (\emph{2} x \emph{n}). AB[1,] corresponds with the A extrem, and AB[2,] represents the B extrem of the domain of each criterion.
#' @param CD A matrix (\emph{2} x \emph{n}). CD[1,] corresponds with the C extrem, and CD[2,] represents the D extrem of the ideal reference of each criterion.
#' @return \code{RIM} returns a data frame which contains the score of the R index and the ranking of the alternatives.
#' @references Cables, E.; Lamata, M.T.; Verdegay, J.L. RIM-reference ideal method in multicriteria decision making. Information Science, 337-338, 1-10, 2016.
#' @examples
#'
#'  d <- matrix(c(30,40,25,27,45,0,9,0,0,15,2,1,3,5,2,3,3,1,3,2,3,2,3,3,3,2,2,2,1,4),
#'  nrow = 5, ncol = 6)
#'  w <- c(0.2262,0.2143,0.1786,0.1429,0.119,0.119)
#'  AB = matrix(c(23,60,0,15,0,10,1,3,1,3,1,5),nrow = 2,ncol = 6)
#'  CD = matrix(c(30,35,10,15,0,0,3,3,3,3,4,5),nrow = 2,ncol = 6)
#'  RIM(d,w,AB,CD)

RIM <- function(decision, #matrix with all the alternatives
                weights,  #vector with the numeric values of the weights
                AB, #matrix with the range [A,B] of the universe of discourse
                CD #matrix with the Reference Ideal [C,D]
)
{
  #Checking the arguments
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  if(sum(weights) != 1)
    stop("The sum of 'weights' is not equal to 1")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(ncol(AB) != ncol(decision))
    stop("length of 'AB' does not match the number of the criteria")
  if(ncol(CD) != ncol(decision))
    stop("length of 'CD' does not match the number of the criteria")


  #1. Normalization and weighting
  N <- matrix(nrow = nrow(decision), ncol = ncol(decision))
  for(j in 1:ncol(decision)){
    for(i in 1:nrow(decision)){
      if((decision[i,j] >= CD[1,j]) && (decision[i,j]<= CD[2,j])){
        N[i,j]=1
      }
      else if( ((decision[i,j]>= AB[1,j]) && (decision[i,j]<= CD[1,j])) && (AB[1,j]!=CD[1,j])){
        N[i,j]=1-(min(abs(decision[i,j]-CD[1,j]),abs(decision[i,j]-CD[2,j]))/abs(AB[1,j]-CD[1,j]))
      }
      else if( ((decision[i,j]>= CD[2,j]) && (decision[i,j]<= AB[2,j])) && (CD[2,j]!=AB[2,j])){
        N[i,j]=1-(min(abs(decision[i,j]-CD[1,j]),abs(decision[i,j]-CD[2,j]))/abs(CD[2,j]-AB[2,j]))
      }
      else stop("Error in normalization procedure: if x is in [A,C], then A != C,
                or if x is in [D,B], then D != B")
    }
  }


  W <- diag(weights)
  NW <- N%*%W

  #2. Distances to the ideal solutions
  posDis = c(1:nrow(decision))
  negDis = c(1:nrow(decision))
  for(i in 1:nrow(decision)){
    posDis[i] = sqrt(sum((NW[i,]-weights)^2))
    negDis[i] = sqrt(sum(NW[i,]^2))
  }


  #4. R index
  R <- negDis/(negDis+posDis)

  #5. Rank the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), R = R, Ranking = rank(-R, ties.method= "first")))

}
