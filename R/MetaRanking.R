#' Implementation of MetaRanking function for Multi-Criteria Decision Making Problems.
#'
#' @description The \code{MetaRanking} function internally calls functions  \code{MMOORA}, \code{RIM}, \code{TOPSISLinear}, \code{TOPSISVector}, \code{VIKOR} and \code{WASPAS} and then calculates a sum of the their rankings and an aggregated ranking by applying the \code{RankAggreg} package.
#' @param decision The decision matrix (\emph{m} x \emph{n}) with the values of the \emph{m} alternatives, for the \emph{n} criteria.
#' @param weights A vector of length \emph{n}, containing the weights for the criteria. The sum of the weights has to be 1.
#' @param cb A vector of length \emph{n}. Each component is either \code{cb(i)='max'} if the \emph{i-th} criterion is benefit or \code{cb(i)='min'} if the \emph{i-th} criterion is a cost.
#' @param lambda A value in [0,1]. It is used in the calculation of the W index for WASPAS method.
#' @param v A value in [0,1]. It is used in the calculation of the Q index for VIKOR method.
#' @param AB A matrix (\emph{2} x \emph{n}). AB[1,] corresponds with the A extrem, and AB[2,] represents the B extrem of the domain of each criterion.
#' @param CD A matrix (\emph{2} x \emph{n}). CD[1,] corresponds with the C extrem, and CD[2,] represents the D extrem of the ideal reference of each criterion.
#' @return \code{MetaRanking} returns a data frame which contains the rankings of the Multi-MOORA, RIM, TOPSISLinear, TOPSISVector, VIKOR, WASPAS Methods and the both MetaRankings of the alternatives.
#' @examples
#'
#'  d <- matrix(c(1,2,5,3000,3750,4500),nrow = 3,ncol = 2)
#'  w <- c(0.5,0.5)
#'  cb <- c('min','max')
#'  lambda <- 0.5
#'  v <- 0.5
#'  AB <- matrix(c(1,5,3000,4500),nrow = 2,ncol=2)
#'  CD <- matrix(c(1,1,4500,4500),nrow = 2,ncol=2)
#'  MetaRanking(d,w,cb,lambda,v,AB,CD)

MetaRanking <- function(decision, #matrix with all the alternatives
                        weights,  #vector with the numeric values of the weights
                        cb,       #vector with the "type" of the criteria (benefit = "max", cost = "min")
                        lambda,   #value with the real number of the 'lambda' parameter to calculate W
                        v,       #value with the real number of the 'v' parameter to calculate Q
                        AB, #matrix with the range [A,B] of the universe of discourse
                        CD #matrix with the Reference Ideal [C,D]
)
{

  #Multi-MOORA method
  MMoora = MMOORA(decision,weights,cb)

  #RIM
  Rim = RIM(decision, weights, AB, CD)

  #TOPSIS method
  TopsisV = TOPSISVector(decision,weights,cb)
  TopsisL = TOPSISLinear(decision,weights,cb)

  #VIKOR method
  Vikor = VIKOR(decision,weights,cb,v)

  #WASPAS method
  Waspas = WASPAS(decision,weights,cb,lambda)

  #Meta-Ranking
  if(Vikor[1,5] == "-"){
    MetaR = MMoora[,8]+Rim[,3]+TopsisV[,3]+TopsisL[,3]+Waspas[,5]
  }else{
    MetaR = MMoora[,8]+Rim[,3]+TopsisV[,3]+TopsisL[,3]+Vikor[,5]+Waspas[,5]
  }
  

  #Ranking Aggregated
  if(Vikor[1,5] == "-"){
    ra = rbind(MMoora[,8],Rim[,3],TopsisV[,3],TopsisL[,3],Waspas[,5])
  }else{
    ra = rbind(MMoora[,8],Rim[,3],TopsisV[,3],TopsisL[,3],Vikor[,5],Waspas[,5])
  }
  
  if(nrow(decision)<=10){
    RA = RankAggreg::BruteAggreg(ra, nrow(decision), distance="Spearman")
  }else{
    RA = RankAggreg::RankAggreg(ra, nrow(decision), method = "GA", distance = "Spearman", verbose=FALSE)
  }
  return(data.frame(Alternatives = 1:nrow(decision), MMOORA = MMoora[,8], RIM = Rim[,3], TOPSISVector = TopsisV[,3],
                    TOPSISLinear = TopsisL[,3], VIKOR = Vikor[,5], WASPAS = Waspas[,5],
                    MetaRanking_Sum = rank(MetaR, ties.method= "first"), MetaRanking_Aggreg = RA$top.list))

}
