TheoryOfDominance <- function (Rrs,Rrp,Rm,decision) {
  # Function that implements the theory of dominance for Multi-MOORA method.
  # It requieres the ranking from the ration system, reference point,
  # the full multiplicative form and the decision matrix.
  Dominance <- cbind(Rrs,Rrp,Rm,seq(1, nrow(decision)))

  for(i in 1:nrow(decision)){
    for(j in i:nrow(decision)){
      resta = Dominance[i,] - Dominance[j,]
      # It is calculated the dominance of the i over j
      if ( (resta[2]<0 && resta[3]<0) || ( (resta[3]<0 && resta[4]<0) || (resta[2]<0 && resta[4]<0)) ) {
        dom = 1 # i domains j
      } else { # j domains i
        aux=Dominance[j,]
        # Change the values between i and j
        Dominance[j,]=Dominance[i,]
        Dominance[i,]=aux
      }
    }
  }
  return(Dominance[,4])
}
