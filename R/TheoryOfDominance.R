TheoryOfDominance <- function (Rrs,Rrp,Rm,decision) {
  #Theory of Dominance
  Dominance <- cbind(Rrs,Rrp,Rm,rep(0, nrow(decision)))
  MMRanking <- rep(0, nrow(decision))
  flag = 0
  for (i in 1:nrow(decision)){

    PosRrs = which.min(Dominance[,1])
    PosRrp = which.min(Dominance[,2])
    PosRm = which.min(Dominance[,3])
    
    #checking if it is "Overall Dominance" or "General Dominance in two of the three methods" 
    if ( (PosRrs == PosRrp) && (PosRrp == PosRm) ){
      if ( Dominance[PosRrs,4] != -1 ){
        MMRanking[PosRrs] = i
        Dominance[PosRrs,4] = -1
        Dominance[PosRrs,1:3] = 999999999999
      } else {
        flag =1
      }
    } else if( PosRrs == PosRrp ){
      if ( Dominance[PosRrs,4] != -1 ){
        MMRanking[PosRrs] = i
        Dominance[PosRrs,4] = -1
        Dominance[PosRrs,1:3] = 999999999999
      } else {
        flag =1
      }
    } else if( PosRrp == PosRm ){
      if ( Dominance[PosRrp,4] != -1 ){
        MMRanking[PosRrp] = i
        Dominance[PosRrp,4] = -1
        Dominance[PosRrp,1:3] = 999999999999
      } else {
        flag =1
      }
    } else if( PosRrs == PosRm ){
      if ( Dominance[PosRrs,4] != -1 ){
        MMRanking[PosRrs] = i
        Dominance[PosRrs,4] = -1
        Dominance[PosRrs,1:3] = 999999999999
      } else {
        flag =1
      }
    } else { # Not the case of "Overall Dominance" neither "General Dominance in two of the three methods"
      # Cheking which dominates
      Comparison <- rbind(Dominance[PosRrs,],Dominance[PosRrp,],Dominance[PosRm,])
      resta1 = Comparison[1,] - Comparison[2,]
      resta2 = Comparison[2,] - Comparison[3,]
      resta3 = Comparison[3,] - Comparison[1,]
      #Dominance of Comparison[1,] and Comparison[2,]
      dom = rep(0,3)
      if ( (resta1[1]<0 && resta1[2]<0) || ( (resta1[2]<0 && resta1[3]<0) || (resta1[1]<0 && resta1[3]<0)) ) {
        dom[1] = 1
      } else {
        dom[1] = 2
      }
      #Dominance of Comparison[2,] and Comparison[3,]
      if ( (resta2[1]<0 && resta2[2]<0) || ( (resta2[2]<0 && resta2[3]<0) || (resta2[1]<0 && resta2[3]<0)) ) {
        dom[2] = 2
      } else {
        dom[2] = 3
      }
      #Dominance of Comparison[3,] and Comparison[1,]
      if ( (resta3[1]<0 && resta3[2]<0) || ( (resta3[2]<0 && resta3[3]<0) || (resta3[1]<0 && resta3[3]<0)) ) {
        dom[3] = 3
      } else {
        dom[3] = 1
      }
      
      if (dom[1] == dom[2]) { #Comparison[2,] dominates
        if( Dominance[PosRrp,4] != -1 ){
          MMRanking[PosRrp] = i
          Dominance[PosRrp,4] = -1
          Dominance[PosRrp,1:3] = 999999999999
        }
      } else if (dom[2] == dom[3]) { #Comparison[3,] dominates
        if( Dominance[PosRm,4] != -1 ){
          MMRanking[PosRm] = i
          Dominance[PosRm,4] = -1
          Dominance[PosRm,1:3] = 999999999999
        }
      } else if (dom[1] == dom[3]) { #Comparison[1,] dominates
        if( Dominance[PosRrs,4] != -1 ){
          MMRanking[PosRrs] = i
          Dominance[PosRrs,4] = -1
          Dominance[PosRrs,1:3] = 999999999999
        }
      } else if ( (dom[1] != dom[2]) && (dom[2] != dom[3]) ){
        
        cuales = rep(0,3)
        if( (PosRrs < PosRrp) && (PosRrs < PosRm) ){
          cuales[1]=1
          Comparison2 <- rbind(Dominance[PosRrs,])
        } else if ( (PosRrp < PosRrs) && (PosRrp < PosRm) ){
          cuales[2]=1
          Comparison2 <- rbind(Dominance[PosRrp,])
        } else if ( (PosRm < PosRrs) && (PosRm < PosRrp) ){
          cuales[3]=1
          Comparison2 <- rbind(Dominance[PosRm,])
        }
        Comparison2 <- rbind(Dominance[PosRrs,],Dominance[PosRrp,])
        resta4 = Comparison2[1,] - Comparison2[2,]
        #Dominance of Comparison[1,] and Comparison[2,]
        dom = rep(0,2)
        if ( (resta4[1]<0 && resta4[2]<0) || ( (resta4[2]<0 && resta4[3]<0) || (resta4[1]<0 && resta4[3]<0)) ) {
          dom[1] = 1
        } else {
          dom[1] = 2
        }
        if (dom[1] == 1) { #Comparison2[1,] dominates
          
          if (cuales[1]==1){ 
            if( Dominance[PosRrs,4] != -1 ){
              MMRanking[PosRrs] = i
              Dominance[PosRrs,4] = -1
              Dominance[PosRrs,1:3] = 999999999999
            }            
          } else { 
            if( Dominance[PosRrp,4] != -1 ){
              MMRanking[PosRrp] = i
              Dominance[PosRrp,4] = -1
              Dominance[PosRrp,1:3] = 999999999999
            }
          }
          
        } else { #Comparison2[1,] dominates
          
          if (cuales[2]==1){
            if( Dominance[PosRrp,4] != -1 ){
              MMRanking[PosRrp] = i
              Dominance[PosRrp,4] = -1
              Dominance[PosRrp,1:3] = 999999999999
            }            
          } else { 
            if( Dominance[PosRm,4] != -1 ){
              MMRanking[PosRm] = i
              Dominance[PosRm,4] = -1
              Dominance[PosRm,1:3] = 999999999999
            }
          }
        }
      }
            
    }
    #Checking the final ranking
    if (flag == 1){
      
      PosRrs = which.min(Dominance[,1])
      PosRrp = which.min(Dominance[,2])
      PosRm = which.min(Dominance[,3])
      
      
      if ( (PosRrs == PosRrp) && (PosRrp == PosRm) ){
        if ( Dominance[PosRrs,4] != -1 ){
          MMRanking[PosRrs] = i
          Dominance[PosRrs,4] = -1
          Dominance[PosRrs,1:3] = 999999999999
        } else {
          flag =1
        }
      } else if( PosRrs == PosRrp ){
        if ( Dominance[PosRrs,4] != -1 ){
          MMRanking[PosRrs] = i
          Dominance[PosRrs,4] = -1
          Dominance[PosRrs,1:3] = 999999999999
        } else {
          flag =1
        }
      } else if( PosRrp == PosRm ){
        if ( Dominance[PosRrp,4] != -1 ){
          MMRanking[PosRrp] = i
          Dominance[PosRrp,4] = -1
          Dominance[PosRrp,1:3] = 999999999999
        } else {
          flag =1
        }
      } else if( PosRrs == PosRm ){
        if ( Dominance[PosRrs,4] != -1 ){
          MMRanking[PosRrs] = i
          Dominance[PosRrs,4] = -1
          Dominance[PosRrs,1:3] = 999999999999
        } else {
          flag =1
        }
      } else { 
        Comparison <- rbind(Dominance[PosRrs,],Dominance[PosRrp,],Dominance[PosRm,])
        resta1 = Comparison[1,] - Comparison[2,]
        resta2 = Comparison[2,] - Comparison[3,]
        resta3 = Comparison[3,] - Comparison[1,]
        #Dominance of Comparison[1,] and Comparison[2,]
        dom = rep(0,3)
        if ( (resta1[1]<0 && resta1[2]<0) || ( (resta1[2]<0 && resta1[3]<0) || (resta1[1]<0 && resta1[3]<0)) ) {
          dom[1] = 1
        } else {
          dom[1] = 2
        }
        #Dominance of Comparison[2,] and Comparison[3,]
        if ( (resta2[1]<0 && resta2[2]<0) || ( (resta2[2]<0 && resta2[3]<0) || (resta2[1]<0 && resta2[3]<0)) ) {
          dom[2] = 2
        } else {
          dom[2] = 3
        }
        #Dominance of Comparison[3,] and Comparison[1,]
        if ( (resta3[1]<0 && resta3[2]<0) || ( (resta3[2]<0 && resta3[3]<0) || (resta3[1]<0 && resta3[3]<0)) ) {
          dom[3] = 3
        } else {
          dom[3] = 1
        }
        
        if (dom[1] == dom[2]) { #Comparison[2,] dominates
          if( Dominance[PosRrp,4] != -1 ){
            MMRanking[PosRrp] = i
            Dominance[PosRrp,4] = -1
            Dominance[PosRrp,1:3] = 999999999999
          }
        } else if (dom[2] == dom[3]) { #Comparison[3,] dominates
          if( Dominance[PosRm,4] != -1 ){
            MMRanking[PosRm] = i
            Dominance[PosRm,4] = -1
            Dominance[PosRm,1:3] = 999999999999
          }
        } else if (dom[1] == dom[3]) { #Comparison[1,] dominates
          if( Dominance[PosRrs,4] != -1 ){
            MMRanking[PosRrs] = i
            Dominance[PosRrs,4] = -1
            Dominance[PosRrs,1:3] = 999999999999
          }
        } 
        
      }
      
    }

    flag = 0
  }
  
  return(MMRanking)
}