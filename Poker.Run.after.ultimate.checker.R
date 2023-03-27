#random number generator of which cards to draw out
rng <- sample(1:52, 5 + 2*n, replace=F)

total <- deck[,rng]

player <- function(number) {
  return(total[,c(2*number-1, 2*number)])
}

river <- function(stage) {
  return(total[,c(5+2*n-stage+1)])
}

#decision-making system

strategy <- function() {
  decision <- vector()
  for (k in 1:n) {
    dec <- sample(1:4, 1, replace=TRUE)
    if(dec>1){
      decision[k] <- TRUE
    } else {
      decision[k] <- FALSE
    }
  }
  assign('decision', decision, envir = .GlobalEnv) #to make the decision variable accessible via the global environment
}

#removing players who folded

remove <- function() {
  for (i in 1:n) {
    if (is.na(hand_river[[i]]) == F && decision[i] == T) {
      hand_river[[i]] <- hand_river[[i]]
    }
    else {
      hand_river[[i]] <- NA 
    }
  }
  assign('hand_river', hand_river, envir = .GlobalEnv) #replacing the old hand_river in the global envir with the new hand_river with removed players
}

#check if the game should continue

stage_checker <- function() {
  if(sum(is.na(hand_river)) == n-1 | sum(is.na(hand_river)) == n) {
    print(hand_river)
    stop('Game Ended')
  } #if all but 1 player is left over in the game, the game is terminated
}

{ #such that the stop() function will work
  #initial card hand-out
  hand_river <- list()
  
  for (i in 1:n) {
    hand_river[[i]] <- player(i)
  }
  
  strategy() #first decision, so use function to generate a new decision variable
  
  remove()
  
  stage_checker()
  
  #flop (first 3 cards)
  for (i in 1:n) {
    if (is.na(hand_river[[i]][1]) == F) {
      hand_river[[i]] <- cbind(player(i), river(c(1,2,3)))
    }
  }
  
  strategy()
  
  remove()
  
  stage_checker()
  
  #turn (+1 card)
  for (i in 1:n) {
    if (is.na(hand_river[[i]][1]) == F) {
      hand_river[[i]] <- cbind(player(i), river(c(1,2,3,4)))
    }
  }  
  
  strategy()
  
  remove()
  
  stage_checker()
  
  #river (all 5 cards shown)
  for (i in 1:n) {
    if (is.na(hand_river[[i]][1]) == F) {
      hand_river[[i]] <- cbind(player(i), river(c(1,2,3,4,5)))
    }
  }
  
  print(hand_river)
  stop('Game Ended')
}

if (length(which(hand_river == 'NA')) < 3) {
  player_points <- vector()
  for (i in 1:n) {
    if (is.na(hand_river[[i]][1]) == F) {
      player_points[i] <- ultimate_checker(hand_river[[i]])
    }
  }
  winner <- which(player_points == max(player_points, na.rm = T))
} else {
  winner <- which(hand_river != 'NA')
}

for (i in 1:n) {
  if (winner %contain% i) {
    results[[i]] <- results[[i]] + 1
  }
}