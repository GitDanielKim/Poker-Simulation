n = 4 #number of players

#full deck
deck <- matrix(, nrow = 2, ncol = 52)
deck[1,] <- rep(1:13, each = 4)
deck[2,] <- rep(1:4, times = 13) 

results <- list()

for (i in 1:n) {
  results[[i]] <- 0
}

"%contain%" <- function(playerhand,combination) { #created contain function that checks whether a vector contains another vector. This is used to check for example, if the player's hand contains a straight combination
  t_ph <- table(playerhand)
  t_comb <- table(combination)
  diff <- t_ph[names(t_comb)] - t_comb
  all(diff >= 0 & !is.na(diff))
}

ultimate_checker <- function(sample_hand) {
  
  #flush
  
  flush <- matrix(, nrow = 4, ncol = 5) #created matrix filled with all possible flushes
  
  for (i in 1:4) {
    flush[i,] <- rep(i, times = 5)
  }
  
  flush_checker <- vector() #check if the sample hand contains any flush
  for (i in 1:4) {
    flush_checker[i] <- any(sample_hand[2,] %contain% flush[i,])
  }
  
  if (any(flush_checker) == T) {
    flush_type <- which(flush_checker) #determines which type of flush it is
    
    flush_hand <- NULL
    for (i in 1:7) {
      if (sample_hand[2,i] == flush_type) {
        flush_hand <- cbind(flush_hand, sample_hand[,i]) #creates a mini-matrix filled with the potential flush hands given the sample hand
      }
    }
    if (any(flush_hand[1,] == 1)) {
      best_flush <- 1 #if there is an ace in the flush, then its the largest
    }
    if (!(any(flush_hand[1,] == 1))) {
      best_flush <- max(flush_hand[1,]) #if there isnt an ace in the flush, then choose largest number
    }
  } else {
    best_flush <- 'No flush'
  }
  
  #straight
  
  straight <- matrix(, nrow = 10, ncol = 5)
  
  i = 5
  while (i <= 13) {
    straight[i-4,] <- (c(i-4, i-3, i-2, i-1, i))
    i <- i + 1
  }
  
  straight[10,] <- c(10, 11, 12, 13, 1) #created a matrix filled with all possible straights
  
  straight_checker <- vector() #created a straight checker that checks if the sample hand contains any straights
  for (i in 1:10)
    straight_checker[i] <- (sample_hand[1,] %contain% straight[i,])
  
  for (i in 1:10) {
    if (any(straight_checker[11-i]) == T) { #used (30-l) to count from the back, because the larger the hand the better. i.e., an ace high straight is better than a queen high straight
      best_straight <- 11-i #took modulo of (30-l) against 10, because there are 10 different possible straights, and so the remainder must determine the straight 
      break #terminate process the moment a straight is found counting from the back
    }
    else {
      best_straight <- 'No straight' #if there are no straights then return the message 'no straight'
    }
  }
  
  #quads
  
  quad <- matrix(, nrow = 13, ncol = 4) #creates a matrix filled with all possible quads
  for (i in 2:13) {
    quad[i-1,] <- rep(i, times = 4)
    quad[13,] <- rep(1, times = 4)
  }
  
  for (i in 1:13) {
    if(sample_hand[1,] %contain% quad[14-i,]) { #counting the quad matrix from the back. as soon as a quad exists, then terminate the process and print out the best quads
      best_quad <- 14-i
      break
    }
    else {
      best_quad <- 'No quad'
    }
  }
  
  #trips
  
  for (i in 1:13) {
    if(sample_hand[1,] %contain% quad[14-i, 1:3]) { #used the same quads matrix as the trips are contained within quads
      best_trip <- 14-i
      break
    }
    else {
      best_trip <- 'No trip'
    }
  }
  
  #pairs
  
  for (i in 1:13) {
    if(sample_hand[1,] %contain% quad[14-i, 1:2]) {
      best_pair <- 14-i
      break
    }
    else {
      best_pair <- 'No pair'
    }
  }
  
  #straight/royal flush
  
  if (best_flush != 'No flush') {
    pot_straight_flush <- NULL
    for (i in 1:7) {
      if (sample_hand[2,][i] == flush_type) {
        pot_straight_flush <- append(pot_straight_flush, sample_hand[1,i])
      }
      for (k in 1:10) {
        if(pot_straight_flush %contain% straight[11-k,] == T) {
          best_straight_flush <- 11-k
          break
        }
        else {
          best_straight_flush <- 'No straight flush'
        }
      }
    }
  } else {
    best_straight_flush <- 'No straight flush'
  }
  
  if (best_straight_flush != 'No straight flush') {
    return(200 + best_straight_flush)
    stop('Found best hand')
  }
  
  if (best_quad != 'No quad') {
    return(180 + best_quad)
    stop('Found best hand')
  }
  
  #fullhouse
  
  if (best_trip != 'No trip' && best_pair != 'No pair' && best_trip[1] != best_pair[1]) {
    best_fullhouse <- best_trip
  } else {
    best_fullhouse <- 'No fullhouse'
  }
  
  if (best_fullhouse != 'No fullhouse') {
    return(160 + best_fullhouse)
    stop('Found best hand')
  }
  
  if (best_flush != 'No flush') {
    if (best_flush == 1) {
      return(140 + 14)
    } else {
      return(140 + best_flush)
    }
    stop('Found best hand')
  }
  
  if (best_straight != 'No straight') {
    return(120 + best_straight)
    stop('Found best hand')
  }
  
  if (best_trip != 'No trip') {
    return(100 + best_trip)
    stop('Found best hand')
  }
  
  #double pair
  
  double_pair_checker <- vector()
  for (i in 1:13) {
    double_pair_checker[i] <- sample_hand[1,] %contain% quad[i, 1:2]
  }
  if (double_pair_checker %contain% c(T,T)) {
    best_double_pair <- which(double_pair_checker)[length(which(double_pair_checker))]
    backup_double_pair <- which(double_pair_checker)[length(which(double_pair_checker))-1]
  } else {
    best_double_pair <- 'No double pair'
  }
  
  if (best_double_pair != 'No double pair') {
    return(80 + best_double_pair + backup_double_pair/10)
    stop('Found best hand')
  }
  
  if (best_pair != 'No pair') {
    return(60 + best_pair)
    stop('Found best hand')
  }
  
  if (any(sample_hand[1,] == 1)) {
    best_card <- 1 #if there is an ace, then its the largest
  } else {
    best_card <- max(sample_hand[1,]) #if not, then return the max number 
  }
  
  if (best_card == 1) {
    return(40 + 14)
  } else {
    return(40 + best_card)
  }
  stop('Found best hand')
}