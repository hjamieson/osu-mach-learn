lcfm <- function(airports, max_layovers) {
  # Write your code here
  # create the matrix of all possible (fr,to) combinations:
  
  # helper function to calculate the cost.  we keep the total cost for the path in accum.
  # args: fr = from
  #   to = destination
  #   vect = hops visited
  #   hops = number of hops left
  #   lcm = lowest cost matrix (for memoization)
  lcdfm <- lowest_cost_direct_flight_matrix(airports)
  helper <- function(fr, to, vect, hops) {
    #cat(sprintf("helper: fr=%s, to=%s,vect=%s, hops=%d, accum=%d\n", fr, to, paste(vect), hops,accum),'\n')
    # terminal condition: hops==0
    if (hops == 0) {
    #  accum <- lowest_cost_direct_flight(fr, to)
      accum <- lcdfm[which(airports==fr), which(airports==to)]
    }
    else {
      # not a direct flight. calculate the costs of all possible previous layovers
      layovers <-
        vect[!(vect %in% c(fr, to))]  # we should get a vector here
      
      #cat(paste(c("layovers= ", layovers)), '\n')
      
      if (length(layovers) < 1)
        stop  # This should never happen if our dim is right!
      
      # if layover==1, we couls have a-b-e, a-c-e, a-d-e.  we have to add the cost of the
      # tail to the cost of the rest of the flight, then take the minimum.
      min_layover <- 2 ^ 30   # prevent using 0 cost as minimum.
      for (layover in layovers) {
        hop_cost <-
          helper(fr, layover, vect[!(vect %in% layover)], hops - 1) +
          helper(layover, to, vect[!(vect %in% layover)], hops - 1)
        
        if (hop_cost < min_layover) {
          min_layover <- hop_cost
        }
      }
      accum <- min_layover
      # if the direct flight is cheaper, just use it.
      if (accum < lowest_cost_direct_flight(fr,to)) return(accum)
      else return( lowest_cost_direct_flight(fr,to))
      
    }
    accum
  }
  
  msize <- length(airports)
  mx <- matrix(0, msize, msize, dimnames = list(airports, airports))
  for (i in airports) {
    for (j in airports) {
      if (i == j) {
        mx[i, j] <- 0
      }
      else {
        mx[i, j] <- helper(i, j, airports, max_layovers)
      }
    }
  }
  mx
}
#lcfm(c("NYC","BOM"), 0)
lcfm(airports, 0)
lcfm(airports, 1)
lcfm(airports, 2)
lcfm(airports, 5)
(lcfm(airports,0)-lcfm(airports,1))


library("hashmap")
flcfm <- function(airports, max_layovers) {
  # cache the direct costs for later
  lcdfm <- lowest_cost_direct_flight_matrix(airports)
  # well use a hashmap tp cache recently computed tails for memoization
  set.seed(13)
  tail_cache <- hashmap(c("xxx"), 0)
  
  makeFlightName <- function(vectr){
    paste(vectr, collapse = '')
  }
  makeFlightVector <- function(fr,to, v){
    c(fr,v,to)
  }
  
  # helper function to calculate the cost.  we keep the total cost for the path in accum.
  # args: fr = from
  #   to = destination
  #   vect = hops visited
  #   hops = number of hops left
  helper <- function(path, hops) {
    #    cat(sprintf("helper=>path(%s), hops(%d)\n",makeFlightName(path),hops))
    # terminal condition: hops==0
    if (hops == 0) {
      accum <- lcdfm[path[1], path[-1]]
    }
    else {
      # not a direct flight. calculate the costs of all possible previous layovers.
      # we try to employ bellman principal by calculating the tails least cost:
      layovers <- airports[!(airports %in% path)]  # we should get a vector here
      
      if (length(layovers) < 1)
        stop  # This should never happen if our dim is right!
      
      # if layover==1, we couls have a-b-e, a-c-e, a-d-e.  we have to add the cost of the
      # tail to the cost of the rest of the flight, then take the minimum.
      min_layover <- 2 ^ 30   # prevent using 0 cost as minimum.
      for (layover in layovers) {
        # insert the layover into the current path
        flight_vector <- c(path[1], layover, path[2:length(path)])
        flight_path <- makeFlightName(flight_vector)
        #       cat(sprintf("flight_path=%s\n", flight_path))
        # if flight has already been costed, use it
        if (tail_cache$has_key(flight_path)) {
          #          cat(sprintf("using cache %s\n", flight_path))
          accum <- tail_cache[[flight_path]]
        }else {
          head_cost <- helper(flight_vector[1:2], hops - 1)
          tail_cost <- helper(flight_vector[2:length(flight_vector)], hops - 1)
          hop_cost <- head_cost+tail_cost
          
          if (hop_cost < min_layover) {
            min_layover <- hop_cost
          }
          # add to cache
          tail_cache$insert(flight_path, min_layover)
        }
      }
      accum <- min_layover
    }
    # if the direct flight is cheaper, just use it.
    if (accum < lcdfm[path[1],path[length(path)]]) return(accum)
    else return( lcdfm[path[1],path[length(path)]])
  }
  
  msize <- length(airports)
  mx <- matrix(0, msize, msize, dimnames = list(airports, airports))
  for (i in airports) {
    for (j in airports) {
      if (i == j) {
        mx[i, j] <- 0
      }
      else {
        mx[i, j] <- helper(c(i, j), max_layovers)
      }
    }
  }
  mx
}