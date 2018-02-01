lcfm <- function(airports, max_layovers) {
  # Write your code here
  # create the matrix of all possible (fr,to) combinations:
  
  # helper function to calculate the cost.  we keep the total cost for the path in accum.
  helper <- function(fr,to, vect, hops, accum){
    # terminal condition: hops==0
    if (hops == 0) accum <- accum + lowest_cost_direct_flight(fr, to)
    else {
      # not a direct flight. calculate the costs of all possible previous layovers
      layovers <- vect[!(vect %in% c(fr,to))]  # we should get a vector here
      if (length(layovers) < 1) stop
      # if layover==1, we couls have a-b-e, a-c-e, a-d-e.  we have to add the cost of the
      # tail to the cost of the rest of the flight, then take the minimum.
      fff<-sapply(layovers, function(s){helper(fr, s, vect[!(vect %in% s)],hops-1, accum) +
          helper(s, to, vect[!(vect %in% s)],hops-1, accum)})
      cat("fff = ", fff, '\n')
      accum <- accum + sapply(fff,min)
    }
    #cat("accum=", accum, "\n")
    accum
  }
  #helper("NYC","BOM", airports, max_layovers, 0)
  msize <- length(airports)
  mx <- matrix(0, msize, msize, dimnames = list(airports, airports))
  for (i in airports){
    ix <- which(airports == i)[1]
    for (j in airports){
      jx <- which(airports == j)[1]
      if (i == j){ 
        cat("i == j")
        mx[ix, jx] <- 0
      }
      else {
        cat(i,'\n')
        cat(j,'\n')
        cat(max_layovers,'\n')
        mx[ix,jx] <- helper(i, j, airports, max_layovers, 0)
      }
    }
  }
  mx
}
#(lcfm(c("NYC","BOM"), 0))
(lcfm(c("NYC","BOM"), 1))
lcfm(airports, 2)

