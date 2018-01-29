#assignment 3 scratch work
cost <- function(from, to){
  rowx <- price_matrix[[from]][to,]
  minx <- 1
  posx <- 1
  while (posx <= ncol(rowx)){
    if (rowx[posx] < rowx[minx]) minx <- posx
    posx <- posx + 1
  }
  c(rowx[minx], colnames(rowx)[minx])
}
cost(1,2)

price_matrix[1][[1]]
length(airports)
mx <- matrix(0,6,6)
mx[1,1]
mx

lowcost <- function(from, to){
  rowx <- price_matrix[[from]][to,]
  minx <- 1
  posx <- 1
  while (posx <= ncol(rowx)){
    if (rowx[posx] < rowx[minx]) minx <- posx
    posx <- posx + 1
  }
  rowx[minx,1]
}
lowcost(1,2)
airports <- c('BOM', 'NYC', 'DXB', 'LHR', 'FRA', 'DOH')
which(airports == 'BOM')
pts <- c(1:6)
all_paths <-function(pts, fr, to, hops){
  if (hops == 0) c(fr,to)
  else {
    all_paths(pts, fr, to, 0) + all_paths(ptd, fr, to, hops-1)
  }  
}
all_paths(c(1,2,3), 1, 2, 0)
all_paths(c(1,2,3), 1, 2, 1)

lmcost <- function(fr, to, hops) {
  airports <- 1:6
  
  helper <- function(fr, to, hops, visited) {
    if (hops == 0)
      lowest_cost_direct(fr, to)
    else{
      # what approaches are available?
      avail <- airports[!(airports %in% visited)]
      costs <-
        sapply(avail, function(approach) {
          helper(approach, to, hops - 1, c(visited, approach)) +
            helper(fr, approach, 0, c(visited,approach))
        })
      min(unlist(costs))
    }
  }
  helper(fr, to, hops, c(fr, to))
}

lmcost(1,2,0)
lmcost(1,2,1)


lcfm <- function(airports, maxhops){
  n <- length(airports)
  m <- matrix(0,n,n)
  for (i in 1:n){
    for (j in 1:n){
      m[i,j]<- lmcost(i, j, maxhops)
    }
  }
  m
}
lcfm(airports, 0)
