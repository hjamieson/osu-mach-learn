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
