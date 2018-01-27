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
