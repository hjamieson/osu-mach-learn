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