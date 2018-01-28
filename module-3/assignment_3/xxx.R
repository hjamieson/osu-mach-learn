airports <- c('BOM', 'NYC', 'DXB', 'LHR', 'FRA', 'DOH')
which(airports == 'BOM')
#pts <- c(1:6)
all_paths <-function(pts, fr, to, hops){
  result <- NULL
  for (i in pts){
    for (j in pts){
      if (i != j) result <- append(result, list(c(i, j)))
    }
  }
  return(result)
}
vvv <- all_paths(c(1,2,3), 1, 2, 0)
nextstop <- function(choices, visited){
  choices[!(choices %in% visited)]
}
nextstop(1:6, c(4,5))

lowc <- function(fr, to, hops, result) {
  if (hops == 0){
    result <- append(result, list(c(fr, to))) 
  } else {
    # what airports are available?
    previous<-nextstop(1:6, c(fr,to))
    for (v in previous){
      result <- append(result, lowc(v,to, hops-1, result))
    }
    #for each nextstop, calc the cost
    result
  }
  result
}

lowc(1,2,0, NULL)
lowc(1,2,1, NULL)
lowc(1,2,2, NULL)
perm_without_replacement <- function(n,r){
  factorial(n) / factorial(n - r)
}
perm_without_replacement(6,0)
