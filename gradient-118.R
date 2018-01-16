install.packages("numDeriv")
library(numDeriv)
f <- function(x) {
  x1 <- x[1]; x2 <- x[2]; x3 <- x[3]
  sin(x1*x2)+cos(x3)
}
gradient <- grad(f, x=1:3)
gradient
?grad

hess <- hessian(f, x=1:3)
hess

rad<-function(deg){
  deg * pi / 180
}
sin(rad(0))
sin(0)
