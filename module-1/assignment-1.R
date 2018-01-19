# create the matrix:
A <- matrix(c(8, -1, 2, -1, 2, -2, 2,-2, 3), 3,3, byrow = TRUE)
A
# compute the determinant of A
detOfA <- det(A)

qr(A)
install.packages("pracma")
library(pracma)
degs<-function(rads) {
  rads*180/pi
}
theta <- function(v1, v2){
  degs(acos(dot(v1, v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))))
}
v1<-c(1,1,0)
v2<-c(0,1,0)
theta(v1,v2)
v1 %*% v2

install.packages("rmarkdown")

e <- eigen(A)
e$vectors[,2]
dot(e$vectors[,1], e$vectors[,2])
v1 <- e$vectors[,1]
v2 <- e$vectors[,2]
(sqrt(sum(v1^2)) * sqrt(sum(v2^2)))

sum(v1^2)
theta(e$vectors[,1], e$vectors[,2])
degs(1.570796)
?t


calculation.e <- function(v, t){
  tinv <- inv(t)
  prd <- t(tinv) %*% v
  t(prd) %*% t
}

calculation.e(A, A.eigen$vectors)
tinv <- inv(A.eigen$vectors)
xxx <- tinv %*% A
yyy <- xxx %*% A.eigen$vectors
yyy
t(A)
t(inv(A.eigen$vectors))
t<-A.eigen$vectors
?t
