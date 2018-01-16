# create the matrix:
A <- matrix(c(8, -1, 2, -1, 2, -2, 2,-2, 3), 3,3, byrow = TRUE)
A
# compute the determinant of A
detOfA <- det(A)

# gaussian reduction using pracma package
install.packages("pracma")
library(pracma)
A.rref<-rref(A)
A.rref

# get the rank of A
library(Matrix)
Arank <-rankMatrix(A)
Arank
determinant(A)
unlist(determinant(A))
det(A)
A

# eigencalues of A
a.eigen<-eigen(A)
# ignore
#dot product
a<-c(2,9,-3)
b<-c(-3,-4,8)
dot(a,b)
length(a)
sum(a^2)
sum(b^2)
a.eigen$vectors[3,1]
a.eigen$vectors[,1]
sum(a.eigen$vectors[,1]^2)
a.eigen$vectors[1,1]^2
a.eigen$vectors[2,1]^2
a.eigen$vectors[3,1]^2

a.eigen$vectors[1,1]^2 +
a.eigen$vectors[2,1]^2 +
a.eigen$vectors[3,1]^2



a.mag <-sqrt(sum(a.eigen$vectors[,1]^2))
b.mag <-sqrt(sum(a.eigen$vectors[,2]^2))
