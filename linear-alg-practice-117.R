#create a matrix
A<-matrix(c(2,-1,0,-1,2,-1,0,-1,2),3,3,byrow = TRUE)
A
#find the determinant
det(A)
#check if the matrix is symetric
isSymmetric(A)
#find the eigen values of a matrix A
eigenvalues <- eigen(A)
eigenvalues
#find the eigen vectors of matrix A
eigenvectors <- eigenvalues$vectors
#compute single value decomposition of A
singular_val_decomp<-svd(A)
singular_val_decomp
#output $d gives the diagonal entries
#each column of $u gives the left singular vectors
#each column of $v gives the right singular vectors

m1 <- matrix(c(0,0,0,0),2,2)
m1
isSymmetric(m1)
m2 <- matrix(c(0,0,0,1),2,2)
m2
isSymmetric(m2)
em1<-eigen(m1)
em2<-eigen(m2)
em1$values
em2$values
m3<-matrix(c(1,0,0,1),2,2)
m3
em3<-eigen(m3)
em3$values
em1$values

q2 <- matrix(c(1, 0.5, 2, 1),2,2, byrow = TRUE)
q2
eq2 <- eigen(q2)
eq2$values
