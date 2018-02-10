setwd("/Users/jamiesoh/Development/osu/osu-mach-learn/module-5")
X <- as.matrix(read.csv("my_data_pca.txt", header = FALSE))
dim(X)
# row = feature, column = image
# compute the mean of each feature
moef <- as.matrix(apply(X, 1, mean))
dim(moef)
Y <- apply(X, 2, function(m){m - t(moef)})
dim(Y)
Y[1,1:20]
# mean of each row should be zero
mean(Y[1,])
# calculate the covariance of Y
C<- t(Y) %*% Y
dim(C)
# (d) find eigenvectors
ev.values <- eigen(C)$values
ev.vectors <- eigen(C)$vectors
# (e)sort eigenvalues
sorted.values<-sort(ev.values, decreasing = TRUE)
plot(sorted.values)

# (f) keep eigen vectors for the 12 largest eigen values; stack as columns in a matrix V
s <- apply(as.matrix(sorted.values), 1, function(n){which(ev.values == n)})[1:12]
dim(s)
class(s)
V<-ev.vectors[,s]
dim(V)
V[1,]

# g) eigenfaces
E <- Y %*% V
dim(E)
E[1,]
rotate <- function(x) t(apply(x, 2, rev))
image(matrix(E[,1], 85, 60), axes=FALSE, col=grey(seq(0,1,length=256)))
image(matrix(E[,2], 85, 60), axes=FALSE, col=grey(seq(0,1,length=256)))
image(rotate(matrix(E[,1], 85, 60)), axes=FALSE, col=grey(seq(0,1,length=256)))
for (img in 1:12){
  image(rotate(matrix(E[,img], 85, 60)), axes=FALSE, col=grey(seq(0,1,length=256)))
}
