housing.data<-read.csv("/Users/hughj/Development/osu/machine-learning/osu-mach-learn/module-2/HousingData.txt", 
   header=FALSE,
   sep = ",",
   col.names = c("area","cost"))

# plot the original input dataset
plot(housing.data$area,housing.data$cost,type="p" ,
     main = "Training Dataset", 
     xlab="area in 100 sqft",
     ylab="price in 100K")
# create the matrix for A
rows <- nrow(housing.data)
A <- as.matrix(cbind(rep(1,rows), housing.data$area))
b <- as.matrix(housing.data$cost)
# calculate the optimal values of theta:
theta <- solve(t(A)%*%A)%*%t(A) %*% b
theta0 <- theta[1]
theta1 <- theta[2]
# plot the hypothesis function f(x):
abline(theta0, theta1, col=3)

# predict the price of 700 sqft home:
prediction <- theta1 * 7.00 + theta0
prediction
