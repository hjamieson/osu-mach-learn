v <- c(1,3,2,5)
y <- c(1,4,3)
ls()
x + y
v + y
v * y
?matrix
x <- c(1,2)
y <- c(3,4)
x + y
x * y
x4 <- c(1,2,3,4)
as.matrix(x4, byrow=TRUE)
matrix(data = x4,  nrow=1)
y4 <- c(5,6,7,8)
matrix(data = c(x4,y4), nrow = 2, byrow = TRUE)
m1 <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)
m1
m2 <- matrix(c(1,2,3,4),2,2, byrow=TRUE)
m2
sqrt(m2)
m2^2
m2 * m1
m1
x <- rnorm(50)
y<-x+rnorm(50, mean=50, sd = .1)
y
plot(y)
hist(y)
cor(x,y)
mean(y)
mean(x)
var(y)
sd(y)
set.seed(100)
x<-rnorm(50)
var(x)
sd(x)
set.seed(100)
y <- rnorm(50)
x - y
x[1]
y[1]
cor(x,y)
y<-x+rnorm(50)
y[1]
cor(x,y)
var(y)
sd(y)
z<-rnorm(50, mean=100, sd=.2)
boxplot(z)
hist(z)
mean(z)
sd(z)
plot(x,y,xlab="x-axis", ylab="y-axis",main = "Hughs plot")
pdf("pretty-plot.pdf")
dev.off()
w<-3:11
w
x<-seq(-pi, pi, length=50)
x[1]
plot(x)
y <- x
f<-outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
