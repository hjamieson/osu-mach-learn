data <- read.csv("/Users/jamiesoh/Development/osu/machine-learning/R/osu-mach-learn/HousingData.txt", header = FALSE)
# plot the dater
plot(data, ylab="price", xlab="area")
# theta LS = solve(t(area) * area)
area <- data$V1
cost <- data$V2
slope <- solve(t(area) %*% area) %*% t(area) %*% cost
# the function 0 + area essentially forced the line thru zero.
#yyy <- lm(cost ~ 0 + area, data = data)
yyy <- lm(cost ~ area, data = data)
### we forced this to 0 to make the intercept 0 (cause cam said to)
theta0 <- 0 # was yyy$coefficients[1]
theta1 <- slope
price <- theta0 + as.vector(slope) * area

lm(cost ~ area)
price2 <- 1.16978 * area  - 3.58096 
abline(area, price2, col=1)
plot(area,price)
abline(-3.581, 1.170, col=6)
lines(area, price, col=5)
summary(yyy)
plot(data, ylab="price", xlab="area")
abline(theta0, slope, col=2)
qd <- -3.58 + 1.170 * 7
qd
predict(yyy)

# cams way
intercept <- c(rep(1, length(area)))
h <- cbind(intercept, area)
camslope <-solve(t(h) %*% h) %*% t(h) %*% cost
camyyy<- lm(cost ~ h)
summary(camyyy)
camintercept<-camyyy$coefficients[1]
camprice <- camintercept + camslope %*% area
plot(area, camprice$area)


