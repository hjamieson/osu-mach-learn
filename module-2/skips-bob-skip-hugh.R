setwd ("C:/Users/Skip/Dropbox/OCLC/Data Science Classes/Machine Learning/R")
HousingData <- read.csv("./HousingData.txt",  col.names = c("area", "cost"), header = FALSE)
# 2a) Import the data into the workspace and plot it with area on x-axis
#     and cost on y-axis. (4 points)
plot(HousingData)

# 2b) Use linear least squares to find the optimal values of \theta ?? .

area <- HousingData$area  # explanatory variable
cost <- HousingData$cost  # response Variable
linearmatrix <- lm(cost ~ area, data = HousingData)
lm_summary <- summary(linearmatrix)
lm_coefficients <- linearmatrix$coefficients
intercept <- lm_coefficients[1]
slope <- lm_coefficients[2]
price <- slope * area + intercept
# still not sure how to answers "optimal values of Theta"

# 2c) Plot the hypothesis function  (2 points)
abline(intercept, slope, col=6)

# 2d) Suppose if the area of the house is 700 sq. feet. Find the estimated price of
#     the house in dollars. (4 points)
estimated_price <- intercept + slope * 7 # costs is in hundreds of dollars, remember.
as.numeric(estimated_price)  # in hundreds of thousands.