f<-function(x) {
  1.5*(x-4)^2 + 5
}
grad<-function(x){
  1.5*2*(x-4)
}
x_val <- seq(0,8,len = 10)
x_val
# plot x_val and f(x_val)
plot(x_val , f (x_val), type="l",xlab="x",ylab=expression(1.5*(x-4)^2 +5))
# plot grad
#plot(x_val , grad (x_val), type="l",xlab="x",ylab=expression(1.5*2*(x-4)))
#initial value x[0]
x <- 0.001
#constant step size
alpha <- 0.5
# stores the values of x to create a plot
x_plot <- x
# stores the values of f(x) to create a plot
f_plot <- f(x)
# x -values are stored
x_optimal <- x
# the algorithm is run for 100 iterations
for (iter in 1:100) {
  #x[k+1] = x[k]-alpha*grad(f(x))
  x <- x - alpha*grad(x)
  # x vector updated for each iteration 
  x_optimal <- c(x_optimal,x)
  # update for the x values of the plot 
  x_plot <- c(x_plot,x)
  # update for the plot
  f_plot <- c(f_plot,f(x))
}
lines ( x_plot , f_plot , type="b",col="red") 
text (0.8,6, "Steepest Descent",col="blue",pos= 4)

#print the values
x_optimal
