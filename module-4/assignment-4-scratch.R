setwd("/Users/hughj/Development/osu/osu-mach-learn/module-4")
# read the image date
image_data <- as.matrix(read.csv("image_data.txt", sep = ';', header = FALSE))
labels <- as.matrix(read.csv("labels.txt", header = FALSE))
w1<-as.matrix(read.csv("W1.txt", header = FALSE, sep = ";"))
w2 <- as.matrix(read.csv("W2.txt", header = FALSE, sep = ";"))

#(c) implement feedforward propagation
sigma <- function(z){
  1 / (1 + exp(-z))
}
<<<<<<< HEAD
library("sigmoid")
p <-function(im){
=======
neural.net <-function(im){
>>>>>>> 0bb2a47efca7ae345ce437d13c61a9ce90c8b65d
  s1<-matrix(im,nrow=1,ncol=400)
  s1b<-rbind(1, t(s1))
  s2 <- sigma(w1 %*% s1b) 
  s2b<-rbind(1, s2)
  s3 <- sigma(w2 %*% s2b)
  s3
}
# predict a sample row
sample.row.test<-neural.net(image_data[1,])
t(sample.row.test)
which(sample.row.test==max(sample.row.test))

output.layer<- t(apply(image_data,1,neural.net))
prediction<-apply(output.layer, 1, function(nodes){which(nodes == max(nodes))})
# print count of each estimated images
library(plyr)
count(prediction)
# count images in labels.txt
count(labels)
# (d) calculate percent accuracy (correct predictions/total images)
table(labels, prediction)
colMeans(prediction==labels)

# (e) generate a random image and plot
test.image = as.matrix(runif(400))
image(test.image)
# test.image - feed to neural net
img.e<-neural.net(test.image)
which(img.e == max(img.e))
# i) The network defines nonlinear boundaries, regardless of the input data.  The
#    random pattern is simply defining the same boundary that would be occupied
#    by one of the digits.