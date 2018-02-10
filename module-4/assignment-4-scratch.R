#setwd("/Users/hughj/Development/osu/osu-mach-learn/module-4")
# read the image date
image_data <- as.matrix(read.csv("image_data.txt", sep = ';', header = FALSE))

labels <- read.csv("labels.txt", header = FALSE)
w1<-as.matrix(read.csv("W1.txt", header = FALSE, sep = ";"))
w2 <- as.matrix(read.csv("W2.txt", header = FALSE, sep = ";"))

sigma <- function(z){
  1 / (1 + exp(-z))
}
library("sigmoid")
p <-function(im){
  s1<-matrix(im,nrow=1,ncol=400)
  s1b<-rbind(1, t(s1))
  s2 <- sigmoid(w1 %*% s1b) 
  s2b<-rbind(1, s2)
  s3 <- sigmoid(w2 %*% s2b)
  s3
}
result<-null
result<-p(image_data[1,])

total<- apply(image_data,1,p)
totali<-t(total)
totali[1:5,]
