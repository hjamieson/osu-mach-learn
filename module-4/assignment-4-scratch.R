setwd("/Users/hughj/Development/osu/osu-mach-learn/module-4")
# read the image date
image_data <- read.csv("image_data.txt", sep = ';', header = FALSE)
labels <- read.csv("labels.txt", header = FALSE)
w1<-read.csv("W1.txt", header = FALSE, sep = ";")
w2 <- read.csv("W2.txt", header = FALSE, sep = ";")
