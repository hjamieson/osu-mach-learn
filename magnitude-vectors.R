7 * cos(100)
cos(pi*100)
pi
cospi(100)
?deg
?degrees
rad<-function(deg){
  deg * pi / 180
}
degs<-function(rads) {
  round(rads*180/pi, digits = 0)
}
rad(100)
cos(rad(45))
asvector<-function(mag, deg){
  c(mag * cos(rad(deg)), mag * sin(rad(deg)))
}
f1(7,100)
f1(5,290)
h1<-function(m1,d1, m2,d2){f1(m1,d1) + f1(m2,d2)}
h1(7,100, 5,290)

asvector(7,100) + asvector(5,290)
asvector(7,160) + asvector(9,20)
asvector(8,140) + asvector(4,40)
asvector(3,310) + asvector(8,190)

v<- matrix(c(1,2,3,4),2,2,byrow = TRUE)
det(v)
qr(v)$rank
library(pracma)
rref(v)
MatrixRank(v)
degs(asin(0.7071))

theta <- function(v1, v2){
  degs(acos(dot(v1, v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))))
}
a<-c(1,2,3,4)
b<-c(5,6,7,8)
theta(a,b)
