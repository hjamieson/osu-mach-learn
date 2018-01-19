---
title: 'CPDA SP18 Assignment #1'
author: Hugh Jamieson
output:
  pdf_document: default
  html_notebook: default
  html_document: default
---

```r
library(matlab)
library(pracma)
library("matrixcalc")
```

Consider the following Matrix:

```r
A <- matrix(c(8,-1,2,-1,2,-2,2,-2,3),3,3,byrow = TRUE)
print(A)
```

```
##      [,1] [,2] [,3]
## [1,]    8   -1    2
## [2,]   -1    2   -2
## [3,]    2   -2    3
```
### a)Compute the determinant and rank of the Matrix A.  What can you say about the rank of the matrix from the determinant?

```r
det(A)
```

```
## [1] 13
```

```r
qr(A)$rank
```

```
## [1] 3
```

Since the determinant(13) is not zero, we know the matrix is not sigular. In this case, A has full rank.  We can confirm rank by putting A in reduced row echelon form, showing 3 non-zero pivots.

```r
rref(A)
```

```
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
```

### b) Write a code to calculate the eigenvalue-eigenvector pairs of the matrix A.


```r
A.eigen <- eigen(A)
A.eigen
```

```
## eigen() decomposition
## $values
## [1] 9.0847569 3.5072388 0.4080043
## 
## $vectors
##            [,1]       [,2]        [,3]
## [1,]  0.8989580 -0.4326343 -0.06857203
## [2,] -0.2318077 -0.6026902  0.76356379
## [3,]  0.3716716  0.6705162  0.64208116
```
### c) Calculate the angle between the eigenvectors corresponding to the two largest eigenvalue in degrees. (2 points)

```r
degs<-function(rads) {
  round(rads*180/pi, digits = 0)
}
theta <- function(v1, v2){
  degs(acos(dot(v1, v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))))
}
theta(A.eigen$vectors[,1], A.eigen$vectors[,2])
```

```
## [1] 90
```

### d) Is the matrix A positive definite? (2 points)

```r
A.eigen
```

```
## eigen() decomposition
## $values
## [1] 9.0847569 3.5072388 0.4080043
## 
## $vectors
##            [,1]       [,2]        [,3]
## [1,]  0.8989580 -0.4326343 -0.06857203
## [2,] -0.2318077 -0.6026902  0.76356379
## [3,]  0.3716716  0.6705162  0.64208116
```

```r
is.positive.definite(A)
```

```
## [1] TRUE
```


All the eigen$values are > 0, so A is positive-definite.

### e) Let T be the matrix constructed by stacking the eigenvectors of A as its columns. Calculate T−1 A T . (2 points)

```r
calculation.e <- function(v, t){
  tinv <- inv(t)
  prd <- tinv %*% v
  prd %*% t
}
T <- A.eigen$vectors
calculation.e(A, T)
```

```
##              [,1]         [,2]         [,3]
## [1,] 9.084757e+00 8.881784e-16 1.332268e-15
## [2,] 9.992007e-16 3.507239e+00 3.552714e-15
## [3,] 3.608225e-16 2.581269e-15 4.080043e-01
```
Using the matrix mult operator yields the same result:

```r
T <- A.eigen$vectors
inv(T) %*% A %*% T
```

```
##              [,1]         [,2]         [,3]
## [1,] 9.084757e+00 8.881784e-16 1.332268e-15
## [2,] 9.992007e-16 3.507239e+00 3.552714e-15
## [3,] 3.608225e-16 2.581269e-15 4.080043e-01
```


### f) Find the trace of the matrix A. (2 points)
By definition, the trace is the sum of the diagonals.

```r
sum(diag(A))
```

```
## [1] 13
```
### g) Find A^2 . (2 points)
A^2 is also A dot A

```r
A %*% A
```

```
##      [,1] [,2] [,3]
## [1,]   69  -14   24
## [2,]  -14    9  -12
## [3,]   24  -12   17
```


