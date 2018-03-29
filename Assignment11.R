library(outliers)
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && outlier(x[,j], logical=TRUE)
    print(outlier(x[,j]), logical=TRUE)
  }
  print(outliers)
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i,])
    }
  return(outlier.vec) }

charges <- c(3,5,2,3,7,5)
price <- c(15,6,18,17,17,55000)
book <- array(c(charges, price), dim=c(6,2))
book
tukey_multiple(book)
