library(ggplot2)
factorial <- function(n){
  if (n <= 1){
    return(1)
  }else{
    return(n * factorial(n-1))
  }
}
estimateExp <- function(x, precision){
  if (x <= 0){
    return(1)
  }
  sum <- c(1)
  counter <- 2
  val <- (x^counter)/factorial(counter)
  print(val)
  sum <- c(sum,val)
  new_precision <- sum[counter - 1]/sum[counter]
  print(new_precision)
  while (new_precision > precision){
    counter <- counter + 1
    val <- (x^counter)/factorial(counter)
    print(val)
    sum <- c(sum,val)
    new_precision <- sum[counter - 1]/sum[counter]
  }
}