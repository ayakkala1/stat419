library(ggplot2)
library(gridExtra)

fib <- function(n){
  A <- replicate(n,1)
  if (n == 1 || n == 2){
    return(A[n])
  }
  for (i in 3:n){
    A[i] <- A[i-1] + A[i-2]
  }
  return(A)
}

fib_ratio <- function(n){
  vals <- fib(n)
  index <- 1:n
  ratios <- c(NA,1)
  for (i in 3:n){
    ratios <- c(ratios,vals[i]/vals[i-1])
  }
  fib_table <- data.frame(index,vals,ratios)
  p1 <- ggplot(fib_table ,aes(x=index,y=vals)) + geom_point() + geom_line() +
    ylab(element_blank()) + ggtitle("Fibonacci Numbers") +
    scale_x_continuous("Iteration", labels = as.character(index), breaks = index)
  p2 <- ggplot(fib_table,aes(index,ratios)) + geom_point() + geom_line() +
    ylab(element_blank()) + ggtitle("Fibonacci Rations and the Golden Number") +
    scale_x_continuous(element_blank(), labels = as.character(index), breaks = index) +
    geom_hline(yintercept = (1 + sqrt(5))/2, color = "red")
  grid.arrange(p1, p2, nrow=2)
}

fib_ratio(10)