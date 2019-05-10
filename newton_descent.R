library(ggplot2)

newton_descent <- function(x){
  vals <- c(0.005)
  counter <- 2
  
  old_val <- vals[counter - 1]
  top_val <- 100 * ((1 + old_val)^30) - (3350.29 * old_val) - 100
  bot_val <- 3000 * (1 + old_val)^29 - 3350.29
  big_calc <- top_val/bot_val
  vals <- c(vals,(old_val - big_calc))
  
  while((abs(vals[counter] - vals[counter - 1])) >= 0.0000001 && counter != x){
    counter <- counter + 1
    old_val <- vals[counter - 1]
    top_val <- 100 * ((1 + old_val)^30) - (3350.29 * old_val) - 100
    bot_val <- 3000 * (1 + old_val)^29 - 3350.29
    big_calc <- top_val/bot_val
    vals <- c(vals,(old_val - big_calc))
  }
  index <- 1:counter
  desc_vals <- data.frame(index,vals)
  ggplot(desc_vals,aes(index,vals)) + geom_point() + geom_line() + ggtitle("Interest Rate: 0.0075") + ylab(element_blank())
}

newton_descent(100)

