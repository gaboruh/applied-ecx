#install.packages("ggplot2")
#install.packages("sn")
library(ggplot2)
library(sn) # library to generate skewed normal distributions.

#----
# Simulating the error to normal approximation as a function of sample size
# and skewness of data.

plot_skewdistr <- function(n, alpha){
  
  xi <- 0
  omega <- 1
  delta <- alpha / sqrt(1+alpha^2)
  mean_sn <- xi + omega*sqrt(2/pi)*delta # mean of skewed normal 
  sd_sn <- omega*sqrt(1-2*delta^2/pi) # standard deviation of skewed normal
  
  
  gen_skewdata <- rsn(n = n, alpha = alpha, xi = 0, omega = 1)
  gen_skewdata <- data.frame(data = gen_skewdata)
  
  ggplot(gen_skewdata, aes(x=data)) +
    geom_density(aes(y = ..density..),
                 colour = "grey",
               fill="grey" ) + 
   stat_function(fun = dnorm, args = list(mean = mean_sn, sd = sd_sn))
}


repeat_experiment <- function(repnr, n, alpha){
  
  xi <- 0
  omega <- 1
  delta <- alpha / sqrt(1+alpha^2)
  mean_sn <- xi + omega*sqrt(2/pi)*delta
  sd_sn <- omega*sqrt(1-2*delta^2/pi)
  
  x <- rsn(n = n*repnr, alpha = alpha, xi = xi, omega = omega)
  x <- matrix(x, nrow = n, ncol = repnr)
  
  x_mean <- colSums(x)/n # column means. alternative: apply(x, 2, mean)
  
  form_clt <- data.frame(clt_stat = sqrt(n)*(x_mean-mean_sn)/sd_sn)
  
  ggplot(form_clt, aes(x=clt_stat)) +
    geom_histogram(aes(y = ..density..),
                   breaks = seq(-5, 5, by = 0.1), 
                   colour = "grey", 
                   fill = "white") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1))
  
}

plot_skewdistr(1000,10)

repeat_experiment(10000,10, 10)

# By trying out various combinations of sample size and skewness parameter 
# we get a mixed evidence. This also whows that the CLT in this simple context
# is actually pretty good already.
