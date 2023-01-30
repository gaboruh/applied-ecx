x0 <- 0
y0 <- 0

n <- 10000

x <- rep(NaN, n)
y <- rep(NaN, n)

x[1] <- x0 + rnorm(1,mean = 0, sd = 2)


epsy <- rnorm(n, mean = 0, sd = 1)
y[1] <- y0 + epsy[1]

for(t in 2:n){
  x[t] <- x[t-1] + rnorm(1,0,2)
}

for(t in 2:n){
  y[t] <- y[t-1] + epsy[t]
}


plot(y, type='l')

dy <- diff(y)
dx <- diff(x)

library(urca)

plot(dy, type='l')
plot(dx, type='l')

summary(ur.ers(x))
summary(ur.ers(y))

summary(ur.ers(dx)) #REject the null of non-stationarity at 1%
summary(ur.ers(dy)) #Reject the null of non-stationarity at 1 % level.

library(lmtest)

grangertest(dx,dy)
#We cannot reject the null hypothesis that the first lag of dx
# does not help forecast dy. -> We cannot reject the null hypothesis
# of dx not (!!!) Granger-causing dy. (-> dx does not granger cause dy).S