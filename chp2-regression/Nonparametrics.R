install.packages("np")
library(np)
library(ggplot2)
library(data.table)

data(cps71)

df <- data.table(cps71)

df <- cbind(df, agesq = df$age^2, age3 = df$age^3, age4= df$age^4)

quickplot(x= df$age, y = df$logwage)


npplot(npregbw(xdat=df$age, ydat=logwage,regtype="ll"), plot.errors.method="asymptotic", ylim=c(11,15.1),plot.errors.style="band")

npplot(npregbw(xdat = df$age, ydat=df$logwage, regtype="ll"), plot.errors.method = "asymptotic", ylim=c(11,15.1), plot.errors.style="band")

param2 <- lm(logwage ~ age + agesq, x=TRUE, y=TRUE, data=df); summary(param2)


pred2 <- predict(param2, interval="confidence")

plot(df$age, df$logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1))
matlines(df$age,pred2,lty=c(1,2,2), col=c("black","black","black"))
