##needs packages: boot, sandwich, ggplot2, estimatr
## Note: Starting data set : hotel_vienna_hot

## Regression Analysis

reg1 <- lm(price ~ distance, data = hotel_vienna_hot)
summary(reg1)

library(ggplot2)

quickplot(x= hotel_vienna_hot$distance, y= hotel_vienna_hot$price)

plot(x= hotel_vienna_hot$distance, y= reg1$residuals)

install.packages("sandwich")
library(sandwich)

reg1_var <- vcovHAC(reg1)
sqrt(diag(reg1_var))
library(ggplot2)
summary(reg1)

install.packages("estimatr")
library(estimatr)

reg1_robust <- lm_robust(price ~ distance, data = hotel_vienna_hot, se_type="HC3")

library(boot)

coefboot <- function(data, idx){
  coef(lm(price~distance, data = data[idx, ]))
}

beta_boot <- boot(hotel_vienna_hot, coefboot, R=2999)

plot(hist(beta_boot$t[,1])) 

plot(hotel_vienna_hot$distance, hotel_vienna_hot$price)

quickplot(hotel_vienna_hot$distance, hotel_vienna_hot$price)

ggplot(hotel_vienna_hot, aes(x = distance, y = price))+
  geom_point()+
  geom_abline(slope = reg1$coefficients[2], intercept= reg1$coefficients[1], col="red")

any(hotel_vienna_hot$distance==0)
which(hotel_vienna_hot$distance==0)
any(hotel_vienna_hot$price == 0)

reg_linlog <- lm(price~log(distance), data = hotel_vienna_hot[-75,])

reg1_v2 <- lm(price~log(distance), data = hotel_vienna_hot[-75,])
summary(reg1_v2)


summary(reg_linlog)

ggplot(hotel_vienna_hot, aes(x = log(distance), y = price))+
  geom_point()+
  geom_abline(slope = reg_linlog$coefficients[2], intercept= reg_linlog$coefficients[1], col="red")

reg_loglin <- lm(log(price) ~ distance, data = hotel_vienna_hot[-75, ])
summary(reg_loglin)


ggplot(hotel_vienna_hot[-75], aes(x = distance, y = log(price)))+
  geom_point()+
  geom_abline(slope = reg_loglin$coefficients[2], intercept= reg_loglin$coefficients[1], col="red")

reg_loglog <- lm(log(price) ~ log(distance), data = hotel_vienna_hot[-75])
summary(reg_loglog)

ggplot(hotel_vienna_hot[-75], aes(x = log(distance), y = log(price)))+
  geom_point()+
  geom_abline(slope = reg_loglog$coefficients[2], intercept= reg_loglog$coefficients[1], col="red")


hotel_vienna_hot <- cbind(hotel_vienna_hot, distsq = hotel_vienna_hot$distance^2)

reg_quad <- lm(price ~ distance + distsq , data= hotel_vienna_hot)
summary(reg_quad)
