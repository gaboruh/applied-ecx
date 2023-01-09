install.packages("pacman")
install.packages("igraph",  type = "binary")

pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot)

setwd(dirname(getActiveDocumentContext()$path)); getwd()

data_repo <- paste0(getwd(),"/data")

dir.create(data_repo)

library(data.table)

hotels_europe_price <- fread(paste0(data_repo,"/hotels-europe_price.csv"))
hotels_europe_features <- fread(paste0(data_repo, "/hotels-europe_features.csv"))

summary(hotels_europe_price)
summary(hotels_europe_features)

hotel_data <- merge(hotels_europe_features, 
                    hotels_europe_price, 
                    by = "hotel_id")

head(hotel_data)

summary(hotel_data)

hotel_vienna <- hotel_data[(year == 2017 & month == 11 & city == "Vienna" & weekend == 0), ]

summary(hotel_vienna)

library(DataExplorer)
#create_report(hotel_vienna)

ggplot(hotel_vienna, aes(x=accommodation_type, y=price)) +
  geom_boxplot()

ggplot(hotel_vienna, aes(x=accommodation_type, y=price))+
  geom_violin()+
  geom_jitter()

hotel_vienna_hot <- hotel_vienna[accommodation_type=="Hotel", ]

hotel_vienna_hot <- hotel_vienna_hot[!is.na(hotel_vienna_hot),]

cor(hotel_vienna_hot$price, hotel_vienna_hot$distance)

head(hotel_vienna_hot$price)
plot(hist(hotel_vienna_hot$price))

avg_price <-sum(hotel_vienna_hot$price)/length(hotel_vienna_hot$price)

bootstrap_pop <- hotel_vienna_hot$price
B <- 1299 #number of bootstrap replications

bootstrap_fun <- function(data, bootrep){
  bootoutcome <- matrix(NA, nrow=length(data), ncol = bootrep)
  randmatrix <- matrix(sample.int(length(data), size = length(data)*bootrep, replace = T), 
                       nrow=length(data), ncol=bootrep)
  for (i in 1:bootrep){
    bootoutcome[ ,i] <- data[randmatrix[ ,i]]
  }
  return(bootoutcome)
}

bootstrap_samples <- bootstrap_fun(bootstrap_pop, B)

plot(hist(bootstrap_samples[,6]))

plot(hist(bootstrap_pop))

boot_averages <- colSums(bootstrap_samples)/dim(bootstrap_samples)[1]

weird_fun <- function(data){
  outcome <- var(data) + sum(data^3) - mean(exp(data))
}

boot_var <- apply(bootstrap_samples, 2, var)
plot(hist(boot_var))

boot_weirdfun <- apply(bootstrap_samples, 2, weird_fun)
#
plot(hist(boot_weirdfun))

# boot_averages_alt <- apply(bootstrap_samples, 2, mean)

plot(hist(boot_averages))

sd(boot_averages)
#CI_basic <- [mean(bootstrap_pop) - 1.96*sd(boot_averages); ]
CI_lower <- mean(bootstrap_pop) - 1.96 * sd(boot_averages)
CI_upper <- mean(bootstrap_pop) + 1.96 * sd(boot_averages)

library(bcaboot)
ci_bc <- bcajack(bootstrap_pop, 1599, mean)


#is.data.table(hotel_data)

bcaplot(ci_bc)

## Regression Analysis

reg1 <- lm(price ~ distance, data = hotel_vienna_hot)
summary(reg1)

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


ggplot(hotel_vienna_hot[-75], aes(x = distance, y = price)) + 
  geom_point() + 
  geom_smooth(method = loess,  se = FALSE)


