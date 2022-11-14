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

boot_var <- apply(bootstrap_samples, 2, var)
plot(hist(boot_var))

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




