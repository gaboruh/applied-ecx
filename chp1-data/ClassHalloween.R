library(ggplot2)
library(datasauRus)


first_vector <- c(1,3,5,8,9)
second_vector <- c(2,5,7,9,22)

multiple_vector <- rep(NA, length(first_vector))

for (i in 1:length(first_vector)){
  multiple_vector[i] <- first_vector[i] * second_vector[i]
}

checkvar <- first_vector * second_vector
  
  any(is.na(multiple_vector))

any(checkvar != multiple_vector)

if(length(first_vector) == length(second_vector)){
  print("Hey, how's going")
}else{
  print("Hey, something's wrong")
}

data(anscombe)

summary(lm(anscombe$y1 ~ anscombe$x1))

plot(anscombe$x1, anscombe$y1,
  main="Scatterplot of the first anscombe pair", 
  xlab = "X1 values",
  ylab = "Y1 values")
abline(a = 3, b=0.5, col="blue")

ggplot(data = anscombe, aes(x = x1, y=y1)) +
  geom_point() + 
  geom_abline(intercept = 3, slope=0.5) + 
  geom_rug()

library(datasauRus)
data(datasaurus_dozen)
summary(datasaurus_dozen)
head(datasaurus_dozen)
tail(datasaurus_dozen)

dinodata <- subset(datasaurus_dozen, dataset == "dino")

sd(dinodata$y)

head(datasaurus_dozen)

unique(datasaurus_dozen$dataset)

#alternatives:
dinodata_alt <- datasaurus_dozen[datasaurus_dozen$dataset=="dino", ]


ggplot(data = dinodata, aes(x = x, y=y))+
  ggtitle("What do you see?")+
  geom_point()

summary(lm(y ~ x, data = dinodata))

otherdata <- subset(datasaurus_dozen, dataset=="wide_lines")

summary(lm(y~x, data = otherdata))

ggplot(data = otherdata, aes(x = x, y=y))+
  ggtitle("What do you see?")+
  geom_point()

summary(datasaurus_dozen)
unique(datasaurus_dozen$dataset)
datasaurus_dozen_wide

head(datasaurus_dozen_wide)
ld <- dim(subset(datasaurus_dozen, dataset == "dino"))
ld <- ld[1]


