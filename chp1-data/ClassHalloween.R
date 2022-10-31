library(ggplo2t)
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

plot(anscombe$x1, anscombe$y1,
  main="Scatterplot of the first anscombe pair", 
  xlab = "X1 values",
  ylab = "Y1 values")
abline(a = 3, b=0.5, col="blue")

