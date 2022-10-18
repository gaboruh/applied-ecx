install.packages("datasauRus")
#install.packages(ggplot2)
library(ggplot2)
library(datasauRus)

data("anscombe")
#alternative: df <- read.csv("anscombe.csv", header = TRUE, sep = ",")

head(anscombe)

dim(anscombe)

anscombe[1:3]

#Better:
anscombe[ ,1:3]

1:3
seq(1,3, by = 1)
seq(1,3, by = 0.01)

anscombe[2,3]

summary(anscombe)

first_vector <- c(1,3,5,8,9)
is.vector(first_vector)

mean(first_vector)
summary(first_vector)
sd(first_vector)
median(first_vector)
dim(first_vector) # oops

length(first_vector)

dim(as.matrix(first_vector, ncol = 1))

is.vector(as.matrix(first_vector, ncol = 1))

first_vector * 2

first_vector * pi

first_vector + first_vector + first_vector

sqrt(first_vector)

second_vector <- c(2,5,7,9,22)

first_vector + second_vector

(long_vector <- c(first_vector, second_vector))

first_matrix <- cbind(first_vector, second_vector); first_matrix

wide_matrix <- rbind(first_vector, second_vector); long_matrix


#functions

FirstElement <- function(a, b) {
  u <- c(NA, NA)
  u[1] <- a[1]
  u[2] <- a[2]
  return(u)
}

FirstElement(first_vector, second_vector)

typeof(c("ponyhof"))

# For loops

# Good practice to initalize fully what you want to fill up.
# It allocates memory and helps you do a unit test
multiple_vector <- rep(NA, length(first_vector))

for (i in 1:length(first_vector)) { 
  multiple_vector[i] <- first_vector[i] * second_vector[i]
}

any(is.na(multiple_vector))


#Also works, but not so good practice:

m_vector <- first_vector[1] * second_vector[2]
for (i in 2:length(first_vector)) { 
  m_vector <- c(m_vector, first_vector[i] * second_vector[i]) 
}

# Often you see people use "apply" this is just a for loop for a function.
# The community is split on whether you should use apply (excessively).
# Especially if you apply a complicated function written by you.
# R's efficiency does not like loops. The key is to vectorize. Apply is a loop.

m_vector_apply <- apply(first_matrix, 1, prod) 

all(m_vector_apply == multiple_vector)

#ifelse statements

if (length(first_vector) == length(second_vector)) {
  print("Hey, how's it going?")
} else {
  print("Something's wrong")
}

#you can write ifelse() and also if () else in one line.

# Basic plotting

plot(anscombe$x1, anscombe$y1)
abline(a=3 , b= 0.5, lwd=4, col="green")

ggplot(data = anscombe, aes(x=x1, y=y1)) +
  geom_point() +
  geom_abline(intercept=3, slope=0.5)+
  geom_rug()

data("datasaurus_dozen")

# Choosing the dino dataset from the datasaurus data.
# This is one version using the most basic operations you already know.
# We will cover a couple of other techniques for extracting specific subsets
# of the data. This is using the logic of R objects!

dinodata <- datasaurus_dozen[datasaurus_dozen$dataset == "dino", ]

# This is also basic, but much more interpretable
# dinodata <- subset(datasaurus_dozen, dataset == "dino")

ggplot(data = dinodata, aes(x = x, y = y)) +
  ggtitle("What do you see?") + 
  geom_point() +
  xlab("Abscissa") + 
  ylab("Ordinate") +
  geom_smooth(method='lm', formula= y~x, se=FALSE) 

# Last line: This is a pretty non-obvious way to fit a regression line.
# Don't worry about this.


