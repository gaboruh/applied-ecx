#library(fpp3)

library(fpp2)
library(seasonal)
library(ggplot2)
data(elecequip)
autoplot(window(elecequip, start=c(2001,1), end=c(2001,12)))

plot(decompose(elecequip, type="additive"))

plot(stl(elecequip, s.window="periodic"))

ts_remainder <- decompose(elecequip, type="additive")$random

autoplot(ts_remainder)

#Unit root testing.



         