library(wooldridge)
library(AER)
data("mroz")

reg1 <- lm(lwage ~ educ + exper + expersq, data= mroz)
summary(reg1)

reg2 <- lm(lwage ~ educ + exper + expersq + motheduc, data = mroz)
summary(reg2)

cor(mroz$motheduc, mroz$educ)
cor(mroz$fatheduc, mroz$educ)

library(AER)

ivreg1 <- ivreg(lwage ~ educ + exper + expersq | exper + expersq + motheduc + fatheduc, data =mroz)

summary(ivreg1, diagnostics = T)




