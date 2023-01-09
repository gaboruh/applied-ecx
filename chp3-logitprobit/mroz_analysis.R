pacman::p_load(mfx, wooldridge)
library(estimatr)

data(mroz)

dim(mroz)
summary(mroz)

model_formula <- inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6

reg_lpm <- lm_robust(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, se_type = "HC3", data = mroz)

summary(reg_lpm)

max(reg_lpm$fitted.values)
min(reg_lpm$fitted.values)

hist(reg_lpm$fitted.values)

reg_logit <- glm(model_formula, family=binomial(link="logit"), data = mroz)

reg_probit <- glm(model_formula, family=binomial(link="probit"), data=mroz)

min(reg_probit$fitted.values)
max(reg_probit$fitted.values)

summary(reg_probit)

#multiply probit coefficients by 1.6 -> get approxiamte logit coeffs.

library(mfx)

logitmfx(model_formula, data = mroz)
probitmfx(model_formula, data = mroz)

summary(reg_logit)

#Manually calculating "marginal effects" (average partial effects)
reg_logit <- glm(model_formula, family=binomial(link="logit"), data = mroz)
summary(reg_logit)

#nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6

Xmat <- cbind(1,mroz$nwifeinc, mroz$educ, mroz$exper, mroz$expersq, mroz$age, mroz$kidslt6, mroz$kidsge6)

Xmat

xpb <- Xmat %*% reg_logit$coefficients

gdot <- rep(NA, dim(Xmat)[1])

for (i in 1:dim(Xmat)[1]) {
gdot[i] <- dlogis(xpb[i]) 
}

#This is what mfx package calculates
xave <- 1/dim(Xmat)[1] * colSums(Xmat)

dlogis(xave %*% reg_logit$coefficients) * reg_logit$coefficients[3]


#This is what I would like it to calculate (APE)

gdot[70] * reg_logit$coefficients[3]

mean(gdot) *reg_logit$coefficients[3]



logitmfx(model_formula, data =mroz)
