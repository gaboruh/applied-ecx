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
