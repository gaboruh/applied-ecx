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

summary(reg_logit)
