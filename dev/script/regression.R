# regression analysis 
# model building, diagnostics, tests

library(car)
prestige <- carData::Prestige
data.table::setDT(prestige)

prestige

# linear regression ----

# x: education, income, women
# y: prestige 

plot(prestige)

# step 1, eda
# seems to have strong relationship between edu v pres, income v pres
# not so much women v pres

# check correlation 
cor(prestige$education, prestige$prestige) # 0.85
cor(prestige$income, prestige$prestige) # 0.71
cor(prestige$women, prestige$prestige) # -0.11

# step 2, build lm 

m1 <- lm(prestige ~ education + income + women, 
         data = prestige)

summary(m1)

plot(m1)


# model selection ----
# backward
m11 <- lm(prestige ~ education + income, 
          data = prestige)

summary(m11)


m10 <- lm(prestige ~ education, data = prestige)

# option 1: F-test
# anova to compare two models
anova(m1, m11)
anova(m1, m10)

# option 2: likelihood ratio test (not common in LR)
lmtest::lrtest(m1, m11) # not sig
lmtest::lrtest(m1, m10) # sig


# adding 'women' does not reduce the errors significantly
# while income is significant


AIC(m1, m11, m10) # m11 is the best

BIC(m1, m11, m10)

# collinearity  ----
?vif




# _______ ----
# logistic regression -----

mtcars
head(mtcars)
plot(mtcars$vs, mtcars$mpg)

mlr1 <- glm(vs ~ mpg + wt, family = 'binomial', data = mtcars)
summary(mlr1)

# z-values are the wald test statistics


mlr2 <- glm(vs ~ mpg, family = 'binomial', data = mtcars)
summary(mlr2)

# the null deviance are the same
# residual deviance slightly different
# we removed an insig variable so not surprising

anova(mlr2, mlr1, test = 'Chisq') # analysis of deviance



# _______ -----
# cox regression ----

lung
head(lung)

coxm1 <- coxph(Surv(time, status) ~ age+sex, data = lung)

coxm2 <- coxph(Surv(time, status) ~ age, data = lung)

summary(coxm1)
summary(coxm2)

# model comparsion with lRT

anova(coxm1, coxm2, test = 'Chisq')

# log rank test, test survival curve differences
# sex is significant
survdiff(Surv(time, status) ~ sex, data = lung)
?survdiff


# test for proportional hazard assumption
cox.zph(coxm1)
# non significant, no covariate violated

# shoenfeld residual 
plot(cox.zph(coxm1))

# martingale residual 

res_mart <- residuals(coxm1, type = 'martingale')
plot(res_mart)

res_dev <- residuals(coxm1, type = 'deviance')
plot(res_dev)



