# code for chapter 12 of What If book
# IP weighting, marginal structural models

library(readr)
nhefs <- read_csv("data/nhefs.csv")
head(nhefs)

nrow(nhefs)
summary(nhefs)

nhefs$wt82 |> summary()
# only keep those with records in 82
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

# provisionally ignore subjects with missing values for weight in 1982
nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82)),]
# n=1556

# select a few variables
d <- nhefs.nmv[, c('qsmk', 
                   'wt82_71',
                   'age',
                   'sex',
                   'wt71',
                   'smokeintensity',
                   'smokeyrs',
                   'race',
                   'education',
                   'exercise')]

boxplot(wt82_71 ~ qsmk, data = d)

# average weight gain
mean(d$wt82_71[d$qsmk == 0]) # 1.98
mean(d$wt82_71[d$qsmk == 1]) # 4.52

t.test(d$wt82_71[d$qsmk == 0], d$wt82_71[d$qsmk == 1])


# IP ----

head(d)

# estimate IP weights
# logistic reg
fit <- glm(
  qsmk ~ sex + race + age + I(age ^ 2) +
    as.factor(education) + smokeintensity +
    I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
    as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
  family = binomial(),
  data = nhefs.nmv
)
summary(fit)

# for quit smokers (1), predict prob
# for non quitters (0), 1-predict prob
p.qsmk.obs <- ifelse(nhefs.nmv$qsmk == 0,
         1 - predict(fit, type = "response"),
         predict(fit, type = "response"))

p.qsmk.obs

# weight: 1 over probability
nhefs.nmv$w <- 1 / p.qsmk.obs

summary(nhefs.nmv$w)


## lm ----

m1 <- lm(wt82_71 ~ qsmk, data = nhefs.nmv)
summary(m1)

# predict gives the mean of either group
# same if you compute the mean by stratification 
predict(m1, data.frame(qsmk = 1)) # 4.52
predict(m1, data.frame(qsmk = 0)) # 1.98

# beta = 2.54 


## lm with weights ----

?lm

m1w <- lm(wt82_71 ~ qsmk, data = nhefs.nmv, 
          weights = w)
summary(m1w)

predict(m1w, data.frame(qsmk = 1)) # 5.22
predict(m1w, data.frame(qsmk = 0)) # 1.77

# beta = 3.44



# g-formula ----

fit <-
  glm(
    wt82_71 ~ qsmk + sex + race + age + I(age * age) + as.factor(education)
    + smokeintensity + I(smokeintensity * smokeintensity) + smokeyrs
    + I(smokeyrs * smokeyrs) + as.factor(exercise) + as.factor(active)
    + wt71 + I(wt71 * wt71) + qsmk * smokeintensity,
    data = nhefs
  )

summary(fit)

nhefs$predicted.meanY <- predict(fit, nhefs)

mean(nhefs$predicted.meanY)

# select one person
nhefs[which(nhefs$seqn == 24770), c(
  "predicted.meanY",
  "qsmk",
  "sex",
  "race",
  "age",
  "education",
  "smokeintensity",
  "smokeyrs",
  "exercise",
  "active",
  "wt71"
)]

# predicted value vs y
summary(nhefs$predicted.meanY[nhefs$cens == 0])
summary(nhefs$wt82_71[nhefs$cens == 0])


# counterfactual datasets ----
# create a dataset with 3 copies of each subject
nhefs$interv <- -1 # 1st copy: equal to original one

interv0 <- nhefs # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$qsmk <- 0 # A=0 in all rows
interv0$wt82_71 <- NA

interv1 <- nhefs # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$qsmk <- 1 # A=1 in all rows
interv1$wt82_71 <- NA

# use row bind
onesample <- rbind(nhefs, interv0, interv1) # combining datasets


std <- glm(
  wt82_71 ~ qsmk + sex + race + age + I(age * age)
  + as.factor(education) + smokeintensity
  + I(smokeintensity * smokeintensity) + smokeyrs
  + I(smokeyrs * smokeyrs) + as.factor(exercise)
  + as.factor(active) + wt71 + I(wt71 * wt71) + I(qsmk * smokeintensity),
  data = onesample # binded data
)
summary(std)

onesample$predicted_meanY <- predict(std, onesample)
# original data
mean(onesample[which(onesample$interv == -1), ]$predicted_meanY) # 2.56
# A=0
mean(onesample[which(onesample$interv == 0), ]$predicted_meanY) #1.66
# A=1
mean(onesample[which(onesample$interv == 1), ]$predicted_meanY) # 5.17

# difference: (treatment effect)
# 5.17 - 1.66
