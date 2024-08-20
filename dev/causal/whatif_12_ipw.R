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


# lm ----

m1 <- lm(wt82_71 ~ qsmk, data = nhefs.nmv)
summary(m1)

# predict gives the mean of either group
# same if you compute the mean by stratification 
predict(m1, data.frame(qsmk = 1)) # 4.52
predict(m1, data.frame(qsmk = 0)) # 1.98

# beta = 2.54 


# lm with weights ----

?lm

m1w <- lm(wt82_71 ~ qsmk, data = nhefs.nmv, 
          weights = w)
summary(m1w)

predict(m1w, data.frame(qsmk = 1)) # 5.22
predict(m1w, data.frame(qsmk = 0)) # 1.77

# beta = 3.44




