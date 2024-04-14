# mixed model
# https://m-clark.github.io/mixed-models-with-R/random_intercepts.html
gpa <- read.csv("~/Documents/GitHub/data-apothecary-notes/data/gpa.csv")

colnames(gpa)
head(gpa)
# occason: 1-5

library(ggplot2)

p <- ggplot(data = gpa, aes(x = occasion, y = gpa, group = student))
p <- p + geom_line(alpha = 0.5)
p


gpa$student_idc <- as.factor(gpa$student)
gpa$occasion_c <- as.factor(gpa$occasion)


# no random effect ----
# initial: occasion only
# gpa ~ N(mu, sigma)
# mu = b0+b1*occ

gpa_lm <- lm(gpa ~ occasion, data = gpa)
summary(gpa_lm)

# b0: 2.60 (se 0.018)
# b1 (occasion): 0.11 (se 0.006)



# random intercept ----
# student-specific intercepts
# occasion + student effect

library(lme4)
gpa_stu <- lmer(gpa ~ occasion + (1|student), 
                data = gpa)
summary(gpa_stu)

str(gpa)

# fixed effects 
# b0: 2.60 (se 0.02) - increased slightly se 
# b1 (occasion): 0.11 (se 0.004)

confint(gpa_stu)

ranef(gpa_stu)$stu |> head()
# install.packages('merTools')
?ranef


# predict 
predict(gpa_stu, re.form = NA) |> head()
# equivalent to linear reg
predict(gpa_lm) |> head()

# add random effect
predict(gpa_stu) |> head()


# if plot together
plot(gpa$gpa[1:5], pch = 20)
points(predict(gpa_lm) |> head(), col = 'red')
points(predict(gpa_stu) |> head(), col = 'blue')


# other structures ----

#install.packages('mmrm')
library(mmrm)
gpa_stu_mmrm <- mmrm(
  formula = gpa ~ occasion_c + us(occasion_c|student_idc),
  data = gpa
)

coef(gpa_stu_mmrm)
# these two are equivalent
# gpa_stu2 <- lmer(gpa ~ occasion_c +  (0 + occasion_c|student_idc), 
#                  control = lmerControl(check.nobs.vs.nRE = "ignore"),
#                 data = gpa)
# summary(gpa_stu2)
# ranef(gpa_stu2)





