# mixed model for survival

# Install the necessary packages if you haven't already
# install.packages("survival")
install.packages("coxme")

# Load the required libraries
library(survival)
library(coxme)


# Load the dataset
head(lung)

hist(lung$time)


# Introduce a random hospital (center) variable for mixed-effects modeling
set.seed(123)  # For reproducibility
lung$center <- sample(1:5, nrow(lung), replace = TRUE)  # 5 hospital centers

# View the first few rows
head(lung)


# cox ----

# Surv()
ss <- survfit2(Surv(time, status) ~1, data = lung)


ggsurvfit(ss) + labs(x = 'Days', 
                     y = 'Overall survival prob')

# add ci 
ggsurvfit(ss) + add_confidence_interval() + add_censor_mark()

# median survival time
survfit(Surv(time, status) ~ 1, data = lung)
# 310 
median(lung$time) # less than median survival time



# surv diff for log rank test
survdiff(Surv(time, status) ~ center, data = lung)

# no significant difference
ss_center <- survfit2(Surv(time, status) ~ center, data = lung)
ggsurvfit(ss_center) + add_confidence_interval()


# cox model 

library(gtsummary)
cox_sex_age <- coxph(Surv(time, status) ~ sex + age, data = lung)
summary(cox_sex_age)
tbl_regression(cox_sex_age, exp = T)

# sex is 0.6 (p=0.002)
# baseline is male, so if being female, at lower risk 


# coxmodel 2
cox_sex_age_c <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)
summary(cox_sex_age_c)
tbl_regression(cox_sex_age_c, exp = T)

# ph.ecog increases the hazard by twice

cox.zph(cox_sex_age_c)
plot(cox.zph(cox_sex_age_c))
# not significant violation of proportional hazard





# cox mixed effect ----

# Fit the Cox mixed-effects model
coxme_model <- coxme(Surv(time, status) ~ age + sex + ph.ecog + (1 | center), data = lung)

# Display the model summary
summary(coxme_model)

# sd 0.024, some variability in survival outcomes between cetners







# ______ ----
# random effect and longidutinal studies
# Load necessary libraries

library(lme4)
library(ggplot2)
set.seed(123)

# Simulate data for 100 individuals over 10 time points
n_subjects <- 100
n_timepoints <- 10
time <- rep(0:(n_timepoints - 1), times = n_subjects)
id <- rep(1:n_subjects, each = n_timepoints)
group <- rep(c("control", "treatment"), each = 50 * n_timepoints)

# Simulate individual random intercepts and slopes for time
intercepts <- rnorm(n_subjects, 70, 5) # random intercepts (baseline weights)
slopes <- rnorm(n_subjects, -0.5, 0.2) # random slopes (rate of weight loss per week)

# Simulate weights based on time, group, and random effects
group_effect <- ifelse(group == "treatment", -1.5, 0)  # treatment effect
weights <- intercepts[id] + slopes[id] * time + group_effect + rnorm(n_subjects * n_timepoints, 0, 1)

# Create the dataframe
df <- data.frame(id = factor(id), time = time, weight = weights, group = factor(group))
head(df)

# Plotting weight trajectories for each individual
ggplot(df, aes(x = time, y = weight, group = id, color = group)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = group), method = "loess", se = FALSE, size = 1.5) +
  labs(title = "Weight Trajectories Over Time",
       x = "Time (weeks)", y = "Weight (kg)") +
  theme_minimal()


# Fit a mixed-effects model with random intercepts
# same slope for everyone
model_random_intercept <- lmer(weight ~ time * group + (1 | id), data = df)

# Model summary
summary(model_random_intercept)


# Fit a mixed-effects model with random intercepts and random slopes for time
model_random_slope <- lmer(weight ~ time * group + (time | id), data = df)

# Model summary
summary(model_random_slope)


# Compare models using AIC
AIC(model_random_intercept, model_random_slope)

# Perform a likelihood ratio test
anova(model_random_intercept, model_random_slope)
















