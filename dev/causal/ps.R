# this script explores 
# Install packages if you haven't already
install.packages("MatchIt")
# install.packages("dplyr")

# Load packages
library(MatchIt)
library(dplyr)


# simulate data ----
set.seed(123)  # For reproducibility

# Simulate covariates
n <- 500
age <- rnorm(n, mean = 50, sd = 10)
income <- rnorm(n, mean = 50000, sd = 15000)
health_score <- rnorm(n, mean = 0, sd = 1)

# Simulate treatment assignment based on covariates
# Higher age and lower health scores increase the likelihood of receiving treatment
treatment_prob <- plogis(0.05 * age - 0.01 * income / 10000 - 0.2 * health_score + 0.2)
plot(treatment_prob)
treatment <- rbinom(n, 1, treatment_prob)
treatment


# Simulate outcome (e.g., health outcome)
# Outcome depends on both treatment and covariates
outcome <- 5 + 2 * treatment + 0.1 * age - 0.01 * income + 0.5 * health_score + rnorm(n)

# Combine into a data frame
data <- data.frame(age, income, health_score, treatment, outcome)
head(data)


# data$treatment <- as.character(data$treatment)


# estimate ps ----

# Estimate propensity scores using logistic regression
ps_model <- glm(treatment ~ age + income + health_score, 
                data = data, 
                family = binomial)

# attach ps
data$propensity_score <- predict(ps_model, type = "response")


plot(data$propensity_score)
# propensity score matching -----
# Perform nearest neighbor matching on the propensity score
matchit_result <- matchit(treatment ~ age + income + health_score, 
                          data = data, method = "nearest", distance = "logit")


# this is a matched object
# Get the matched data
matched_data <- match.data(matchit_result)

# Check balance
summary(matchit_result)

# Estimate treatment effect
treatment_effect <- matched_data %>%
  group_by(treatment) %>%
  summarize(mean_outcome = mean(outcome))
ATE <- treatment_effect$mean_outcome[2] - treatment_effect$mean_outcome[1]
print(ATE)





# propensity score weighting ----

# install.packages('twang')
# install.packages('survey')

library(twang)
library(survey)
library(magrittr)
library(dplyr)
# Calculate IPTW weights
data <- data %>%
  mutate(weight = ifelse(treatment == 1, 1 / propensity_score, 1 / (1 - propensity_score)))


data

plot(data$weight)
data$treatment |> table()

# Inspect weight distribution
summary(data$weight)
hist(data$weight, main = "Distribution of Weights", xlab = "Weight")


# Check covariate balance using weighted standardized mean differences
bal <- bal.table(treatment ~ age + income + health_score, 
                 data = data, weights = data$weight)
?bal.table
print(bal)


# Apply truncation to weights to avoid extreme values
data <- data %>%
  mutate(weight = pmin(pmax(weight, 0.1), 10))  # Truncate weights between 0.1 and 10

# Recheck balance with truncated weights
bal <- bal.table(treatment ~ age + income + health_score, data = data, weights = data$weight)
print(bal)



# install.packages('WeightIt')
library(WeightIt)
# Step 1: Estimate propensity scores and compute weights using the weightIt package
psw_model <- weightit(treatment ~ age + income + health_score, data = data, method = "ps", estimand = "ATE")

# Step 2: Check covariate balance after weighting
# The balance function from the 'cobalt' package can be used to assess balance
bal.table <- bal.table(psw_model)
print(bal.table)

# Visualize the balance using Love plots
love.plot(bal.table, threshold = 0.1)

# Step 3: Estimate the treatment effect using weighted data
# Use survey package to apply weights and estimate treatment effect
design <- svydesign(ids = ~1, data = data, weights = ~weights(psw_model))

# Estimate the treatment effect using a weighted linear model
weighted_model <- svyglm(outcome ~ treatment, design = design)
summary(weighted_model)
