# sleepstudy data from lme4
# http://optimumsportsperformance.com/blog/mixed-models-in-sport-science-frequentist-bayesian/
library(ggplot2)
library(magrittr)
library(dplyr)
library(broom)
library(gt)


d <- lme4::sleepstudy
head(d)

# reaction, days, subject

# overall
d %>%
  group_by(Days) %>%
  summarize(N = n(),
            avg = mean(Reaction),
            se = sd(Reaction) / sqrt(N)) %>%
  ggplot(aes(x = Days, y = avg)) +
  geom_ribbon(aes(ymin = avg - 1.96*se, ymax = avg + 1.96*se),
              fill = "light grey",
              alpha = 0.8) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(labels = seq(from = 0, to = 9, by = 1),
                     breaks = seq(from = 0, to = 9, by = 1)) +
  labs(x = "Days of Sleep Deprivation",
       y = "Average Reaction Time",
       title = "Reaction Time ~ Days of Sleep Deprivation",
       subtitle = "Mean ± 95% CI")


# plot per subject
# one regression line per person
# 335 shows negative slope
# 308 shows very positive slope

d %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_line(size = 1) +
  geom_point(shape = 21,
             size = 2,
             color = "black",
             fill = "white") +
  geom_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~Subject) +
  scale_x_continuous(labels = seq(from = 0, to = 9, by = 3),
                     breaks = seq(from = 0, to = 9, by = 3)) +
  labs(x = "Days of Sleep Deprivation",
       y = "Average Reaction Time",
       title = "Reaction Time ~ Days of Sleep Deprivation")


# lm -----

fit_lm <- lm(Reaction ~ Days, data = d)
summ_lm <- summary(fit_lm)

# 1 unit increase of sleep deprivation increases 10s in reaction
# 28.7% variance explained
# se: 1.238

confint(fit_lm)

# summ_lm$df
# by hand: use t-dist rather than norm
summ_lm$coefficients[,1] + qt(0.025, df = 178)*summ_lm$coefficients[,2]
summ_lm$coefficients[,1] + qt(0.975, df = 178)*summ_lm$coefficients[,2]


# lm by subject ----

ind_reg <- d %>%
  group_by(Subject) %>%
  group_modify(~ tidy(lm(Reaction ~ Days, data = .x)))

# extract coefficient for Days
ind_days_slope <- ind_reg %>%
  filter(term == "Days")


ind_days_slope %>%
  ungroup() %>%
  gt() %>%
  fmt_number(columns = estimate:statistic,
             decimals = 2) %>%
  fmt_number(columns = p.value,
             decimals = 3)


# ind_days_slope
# can compare the results with the graph
# 335 has -2.88
# 308 has 21.76

# compare with population (10.47)
pop_avg <- summ_lm$coefficients[2,1] 


plt_to_avg <- ind_days_slope %>%
  mutate(pop_avg = pop_avg,
         diff = estimate - pop_avg) %>%
  ggplot(aes(x = estimate, y = as.factor(Subject))) +
  geom_vline(xintercept = pop_avg) +
  geom_segment(aes(x = diff + pop_avg, 
                   xend = pop_avg, 
                   y = Subject, 
                   yend = Subject),
               size = 1.2) +
  geom_point(size = 4) +
  labs(x = NULL,
       y = "Subject",
       title = "Difference compared to population\naverage change in reaction time (10.47 sec)")

plt_to_avg

# mm (intercept only) -----
# in lm, subjects share the same variance
library(lme4)

fit1 <- lmer(Reaction ~ 1 + (1|Subject), data = d)
summ_f1 <- summary(fit1)
summ_f1
summ_f1$coefficients
summ_f1$vcov
summ_f1$varcor

mean(d$Reaction) # 298.51
# error terms: sd around population intercept 35.75
# this is the between subject variability
# residual: within-subject variability, 44.2

ranef(fit1)
# this is how far each subject deviates from the population intercept 298

# mm one fixed effect ----

fit2 <- lmer(Reaction ~ Days + (1|Subject), data = d)
summary(fit2)
# beta1 = 10.46, very close to lm

summ_f2 <- summary(fit2)
summ_f2$coefficients
# now fixed effect (global) has both intercept and slope
ranef(fit2)
# compute intercept for each person by hand
251.405 + ranef(fit2)$Subject
# equivalent to 
coef(fit2)

# is it equivalent to lm per subject (inter only)?
# no 
ind_reg0 <- d %>%
  group_by(Subject) %>%
  group_modify(~ tidy(lm(Reaction ~ 1, data = .x)))
ind_reg0$estimate

coef(fit2)$Subject[,1] - ind_reg0$estimate
# differ by around 49
plot(coef(fit2)$Subject[,1], ind_reg0$estimate)

# mm (random slope and intercept) ----
# equivalent to (Days|Subject)

fit3 <- lmer(Reaction ~ Days + (1 + Days|Subject), data = d)
summ_f3 <- summary(fit3)
summ_f3
# fixed: 
# days 10.47
# b0 251.40 (global intercept)


# random:
coef(fit3) 


# compare ----
# unable to comapre fit_lm
# fit1: intercept only, one subject per group
# fit2: fixed effect (day), one subject per group

anova(fit1, fit2)

# significant, fit2 better

# estimate population effect, while adjusting the se given
# the violation of independence assumption for lm

# complete pooling: fit_lm
# partial-pooling: fit1, fit2

# no pooling (treat each as their own dataset)
# each subject is a fixed effect
# remove the intercept
fit_no_pool <- lm(Reaction ~ Days + Subject -1, data = d)
summary(fit_no_pool)


# select 4 subjects
Subject <- as.factor(c(308, 309, 335, 331))
Days <- seq(from = 0, to = 9, by = 1)
pred_df <- tidyr::crossing(Subject, Days) # grid.expand
pred_df

## 1. complete pooling predictions (lm)
pred_df$complete_pool_pred_lm <- predict(fit_lm, newdata = pred_df)

## 2. no pooling predictions
pred_df$no_pool_pred <- predict(fit_no_pool, newdata = pred_df)


## 3. summary measures/individual regression
subject_coefs <- ind_reg %>%
  filter(Subject %in% unique(pred_df$Subject)) %>%
  dplyr::select(Subject, term, estimate) %>%
  mutate(term = ifelse(term == "(Intercept)", "Intercept", "Days")) %>%
  tidyr::pivot_wider(names_from = term,
              values_from = estimate)

subject_coefs %>%
  as.data.frame()

pred_df <- pred_df %>%
  mutate(ind_reg_pred = ifelse(
    Subject == 308, 244.19 + 21.8*Days,
    ifelse(
      Subject == 309, 205.05 + 2.26*Days,
      ifelse(
        Subject == 331, 285.74 + 5.27*Days,
        ifelse(Subject == 335, 263.03 - 2.88*Days, NA)
      )
    )
  ))


## 4. partial pooling predictions (fit2)
pred_df$partial_pool_pred_rndint <- predict(fit2, 
                                     newdata = pred_df,
                                     re.form = ~(1 + 1|Subject))

## 5. partial pooling predictions (fit3)
pred_df$partial_pool_pred_rndintslp <- predict(fit3, 
                                            newdata = pred_df,
                                            re.form = ~(1 + Days|Subject))




# attach original data
## get original results and add to the predicted data frame
subject_obs_data <- d %>%
  filter(Subject %in% unique(pred_df$Subject)) %>%
  dplyr::select(Subject, Days, Reaction)

pred_df <- pred_df %>%
  left_join(subject_obs_data)


## final predicted data set with original observations
pred_df %>%
  head()

## plot results
dp <- pred_df %>%
  tidyr::pivot_longer(cols = complete_pool_pred_lm:partial_pool_pred_rndintslp,
               names_to = "model_pred") %>%
  arrange(model_pred) 

ggplot(dp, aes(x = Days, y = Reaction)) +
  geom_point(size = 4,
             shape = 21,
             color = "black",
             fill = "white") +
  geom_line(aes(y = value, color = model_pred),
            size = 1.1) +
  facet_wrap(~Subject)

# complete pool: lm, same reg line for all subjects
# no pool pred, partial pool pred ver close
# the slope is the same as lm
summary(fit2)
coef(fit2)
summary(fit_no_pool)
# estimate for each subject differs a little bit

# ind_reg_pred: its own slope and intercept

