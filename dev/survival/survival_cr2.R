# competing risk 2

# simulation 

# hazard ----
# the larger hazard is, the quickier event happens
# lambda is the constant hazard 
# p(event by time t) is 1-exp(-lambda*t)

lambda <- 0.9
t <- seq(0, 8, by = 0.01)
t
pt <- 1-exp(-lambda * t)
plot(t, pt, type = 'l')

# at time t=1
pt[which(t == 1)] # 0.59
# equivalent to
pexp(q = 1, rate = 0.9)

1/lambda

lambda2 <- 0.5
pt2 <- 1-exp(-lambda2*t)
lines(t, pt2, type = 'l', col = 'red')
pt2[which(t == 1)] # 0.39


lambda3 <- 2
pt3 <- 1-exp(-lambda3*t)
lines(t, pt3, type = 'l', col = 'blue')
pt3[which(t == 1)] # 0.86






# data generation ----

# (time constant hazard)
# cause specific hazards
a01 <- 0.3
a02 <- 0.6

# all cause hazard
a0 <- a01 + a02
a0

# use exponential dist for failure time
event_times <- rexp(100, a0)
hist(event_times)

# simulate failure causes
# binomial experiment with prespecified prob
# failure type 1 reutrn 1, else 0
f_cause <- rbinom(100, size = 1, prob = a01/a0)
f_cause |> table()
# roughly 1/3 are type 1

# make it such that outcome corresponds to type 1 and 2
f_cause <- ifelse(f_cause == 0, 2, 1)
f_cause |> table()


# censor time 
# generate 100 censor times, uniformly distributed 0-5
cens_times <- runif(100, min = 0, max = 5)
hist(cens_times)

# observable times
# pmin takes pair-wise minimum
# x1 <- c(1, 2, 3)
# x2 <- c(0, 3, 2)
# pmin(x1, x2)

obs_time <- pmin(event_times, cens_times)
hist(obs_time)

# event indicator
obs_cause <- c(event_times <= cens_times)*f_cause
table(obs_cause)
table(c(event_times <= cens_times))
# 28 no event, 72 event
# among 72 events, 1/3 should be type 1


# cum. cause-specific hazard ----
# nelson aalen estimator 

# install.packages('mvna')
# describe cr multistate model
# possible transition states
tra <- matrix(F, ncol = 3, nrow = 3)
dimnames(tra) <- list(c('0', '1', '2'), 
                      c('0', '1', '2'))

tra[1, 2:3] <- T
tra
# 0 to 1 possible
# 0 to 2 possible

# process observed data
obs_cause

id <- seq_along(obs_cause)
from <- rep(0, length(obs_cause)) # all state 0
to <- as.factor(ifelse(
  obs_cause == 0, 'cens', obs_cause
))
d <- data.frame(id, from, to, time = obs_time)
head(d)

# those from 0 to 0 (stayed in the original state)
# are treated as censored - event did not happen

library(mvna)
?mvna # na estimator in multistate models

nelaal <- mvna(data = d, 
               state.names = c('0', '1', '2'), 
               tra = tra, 
               cens.name = 'cens')
nelaal

# two transitions
nelaal$`0 1`$na |> plot()
nelaal$`0 1`$time

library(lattice)
xyplot(nelaal, strip = strip.custom(bg = 'white'), 
       ylab = 'Cumulative hazard', lwd = 2)


# survival ----
library(survival)
surv <- survfit(Surv(time, to != 'cens') ~1, 
                data = d,
                conf.type = 'log-log')
plot(surv, 
     xlab = 'time', 
     ylab = 'survival probability')
# add theoretical
# 0.9 is the all-cause hazard 
curve(exp(-0.9*x), add = T) 


nelaal$time |> head()
ch01 <- nelaal$`0 1`$na 
st01 <- exp(-1*ch01)
plot(nelaal$time, 1-st01, type = 'l', ylim = c(0, 1))
lines(cif$`1 1`$time, cif$`1 1`$est, col = 'red')

par(mfrow = c(1, 1))

ch02 <- nelaal$`0 2`$na 
st02 <- exp(-1*ch02)
plot(nelaal$time, 1-st02, type = 'l', ylim = c(0, 1))
lines(cif$`1 2`$time, cif$`1 2`$est, col = 'red')

# cum. incidence functions ----
# P(T <= t, XT = 1)
# P(T <= t, XT = 2)

library(cmprsk)
cif <- cuminc(d$time, d$to, cencode = 'cens')
cif
cif$`1 1`$time |> head()

length(cif$`1 1`$time)

plot(cif$`1 1`$time, cif$`1 1`$est, ylim = c(0, 1), type = 'l')
plot(cif$`1 2`$time, cif$`1 2`$est, ylim = c(0, 1), type = 'l')

# 1 1: failure type 1
# 1 2: failure type 2

plot(cif)






# _______ ----
# hospital data ----
# impact of pneumonia on mortality

data("sir.adm")
head(sir.adm)
nrow(sir.adm)
# 747 subjects
sir.adm$status |> table()
# status 0: censored; 1: discharged; 2: dead
sir.adm$pneu |> table()
# 650 0 (no pneumonia), 97 1 (pneumonia)
?sir.adm


# 1. cum. cause-specific hazards
# recode the status

(sir.adm$status == 1) # those discharged
# make them to correspond to status 2 (competing interest)
table(sir.adm$status)
ifelse(sir.adm$status == 1, 2, 1)
to <- ifelse(sir.adm$status == 0, 'cens', ifelse(sir.adm$status == 1, 2, 1))
table(to)
# this step can also be achieved by renaming

dsir <- data.frame(id = sir.adm$id, 
                   from = 0, 
                   to = to, 
                   time = sir.adm$time, 
                   pneu = sir.adm$pneu)
head(dsir)

# compute cum cause specific hazard for death and discharge
# stratified for pneumonia
tra # 0-1, 0-2
# filter on pneumonia
nelaal_nop <- mvna(dsir[dsir$pneu == 0, ], 
                   state.names = c('0', '1', '2'), 
                   tra, 'cens')

nelaal_p <- mvna(dsir[dsir$pneu == 1, ], 
                   state.names = c('0', '1', '2'), 
                   tra, 'cens')


xyplot(nelaal_nop, strip = strip.custom(bg = 'white'), 
       ylab = 'Cumulative hazard pneumonia no', lwd = 2)

xyplot(nelaal_p, strip = strip.custom(bg = 'white'), 
       ylab = 'Cumulative hazard pneumonia yes', lwd = 2)










# ahus ----
library(ggplot2)
# install.packages('ggfortify')
library(ggfortify)
dahus <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Demograpphics")
dahus
dahus <- dahus[-6,]
hist(dahus$los)

dahus$status <- 1
dahus$time <- dahus$los
?Surv
sur <- with(dahus, Surv(time, status))
sur
km <- survfit(sur ~ 1)
plot(km)
autoplot(km)
# stratify by adm_type
km_adm <- survfit(Surv(time, status) ~ adm_type, data = dahus)
plot(km_adm)
autoplot(km_adm)








