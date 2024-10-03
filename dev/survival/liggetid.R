# analysis on liggetid data

library(dplyr)
library(ggplot2)
library(survival)
library(ggsurvfit)
library(data.table)


los <- readRDS("~/Documents/GitHub/data-apothecary-notes/data/los.rds")
# table(dlos$admission_from)
head(los)

# to increase readability, process
data.table::setDT(los)

los$admission_year |> table()
table(los$admission_year, los$stroke)

# code a status: 1 indicates released. all of them
los$discharge <- 1

# take those from 1985 onwards
los <- los[admission_year > 1985]

# outcome: time to release (los)
# no censoring

boxplot(los ~ sex, data = los)


# we know the true time (los)
# now randomly select subjects (20%), make a new time, then mark censor

set.seed(1)
n <- nrow(los)
id_time <- sample(1:n, size = 0.2*n, replace = F)

los[, time := los]
los[id_time, time := round(0.9*los, digits = 0)]

# make new status
los[, status := 1]
los[id_time, status := 0] # censored


# for those with stroke, simulate a time
# if no stroke, the time is same as overall time
los[stroke == 1, time_stroke := round(0.7*time, digits = 0)]
los[stroke == 0, time_stroke := time]

head(los)





# km plot ----

Surv(los$los, los$status)[1:6]
# subject 1 has event day at 13, ...

# create survival curve

s1 <- survfit(Surv(time, status) ~ 1, data = los)


# from ggsurvfit pkg
ss <- survfit2(Surv(time, status) ~1, data = los)


ggsurvfit(ss) + labs(x = 'Days', 
                     y = 'Overall survival prob')

# add ci 
ggsurvfit(ss) + add_confidence_interval() + add_censor_mark()


# add risk table
ggsurvfit(ss) + add_confidence_interval() + add_risktable()

# x-day survival -----
# probability of survival (discharge) beyond x days

nrow(los) 

# beyond 30 days
summary(survfit(Surv(los, status)~1, data = los), 
        times = 30)
los[los>=30] # n at risk
# n at risk: 697; n event 450
# p = 60.5

summary(survfit(Surv(los, status)~1, data = los), 
        times = 100)
los[los>=100] # n at risk
# n at risk: 289; n event: 851
# p = 25.3%

ggsurvfit(ss) + geom_vline(xintercept = c(30, 100), col = 'red')
# 30 day hospital discharge probability is 1-0.6

summary(survfit(Surv(los, status)~1, data = los), 
        times = 500)
# n at risk: 75; n event: 1064
los[los>=500] # n at risk


los[los<30] # 442
los[los<=30] # 450
los[los<=100] # 851
los[los<500] # 1064

# ignoring censoring -> overestimate os, underestimate median survival time
# n at risk are consistent - we have enough info on when event havent' happened 
# n event is slightly more than the observed -> it counts t <= T!


?survfit
summary_surv <- summary(survfit(Surv(los, status)~1, data = los))
summary_surv$n.risk + cumsum(summary_surv$n.event)
head(summary_surv)

ss <- data.frame(time = summary_surv$time, 
                 n_risk = summary_surv$n.risk, 
                 n_event = summary_surv$n.event) |> data.table::setDT()

ss[time >450 & time <550]



# median survival time ----


survfit(Surv(time, status) ~ 1, data = los)
# median: 35

median(los$los)
median(los$time)
# it is consistent only because we don't have censoring



# log rank test ----

survdiff(Surv(time, status) ~ sex, data = los)
# sig diff between sex

ss_sex <- survfit2(Surv(time, status) ~ sex, data = los)
ggsurvfit(ss_sex) + add_confidence_interval()

summary(survfit(Surv(los, status)~sex, data = los), 
        times = 100)



# stroke?
ss_stk <- survfit2(Surv(time, status) ~ stroke, data = los)
ggsurvfit(ss_stk) + add_confidence_interval()


# sightly longer



# cox model ----

library(gtsummary)
cox_sex <- coxph(Surv(time, status) ~ sex, data = los)
summary(cox_sex)
tbl_regression(cox_sex, exp = T)
# male, hr = 1.42
# male at higher risk of discharge (a good thing..)

# at any time, male has more chance of being discharged

# proportional hazard
cox.zph(cox_sex)
# p = 0.16
# testing an interaction term between covariates and log(time)
plot(cox.zph(cox_sex))

cox_stk <- coxph(Surv(time, status) ~ stroke, data = los)
summary(cox_stk)
tbl_regression(cox_stk, exp = T)

# age?
cox_sexage <- coxph(Surv(time, status) ~ sex + age, data = los)
summary(cox_sexage)
tbl_regression(cox_sexage, exp = T)

colnames(los)

coxph(Surv(time, status) ~ sex + age + stroke + admission_from, 
      data = los) |> tbl_regression(exp = T)



# time varying ------

library(SemiCompRisks)
lls <- copy(los)
lls$id <- 1:nrow(los)

newdata <- tmerge(data1 = lls[, c('id', 'time', 'status')], 
                  data2 = lls[, c('id', 'time', 'status', 'time_stroke', 'stroke')], 
                  id = id, 
                  discharge = event(time, status), 
                  stroke = tdc(time_stroke)) 

newdata |> head()

coxph(Surv(time = tstart, time2 = tstop, event = discharge) ~ stroke, 
      data = newdata) |> tbl_regression()




