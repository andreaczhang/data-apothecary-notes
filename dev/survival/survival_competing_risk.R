# competing risk ----
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Part_3:_Competing_Risks
library(data.table)
library(dplyr)

data(Melanoma, package = 'MASS')

Melanoma |> head()
# time: survival in days
# status: 1 dead from melanoma, 2 alive, 3 dead other causes
# sex: 1 male 0 female
# year: of operation
# thickness: tumor thickness in mm
# ulcer: 1 presence, 0 absence

melanoma <- data.table(Melanoma)

# recode status: 0 alive, 1 dead mela, 2 dead other
# need to be a factor
melanoma[status == 1, status_new := 1]
melanoma[status == 2, status_new := 0]
melanoma[status == 3, status_new := 2]
melanoma$status_new <- as.factor(melanoma$status_new)

melanoma
# cumulative incidence
library(survival)
library(tidycmprsk)
# use cuminc
# two types of failors
cuminc(Surv(time, status_new) ~ 1, data = melanoma)
melanoma$time |> hist()


# cr regression ----
# 1. cause specific hazards - cox reg, coxph
# 2. subdistribution hazards - fine-gray reg, crr


# 1. need to censor the subjects who didn't have event of interest (1)
# then use coxph
# the censoring means we only care about death from melanoma, or 
# any other outcome (alive, or death other causes)

coxph(
  Surv(time, ifelse(status_new == 1, 1, 0)) ~ sex + age, 
  data = melanoma
)



# 2. crr
# outcome of interest is still (1)

crr(Surv(time, status_new) ~ sex + age, data = melanoma)
# exp(0.588)
# the hazards would be subdistribution hazards
# results are similar



# landmark analysis ----
# install.packages('SemiCompRisks')
data(BMT, package = 'SemiCompRisks')

head(BMT)
# association between acute graft vs host disease (aGVHD) and survival
# aGVHD is assessed after transplant (baseline, start of followup)
?SemiCompRisks::BMT

# g: disease group
# T1: time to death or study time
# T2: disease free survival time (time to relapse, death or end of study)
# delta1: death 1d 0alive
# TA: time to aGVHD
# deltaA: aGVHD indicator 

BMT$T1 |> hist()

# select landmark time: 90 days, based on clinical information 
# subset population for those followed at least until landmark

lm_dat <- BMT |> dplyr::filter(T1>=90)
nrow(BMT)
nrow(lm_dat) # 15 excluded

# set new time, by substracting the time before landmark

lm_dat <- 
  lm_dat %>% 
  dplyr::mutate(
    lm_T1 = T1 - 90
  )

# with coxph, can directly use BMT (full dataset) but set subset
coxph(
  Surv(T1, delta1) ~ deltaA, 
  subset = T1 >= 90, 
  data = BMT
) 


# time dependent covariate ----

# requires constructing a new dataset
# create id variable

library(tibble)

bmt <- rowid_to_column(BMT, 'new_id')
head(bmt)

# tmerge: long dataset with multiple time intervals for different covariate values
# event: new event indicator
# tdc: time dependent covariate indicator

td_dat <- 
  tmerge(
    data1 = bmt %>% select(new_id, T1, delta1), 
    data2 = bmt %>% select(new_id, T1, delta1, TA, deltaA), 
    id = new_id, 
    death = event(T1, delta1),
    agvhd = tdc(TA)
  )

head(bmt)
head(td_dat)
# g: disease group
# T1: time to death or study time
# T2: disease free survival time (time to relapse, death or end of study)
# delta1: death 1d 0alive
# TA: time to aGVHD
# deltaA: aGVHD indicator 

# patient 1: 0-67 fine, 67-2081 agvhd
# patient 2: 0-1602 fine
# patient 4: 0-70: fine; 70-1462: agvhd

# find a dead patient with also agvhd

filter(bmt, delta1 == 1 & deltaA == 1) # 16 patients
# id = 18
filter(bmt, new_id == 18)
filter(td_dat, new_id == 18)
# 0-28 fine, 28-156 agvhd, dead

head(td_dat)
# cox regression on the new data
# agvhd is the covariate (only one), indicator 1 and 0
# this allows the same patient to be accounted for multiple times
# tstart is the beginning of each interval
# tstop is the end of the interval
# for those who have status change (e.g. agvhd), he would have two intervals

coxph(
  Surv(time = tstart, time2 = tstop, event = death) ~ agvhd, 
  data = td_dat
) 
# HR: 1.39
# HR from the landmark analysis was 1.08
# both are not significant


