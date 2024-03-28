# process data into wide format
# only use c51 data

# 1. outcome (drug pattern)
# 2. risk factors (static)
# 3. temporal

library(public.ctn0094data)
library(public.ctn0094extra)
library(CTNote)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)




# outcome ----
# relapse is the primary outcome of ctn51

out <- CTNote::outcomesCTN0094

# take c51 

everybody$project |> table() # this includes all eligible
out_c51 <- left_join(everybody, out) |> 
  filter(project == 51)
out_c51

# check length
# between 22 to 25. this looks valid
out_c51$usePatternUDS |> nchar() |> hist()


nchar('ooo')




# risk factors ----
# select the relevant ones, merge into a big risk factor df

id <- data.frame(who = 1:3560)

data_rf <- left_join(id, everybody) |> 
  left_join(demographics) |> 
  left_join(qol) |> 
  left_join(fagerstrom) |> 
  left_join(psychiatric) |> 
  left_join(pain) |> 
  left_join(asi)

# filter c51
rf_c51 <- filter(data_rf, project == 51)



# _________ -----
# treatment related ----


randomization$who |> unique() |> length() # 2492

# ctn51, elibigle 772, rx 570
rx_c51 <- left_join(randomization, everybody) |> 
  filter(project == 51) |> 
  filter(which == 1) |> 
  select(who, treatment, when)




# c51 relapse
# data_c51_out <- left_join(data_c51, relapse)
# lm_c51 <- glm(ctn0094_relapse_event ~ treatment,
#               data = data_c51_out, 
#               family = 'binomial')
# 
# lm_c51 |> summary()
# 
# exp(0.385) # NR-NTX more likely to relapse (OR 1.47)


# treatment ------

id_c51 <- filter(everybody, project == 51)

# make a function to extract ONE individual's temporal events
# time: 0-15 (study day)
filter(randomization, who %in% id_c51$who)$when |> hist()




# treatment can have 1 (injection), doses
# time: study day
?treatment
filter(treatment, who %in% id_c51$who)$amount |> table()
filter(treatment, who %in% id_c51$who)$when |> hist()
# time: 0-200+



# withdrawal
# 0: none; 1: mild; 2: moderate; 3: severe
# time: study day
?withdrawal
filter(withdrawal, who %in% id_c51$who)
filter(withdrawal, who %in% id_c51$who)$when |> hist()
# time: 0-300+



# risky behavior (not time related)
# fro each person, all 5 types are recorded
filter(rbs, who %in% id_c51$who)
?rbs
rbs$what |> table() # risk behavior
rbs$days |> table() # continuous range
# rbs_iv # elaborated rbs, injections


# drugs 
#filter(tlfb, who ==22)
#tlfb # self reportd drugs
# time: study day
filter(all_drugs, who ==22 & source != 'TFB') 
# more than one per patient; not all patients
# for c51, only tfb and uds
filter(all_drugs, who %in% id_c51$who)$source |> table()
# types are more complicated
filter(all_drugs, who %in% id_c51$who)$what |> table()
# time: -60 - 300+
filter(all_drugs, who %in% id_c51$who)$when |> hist()




# detox
# time: day
filter(detox, who==22)

# detox event should be taken into account
filter(detox, who %in% id_c51$who)$when |> hist()

detox # start and end time of detox, if known
detox$who |> unique() |> length() # 658
?detox

# filter(visit, who %in% id_c51$who)$visit |> table()
# ?visit


# ______ ----
# gather temporal information, understand process

# some timestamps are weekly
# some are daily
# not all are complete

get_events_temporal(id = 22)
get_events_temporal(id = 3)

# try to use id=22

d22 <- get_events_temporal(id = 22)


24*7 # 168
d22$drg$when |> range()

d22$drug_pattern |> nchar() # 24

drg_tfb <- d22$drg_tfb
drg_uds <- d22$drg_uds

# split each drug into wider format
make_wide_drug(data_long = drg_tfb)
make_wide_drug(data_long = drg_uds)






# dd <- data.frame(what = c('Heroin', 'B', 'Heroin', 'B', 'Heroin'), 
#                  when = c(1,1,2,2,3), 
#                  used = c(1,1,1,0,1))
# dd
# pivot_wider(dd, names_from = what, values_from = used)

# need a df: row is time, col is event

# time frame
t0 <- -35
tt <- 248




# utility ----

get_events_temporal <- function(id, 
                                d_outcome = outcomesCTN0094,
                                d_randomization = randomization, 
                                d_treatment = treatment,
                                d_withdrawl = withdrawal,
                                d_detox = detox,
                                d_riskybehavior = rbs,
                                d_drugs = all_drugs){
  # dev
  # id <- 22
  # d_outcome <- outcomesCTN0094
  # d_randomization <- randomization
  # d_treatment <- treatment
  # d_withdrawl <- withdrawal
  # d_detox <- detox
  # d_riskybehavior <- rbs
  # d_drugs <- all_drugs
  
  # radomization: treatment group and time
  rx <- filter(d_randomization, who == id & which == 1) |> 
    select(treatment, when)
  if(nrow(rx) == 0){
    rx <- data.frame(
      amount = NA_integer_,
      when = NA_integer_
    )
  }
  
  # outcome: get pattern
  drug_pattern <- filter(d_outcome, who == id) |> 
    select(usePatternUDS)
  
  # outcome: pattern class
  drug_pattern_class <- filter(d_outcome, who == id) |> 
    select(-c(who, usePatternUDS))
  
  # treatment: amount and time
  tx <- filter(d_treatment, who == id) |> 
    select(amount, when)
  if(nrow(tx) == 0){
    tx <- data.frame(
      amount = NA_integer_,
      when = NA_integer_
    )
  }
  
  # withdrawl
  wthdrw <- filter(d_withdrawl, who == id) |> 
    select(withdrawal, when)
  if(nrow(wthdrw) == 0){
    wthdrw <- data.frame(
      withdrawal = NA_integer_,
      when = NA_integer_
    )
  }
  
  # detox 
  dtx <- filter(d_detox, who == id) |> 
    select(what, when)
  if(nrow(dtx) == 0){
    dtx <- data.frame(
      what = NA_character_,
      when = NA_integer_
    )
  }
  
  # risky behavior
  rb <- filter(d_riskybehavior, who == id) |> 
    select(what, did_use, days)
  if(nrow(rb) == 0){
    rb <- data.frame(
      what = c('cocaine', 'heroin', 'speedball', 'opioid', 'speed'),
      did_use = c('No', 'No', 'No', 'No', 'No'),
      days = rep(0, 5)
    )
  }
  
  # drugs (self-report)
  drg_tfb <- filter(d_drugs, who == id & source == 'TFB') |> 
    select(what, when)
  
  if(nrow(drg_tfb) == 0){
    drg_tfb <- data.frame(
      what = NA_character_,
      when = NA_integer_
    )
  }
  
  # drugs (uds)
  drg_uds <- filter(d_drugs, who == id & source == 'UDS') |> 
    select(what, when)
  
  if(nrow(drg_uds) == 0){
    drg_uds <- data.frame(
      what = NA_character_,
      when = NA_integer_
    )
  }
  
  # put in a list
  res_list <- list(
    id = id,
    rx = rx,
    drug_pattern = drug_pattern,
    drug_pattern_class = drug_pattern_class,
    tx = tx,
    wthdrw = wthdrw,
    dtx = dtx,
    rb = rb,
    drg_tfb = drg_tfb,
    drg_uds = drg_uds
  )
  
  return(res_list)
}



make_wide_drug <- function(data_long){
  
  # attach use indicator
  d <- cbind(data_long, data.frame(used = 'Yes'))
  # make wider
  dw <- pivot_wider(d, names_from = what, values_from = used)
  
  return(dw)
}








