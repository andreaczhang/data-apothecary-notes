# process data into wide format
# 1. outcome
# 2. risk factors

library(public.ctn0094data)
library(public.ctn0094extra)
library(CTNote)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(data.table)





# risk factors ----
# select the relevant ones, merge into a big risk factor df

id <- data.frame(who = 1:3560)
head(id)

data_rf <- left_join(id, everybody) |> 
  left_join(demographics) |> 
  left_join(qol) |> 
  left_join(fagerstrom) |> 
  left_join(psychiatric) |> 
  left_join(pain) |> 
  left_join(asi)

head(data_rf)
colnames(data_rf)


# outcome ----
# relapse (for now)

out <- CTNote::outcomesCTN0094

out$ctn0094_relapse_event |> table() # relapse: 619 F 2941 T
out$ctn0094_dropout_event |> table() # 1204 F 2356 T

out$kosten1993B_red |> table() # reduction 2216 F 1344 T

relapse <- select(out, c(who, ctn0094_relapse_event))

# reduction might be a more interesting outcome of interest
reduction <- select(out, c(who, kosten1993B_red))


# M1: rf (no tx) only ----

# merge outcome and rf
df <- left_join(reduction, data_rf)
head(df)

table(df$kosten1993B_red, df$project)





# _________ -----
# treatment related ----


public.ctn0094data::treatment$who |> unique() |> length() # 2373
?public.ctn0094data::randomization
# 
public.ctn0094data::randomization$who |> unique() |> length() # 2492

# simplify the treatment by grouping projects
# only use the first randomzation from ctn30



randomization$which |> table()
id <- data.frame(who = 1:3560)
head(id)

everybody$project |> table()
table(data_rx$treatment, data_rx$project)

# filter data based on project id
# somee require filter due to duplicates
data_c27 <- left_join(randomization, everybody) |> 
  filter(project == 27) |> 
  filter(which == 1)

# ctn51 seems to have complete treatment data
data_c51 <- left_join(randomization, everybody) |> 
  filter(project == 51) |> 
  filter(which == 1)

data_c30 <- left_join(randomization, everybody) |> 
  filter(project == 30) 

data_c30$which |> table() 

filter(data_rx, which == 2)$project |> table()


table(data_rx$project, data_rx$which)

# crude analysis on c27, c51 group 
# c27 reduction
data_c27_out <- left_join(data_c27, reduction)

lm_c27 <- glm(kosten1993B_red ~ treatment,
              data = data_c27_out, 
              family = 'binomial')

lm_c27 |> summary()

# c51 relapse
data_c51_out <- left_join(data_c51, relapse)
lm_c51 <- glm(ctn0094_relapse_event ~ treatment,
              data = data_c51_out, 
              family = 'binomial')

lm_c51 |> summary()

exp(0.385) # NR-NTX more likely to relapse (OR 1.47)


# treatment ------
?treatment
?withdrawal
withdrawal$who |> unique() |> length() # 2580

public.ctn0094data::withdrawal

?rbs
public.ctn0094data::rbs$what |> table() # risk behavior
public.ctn0094data::rbs_iv # elaborated rbs, injections

public.ctn0094data::tlfb # self reportd drugs

# more than one per patient; not all patients
public.ctn0094data::all_drugs # drug records 
public.ctn0094data::all_drugs$source |> table()

public.ctn0094data::detox # start and end time of detox, if known
detox$who |> unique() |> length() # 658

# public.ctn0094data::meta_substance_groups_uds
# public.ctn0094data::sex # sex behavior


