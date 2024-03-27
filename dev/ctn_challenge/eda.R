# data challenge 
# https://rconsortium.github.io/RMedicine_website/Competition.html
# objective:
# transform datasets into indicators that are important
# to understand opioid use disorder treatment, and why 
# others dropout or relapse


# understand feasibility

install.packages('public.ctn0094data')
install.packages('public.ctn0094extra')

library(public.ctn0094data)
library(public.ctn0094extra)

public.ctn0094data::demographics
public.ctn0094extra::derived_inductDelay

public.ctn0094extra::derived_visitImputed
public.ctn0094extra::derived_weeklyOpioidPattern[,6]

?public.ctn0094extra::derived_weeklyOpioidPattern

# demographics associated with dropout / relapse
# any drug / substances associated with drop out



# derive pattern data ----
# this helps understand the study procedure

library(public.ctn0094data)
library(public.ctn0094extra)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)


# induction delay (first randomization event)

examplePeople_int <- c(1L, 163L, 210L, 242L, 4L, 17L, 13L, 1103L, 233L, 2089L)

data_ls <- loadRawData(c("randomization", "treatment"))
data_ls$treatment
data_ls$randomization
?loadRawData
# tx: 6 different ones, mixed from 6 studies
# treatment is multiple times
data_ls$randomization$treatment |> table()

# only take first randomization
data_ls$randomization <-
  data_ls$randomization %>%
  select(who, when, treatment, randomized = which) %>%
  # Remove second randomization events
  filter(randomized != 2) %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)

data_ls$treatment <- 
  data_ls$treatment %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)




treatTimeLong_df <-
  # Collapse Data List
  data_ls %>%
  reduce(.f = full_join, by = c("who", "when")) %>%
  as_tibble() %>%
  arrange(who, when) %>%
  # First pass: find any day with a dose of treatment drug
  rowwise() %>%
  mutate(
    treated = if_else(
      condition = !is.na(amount) & amount > 0,
      true = TRUE,
      false = FALSE,
      missing = FALSE
    )
  )


# this table merges the treatment and timestamps
inductDelay_df <-
  treatTimeLong_df %>%
  # Find the day of the first treatment
  group_by(who) %>%
  arrange(when) %>%
  filter(treated) %>%
  slice(1) %>%
  mutate(treatStart = when) %>%
  select(who, when, treatStart) %>%
  # Add the first day back to the original data
  left_join(treatTimeLong_df, ., by = c("who", "when")) %>%
  group_by(who) %>%
  fill(treatStart, .direction = "updown") %>%
  # Calculate the delay
  filter(randomized == 1) %>%
  # This sets time to be missing if the induction was not observed
  mutate(inductDelay = treatStart - when) %>%
  select(who, treatment, inductDelay) %>%
  ungroup()

# 242 has 5 days delay, his first treatment is F
filter(treatTimeLong_df, who == 242)

# skeleton ----
# the skeleton is to have all day index, to mark the missing
# derive pre-study/baaseline, baseline period
# protocol 27 and 51 only randomized once
# 30d prior to consent - 24weeks (24*7 days)

start_int <- c(`27` = -30L, `51` = -30L)
end_int   <- c(`27` = 168L, `51` = 168L)
backbone2751_df <- 
  CreateProtocolHistory(
    start_vec = start_int,
    end_vec = end_int
  ) %>% 
  filter(who %in% examplePeople_int)

# this is the placeholder

filter(backbone2751_df, who == 13) |> tail()
backbone2751_df$project |> table() # ssome are 27, somee are 51
# create protocol might take this info innto account already

# CTN30 is adaptive, some have longer eperiod

backbone30_df <-
  randomization %>%
  full_join(everybody, by = "who") %>%
  filter(project == "30") %>%
  filter(who %in% examplePeople_int) %>% 
  CreateCTN30ProtocolHistory() %>%
  mutate(project = "30") %>%
  select(who, project, when)

backbone30_df

backbone30_df %>% 
  group_by(who) %>% 
  summarise(lastDay = max(when))

# put together
backbone_df <-
  bind_rows(backbone2751_df, backbone30_df) %>%
  arrange(who)


data_ls <- loadRawData(c("randomization", "visit"))

data_ls$randomization <-
  data_ls$randomization %>%
  select(who, when, treatment, randomized = which) %>%
  # Remove second randomization events
  filter(randomized != 2) %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)

data_ls$visit <- 
  data_ls$visit %>% 
  filter(who %in% examplePeople_int)

# join back to the skeleton (daily data)

timelineRand1_df <-
  data_ls$randomization %>%
  mutate(randomized = randomized == "1") %>%
  # Join to backbone and arrange within subject by day
  full_join(backbone_df, by = c("who", "when")) %>%
  group_by(who) %>%
  arrange(when, .by_group = TRUE) %>%
  select(who, project, when, randomized)

timelineRand1_df

timelineVisit1_df <-
  data_ls$visit %>%
  select(who, when, visit, status = what) %>%
  filter(status %in% c("visit", "final")) %>%
  mutate(visit = TRUE) %>%
  select(who, when, visit) %>%
  left_join(timelineRand1_df, ., by = c("who", "when"))

timelineVisit1_df

timelineMissing1_df <- MarkMissing(timelineVisit1_df) 


derived_visitImputed <-
  timelineMissing1_df %>%
  mutate(visit = as.character(visit)) %>%
  replace_na(list(visit = "", visitYM = "")) %>%
  mutate(visitImputed = paste0(visit, visitYM)) %>%
  mutate(
    visitImputed = str_replace(
      visitImputed, pattern = "TRUETRUE", replacement = "Present"
    )
  ) %>%
  select(who, when, visitImputed) %>%
  filter(visitImputed != "") %>%
  ungroup()



# _________ --------
# derived datasets ----

induct_delay <- public.ctn0094extra::derived_inductDelay
induct_delay
# not all subjects


pattern_o <- public.ctn0094extra::derived_weeklyOpioidPattern
pattern_o
pattern_o$randWeek1 |> table() # 2492 0


pattern_t <- public.ctn0094extra::derived_weeklyTLFBPattern
pattern_t
pattern_t$randWeek1 |> table()
# there is slight difference




# TO DO: outcome as per CTNote ----
library(magrittr)
library(dplyr)
library(CTNote)

udsOutcomes_df <- 
  CTNote::outcomesCTN0094 %>% 
  select(who, usePatternUDS)

?CTNote::outcomesCTN0094

# Make a copy
outcomesAbs_df <- udsOutcomes_df


###  Examples  ###
examplePeople_int <- c(1, 163, 210, 242, 4, 17, 13, 1103, 233, 2089)
outcomesAbs_df %>% 
  filter(who %in% examplePeople_int)

outcomesAbs_df <- 
  outcomesAbs_df %>%
  rowwise() %>% 
  # mixed results != abstinence
  mutate(
    udsPattern = recode_missing_visits(
      use_pattern = usePatternUDS,
      missing_is = "*"
    )
  ) %>% 
  mutate(
    Ab_ctnNinetyFour_2023 = any(
      detect_subpattern(
        use_pattern = udsPattern,
        subpattern = c("----", "o---", "-o--", "--o-", "---o"),
        start = 12,
        end = 15
      )
    )
  ) %>% 
  select(who, Ab_ctnNinetyFour_2023) %>% 
  left_join(outcomesAbs_df, ., by = "who")




outcomesAbs_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, Ab_ctnNinetyFour_2023)

outcomesAbs_df$Ab_ctnNinetyFour_2023 |> table()


# compare 
out <- CTNote::outcomesCTN0094
out$ctn0094_relapse_event |> table() # relapse


udsOutcomes_df <- 
  CTNote::outcomesCTN0094 %>% 
  select(who, usePatternUDS)

outcomesRel_df <- udsOutcomes_df

outcomesRel_df <- 
  outcomesRel_df %>%
  rowwise() %>% 
  mutate(
    udsPattern = recode_missing_visits(
      use_pattern = usePatternUDS
    )
  ) %>%
  mutate(
    udsPattern = recode_missing_visits(
      use_pattern = udsPattern,
      missing_is = "*"
    )
  ) %>% 
  mutate(
    ctn0094_relapse = detect_in_window(
      use_pattern = udsPattern,
      window_width = 4L,
      threshold = 4L
    )
  ) %>% 
  unnest(cols = "ctn0094_relapse", names_sep = "_") %>% 
  select(who, starts_with("ctn0094_relapse")) %>% 
  rename(
    RsT_ctnNinetyFour_2023 = ctn0094_relapse_time,
    RsE_ctnNinetyFour_2023 = ctn0094_relapse_event
  ) %>% 
  left_join(outcomesRel_df, ., by = "who")


outcomesRel_df %>% 
  filter(who %in% examplePeople_int) %>% 
  select(who, usePatternUDS, RsT_ctnNinetyFour_2023, RsE_ctnNinetyFour_2023)

outcomesRel_df$RsE_ctnNinetyFour_2023 |> table()


# TO DO: risk factors ----

# select the relevant ones, merge into a big risk factor df

public.ctn0094data::everybody # project ID


public.ctn0094data::demographics
# age, race, job, stable living, educaton, marital, sex


public.ctn0094data::all_drugs # drug records 
public.ctn0094data::all_drugs$source |> table()

public.ctn0094data::asi # intravenous

public.ctn0094data::detox # start and end time of detox, if knowns

public.ctn0094data::fagerstrom # nicotine dependence

public.ctn0094data::pain # pain (not for everyone possible)


public.ctn0094data::psychiatric # psychiatric

public.ctn0094data::qol # only for 50, so probably irrelevant


public.ctn0094data::rbs$what |> table() # risk behavior

public.ctn0094data::rbs_iv # elaborated rbs, injections

?public.ctn0094data::sex # sex behavior

public.ctn0094data::tlfb # self reportd drugs

public.ctn0094data::treatment
public.ctn0094data::randomization


public.ctn0094data::withdrawal
# public.ctn0094data::meta_substance_groups_uds


