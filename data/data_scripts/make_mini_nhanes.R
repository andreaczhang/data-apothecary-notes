# process nhanes data ----

NHANES |> summary()
NHANES[1:3,] |> as.data.frame() # duplicates from resample

# raw
NHANESraw |> nrow()
ncol(NHANESraw) # 78
NHANESraw$ID |> unique() |> length()

# take a smaller subsample from nhanes raw
# should focus on one sample year, and above 18yr
NHANESraw$SurveyYr |> unique()
nhanes_1112_adult <- dplyr::filter(NHANESraw, 
                                   SurveyYr == '2011_12' & 
                                     Age >= 18)


# take 500
set.seed(1)
rowid <- sample(x = 1:nrow(nhanes_1112_adult), 
                size = 500, replace = F)

data500 <- nhanes_1112_adult[rowid,]

# check missing
summary(data500)

# narrow down variables

# ID
# SurveyYr (might indicate some missing pattern)
# Gender
# Age
# Race1
# Education 
# MaritalStatus 
# HHIncome 
# Work 
# BMI 
# Diabetes 
# HealthGen self reported
# Depressed 
# SleepHrsNight 
# SleepTrouble 
# PhysActive
# AlcoholYear
# SmokeNow
# Smoke100

colnames(data500)
varnames <- c('ID',
              'Gender',
              'Age',
              # 'AgeDecade',
              'Race1',
              'Education',
              'MaritalStatus',
              'HHIncome',
              'Work',
              'BMI',
              'Diabetes',
              'HealthGen',
              'Depressed',
              'SleepHrsNight',
              'SleepTrouble',
              'PhysActive',
              'AlcoholYear',
              'SmokeNow',
              'Smoke100')


d <- dplyr::select(data500, all_of(varnames))
#ncol(d)
#d[, 17:18]

# smoke status
d <- dplyr::mutate(d, Smokestatus = case_when(
  SmokeNow == 'Yes' & Smoke100 == 'Yes' ~ 'Current',
  SmokeNow == 'No' & Smoke100 == 'Yes' ~ 'Former',
  .default = 'Never'
))
d$Smokestatus <- factor(d$Smokestatus)
d$Smokestatus


summary(d)

# this data does not have many numerical values
hist(d$AlcoholYear)
hist(d$SleepHrsNight)
hist(d$BMI)


# initial missing vis
mice::md.pattern(d, rotate.names = T)


# mice::nhanes
# mice::nhanes2
# save data

saveRDS(d, file = 'data/mini_nhanes.rds')
write.csv(d, row.names = F, file = 'data/mini_nhanes.csv')

