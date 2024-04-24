# try some code for survey analysis 

# install.packages('survey')
library(survey)

mini_nhanes <- readRDS("~/Documents/GitHub/data-apothecary-notes/data/mini_nhanes.rds")

# make data.table format
nhanes <- data.table::data.table(mini_nhanes)

nhanes



?svydesign
?apistrat
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

summary(dstrat)
