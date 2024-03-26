# try mmrm

# install.packages('mmrm')
library(data.table)
library(mmrm)

# fev data (simulated)
# id, visit number, treatmentt or control
# race, sex, fev1 at baseline, fev1 at study visits
# weighting variable

fev_data |> head()
fev <- fev_data
setDT(fev)

# 200 patients, 4 visits per person
fev

fev$ARMCD |> table() # more or less balanced



# eda -----
p <- ggplot(data = fev, 
            aes(x = AVISIT, y = FEV1, 
                group = USUBJID, 
                color = RACE))
p <- p + geom_line(alpha = 0.5)
p <- p + facet_wrap(~ ARMCD + SEX)
p






# unstructured (us) ----
# unstrucured covariance matrix for visits (timepoints)

fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + 
    us(AVISIT | USUBJID), 
  data = fev_data
)
summary(fit)




