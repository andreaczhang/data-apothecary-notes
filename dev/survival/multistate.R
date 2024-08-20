# multistate modeling

# install.packages('msm')
library(msm)
psor |> head() # psoriatic arthritis
?psor
psor$state |> table()
psor$ptnum |> unique() |> length() # 305 patients
# 4 states, from 1 to 4 increasing severity of damaged joints

cav |> head()
cav$PTNUM |> unique() |> length() # 622 patients
cav$state |> table()
# 4 states
# 4 is death, no returning
# while other 3 are increasing severity of cav (coronary artery disease)


# basic msm ----

# psor
# p(1->4)
# time expected over next 10 years with 10 or more damaged (4)

# state need to be in numbers
# need to specify the transition structure of the model
psor |> head() # psoriatic arthritis

# time (month): time of observed state, not transition!!!

# summarise the transition 
# 1-2, 2-3, ...
# by counting
# this would require the patient observation time to be ordered

statetable.msm(state = state, subject = ptnum, data = psor)

# 16 people wen from 1 to 3

hist(psor$months)

# define transition structure

Q <- rbind(c(0, 1, 0, 0), 
           c(0, 0, 1, 0), 
           c(0, 0, 0, 1),
           c(0, 0, 0, 0))

# if compare with the state table, some patients went from 1 to 3 directly
# this is not allowed in transition structure
# these people must have gone through 2, without being recorded


psor_msm <- msm(state ~ months, 
                subject = ptnum, 
                data = psor, 
                qmatrix = Q, 
                gen.inits = T)
psor_msm
summary(psor_msm)

# the output is the intensity

# prediction ----

# mean sojourn time
# in a state r, is the expected length of period spent in that state
# defined as -1/q_rr

sojourn.msm(psor_msm)
# 1/0.09125


# transition probability
# need to specify interval t
psor$months |> hist()

pmatrix.msm(psor_msm, t=1)
pmatrix.msm(psor_msm, t = 10)
pmatrix.msm(psor_msm, t = 20)

# confidence interval can be estimated via bootstrap

# total length of stay in states
totlos.msm(psor_msm, t = 1)
totlos.msm(psor_msm, t = 10)




