
# https://m-clark.github.io/mixed-models-with-R/random_intercepts.html
gpa <- read.csv("~/Documents/GitHub/data-apothecary-notes/data/gpa.csv")

colnames(gpa)
# occason: 1-5

library(ggplot2)

p <- ggplot(data = gpa, aes(x = occasion, y = gpa, group = student))
p <- p + geom_line(alpha = 0.5)
p


# initial: occasion only



# random intercept ----
# student-specific intercepts
# occasion + student effect






