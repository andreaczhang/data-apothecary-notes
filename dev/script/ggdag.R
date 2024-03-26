library(ggdag)
library(ggplot2)
# time ordered dags

theme_set(theme_dag())

# time dependent ----

dag <- dagify(
  z ~ a + c + d + e + f,
  d ~ a + b + c, 
  e ~ a + b + c,
  f ~ a + b + c
)
ggdag(dag)

# if want time ordered

dag <- dagify(
  z ~ a + c + d + e + f,
  d ~ a + b + c, 
  e ~ a + b + c,
  f ~ a + b + c, 
  coords = time_ordered_coords()
)
ggdag(dag)


# pruning ----





