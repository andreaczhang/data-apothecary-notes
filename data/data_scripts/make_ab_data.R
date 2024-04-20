# simulate ab data


id <- c(
  rep(1, 3), 
  rep(2, 2), 
  rep(3, 6), 
  rep(4, 1)
)

ab_types <- c(
  'ampicillin', 'gentamicin', 'cefalotin', 'metronidazol'
)
ab <- sample(ab_types, size = length(id), replace = T)

dt <- data.table::data.table(
  id = id, ab = ab
)
dt
