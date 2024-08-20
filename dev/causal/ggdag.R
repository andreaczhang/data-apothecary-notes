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


# pruning ----# 


# example from r-causal ----

# set theme, remove grid background
theme_set(
  theme_dag() %+replace%
    # also add some additional styling
    theme(
      legend.position = "bottom",
      strip.text.x = element_text(margin = margin(2, 0, 2, 0, "mm"))
    )
)

# dagify(
#   podcast ~ mood + humor + prepared,
#   exam ~ mood + prepared
# )

# same dag, ordered by time
podcast_dag <- dagify(
  podcast ~ mood + humor + prepared,
  exam ~ mood + prepared,
  coords = time_ordered_coords(
    list(
      # time point 1
      c("prepared", "humor", "mood"),
      # time point 2
      "podcast",
      # time point 3
      "exam"
    )
  ),
  exposure = "podcast",
  outcome = "exam",
  # add labels
  labels = c(
    podcast = "podcast",
    exam = "exam score",
    mood = "mood",
    humor = "humor",
    prepared = "prepared"
  )
)
gd <- ggdag(podcast_dag, use_labels = "label", text = FALSE) +
  theme_dag()
gd

# if no coordinates specified
pod_dag <- dagify(
  podcast ~ mood + humor + prepared,
  exam ~ mood + prepared
)

## automatic layouts ----
ggdag(pod_dag, text_size = 2.8)


## sugiyama ----
ggdag(pod_dag, layout = 'sugiyama', text_size = 2.8)

## default time ordered ----
ggdag(pod_dag, layout = 'time_ordered', text_size = 2.8)



# paths ----
gd

# question: is there causal relationship between 
# listening to podcast and exam scores?

# no direct path; but are there other paths?
# 1) mood as a confounder
# 2) prepared as a confounder
# (backdoor paths)

podcast_dag |>
  # show the whole dag as a light gray "shadow"
  # rather than just the paths
  ggdag_paths(shadow = TRUE, text = FALSE, use_labels = "label")

# tidy dag object
# a table
tidy_dagitty(podcast_dag)


# account for backdoor paths ----
# visualize any valid adjustment sets
# two variables are required to block the backdoor path
# mood and prepared

ggdag_adjustment_set(
  podcast_dag,
  text = FALSE,
  use_labels = "label"
)



# advanced confounding -----
# add 2 more:
# alertness - mood, exam
# skills_course - prepared, podcast

podcast_dag2 <- dagify(
  podcast ~ mood + humor + skills_course,
  alertness ~ mood,
  mood ~ humor,
  prepared ~ skills_course,
  exam ~ alertness + prepared,
  # coordinates
  coords = time_ordered_coords(),
  exposure = "podcast",
  outcome = "exam",
  labels = c(
    podcast = "podcast",
    exam = "exam score",
    mood = "mood",
    alertness = "alertness",
    skills_course = "college\nskills course",
    humor = "humor",
    prepared = "prepared"
  )
)

ggdag(podcast_dag2, use_labels = "label", text = FALSE)

# identify the backdoor paths 

ggdag_paths(podcast_dag2, use_labels = "label", text = FALSE, shadow = TRUE)

# minimal adjustment sets
ggdag_adjustment_set(
  podcast_dag2,
  text = FALSE,
  use_labels = "label"
)

# multiple exist; if everything is measured and dag is correct,
# whichever would work


