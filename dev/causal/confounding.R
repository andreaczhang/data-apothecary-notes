# test out examples by Andrew Heiss
# https://www.andrewheiss.com/blog/2020/02/25/closing-backdoors-dags/

# clsoe backdoor

# hypothetical math camp
# what is the causal effect of math camp on final student outcomes

# install.packages('ggdag')
library(tidyverse)  # ggplot, dplyr, %>%, and friends
library(ggdag)  # Make DAGs with ggplot
library(dagitty)  # Do basic DAG math
library(broom)  # For converting model output to data frames

# draw dag ----

node_details <- tribble(
  ~name, ~label, ~x, ~y,
  "math_camp", "Math camp", 2, 1,
  "final_grade", "Final grade", 4, 1,
  "needs_camp", "Needs camp", 1, 2,
  "gre_quant", "GRE quantitative", 2.5, 2,
  "gre_verbal", "GRE verbal", 5, 2,
  "background", "Background", 2, 3,
  "undergraduate_gpa", "Undergraduate GPA", 4, 3
)

node_labels <- node_details$label
names(node_labels) <- node_details$name

math_camp_dag <- dagify(final_grade ~ math_camp + gre_quant + gre_verbal + 
                          undergraduate_gpa + background,
                        math_camp ~ needs_camp, 
                        needs_camp ~ background + undergraduate_gpa + gre_quant,
                        gre_quant ~ background + undergraduate_gpa,
                        gre_verbal ~ background + undergraduate_gpa,
                        undergraduate_gpa ~ background,
                        exposure = "math_camp",
                        outcome = "final_grade",
                        latent = "background",
                        coords = node_details,
                        labels = node_labels)
math_camp_dag
# Turn DAG into a tidy data frame for plotting
math_camp_dag_tidy <- math_camp_dag %>% 
  tidy_dagitty() %>%
  node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

# Fancier graph
ggplot(math_camp_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                       color = "white", fontface = "bold") +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()


# treatment: math camp (blue)
# outcome: final grade (red)


# confounding (common cause): undergraduate gpa, gre quant 
# to block backdoor, adjust 'needs camp' 

paths(math_camp_dag)
adjustmentSets(math_camp_dag) # this points out which is the adjustment set 


# simulate data ----
# Make these random draws the same every time
set.seed(1234)

# Create 2,000 rows
num <- 2000

# Create confounder variables that are related to each other
mu <- c(undergrad_gpa = 3, gre_verbal = 160, gre_quant = 145)
stddev <- c(undergrad_gpa = 0.5, gre_verbal = 10, gre_quant = 5)

# Correlation matrix: undergrad GPA and verbal GRE have a correlation of 0.8;
# undergrad GPA and quantitative GRE have a correlation of 0.6, and verbal GRE
# and quantitative GRE have a correlation of 0.4
cor_matrix <- matrix(c(1.0, 0.8, 0.6,
                       0.8, 1.0, 0.4,
                       0.6, 0.4, 1.0),
                     ncol = 3)

# Convert correlation matrix to covariance matrix using fancy math
cov_matrix <- stddev %*% t(stddev) * cor_matrix

# Draw random numbers
confounders <- MASS::mvrnorm(n = num, mu = mu, Sigma = cov_matrix, empirical = TRUE) %>%
  as_tibble() %>%
  # Truncate values so they're within 130-170 range for GRE and less than 4.0 for GPA
  mutate_at(vars(gre_verbal, gre_quant),
            ~case_when(
              . > 170 ~ 170,
              . < 130 ~ 130,
              TRUE ~ .
            )) %>%
  mutate(undergrad_gpa = ifelse(undergrad_gpa > 4, 4, undergrad_gpa))

# Make official dataset of simulated values
math_camp <- tibble(id = 1:num) %>%
  bind_cols(confounders) %>%  # Bring in confounders
  # People need math camp if their GRE and GPA is lower than the average
  mutate(needs_camp = gre_quant < mean(gre_quant) & 
           undergrad_gpa < mean(undergrad_gpa)) %>%
  # Build in some noncompliance: 80% of those who need math camp do it; 20% of
  # those who don't need it do it.
  mutate(math_camp = case_when(
    needs_camp == TRUE ~ rbinom(n(), 1, 0.8),
    needs_camp == FALSE ~ rbinom(n(), 1, 0.2)
  )) %>%
  # Create random error in grades
  mutate(grade_noise = rnorm(num, 15, 5)) %>%
  # Create final grade based on all the arrows going into the node in the DAG.
  # There's a 10 point causal effect
  mutate(final_grade = (0.3 * gre_verbal) + (0.5 * gre_quant) + 
           (0.4 * undergrad_gpa) + (10 * math_camp) + grade_noise) %>%
  mutate(math_camp = as.logical(math_camp))  # Make true/false

math_camp

# 40% completed the camp
math_camp %>% 
  count(math_camp) %>% 
  mutate(prop = n / sum(n))

# no adjustment: lm(score ~ camp) -----
# compare mean: camp, not camp
# camp 144, not camp 138 - higher for those with camp
math_camp %>% 
  group_by(math_camp) %>% 
  summarize(avg = mean(final_grade))

ggplot(math_camp, aes(x = final_grade, fill = math_camp)) +
  geom_histogram(binwidth = 2, color = "white") + 
  guides(fill = FALSE) +
  facet_wrap(vars(math_camp), ncol = 1) +
  theme_light()

# do lm 
model_wrong <- lm(final_grade ~ math_camp, data = math_camp)
tidy(model_wrong)
# with camp, 6.34 more points 


# adjustment: backdoor ----
# needs camp is the backdoor confounder







