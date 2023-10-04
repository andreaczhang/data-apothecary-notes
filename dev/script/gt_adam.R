# test adam clinical trial 

# rx_adsl: subject lvl demo from 182 dummy patients
# tx, pl
# rx_addv: protocol devations

library(gt)

rx_adsl
?rx_adsl
# 14 variables:
# study id
# subject id
# study intervention: placebo; drug; NA; 
table(rx_adsl$TRTA)
# ITT flag
table(rx_adsl$ITTFL) # 2 not itt
# randomization flag
table(rx_adsl$RANDFL) # 2 not randomized
# screen failure
table(rx_adsl$SCRFREAS)
# age, sex, age below 40, ethnicity
# event flag
table(rx_adsl$EVNTFL)



gt::rx_adsl |>
  dplyr::filter(RANDFL == "N") |>
  dplyr::select(USUBJID, SCRFREAS) |>
  gt() |>
  tab_row_group(label = "Subjects not randomized:",
                rows = everything()) |>
  summary_rows(
    columns = "USUBJID",
    fns = list("Screen Failures" ~ paste0("n=", dplyr::n())),
    side = "top"
  )

demo_df <- tibble::tribble(
  ~variable,               ~lbl, ~col_n, ~col_median, ~col_q1, ~col_q3,
  "Age (years)",                 NA,     NA,          NA,      NA,      NA,
  " ",                "n",      7,          NA,      NA,      NA,
  " ", "Median (Q1 - Q3)",     NA,          42,      31,      55
)
demo_df

# merge col 2,3,4 
demo_df |>
  gt() |>
  cols_merge(columns = c("col_n", "col_median", "col_q1", "col_q3"),
             pattern = "<<{1}>><<{2} ({3} - {4})>>") |>
  cols_label("col_n" = "Drug 1")






