#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   assigning households as predicted poor
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

data <- readRDS(file = "../data/processed/14_data.RData")

# libraries ----
library(dplyr)
library(tidyr)
library(knitr)
library(rlang)

# Assigning households as predicted poor ----

predict_vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
                  "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
                  "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
                  "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_ae_consump",
                  "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump", "yhat_pmt_ex_qr20_cons",
                  "yhat_pmt_ex_qr40_cons", "yhat_pmt_ex_qr50_cons", "yhat_w20_cons",
                  "yhat_w40_cons", "yhat_w60_cons", "yhat_pmt_fs_consump", "yhat_step_consump",
                  "yhat_pmt_com_consump", "yhat_pmt_sh_cons_burk")

data <- data %>%
  mutate(across(all_of(predict_vars),
                list(pr20 = ~ifelse(!is.na(.), ifelse(. <= pov_line_20, 1, 0), NA),
                     pr40 = ~ifelse(!is.na(.), ifelse(. <= pov_line_40, 1, 0), NA))))

# Define the variables
vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
          "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
          "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_ae_consump",
          "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump", "yhat_pmt_ex_qr20_cons",
          "yhat_pmt_ex_qr40_cons", "yhat_pmt_ex_qr50_cons", "yhat_w20_cons",
          "yhat_w40_cons", "yhat_w60_cons", "yhat_pmt_fs_consump", "yhat_step_consump",
          "yhat_pmt_com_consump", "yhat_pmt_sh_cons_burk")

means <- data %>%
  dplyr::select(all_of(vars)) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Print the means
print(means)

# Generate pr20 and pr40 variables
for (var in vars) {
  # For pr20
  data[[paste0("pr20_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
  data[[paste0("pr20_", var)]] <- ifelse(data[[var]] <= data$pov_line_20 & !is.na(data[[var]]), 1, data[[paste0("pr20_", var)]])

  # For pr40
  data[[paste0("pr40_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
  data[[paste0("pr40_", var)]] <- ifelse(data[[var]] <= data$pov_line_40 & !is.na(data[[var]]), 1, data[[paste0("pr40_", var)]])
}

# TABLE 3 ----

# Calculate weighted means by country
weighted_means <- data %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    pr20_yhat_pmt_sh_consump = weighted.mean(pr20_yhat_pmt_sh_consump, w = hhweight, na.rm = TRUE),
    pr20_yhat_pmt_ext_consump = weighted.mean(pr20_yhat_pmt_ext_consump, w = hhweight, na.rm = TRUE),
    pr40_yhat_pmt_sh_consump = weighted.mean(pr40_yhat_pmt_sh_consump, w = hhweight, na.rm = TRUE),
    pr40_yhat_pmt_ext_consump = weighted.mean(pr40_yhat_pmt_ext_consump, w = hhweight, na.rm = TRUE)
  )

# Transpose the data
transposed_means <- weighted_means %>%
  tidyr::pivot_longer(cols = -country, names_to = "variable", values_to = "value") %>%
  tidyr::pivot_wider(names_from = country, values_from = value)

# Format the results
formatted_results <- transposed_means %>%
  dplyr::mutate(dplyr::across(-variable, ~sprintf("%.3g", .)))

# Print the results in a table format
print(kable(formatted_results, format = "pipe", align = "r"))

# poverty rates ----

# cutoff point ----
# Calculate weighted means for poor_20 and poor_40
data <- data %>%
  group_by(country) %>%
  mutate(
    poor_20_mean = weighted.mean(poor_20, w = hhweight, na.rm = TRUE),
    poor_40_mean = weighted.mean(poor_40, w = hhweight, na.rm = TRUE)
  ) %>%
  ungroup()

# Summary statistics by country
data %>%
  group_by(country) %>%
  summarise(across(c(poor_20_mean, poor_40_mean), \(x) mean(x, na.rm = TRUE)))

# Define variables
vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
          "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
          "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_ae_consump",
          "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump", "yhat_w20_cons",
          "yhat_w40_cons", "yhat_w60_cons", "yhat_pmt_fs_consump", "yhat_step_consump",
          "yhat_pmt_com_consump")

# Summary statistics by country for these variables
data %>%
  group_by(country) %>%
  summarise(across(all_of(vars), list(mean = \(x) mean(x, na.rm = TRUE))))

# percentiles - all regressions with all countries ----

vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
          "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
          "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_urb_consump",
          "yhat_pmt_ext_consump", "yhat_pmt_ex_qr20_cons", "yhat_pmt_ex_qr40_cons",
          "yhat_pmt_ex_qr50_cons", "yhat_w20_cons", "yhat_w40_cons", "yhat_w60_cons",
          "yhat_step_consump")

countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

# Function to calculate percentiles
calculate_percentiles <- function(data, var, countries) {
  data %>%
    group_by(country) %>%
    mutate(
      !!paste0("pc_", var) := case_when(
        country %in% countries & !is.na(!!sym(var)) ~
          percent_rank(!!sym(var)) * hhweight / sum(hhweight[!is.na(!!sym(var))]),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
}

# Apply the function to all variables
for (var in vars) {
  data <- calculate_percentiles(data, var, countries)
}

# adult equivalence----

# Function to calculate weighted percentiles
calculate_weighted_percentiles <- function(data, var, countries) {
  data %>%
    group_by(country) %>%
    mutate(
      !!paste0("pc_", var) := case_when(
        country %in% countries & !is.na(!!sym(var)) & !is.na(hhweight) ~ {
          # Calculate weighted cumulative sum
          wt_order <- order(!!sym(var))
          wt_cum <- cumsum(hhweight[wt_order]) / sum(hhweight)
          # Assign percentiles
          percentiles <- findInterval(wt_cum, seq(0, 1, length.out = 101)[-1])
          percentiles[order(wt_order)]  # Reorder to match original data
        },
        TRUE ~ NA_real_
      ),
      !!paste0("diagnostic_", var) := case_when(
        !country %in% countries ~ "Country not in list",
        is.na(!!sym(var)) ~ "Missing original value",
        is.na(hhweight) ~ "Missing weight",
        TRUE ~ "Calculated"
      )
    ) %>%
    ungroup()
}

# Adult equivalence
countries_ae <- c("Ethiopia", "Ghana", "Malawi", "Niger", "Tanzania", "Uganda")
data <- calculate_weighted_percentiles(data, "yhat_pmt_sh_ae_consump", countries_ae)
# table(data$diagnostic_yhat_pmt_sh_ae_consump, useNA = "ifany")
# Food security
countries_fs <- c("BurkinaFaso", "Ethiopia", "Malawi", "Niger", "Nigeria", "Tanzania", "Uganda")
data <- calculate_weighted_percentiles(data, "yhat_pmt_fs_consump", countries_fs)
# table(data$diagnostic_yhat_pmt_fs_consump, useNA = "ifany")
# Community vars
countries_com <- c("Ethiopia", "Malawi", "Niger", "Nigeria", "Tanzania", "Uganda")
data <- calculate_weighted_percentiles(data, "yhat_pmt_com_consump", countries_com)

# Burkina Faso
countries_burk <- c("BurkinaFaso")
data <- calculate_weighted_percentiles(data, "yhat_pmt_sh_cons_burk", countries_burk)

# Adjust all percentiles to be between 0 and 1
data <- data %>%
  mutate(across(starts_with("pc_"), ~./100))

# generating indicator variable for predicted poor or not  ----

vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
          "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
          "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_ae_consump",
          "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump", "yhat_pmt_ex_qr20_cons",
          "yhat_pmt_ex_qr40_cons", "yhat_pmt_ex_qr50_cons", "yhat_w20_cons",
          "yhat_w40_cons", "yhat_w60_cons", "yhat_pmt_fs_consump", "yhat_step_consump",
          "yhat_pmt_com_consump", "yhat_pmt_sh_cons_burk")

data <- data %>%
  mutate(across(all_of(vars),
                list(po20 = ~ifelse(!is.na(.), ifelse(get(paste0("pc_", cur_column())) <= poor_20_mean, 1, 0), NA),
                     po40 = ~ifelse(!is.na(.), ifelse(get(paste0("pc_", cur_column())) <= poor_40_mean, 1, 0), NA)),
                .names = "{.fn}_{.col}")) %>%
  dplyr::select(-starts_with("pc_"))

# checking - mean should be equal to 0.2 or 0.4 ----

check_vars <- c(
  "po20_yhat_pmt_sh_consump", "po40_yhat_pmt_sh_consump",
  "po20_yhat_pmt_sh_poor_20", "po40_yhat_pmt_sh_poor_20",
  "po20_yhat_pmt_sh_pr_poor_20", "po40_yhat_pmt_sh_pr_poor_20",
  "po20_yhat_pmt_sh_poor_40", "po40_yhat_pmt_sh_poor_40",
  "po20_yhat_pmt_sh_pr_poor_40", "po40_yhat_pmt_sh_pr_poor_40",
  "po20_yhat_w20_sh_cons", "po40_yhat_w20_sh_cons",
  "po20_yhat_w40_sh_cons", "po40_yhat_w40_sh_cons",
  "po20_yhat_w60_sh_cons", "po40_yhat_w60_sh_cons",
  "po20_yhat_pmt_sh_ae_consump", "po40_yhat_pmt_sh_ae_consump",
  "po20_yhat_pmt_sh_urb_consump", "po40_yhat_pmt_sh_urb_consump",
  "po20_yhat_pmt_ext_consump", "po40_yhat_pmt_ext_consump",
  "po20_yhat_w20_cons", "po40_yhat_w20_cons",
  "po20_yhat_w40_cons", "po40_yhat_w40_cons",
  "po20_yhat_w60_cons", "po40_yhat_w60_cons",
  "po20_yhat_pmt_fs_consump", "po40_yhat_pmt_fs_consump",
  "po20_yhat_step_consump", "po40_yhat_step_consump",
  "po20_yhat_pmt_com_consump", "po40_yhat_pmt_com_consump"
)

# Perform the summary
check_results <- data %>%
  group_by(country) %>%
  summarise(across(all_of(check_vars),
                   ~ weighted.mean(., w = hhweight, na.rm = TRUE))) %>%
  ungroup()

# Transpose the results
check_results_t <- check_results %>%
  pivot_longer(-country, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = country, values_from = value)

# Add a total column (overall weighted mean for each variable)
check_results_t <- check_results_t %>%
  mutate(Total = data %>%
           summarise(across(all_of(check_vars),
                            ~ weighted.mean(., w = hhweight, na.rm = TRUE))) %>%
           unlist())

# Print the results
print(check_results_t, n = Inf)

saveRDS(data, file = "../data/processed/15_data.RData")
