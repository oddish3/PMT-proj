#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   Inclusion and Exclusino Errors
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

data <- readRDS(file = "../data/processed/15_data.RData")

# libraries ----
library(dplyr)
library(tidyr)
library(knitr)

data <- data %>%
  group_by(country) %>%
  mutate(nval = row_number() == 1) %>%
  ungroup()

vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
          "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40", "yhat_pmt_sh_qr20_cons",
          "yhat_pmt_sh_qr40_cons", "yhat_pmt_sh_qr50_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_w60_sh_cons", "yhat_pmt_sh_ae_consump",
          "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump", "yhat_pmt_ex_qr20_cons",
          "yhat_pmt_ex_qr40_cons", "yhat_pmt_ex_qr50_cons", "yhat_w20_cons",
          "yhat_w40_cons", "yhat_w60_cons", "yhat_pmt_fs_consump", "yhat_step_consump",
          "yhat_pmt_com_consump", "yhat_pmt_sh_cons_burk")

num <- c("20", "40")

for (var in vars) {
  for (x in num) {
    # browser()
    # Line
    # Correctly included
    data[[paste0("ci1_pr", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ci1_pr", x, "_", var)]][data[[paste0("poor_", x)]] == 1 & data[[paste0("pr", x, "_", var)]] == 1] <- 1

    # Incorrectly included
    data[[paste0("ie1_pr", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ie1_pr", x, "_", var)]][data[[paste0("poor_", x)]] == 0 & data[[paste0("pr", x, "_", var)]] == 1] <- 1

    # Incorrectly excluded
    data[[paste0("ee1_pr", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ee1_pr", x, "_", var)]][data[[paste0("poor_", x)]] == 1 & data[[paste0("pr", x, "_", var)]] == 0] <- 1

    # Calculate shares using dplyr
    data <- data %>%
      group_by(country) %>%
      mutate(
        # Correctly included as share of total pop
        !!paste0("ci_pr", x, "_", var) :=
          weighted.mean(.data[[paste0("ci1_pr", x, "_", var)]],
                        hhweight, na.rm = TRUE),

        # Incorrectly included as share of predicted poor
        !!paste0("ie_pr", x, "_", var) :=
          ifelse(sum(.data[[paste0("pr", x, "_", var)]] == 1, na.rm = TRUE) > 0,
                 weighted.mean(.data[[paste0("ie1_pr", x, "_", var)]][.data[[paste0("pr", x, "_", var)]] == 1],
                               hhweight[.data[[paste0("pr", x, "_", var)]] == 1],
                               na.rm = TRUE),
                 NA),

        # Incorrectly excluded as share of actual poor
        !!paste0("ee_pr", x, "_", var) :=
          ifelse(sum(.data[[paste0("poor_", x)]] == 1, na.rm = TRUE) > 0,
                 weighted.mean(.data[[paste0("ee1_pr", x, "_", var)]][.data[[paste0("poor_", x)]] == 1],
                               hhweight[.data[[paste0("poor_", x)]] == 1],
                               na.rm = TRUE),
                 NA)
      ) %>%
      ungroup()

    # Remove intermediate columns
    data[[paste0("ci1_pr", x, "_", var)]] <- NULL
    data[[paste0("ie1_pr", x, "_", var)]] <- NULL
    data[[paste0("ee1_pr", x, "_", var)]] <- NULL

    # Rate
    data[[paste0("ci1_po", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ci1_po", x, "_", var)]][data[[paste0("poor_", x)]] == 1 & data[[paste0("po", x, "_", var)]] == 1] <- 1

    data[[paste0("ie1_po", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ie1_po", x, "_", var)]][data[[paste0("poor_", x)]] == 0 & data[[paste0("po", x, "_", var)]] == 1] <- 1

    data[[paste0("ee1_po", x, "_", var)]] <- ifelse(!is.na(data[[var]]), 0, NA)
    data[[paste0("ee1_po", x, "_", var)]][data[[paste0("poor_", x)]] == 1 & data[[paste0("po", x, "_", var)]] == 0] <- 1

    # Calculate shares for 'po' variables
    data <- data %>%
      group_by(country) %>%
      mutate(
        # Share of total pop
        !!paste0("ci_po", x, "_", var) :=
          weighted.mean(.data[[paste0("ci1_po", x, "_", var)]],
                        hhweight, na.rm = TRUE),

        # Share of predicted poor
        !!paste0("ie_po", x, "_", var) :=
          ifelse(sum(.data[[paste0("po", x, "_", var)]] == 1, na.rm = TRUE) > 0,
                 weighted.mean(.data[[paste0("ie1_po", x, "_", var)]][.data[[paste0("po", x, "_", var)]] == 1],
                               hhweight[.data[[paste0("po", x, "_", var)]] == 1],
                               na.rm = TRUE),
                 NA),

        # Share of actual poor
        !!paste0("ee_po", x, "_", var) :=
          ifelse(sum(.data[[paste0("poor_", x)]] == 1, na.rm = TRUE) > 0,
                 weighted.mean(.data[[paste0("ee1_po", x, "_", var)]][.data[[paste0("poor_", x)]] == 1],
                               hhweight[.data[[paste0("poor_", x)]] == 1],
                               na.rm = TRUE),
                 NA)
      ) %>%
      ungroup()

    # Remove intermediate columns
    data[[paste0("ci1_po", x, "_", var)]] <- NULL
    data[[paste0("ie1_po", x, "_", var)]] <- NULL
    data[[paste0("ee1_po", x, "_", var)]] <- NULL
  }
}

# Tables - Inclusion exclusion errors
# Table 4 (table 2 in the paper)
# Function to create summary tables
create_summary_table <- function(data, vars, weight_var = "hhweight") {
  summary_table_list <- list()

  for (var in vars) {
    summary_vars <- c(paste0("ie_pr20_", var), paste0("ee_pr20_", var),
                      paste0("ie_pr40_", var), paste0("ee_pr40_", var),
                      paste0("ie_po20_", var), paste0("ee_po20_", var),
                      paste0("ie_po40_", var), paste0("ee_po40_", var))

    summary_table <- data %>%
      group_by(country) %>%
      summarise(across(all_of(summary_vars),
                       ~weighted.mean(., .data[[weight_var]], na.rm = TRUE),
                       .names = "{.col}")) %>%
      mutate(across(where(is.numeric), ~sprintf("%.3g", .)))

    summary_table_list[[var]] <- summary_table
  }

  return(summary_table_list)
}

# Define variables for each table
vars_table4 <- c("yhat_pmt_sh_consump")
vars_table5 <- c("yhat_pmt_sh_qr20_cons", "yhat_pmt_sh_qr40_cons")
vars_table6 <- c("yhat_w20_sh_cons", "yhat_w40_sh_cons")
vars_table7 <- c("yhat_pmt_ext_consump")

# Create summary tables for each set of variables
table4_results <- create_summary_table(data, vars_table4)
table5_results <- create_summary_table(data, vars_table5)
table6_results <- create_summary_table(data, vars_table6)
table7_results <- create_summary_table(data, vars_table7)

# View results (example for Table 4)
View(table4_results[[1]])

View(table5_results[[1]])
View(table5_results[[2]])

View(table6_results[[1]])
View(table6_results[[2]])

View(table7_results[[1]])

create_summary_table <- function(data, vars, weight_var = "hhweight", by_country = TRUE, format = "%.3g") {
  summary_table_list <- list()

  for (var in vars) {
    summary_vars <- c(paste0("ie_pr20_", var), paste0("ee_pr20_", var),
                      paste0("ie_pr40_", var), paste0("ee_pr40_", var),
                      paste0("ie_po20_", var), paste0("ee_po20_", var),
                      paste0("ie_po40_", var), paste0("ee_po40_", var))

    if (by_country) {
      summary_table <- data %>%
        group_by(country) %>%
        summarise(across(all_of(summary_vars),
                         ~weighted.mean(., .data[[weight_var]], na.rm = TRUE),
                         .names = "{.col}")) %>%
        mutate(across(where(is.numeric), ~sprintf(format, .)))
    } else {
      summary_table <- data %>%
        summarise(across(all_of(summary_vars),
                         ~weighted.mean(., .data[[weight_var]], na.rm = TRUE),
                         .names = "{.col}")) %>%
        mutate(across(where(is.numeric), ~sprintf(format, .)))
    }

    summary_table_list[[var]] <- summary_table
  }

  return(summary_table_list)
}

# Text numbers for Burkina
vars_burkina <- c("yhat_pmt_sh_cons_burk")
burkina_results <- create_summary_table(
  data %>% filter(country == "BurkinaFaso"),
  vars_burkina,
  by_country = FALSE
)

# Print Burkina Faso results
for (var in names(burkina_results)) {
  cat("Burkina Faso -", var, "\n")
  print(burkina_results[[var]])
  cat("\n")
}

# Tables in appendix
vars_appendix <- c("yhat_pmt_sh_ae_consump", "yhat_w40_sh_cons", "yhat_w60_sh_cons",
                   "yhat_pmt_sh_urb_consump", "yhat_pmt_ex_qr20_cons", "yhat_pmt_ex_qr40_cons",
                   "yhat_pmt_ex_qr50_cons", "yhat_w20_cons", "yhat_w40_cons", "yhat_w60_cons",
                   "yhat_pmt_fs_consump", "yhat_step_consump", "yhat_pmt_com_consump",
                   "yhat_pmt_sh_cons_burk")

appendix_results <- create_summary_table(data, vars_appendix)

# Print appendix results
for (var in names(appendix_results)) {
  cat("Appendix Table -", var, "\n")
  print(appendix_results[[var]])
  cat("\n")
}

# Indicator variables
vars_indicators <- c("yhat_pmt_sh_poor_20", "yhat_pmt_sh_pr_poor_20",
                     "yhat_pmt_sh_poor_40", "yhat_pmt_sh_pr_poor_40")

create_indicator_summary <- function(data, vars, weight_var = "hhweight", format = "%.3g") {
  summary_table_list <- list()

  for (var in vars) {
    summary_vars <- c(paste0("ie_po20_", var), paste0("ee_po20_", var),
                      paste0("ie_po40_", var), paste0("ee_po40_", var))

    summary_table <- data %>%
      group_by(country) %>%
      summarise(across(all_of(summary_vars),
                       ~weighted.mean(., .data[[weight_var]], na.rm = TRUE),
                       .names = "{.col}")) %>%
      mutate(across(where(is.numeric), ~sprintf(format, .)))

    summary_table_list[[var]] <- summary_table
  }

  return(summary_table_list)
}

indicator_results <- create_indicator_summary(data, vars_indicators)

# Print indicator results
for (var in names(indicator_results)) {
  cat("Indicator Table -", var, "\n")
  print(indicator_results[[var]])
  cat("\n")
}

# Targeting differential ----

vars <- c("yhat_pmt_sh_consump", "yhat_pmt_sh_qr20_cons", "yhat_w20_sh_cons",
          "yhat_w40_sh_cons", "yhat_pmt_sh_urb_consump", "yhat_pmt_ext_consump",
          "yhat_pmt_ex_qr20_cons", "yhat_w20_cons", "yhat_w40_cons",
          "yhat_step_consump", "yhat_pmt_fs_consump", "yhat_pmt_com_consump")

# Create nval (equivalent to bys country: gen nval = _n == 1)
data <- data %>%
  group_by(country) %>%
  mutate(nval = row_number() == 1) %>%
  ungroup()

# Create H hat - average predicted poor
for (var in vars) {
  data <- data %>%
    group_by(country) %>%
    mutate(!!paste0("pr20_", var, "_m") := weighted.mean(get(paste0("pr20_", var)), hhweight, na.rm = TRUE)) %>%
    ungroup()
}

# Display summary statistics
summary_vars <- paste0("pr20_", vars, "_m")
summary_table <- data %>%
  group_by(country) %>%
  summarise(across(all_of(summary_vars),
                   ~weighted.mean(., hhweight, na.rm = TRUE),
                   .names = "{.col}")) %>%
  mutate(across(where(is.numeric), ~sprintf("%.3g", .)))

print(summary_table)

# Create max var
for (var in vars) {
  data <- data %>%
    group_by(country) %>%
    mutate(!!paste0("ee_pr20_", var, "1") := max(get(paste0("ee_pr20_", var)), na.rm = TRUE)) %>%
    ungroup()
}

# Calculate targeting differential
for (var in vars) {
  data <- data %>%
    mutate(!!paste0("td_pr20_", var) := (1 - get(paste0("pr20_", var, "_m")) - get(paste0("ee_pr20_", var, "1"))) / 0.8)
}

# Table 8
td_vars <- paste0("td_pr20_", vars)
table_8 <- data %>%
  filter(nval == 1) %>%
  group_by(country) %>%
  summarise(across(all_of(td_vars),
                   ~mean(., na.rm = TRUE),
                   .names = "{.col}")) %>%
  mutate(across(where(is.numeric), ~sprintf("%.3g", .)))

print(table_8)



















saveRDS(data, file = "../data/processed/16_data.RData")
















