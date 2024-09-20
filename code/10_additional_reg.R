#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   adding in additional measures
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rm(list=ls())

data <- readRDS(file = "../data/processed/09_data.RData")

# Load required libraries
library(dplyr)
library(broom)
library(stargazer)

# Assuming the data is already loaded into a dataframe called 'data'

# Add additional measures (food security and shocks)
fs_vars <- c("shock_death_mainmem", "shock_death_othermem", "shock_jobloss", "shock_conflict",
             "shock_drought", "shock_flood", "shock_livestock", "fs_notenough", "fs_prefer",
             "fs_portion", "fs_meals", "fs_consump", "fs_borrow")

data <- data %>%
  mutate(across(all_of(fs_vars),
                ~ifelse(is.na(.) & !(country %in% c("Ghana", "Mali")), 0, .)))

# Calculate weighted statistics
weighted_stats <- data %>%
  group_by(country) %>%
  summarise(across(all_of(fs_vars), ~weighted.mean(., w = hhweight, na.rm = TRUE)))

print(weighted_stats)

# Create new variable for predicted values
data$yhat_pmt_fs_consump <- NA
attr(data$yhat_pmt_fs_consump, "label") <- "Predicted values PMT food security"

# Define variable groups
wealth_vars <- c("water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_finish",
                 "wall_finish", "roof_finish", "members_per_room", "kitchen_room",
                 "fuel_elecgas", "fuel_charcoal")

assets_vars <- c("electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car",
                 "telephone", "mobile_phone", "computer", "video", "stove_any",
                 "sew_machine", "aircon", "iron", "satelite", "generator", "own_house")

hh_vars <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary",
             "max_edu_primary", "max_edu_secondary", "div_sep_head", "widow_head",
             "nevermar_head", "female_head_widow", "female_head_div", "female_head_nevermar",
             "work_paid_head", "work_selfemp_nonf_head", "work_daily_head", "muslim",
             "christian", "share_05f", "share_05m", "share_614f", "share_614m",
             "share_65plusf", "share_65plusm", "share_widow", "share_disabledm",
             "share_disabledf", "share_orphanm", "share_orphanf")

# Combine all variables
all_vars <- c(wealth_vars, assets_vars, hh_vars, fs_vars)

# List of countries
countries <- c("BurkinaFaso", "Ethiopia", "Malawi", "Niger", "Nigeria", "Tanzania", "Uganda")

# Function to create factor variables safely
create_safe_factor <- function(x) {
  f <- factor(x)
  if (length(levels(f)) > 1) f else NULL
}

is_constant <- function(x) {
  length(unique(x)) == 1
}

# Regression and prediction for each country
models <- list()
for (country_name in countries) {
  # Filter data for the current country
  country_data <- data %>%
    filter(country == country_name) %>%
    dplyr::select(consump, hhweight, hhsize_cat, age_head_cat, state, month, all_of(all_vars))

  # Identify non-constant factor variables
  factor_vars <- c("hhsize_cat", "age_head_cat", "state", "month")
  valid_factors <- factor_vars[sapply(country_data[factor_vars], function(x) !is_constant(x))]

  # Create factor variables only for non-constant variables
  for (var in valid_factors) {
    country_data[[var]] <- factor(country_data[[var]])
  }

  # Prepare formula
  formula_str <- paste("consump ~",
                       paste(c(all_vars, paste0("factor(", valid_factors, ")")), collapse = " + "))

  # Fit model
  model <- lm(as.formula(formula_str), data = country_data, weights = hhweight)
  models[[country_name]] <- model

  # Print information about omitted variables
  constant_vars <- setdiff(factor_vars, valid_factors)
  if (length(constant_vars) > 0) {
    cat("For", country_name, ", the following variables were constant and omitted:",
        paste(constant_vars, collapse = ", "), "\n")
  }

  # Predict
  pred_data <- country_data
  for (f in valid_factors) {
    if (!is.null(model$xlevels[[f]])) {
      pred_data[[f]] <- factor(pred_data[[f]], levels = model$xlevels[[f]])
    }
  }
  data$yhat_pmt_fs_consump[data$country == country_name] <- predict(model, newdata = pred_data)
}

saveRDS(data, file = "../data/processed/10_data.RData")










