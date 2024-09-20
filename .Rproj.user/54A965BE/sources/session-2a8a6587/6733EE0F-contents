#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   weighted regressino extend (uses subsample not weights)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# weighted regressions for extended model ----
rm(list=ls())

# libraries ----
library(data.table)
library(fixest)
library(haven)

# Load data
data <- readRDS(file = "../data/processed/07_data.RData")

# Define variable groups
wealth <- c("water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_finish", "wall_finish", "roof_finish", "members_per_room", "kitchen_room", "fuel_elecgas", "fuel_charcoal")
assets <- c("electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car", "telephone", "mobile_phone", "computer", "video", "stove_any", "sew_machine", "aircon", "iron", "satelite", "generator", "own_house")
hh <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary", "max_edu_primary", "max_edu_secondary", "div_sep_head", "widow_head", "nevermar_head", "female_head_widow", "female_head_div", "female_head_nevermar", "work_paid_head", "work_selfemp_nonf_head", "work_daily_head", "muslim", "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm", "share_widow", "share_disabledm", "share_disabledf", "share_orphanm", "share_orphanf")
countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

# Function to run regression and predict
run_regression_and_predict <- function(dt, country, percentile) {
  tryCatch({
    # cat(sprintf("\nProcessing country: %s, percentile: %d\n", country, percentile))

    # Filter data
    filtered_data <- dt[country == country & consumption_pctile <= percentile]
    # cat(sprintf("Filtered data rows: %d\n", nrow(filtered_data)))

    # Prepare formula
    formula_str <- paste("consump ~",
                         paste(c(wealth, assets, hh), collapse = " + "),
                         "+ factor(hhsize_cat) + factor(age_head_cat) + factor(state) + factor(month)")
    # cat("Formula prepared\n")

    # Run regression
    model <- feols(as.formula(formula_str), data = filtered_data, cluster = ~EA)
    # cat("Regression complete\n")

    # Prepare prediction data
    prediction_data <- dt[country == country]
    # cat(sprintf("Prediction data rows: %d\n", nrow(prediction_data)))

    # Predict
    predictions <- predict(model, newdata = prediction_data)
    # cat(sprintf("Predictions made: %d\n", length(predictions)))

    # Add predictions to data
    target_column <- paste0("yhat_", percentile, "_", country)
    dt[country == country, (target_column) := predictions]
    # cat(sprintf("Added predictions to column: %s\n", target_column))

    # Verify the column was added
    if (target_column %in% names(dt)) {
      # cat(sprintf("Column %s successfully added\n", target_column))
    } else {
      # cat(sprintf("WARNING: Column %s not found in the dataset\n", target_column))
    }

    return(dt)
  }, error = function(e) {
    cat(sprintf("ERROR in run_regression_and_predict for country %s, percentile %d: %s\n",
                country, percentile, e$message))
    return(dt)  # Return the original dataset if an error occurs
  })
}

# Main analysis loop
main_analysis <- function() {
  for (country in countries) {
    # cat(sprintf("\n--- Starting analysis for %s ---\n", country))
    for (percentile in c(20, 40, 60)) {
      data <<- run_regression_and_predict(data, country, percentile)

      # Check if the column was added
      target_column <- paste0("yhat_", percentile, "_", country)
      if (target_column %in% names(data)) {
        # cat(sprintf("Confirmed: Column %s exists in the dataset\n", target_column))
      } else {
        # cat(sprintf("WARNING: Column %s is missing from the dataset\n", target_column))
      }
    }
    # cat(sprintf("--- Completed analysis for %s ---\n", country))
  }

  # Print all column names after analysis
  # cat("\nAll column names after analysis:\n")
  # print(names(data))
}

# Run the main analysis
main_analysis()


# Create consolidated variables
data[, `:=`(
  yhat_w20_cons = NA_real_,
  yhat_w40_cons = NA_real_,
  yhat_w60_cons = NA_real_
)]

# Set variable labels
attr(data$yhat_w20_cons, "label") <- "Predicted values extended PMT, weighted bottom 20"
attr(data$yhat_w40_cons, "label") <- "Predicted values extended PMT, weighted bottom 40"
attr(data$yhat_w60_cons, "label") <- "Predicted values extended PMT, weighted bottom 60"

# Consolidate predictions
data <- data %>%
  rowwise() %>%
  mutate(
    yhat_w20_cons = case_when(
      country %in% countries ~ get(paste0("yhat_20_", country)),
      TRUE ~ yhat_w20_cons
    ),
    yhat_w40_cons = case_when(
      country %in% countries ~ get(paste0("yhat_40_", country)),
      TRUE ~ yhat_w40_cons
    ),
    yhat_w60_cons = case_when(
      country %in% countries ~ get(paste0("yhat_60_", country)),
      TRUE ~ yhat_w60_cons
    )
  ) %>%
  ungroup()

# Drop country-specific columns
cols_to_drop <- flatten_chr(map(countries, ~paste0("yhat_", c(20, 40, 60), "_", .x)))
data <- data %>% dplyr::select(-all_of(cols_to_drop))

# Save final data
saveRDS(data, file = "../data/processed/08_data.RData")





