#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   adult equivalent expenditures
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

# libraries ----
library(labelled)
library(estimatr)
library(purrr)

data <- readRDS(file = "../data/processed/04_data.RData")


# No adult equivalence for Burkina, Mali, Nigeria

# Equivalent to tabstat in Stata
summary_stats <- data %>%
  group_by(country) %>%
  summarise(
    mean = mean(real_consumption_ae, na.rm = TRUE),
    sd = sd(real_consumption_ae, na.rm = TRUE),
    min = min(real_consumption_ae, na.rm = TRUE),
    max = max(real_consumption_ae, na.rm = TRUE)
  )
print(summary_stats)

# Generate log of real consumption
data <- data %>%
  mutate(consump_ae = log(real_consumption_ae))

# Label variable (using labelled package for Stata-like labels)
data$consump_ae <- labelled::set_variable_labels(data$consump_ae, "Log real consumption adult equivalent")

# Generate new variable
data$yhat_pmt_sh_ae_consump <- NA_real_
data$yhat_pmt_sh_ae_consump <- labelled::set_variable_labels(data$yhat_pmt_sh_ae_consump, "Predicted values quantile reg short PMT")

# Define variables
wealth <- c("toilet_flush", "toilet_pit", "floor_finish", "wall_finish", "roof_finish", "fuel_elecgas", "fuel_charcoal")
hh <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary", "div_sep_head", "widow_head", "work_paid_head", "work_selfemp_nonf_head", "muslim", "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm")

countries <- c("Ethiopia", "Ghana", "Malawi", "Niger", "Tanzania", "Uganda")

# Loop through countries
for (x in countries) {
  # browser()
  # Subset data for the current country
  country_data <- data[country == x]

  # Identify variables with more than one level
  check_cols <- c(wealth, hh, "hhsize_cat", "age_head_cat", "state", "month")
  valid_vars <- sapply(check_cols, function(col) length(unique(country_data[[col]])) > 1)

  # Create formula only with valid variables
  valid_terms <- c(wealth[valid_vars[wealth]],
                   hh[valid_vars[hh]],
                   if(valid_vars["hhsize_cat"]) "factor(hhsize_cat)" else NULL,
                   if(valid_vars["age_head_cat"]) "factor(age_head_cat)" else NULL,
                   if(valid_vars["state"]) "factor(state)" else NULL,
                   if(valid_vars["month"]) "factor(month)" else NULL)

  formula <- as.formula(paste("consump_ae ~", paste(valid_terms, collapse = " + ")))

  # Run regression
  model <- tryCatch({
    lm_robust(formula, data = country_data, clusters = EA)
  }, error = function(e) {
    warning(paste("Error in regression for country", x, ":", e$message))
    return(NULL)
  })

  if (!is.null(model)) {
    # Store estimates
    assign(paste0("est_", x), model)

    # Predict and replace values
    predictions <- predict(model, newdata = country_data)
    data[country == x, yhat_pmt_sh_ae_consump := predictions]
  }
}

# Export results
results <- map_df(countries, function(x) {
  model <- get(paste0("est_", x))
  tidy(model) %>%
    filter(term %in% c(wealth, hh)) %>%
    mutate(country = x)
})

# Drop consump_ae
data <- data %>% dplyr::select(-consump_ae)

# Separate for urban rural
data$yhat_pmt_sh_urb_consump <- NA_real_
data$yhat_pmt_sh_urb_consump <- labelled::set_variable_labels(data$yhat_pmt_sh_urb_consump, "Predicted values urban/rural short PMT")

countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

results_list <- list()

# Loop through urban/rural and countries
for (num in 0:1) {
  for (x in countries) {
    # Subset data
    subset_data <- data[country == x & urban == num]

    # Create factor variables
    factor_vars <- c("hhsize_cat", "age_head_cat", "state", "month")
    for (var in factor_vars) {
      subset_data[[var]] <- factor(subset_data[[var]])
    }

    # Identify variables with more than one level
    valid_factors <- sapply(subset_data[, ..factor_vars], function(x) length(levels(x)) > 1)
    valid_factor_vars <- factor_vars[valid_factors]

    # Create formula
    formula_terms <- c(wealth, hh, paste0("factor(", valid_factor_vars, ")"))
    formula <- as.formula(paste("consump ~", paste(formula_terms, collapse = " + ")))

    # Run regression
    model <- tryCatch({
      lm_robust(formula, data = subset_data, clusters = subset_data$EA)
    }, error = function(e) {
      warning(paste("Error in regression for country", x, "and urban", num, ":", e$message))
      return(NULL)
    })

    if (!is.null(model)) {
      # Store estimates
      assign(paste0("est_", x, "_", num), model)

      # Predict and replace values
      predictions <- predict(model, newdata = subset_data)
      data[country == x & urban == num, yhat_pmt_sh_urb_consump := predictions]

      # Store results for later output
      results_list[[paste0(x, "_", num)]] <- tidy(model) %>%
        filter(term %in% c(wealth, hh)) %>%
        mutate(country = x, urban = num)
    }
  }
}

# Combine all results
results <- do.call(rbind, results_list)

# Create a data frame with all combinations of country and urban
combinations <- expand.grid(country = countries, urban = 0:1) %>%
  arrange(urban, country)

# Use map_df with .id parameter to iterate over rows
results_urban_rural <- map_df(seq_len(nrow(combinations)), function(i) {
  row <- combinations[i, ]
  model_name <- paste0("est_", row$country, "_", row$urban)

  # Check if the model exists
  if (exists(model_name)) {
    model <- get(model_name)
    tidy(model) %>%
      filter(term %in% c(wealth, hh)) %>%
      mutate(country = row$country, urban = row$urban)
  } else {
    # Return an empty data frame if the model doesn't exist
    data.frame()
  }
}, .id = "row_id")

# Remove the row_id column if you don't need it
results_urban_rural <- results_urban_rural %>% dplyr::select(-row_id)

saveRDS(data, file = "../data/processed/05_data.RData")
