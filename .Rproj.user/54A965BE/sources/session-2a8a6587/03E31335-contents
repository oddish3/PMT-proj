#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   ADE quantile regression extended
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rm(list=ls())
# libraries ----
library(quantreg)
library(parallel)

data <- readRDS(file = "../data/processed/06_data.RData")

# Define variables
nums <- c("20", "40", "50")
wealth <- c("water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_finish", "wall_finish", "roof_finish", "members_per_room", "kitchen_room", "fuel_elecgas", "fuel_charcoal")
assets <- c("electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car", "telephone", "mobile_phone", "computer", "video", "stove_any", "sew_machine", "aircon", "iron", "satelite", "generator", "own_house")
hh <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary", "max_edu_primary", "max_edu_secondary", "div_sep_head", "widow_head", "nevermar_head", "female_head_widow", "female_head_div", "female_head_nevermar", "work_paid_head", "work_selfemp_nonf_head", "work_daily_head", "muslim", "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm", "share_widow", "share_disabledm", "share_disabledf", "share_orphanm", "share_orphanf")
countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

# Function to run quantile regression and update predictions
run_qreg <- function(params) {
  num <- params$num
  country <- params$country

  country_data <- data[data$country == country, ]

  # Create factor variables
  country_data$hhsize_cat <- factor(country_data$hhsize_cat)
  country_data$age_head_cat <- factor(country_data$age_head_cat)
  country_data$state <- factor(country_data$state)
  country_data$month <- factor(country_data$month)

  # Remove factors with only one level
  factors_to_include <- sapply(country_data[, c("hhsize_cat", "age_head_cat", "state", "month")], function(x) length(levels(x)) > 1)
  factor_vars <- names(factors_to_include)[factors_to_include]

  # Construct formula
  formula_vars <- c(wealth, assets, hh, paste0("factor(", factor_vars, ")"))
  formula <- as.formula(paste("consump ~", paste(formula_vars, collapse = " + ")))

  # Run regression
  model <- try(rq(formula, data = country_data, tau = as.numeric(paste0("0.", num))), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("Error in regression for country:", country, "num:", num, "\n")
    return(NULL)
  }

  # Generate predictions
  predictions <- predict(model, newdata = country_data)

  country_data[[paste0("yhat_pmt_ex_qr", num, "_cons")]] <- predictions

  return(list(data = country_data, model = model, num = num, country = country))
}

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all cores except one
cl <- makeCluster(num_cores)

# Export necessary objects to the cluster
clusterExport(cl, c("run_qreg", "data", "nums", "countries", "wealth", "assets", "hh"))

# Load required packages on each worker
clusterEvalQ(cl, {
  library(quantreg)
})

# Prepare parameter combinations
param_grid <- expand.grid(num = nums, country = countries)

# Run parallel computations
results <- parLapply(cl, split(param_grid, seq(nrow(param_grid))), run_qreg)

# Stop the cluster
stopCluster(cl)

# Combine results
for (result in results) {
  if (!is.null(result)) {
    data[data$country == result$country, paste0("yhat_pmt_ex_qr", result$num, "_cons")] <-
      result$data[[paste0("yhat_pmt_ex_qr", result$num, "_cons")]]
  }
}

# Export results
saveRDS(data, file = "../data/processed/07_data.RData")
