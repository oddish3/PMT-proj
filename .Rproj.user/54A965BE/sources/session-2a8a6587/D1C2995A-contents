#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   Quantile Regression
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rm(list=ls())

# libraries ----
# libraries ----
library(quantreg)

data <- readRDS(file = "../data/processed/02_data.RData")

nums <- c("20", "40", "50")

# Create new variables for predicted values
for (num in nums) {
  data[, (paste0("yhat_pmt_sh_qr", num, "_cons")) := NA]
  setattr(data[[paste0("yhat_pmt_sh_qr", num, "_cons")]], "label", paste("Predicted values quantile reg short PMT", num))
}

# Define variable groups
wealth <- c("toilet_flush", "toilet_pit", "floor_finish", "wall_finish", "roof_finish", "fuel_elecgas", "fuel_charcoal")
hh <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary", "div_sep_head", "widow_head", "work_paid_head", "work_selfemp_nonf_head", "muslim", "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm")

# Define countries
countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

# Create formula
formula <- as.formula(paste("consump ~",
                            paste(c(wealth, hh,
                                    "factor(hhsize_cat)", "factor(age_head_cat)",
                                    "factor(state)", "factor(month)"),
                                  collapse = " + ")))

# Run quantile regressions
models <- list()
for (num in nums) {
  for (country in countries) {
    # Subset data for the current country
    country_data <- data[country == country, ]

    # Run quantile regression with "fn" method
    model <- try(rq(formula, tau = as.numeric(paste0("0.", num)),
                    data = country_data,
                    method = "fn"))

    if (!inherits(model, "try-error")) {
      models[[paste(num, country, sep = "_")]] <- model

      # Generate predictions
      predictions <- predict(model, newdata = country_data)

      # Update the main dataset
      data[country == country, (paste0("yhat_pmt_sh_qr", num, "_cons")) := predictions]
    }
  }
}

# Save the updated data
saveRDS(data, file = "../data/processed/03_data.RData")
