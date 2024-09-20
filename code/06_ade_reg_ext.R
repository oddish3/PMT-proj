#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   adult equivalent expenditure extended regression
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

# libraries ----

data <- readRDS(file = "../data/processed/05_data.RData")

# extended regression ----

setDT(data)
copy <- data

# Generate new variables
data[, ':=' (
  female_head_widow = 0,
  female_head_div = 0,
  female_head_nevermar = 0
)]

# Replace values
data[female_head == 1 & widow_head == 1, female_head_widow := 1]
data[female_head == 1 & div_sep_head == 1, female_head_div := 1]
data[female_head == 1 & nevermar_head == 1, female_head_nevermar := 1]

# Label variables
data[, ':=' (
  female_head_widow = labelled::set_variable_labels(female_head_widow, "Widowed female head"),
  female_head_div = labelled::set_variable_labels(female_head_div, "Divorced female head"),
  female_head_nevermar = labelled::set_variable_labels(female_head_nevermar, "Never married female head")
)]

# Generate new variable for predictions
data[, yhat_pmt_ext_consump := as.numeric(NA)]
data[, yhat_pmt_ext_consump := labelled::set_variable_labels(yhat_pmt_ext_consump, "Predicted values extended PMT")]

# Define variables
wealth <- c("water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_finish", "wall_finish", "roof_finish", "members_per_room", "kitchen_room", "fuel_elecgas", "fuel_charcoal")
assets <- c("electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car", "telephone", "mobile_phone", "computer", "video", "stove_any", "sew_machine", "aircon", "iron", "satelite", "generator", "own_house")
hh <- c("urban", "female_head", "edu_head_primary", "edu_head_secondary", "max_edu_primary", "max_edu_secondary", "div_sep_head", "widow_head", "nevermar_head", "female_head_widow", "female_head_div", "female_head_nevermar", "work_paid_head", "work_selfemp_nonf_head", "work_daily_head", "muslim", "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm", "share_widow", "share_disabledm", "share_disabledf", "share_orphanm", "share_orphanf")

# Clear any existing stored estimates
# rm(list = ls(pattern = "^est"))

models <- list()

countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

for (x in countries) {
  # Create formula
  formula_terms <- c(wealth, assets, hh, "i(hhsize_cat)", "i(age_head_cat)", "i(state)", "i(month)")
  formula <- as.formula(paste("consump ~", paste(formula_terms, collapse = " + ")))

  # Run regression
  model <- feols(formula, data = data[country == x], cluster = ~EA)

  # Store model
  models[[x]] <- model

  # Predict and replace values
  yhat <- predict(model)
  data[country == x, yhat_pmt_ext_consump := yhat]
}

# Export results
# modelsummary(models,
#              output = "results_extended.csv",
#              fmt = "%.3f",
#              stars = c('*' = .1, '**' = .05, '***' = .01),
#              coef_omit = -c(wealth, assets, hh),
#              gof_map = c("nobs", "r.squared"),
#              type = "html")

saveRDS(data, file = "../data/processed/06_data.RData")
