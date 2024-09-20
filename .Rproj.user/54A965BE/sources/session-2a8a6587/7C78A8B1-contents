#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   Replication of A Proxy Means Test
# file 1 to clean data
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

# libraries ----
library(data.table)
library(dplyr)
library(haven)
library(Hmisc)

# data cleaning ----
# Read the data
data <- read_dta("../PMT_ReplicationFiles_Stata/Brown_Ravallion_vandeWalle_PMT.dta")
copy <- data
initial_obs <- nrow(data)

# Create household size categories
data$hhsize_cat <- cut(data$hhsize, breaks = c(1, 3, 5, 7, 9, Inf), right = FALSE, labels = FALSE)
data$hhsize_cat[data$hhsize >= 9] <- 9

# Replace month for specific countries
data$month[data$country %in% c("BurkinaFaso", "Ethiopia", "Malawi", "Mali", "Ghana")] <- 1

# Generate population weight
data$popweight1 <- data$hhweight * data$hhsize
data$popweight1[is.na(data$popweight1)] <- data$popweight[is.na(data$popweight1)]

# Calculate hhweight for Nigeria
data$hhweight[data$country == "Nigeria"] <- data$popweight[data$country == "Nigeria"] / data$hhsize[data$country == "Nigeria"]

# Drop observations with missing weights
data <- data[!is.na(data$hhweight) & !is.na(data$popweight1), ]
obs_after_weight_drop <- nrow(data)
initial_obs - obs_after_weight_drop # correct

# Check for missing EA by country
table(data$country[is.na(data$EA)])

# Replace missing values with 0 for specific variables
vars <- c("share_05f", "share_05m", "share_614f", "share_614m", "share_65plusf", "share_65plusm",
          "share_widow", "share_disabledm", "share_disabledf", "share_orphanm", "share_orphanf")
data[vars] <- lapply(data[vars], function(x) ifelse(is.na(x), 0, x))

# Calculate mean consumption and below mean indicator
data <- data %>%
  group_by(country) %>%
  mutate(mean_consump = wtd.mean(real_consumption_pc, weights = popweight1),
         below_mean = as.numeric(real_consumption_pc <= mean_consump))

# Replace consumption with NA for missing values in specified variables
vars <- c("water_piped", "water_well", "toilet_flush", "toilet_pit", "floor_finish", "wall_finish",
          "roof_finish", "members_per_room", "kitchen_room", "fuel_elecgas", "fuel_charcoal",
          "electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car", "telephone",
          "mobile_phone", "computer", "video", "stove_any", "sew_machine", "aircon", "iron",
          "satelite", "generator", "own_house", "urban", "female_head", "edu_head_primary",
          "edu_head_secondary", "max_edu_primary", "max_edu_secondary", "div_sep_head",
          "widow_head", "nevermar_head", "work_paid_head", "work_selfemp_nonf_head",
          "work_daily_head", "share_05f", "share_05m", "share_614f", "share_614m",
          "share_65plusf", "share_65plusm", "muslim", "christian", "month", "age_head_cat",
          "hhsize_cat")

for (var in vars) {
  data$real_consumption_pc[is.na(data[[var]])] <- NA
}

# Drop observations with NA in real_consumption_pc
data <- data[!is.na(data$real_consumption_pc), ]
final_obs <- nrow(data)
obs_after_weight_drop - final_obs # correct 1942 dropped

# Generate log of real consumption per capita
data$consump <- log(data$real_consumption_pc)

# Summarize consump by country
consump_summary <- data %>%
  group_by(country) %>%
  summarise(mean_consump = mean(consump, na.rm = TRUE),
            sd_consump = sd(consump, na.rm = TRUE),
            min_consump = min(consump, na.rm = TRUE),
            max_consump = max(consump, na.rm = TRUE))
print(consump_summary)

# Drop consumption_pctile if it exists
data$consumption_pctile <- NULL

# Generate consumption percentiles by country
countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

for (country in countries) {
  country_data <- data[data$country == country & !is.na(data$consump), ]
  percentiles <- wtd.quantile(country_data$consump, weights = country_data$hhweight, probs = seq(0.01, 1, 0.01))
  data$consumption_pctile[data$country == country] <- findInterval(data$consump[data$country == country], percentiles) + 1
}

# Define variable groups for Table A1
wealth_vars <- c("real_consumption_pc", "water_piped", "water_well", "toilet_flush", "toilet_pit",
                 "floor_natural", "floor_rudiment", "floor_finish", "wall_natural", "wall_rudiment",
                 "wall_finish", "roof_natural", "roof_rudiment", "roof_finish", "members_per_room",
                 "kitchen_room", "fuel_elecgas", "fuel_charcoal", "fuel_wood")

asset_vars <- c("electric", "radio", "telev", "fridge", "bicycle", "motorbike", "car", "telephone",
                "mobile_phone", "computer", "video", "stove_any", "sew_machine", "aircon", "iron",
                "satelite", "generator", "own_house")

hh_vars <- c("urban", "age_head", "female_head", "edu_head_primary", "edu_head_secondary",
             "max_edu_primary", "max_edu_secondary", "ever_married_head", "married_head",
             "div_sep_head", "widow_head", "nevermar_head", "hhsize", "work_paid_head",
             "work_selfemp_nonf_head", "work_selfemp_farm_head", "work_daily_head", "muslim",
             "christian", "share_05f", "share_05m", "share_614f", "share_614m", "share_1564f",
             "share_1564m", "share_65plusf", "share_65plusm", "share_widow", "share_disabledm",
             "share_disabledf", "share_orphanm", "share_orphanf")

# Combine all variables
all_vars <- c(wealth_vars, asset_vars, hh_vars)

# Calculate weighted means for all variables by country
summary_stats <- data %>%
  group_by(country) %>%
  summarise(across(all_of(all_vars),
                   ~weighted.mean(., w = hhweight, na.rm = TRUE),
                   .names = "mean_{.col}")) %>%
  ungroup()

# Format the results to 3 decimal places
summary_stats <- summary_stats %>%
  mutate(across(-country, ~round(., 3)))

# Print the results
print(summary_stats) #appendix A1?

# Generate poverty lines (in log terms)
data <- data %>%
  group_by(country) %>%
  mutate(
    pov_line_20 = max(consump[consumption_pctile == 20], na.rm = TRUE),
    pov_line_40 = max(consump[consumption_pctile == 40], na.rm = TRUE)
  ) %>%
  ungroup()

# Add variable labels
attr(data$pov_line_20, "label") <- "Poverty line 20 percent"
attr(data$pov_line_40, "label") <- "Poverty line 40 percent"

# Generate poverty rates pre-transfers
data <- data %>%
  mutate(
    poor_20 = ifelse(!is.na(consump), ifelse(consump <= pov_line_20, 1, 0), NA),
    poor_40 = ifelse(!is.na(consump), ifelse(consump <= pov_line_40, 1, 0), NA)
  )

# Add variable labels
attr(data$poor_20, "label") <- "Poor (in 20th pctile)"
attr(data$poor_40, "label") <- "Poor (in 40th pctile)"

# Check poverty rates
poverty_rates <- data %>%
  group_by(country) %>%
  summarise(
    poor_20_rate = weighted.mean(poor_20, hhweight, na.rm = TRUE),
    poor_40_rate = weighted.mean(poor_40, hhweight, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(poverty_rates)
setDT(data)

saveRDS(data, file = "../data/processed/data_proc.RData")
