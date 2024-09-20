#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                   generating poverty indicators
#  Figure 1
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())

data <- readRDS(file = "../data/processed/12_data.RData")

# libraries ----
library(dplyr)
library(ggplot2)
library(purrr)
library(gridExtra)

# Define the list of countries
countries <- c("BurkinaFaso", "Ethiopia", "Ghana", "Malawi", "Mali", "Niger", "Nigeria", "Tanzania", "Uganda")

# Function to create a scatter plot for a single country
create_scatter_plot <- function(data, country) {
  # Calculate mean poverty line for the country
  pov_line_mean <- mean(data$pov_line_20[data$country == country], na.rm = TRUE)

  # Create the scatter plot
  plot <- ggplot(data[data$country == country, ], aes(x = yhat_pmt_sh_consump, y = consump)) +
    geom_point(size = 0.5, alpha = 0.5) +
    geom_vline(xintercept = pov_line_mean, linetype = "dashed", color = "red") +
    geom_hline(yintercept = pov_line_mean, linetype = "dashed", color = "red") +
    labs(x = "Predicted log consumption",
         y = "Actual log consumption",
         title = country) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"))

  return(plot)
}

# Create and save plots for each country
plots <- map(countries, ~create_scatter_plot(data, .x))

# Save individual plots
# walk2(plots, countries, ~{
#   ggsave(paste0("pmt_sh_", .y, ".png"), plot = .x, width = 8, height = 6, dpi = 300)
# })

# Optionally, create a grid of all plots
grid_plot <- do.call(grid.arrange, c(plots, ncol = 3))
# ggsave("all_countries_grid.png", plot = grid_plot, width = 24, height = 18, dpi = 300)

saveRDS(data, file = "../data/processed/13_data.RData")
