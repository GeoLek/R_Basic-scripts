# Load the necessary core tidyverse libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)  # Ensure purrr is loaded
library(tibble)
library(stringr)
library(forcats)

# Set the working directory
setwd("/home/orion/Geo/Projects/R_Basic-scripts/")

# Load the datasets, skipping the first 4 rows which contain metadata
male_employment <- read_csv("male_employment.csv", skip = 4, col_types = cols(.default = "c"))
female_employment <- read_csv("female_employment.csv", skip = 4, col_types = cols(.default = "c"))

# Define the range of years to keep
years_to_keep <- as.character(1991:2022)

# Filter and select required columns for each dataset
male_eu <- male_employment %>%
  filter(`Country Name` == "European Union") %>%
  select(`Country Name`, all_of(years_to_keep))

female_eu <- female_employment %>%
  filter(`Country Name` == "European Union") %>%
  select(`Country Name`, all_of(years_to_keep))

# Convert the necessary columns to numeric using map
male_eu <- male_eu %>%
  mutate(across(all_of(years_to_keep), ~ map_dbl(., as.numeric)))

female_eu <- female_eu %>%
  mutate(across(all_of(years_to_keep), ~ map_dbl(., as.numeric)))

# Merge the datasets
eu_employment <- left_join(male_eu, female_eu, by = "Country Name", suffix = c("_Male", "_Female"))

# Display the first few rows of the cleaned and merged dataset
print(eu_employment, n = 32, width = Inf)

# Create a subset of the data for the years 1992, 2002, 2012, and 2022
selected_years <- c("1992", "2002", "2012", "2022")
eu_employment_subset <- eu_employment %>%
  select(`Country Name`, matches(paste(selected_years, collapse = "|")))

# Reshape data for plotting
eu_employment_long <- eu_employment_subset %>%
  pivot_longer(cols = -`Country Name`, names_to = c("Year", "Gender"), names_sep = "_", values_to = "Employment_Rate") %>%
  mutate(Year = str_extract(Year, "\\d{4}"),
         Gender = ifelse(str_detect(Gender, "Male"), "Male", "Female"),
         Employment_Rate = as.numeric(Employment_Rate))

# Display the subset
print(eu_employment_long, n = 20, width = Inf)

# Histogram
ggplot(eu_employment_long, aes(x = Employment_Rate, fill = Gender)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  facet_wrap(~ Year) +
  labs(title = "Histogram of Employment Rates in the European Union",
       x = "Employment Rate (%)",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal()

# Boxplot
ggplot(eu_employment_long, aes(x = factor(Year), y = Employment_Rate, fill = Gender, color = Gender)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Boxplot of Employment Rates in the European Union",
       x = "Year",
       y = "Employment Rate (%)",
       fill = "Gender",
       color = "Gender") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

# Scatter plot
eu_employment_wide <- eu_employment_long %>%
  pivot_wider(names_from = Gender, values_from = Employment_Rate)

ggplot(eu_employment_wide, aes(x = Male, y = Female, color = factor(Year))) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Male vs Female Employment Rates in the European Union",
       x = "Male Employment Rate (%)",
       y = "Female Employment Rate (%)",
       color = "Year") +
  theme_minimal()

# Function to calculate the average employment rate for a given year and gender
calculate_average_employment_rate <- function(data, year, gender) {
  year_col <- paste0(year, "_", gender)
  avg_rate <- data %>%
    select(`Country Name`, all_of(year_col)) %>%
    summarise(Average_Rate = mean(get(year_col), na.rm = TRUE)) %>%
    pull(Average_Rate)

  return(avg_rate)
}

# Example usage
average_male_2022 <- calculate_average_employment_rate(eu_employment, "2022", "Male")
average_female_2022 <- calculate_average_employment_rate(eu_employment, "2022", "Female")

print(paste("Average Male Employment Rate in 2022:", average_male_2022))
print(paste("Average Female Employment Rate in 2022:", average_female_2022))

# Function to plot employment rate trends for males and females over a range of years
plot_employment_rate_trends <- function(data, start_year, end_year) {
  years <- as.character(start_year:end_year)
  selected_columns <- c("Country Name", outer(years, c("_Male", "_Female"), paste0))

  long_data <- data %>%
    select(all_of(selected_columns)) %>%
    pivot_longer(cols = -`Country Name`, names_to = c("Year", "Gender"), names_sep = "_", values_to = "Employment_Rate") %>%
    mutate(Year = as.numeric(Year))

  ggplot(long_data, aes(x = Year, y = Employment_Rate, color = Gender)) +
    geom_line(linewidth = 1) +
    labs(title = "Employment Rate Trends in the European Union",
         x = "Year",
         y = "Employment Rate (%)",
         color = "Gender") +
    theme_minimal()
}

# Example usage
plot_employment_rate_trends(eu_employment, 1992, 2022)

# Function to compare employment rates between two specified years
compare_employment_rates <- function(data, year1, year2) {
  year1_cols <- paste0(year1, c("_Male", "_Female"))
  year2_cols <- paste0(year2, c("_Male", "_Female"))

  comparison_data <- data %>%
    select(`Country Name`, all_of(year1_cols), all_of(year2_cols)) %>%
    pivot_longer(cols = -`Country Name`, names_to = c("Year", "Gender"), names_sep = "_", values_to = "Employment_Rate") %>%
    mutate(Year = str_extract(Year, "\\d{4}"),
           Gender = ifelse(str_detect(Gender, "Male"), "Male", "Female"),
           Employment_Rate = as.numeric(Employment_Rate)) %>%
    pivot_wider(names_from = Year, values_from = Employment_Rate)

  ggplot(comparison_data, aes(x = get(year1), y = get(year2), color = Gender)) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste("Comparison of Employment Rates:", year1, "vs", year2),
         x = paste("Employment Rate in", year1, "(%)"),
         y = paste("Employment Rate in", year2, "(%)"),
         color = "Gender") +
    theme_minimal()
}

# Example usage
compare_employment_rates(eu_employment, "2012", "2022")

#Research Question - Linear Regression
# How does the employment rate for males and females in the European Union change over time?

# Function to fit and summarize a linear regression model for employment rates over time
fit_linear_regression <- function(data, gender) {
  long_data <- data %>%
    select(`Country Name`, matches("_Male|_Female")) %>%
    pivot_longer(cols = -`Country Name`, names_to = c("Year", "Gender"), names_sep = "_", values_to = "Employment_Rate") %>%
    filter(Gender == gender) %>%
    mutate(Year = as.numeric(Year),
           Employment_Rate = as.numeric(Employment_Rate))

  # Fit the linear regression model
  model <- lm(Employment_Rate ~ Year, data = long_data)

  # Summarize the model
  model_summary <- summary(model)
  print(model_summary)

  # Plot the data and the regression line
  ggplot(long_data, aes(x = Year, y = Employment_Rate)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
    labs(title = paste("Linear Regression of", gender, "Employment Rates Over Time"),
         x = "Year",
         y = paste(gender, "Employment Rate (%)")) +
    theme_minimal()
}

# Example usage for Male and Female employment rates
fit_linear_regression(eu_employment, "Male")
fit_linear_regression(eu_employment, "Female")


# Results
# Both models indicate a statistically significant downward trend in employment rates for both males and females in the European Union over the years.
# The R-squared values are high for both models, indicating that a large proportion of the variance in employment rates is explained by the year.
# The slopes for both models are negative, which suggests that employment rates have been decreasing over time for both genders.
# The residuals for both models are relatively small, indicating that the model fits the data well.