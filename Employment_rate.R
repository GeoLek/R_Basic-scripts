# Load the necessary core tidyverse libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)

# Set the working directory
setwd("/home/orion/Geo/Projects/R_Basic-scripts/")

# Load the datasets, skipping the first 4 rows which contain metadata
male_employment <- read_csv("male_employment.csv", skip = 4, col_types = cols(.default = "c"))
female_employment <- read_csv("female_employment.csv", skip = 4, col_types = cols(.default = "c"))

# Check the column names of each data frame
colnames(female_employment)
colnames(male_employment)

# Define the range of years to keep
years_to_keep <- as.character(1991:2022)

# Convert the necessary columns to numeric
male_employment[years_to_keep] <- lapply(male_employment[years_to_keep], as.numeric)
female_employment[years_to_keep] <- lapply(female_employment[years_to_keep], as.numeric)

# Filter and select required columns for each dataset
male_eu <- male_employment %>%
  filter(`Country Name` == "European Union") %>%
  select(`Country Name`, any_of(years_to_keep))

female_eu <- female_employment %>%
  filter(`Country Name` == "European Union") %>%
  select(`Country Name`, any_of(years_to_keep))

# Merge the datasets
eu_employment <- left_join(male_eu, female_eu, by = "Country Name", suffix = c("_Male", "_Female"))

# Display the first few rows of the cleaned and merged dataset
print(eu_employment, n = 32, width = Inf)
