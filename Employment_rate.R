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

# Load the datasets
male_employment <- read_csv("male_employment.csv")
female_employment <- read_csv("female_employment.csv")

# Display the first few rows of each dataset to understand their structure
print("Male Employment Data:")
head(male_employment)

print("Female Employment Data:")
head(female_employment)
