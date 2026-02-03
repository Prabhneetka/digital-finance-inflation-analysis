#########################################################
# Digital Finance Project
# Inflation, GDP Growth & Private Credit Analysis
# Countries: Germany, France, Italy
#########################################################

# 1. Load required libraries
library(dplyr)
library(tidyr)

# 2. Load datasets (World Bank CSVs)
inflation <- read.csv(
  "API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_32.csv",
  skip = 4
)

gdp <- read.csv(
  "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_40824.csv",
  skip = 4
)

credit <- read.csv(
  "API_FS.AST.PRVT.GD.ZS_DS2_en_csv_v2_76.csv",
  skip = 4
)

# 3. Select countries
countries <- c("Germany", "France", "Italy")

inflation_eu <- inflation %>%
  filter(Country.Name %in% countries)

gdp_eu <- gdp %>%
  filter(Country.Name %in% countries)

credit_eu <- credit %>%
  filter(Country.Name %in% countries)

# 4. Convert data to long format
inflation_long <- inflation_eu %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Inflation"
  ) %>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  filter(!is.na(Inflation))

gdp_long <- gdp_eu %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GDP_Growth"
  ) %>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  filter(!is.na(GDP_Growth))

credit_long <- credit_eu %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Private_Credit"
  ) %>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  filter(!is.na(Private_Credit))

# 5. Merge datasets
final_data <- inflation_long %>%
  select(Country.Name, Year, Inflation) %>%
  left_join(
    gdp_long %>% select(Country.Name, Year, GDP_Growth),
    by = c("Country.Name", "Year")
  ) %>%
  left_join(
    credit_long %>% select(Country.Name, Year, Private_Credit),
    by = c("Country.Name", "Year")
  )

# 6. Focus on post-2000 period
final_data <- final_data %>%
  filter(Year >= 2000)

# 7. Regression model
model <- lm(
  Inflation ~ GDP_Growth + Private_Credit,
  data = final_data
)

# 8. Model summary
summary(model)
