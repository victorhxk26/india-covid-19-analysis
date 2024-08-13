# VICTOR HEW XIN KAI
# TP078400

## TASK 1
# Install relevant packages
install.packages("dplyr")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("skimr")
install.packages("ggplot2")
install.packages("stringr")

# Load relevant packages
library(dplyr)
library(magrittr)
library(tidyverse)
library(skimr)
library(ggplot2)
library(stringr)

# Read the data set from the CSV file and name the data frame as "COVID19_India"
COVID19_India <- read.csv(file ="covid_19_india.csv", header = TRUE)

# View data
View(COVID19_India)

# Change data types of cumulative number of confirmed Indian & foreign nationals and replace '-' values with 
# missing values labelled as "NA"
COVID19_India <- COVID19_India %>% 
  mutate(ConfirmedIndianNational = as.numeric(ConfirmedIndianNational, na.rm = TRUE)) %>% 
  mutate(ConfirmedForeignNational = as.numeric(ConfirmedForeignNational, na.rm = TRUE)) %>%
# Sort the updated table by State/Union Territory
  arrange(State.UnionTerritory)

# Check if there are any missing values 
anyNA(COVID19_India)
# and remove them if any
COVID19_India <- drop_na(COVID19_India)

# Check data types of each column
skim(COVID19_India)


## TASK 2
# Summary for the data set
summary(COVID19_India)

# Total number of records in the data set, labelled as "total_record"
total_record <- as.numeric(nrow(COVID19_India)) # 446

# The day with the highest number of confirmed and cured cases
Filtered <- COVID19_India %>% 
  filter(Confirmed == max(Confirmed), Cured == max(Cured)) # Filter to find the highest confirmed and cured cases 

Day_Highest_Cases <- Filtered[, "Date"] # Extract "Date" column from Filtered data frame 

# Visualization - Distribution of Confirmed COVID-19 cases in India using histogram
ggplot(COVID19_India, mapping = aes(Confirmed, fill = 'red')) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribution of Confirmed COVID-19 Cases in India")

# Visualization - Distribution of Confirmed COVID-19 cases on Locals in India using histogram
ggplot(COVID19_India, mapping = aes(ConfirmedIndianNational, fill = 'red')) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribution of Cumulative Confirmed COVID-19 Cases on Indian Nationals in India")

# Visualization - Distribution of Confirmed COVID-19 cases on Foreigners in India using histogram
ggplot(COVID19_India, mapping = aes(ConfirmedForeignNational, fill = 'red')) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribution of Cumulative Confirmed COVID-19 Cases on Foreign Nationals in India")

# Visualization - Distribution of Cured COVID-19 cases in India using histogram
ggplot(COVID19_India, mapping = aes(Cured, fill = 'red')) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribution of Cured COVID-19 Cases in India")

# Visualization - Distribution of Death COVID-19 cases in India using histogram
ggplot(COVID19_India, mapping = aes(Deaths, fill = 'red')) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Distribution of Death COVID-19 Cases in India")


## TASK 3
# Extract "Cured", "Deaths" and "Confirmed" columns from "COVID19_India" data frame and form new one labelled "Avg_Cases" 
Avg_Cases <- COVID19_India %>% 
  subset(select = c(Cured, Deaths, Confirmed)) 

# Add new variables to "COVID19_India" data frame
COVID19_India <- COVID19_India %>% 
  mutate(Percentage_Cured = (Cured / Confirmed) * 100) %>% # % of cured cases
  mutate(Percentage_Death = (Deaths / Confirmed) * 100) %>% # % of death cases
  mutate(Percentage_Confirmed_Indian_National = (ConfirmedIndianNational / Confirmed) * 100) %>% # % of confirmed cases (Indians)
  mutate(Percentage_Confirmed_Foreign_National = (ConfirmedForeignNational / Confirmed) * 100) # % of confirmed cases (Foreigners)

# Separate variable "Date" into 3 levels - Jan, Feb, Mar
COVID19_India <- COVID19_India %>%
  mutate(Date = case_when(
    str_detect(Date, "/1/") ~ "Jan", # If there's "/1/" in any Date value, then classified as "Jan"
    str_detect(Date, "/2/") ~ "Feb", # If there's "/2/" in any Date value, then classified as "Feb"
    str_detect(Date, "/3/") ~ "Mar", # If there's "/3/" in any Date value, then classified as "Mar"
    TRUE ~ Date # If there's any other value, then remain as Date
  )) %>% 
  rename(Month = Date) # rename column name Date to Month


## TASK 4
# Average, total and standard deviation number of cured, deaths, and confirmed cases all over the states/union territory
Avg_Cases <- Avg_Cases %>% 
  summarise(across(where(is.numeric), list(mean = mean, sum = sum, sd = sd)))

# Average number of confirmed cases for each month
Month_Avg_Confirmed <- COVID19_India %>% 
  group_by(Month) %>% 
  summarise(Avg_Confirmed = mean(Confirmed, na.rm = TRUE))

# Average number of cured cases for each month
Month_Avg_Cured <- COVID19_India %>% 
  group_by(Month) %>% 
  summarise(Avg_Cured = mean(Cured, na.rm = TRUE))

# Average number of death cases for each month
Month_Avg_Death <- COVID19_India %>% 
  group_by(Month) %>% 
  summarise(Avg_Deaths = mean(Deaths, na.rm = TRUE))

# Average number of confirmed cases on locals for each month
Month_Avg_ConfirmedIndian <- COVID19_India %>% 
  group_by(Month) %>% 
  summarise(Avg_Confirmed_Indian = mean(ConfirmedIndianNational, na.rm = TRUE))

# Average number of confirmed cases on foreigners for each month
Month_Avg_ConfirmedForeign <- COVID19_India %>% 
  group_by(Month) %>% 
  summarise(Avg_Confirmed_Foreign = mean(ConfirmedForeignNational, na.rm = TRUE))

# Number of confirmed cases in every state / union territory
State.UnionTerritory_Confirmed <- COVID19_India %>% 
  group_by(State.UnionTerritory) %>% 
  summarise(sum(Confirmed)) %>% 
  rename(Num_Confirmed = "sum(Confirmed)") %>% 
  arrange(-Num_Confirmed)

# Number of confirmed cured cases in every state / union territory
State.UnionTerritory_Cured <- COVID19_India %>% 
  group_by(State.UnionTerritory) %>% 
  summarise(sum(Cured)) %>% 
  rename(Num_Cured = "sum(Cured)") %>% 
  arrange(-Num_Cured)

# Number of death cases in every state / union territory
State.UnionTerritory_Deaths <- COVID19_India %>% 
  group_by(State.UnionTerritory) %>% 
  summarise(sum(Deaths)) %>% 
  rename(Num_Deaths = "sum(Deaths)") %>% 
  arrange(-Num_Deaths)

# Number of confirmed cases (locals) in every state / union territory
State.UnionTerritory_Confirmed_Indian <- COVID19_India %>% 
  group_by(State.UnionTerritory) %>% 
  summarise(sum(ConfirmedIndianNational)) %>% 
  rename(Num_ConfirmedIndianNational = "sum(ConfirmedIndianNational)") %>% 
  arrange(-Num_ConfirmedIndianNational)

# Number of confirmed cases (foreigners) in every state / union territory
State.UnionTerritory_Confirmed_Foreign <- COVID19_India %>% 
  group_by(State.UnionTerritory) %>% 
  summarise(sum(ConfirmedForeignNational)) %>% 
  rename(Num_ConfirmedForeignNational = "sum(ConfirmedForeignNational)") %>% 
  arrange(-Num_ConfirmedForeignNational)

# Find covariation between numeric variables
data_numeric <- data.frame(COVID19_India$ConfirmedIndianNational, COVID19_India$ConfirmedForeignNational, 
                           COVID19_India$Cured, COVID19_India$Deaths, COVID19_India$Confirmed)
# Get the correlation matrix
correlation_table <- cor(data_numeric)
