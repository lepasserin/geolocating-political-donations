# Setup
library(tidyverse)
raw_data <- read_csv("data/raw_data/raw_data_MVP.csv")

raw_EC_data <- read_csv(
  "/Users/benedictcummins-mburu/Library/CloudStorage/OneDrive-UniversityofToronto/Desktop/PoliticalFinance/od_cntrbtn_de_e.csv"
)

# Talking Points

#

electoral_event_distribution <- raw_data %>%
  group_by(electoral_event) %>%
  summarize(n = n()) %>%
  arrange(n)
electoral_event_distribution_raw <- raw_EC_data %>%
  group_by(`Electoral event`) %>%
  summarize(n = n()) %>%
  arrange(n)


the_data <- raw_data %>% filter(electoral_event == "0")
unique(the_data$political_entity)

the_data <- raw_data


the_data2 <- raw_EC_data %>% filter(`Electoral event` == 0.0)
unique(the_data2$`Political Entity`)

the <- raw_data %>%
  filter(amount + amount_monetary + amount_non_monetary <= 0) %>%
  filter(year >= 2013)

unique(raw_data$donation_year)

the <-
  View(head(the))

the


head(the_data2)
