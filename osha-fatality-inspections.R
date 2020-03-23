# Import packages
library(abbr2state)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(rusps)
library(stringr)
library(XML)

# Set working directory
setwd("/home/joe/Projects/osha-fatality-inspections/")

# Import the inspections data
inspections <- list.files(path = "data/osha_inspections/", pattern = "*.csv", full.names = T) %>% 
  # Convert all columns to character type to minimize data type errors
  lapply(read_csv, col_types = cols(.default = "c")) %>% 
  # Concatenate the CSVs
  bind_rows() %>% 
  # Convert the date columns to date
  mutate_at(c("open_date", "case_mod_date", "close_conf_date", "close_case_date"), funs(ymd)) %>% 
  # The catastrophic/fatality inspetions data only goes back to April 20, 2011. So filter to inspections conducted on April 21, 2001 or later.
  filter(open_date >= "2001-04-20")

# How many inspections happen each year?
inspections_by_year <- inspections %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_inspections = n()) %>% 
  arrange(desc(year))

# Chart the last six years
inspections_by_year %>%
  filter(year >= 2014) %>% 
  ggplot(aes(x = year, y = num_inspections)) +
  geom_col() +
  labs(title = "Total inspections by year",
       caption = "Source: OSHA",
       x = "Year",
       y = "Inspections")

# How many of these are catastrophic/fatality inspections?
fatality_inspections_by_year <- inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_fatality_inspections = n()) %>% 
  arrange(desc(year))

# Chart the last six years
fatality_inspections_by_year %>%
  filter(year >= 2014) %>% 
  ggplot(aes(x = year, y = num_fatality_inspections)) +
  geom_col() +
  labs(title = "Catastrophic/fatality inspections by year",
       caption = "Source: OSHA",
       x = "Year",
       y = "Inspections")

fatality_inspections_by_year %>% 
  summarize(sum(num_fatality_inspections))

# How many different workplaces received a catastrophic/fatality inspection when grouped by address?
fatality_inspections_by_workplace <- inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           fatality_insp_date = open_date) %>% 
  select(estab_address, fatality_insp_date)

# How many different workplaces received any sort of an inspection when grouped by address?
all_inspections_by_workplace <- inspections %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           all_insp_date = open_date) %>% 
  select(estab_address, all_insp_date)

# Join the data frames to see every inspection prior to each workplace's catastrophic/fatal accidents.
inspected_prior <- inner_join(fatality_inspections_by_workplace, all_inspections_by_workplace, by = "estab_address") %>% 
  mutate(days_between = time_length(fatality_insp_date - all_insp_date, "day"),
         prior_inspection = case_when(
           days_between > 0 & days_between < 3652.5 ~ T,
           days_between == 0 | days_between >= 3652.5 ~ F
           )) %>% 
  filter(days_between >= 0)

# And how many of these catastrophic/fatal accidents were there?
inspected_prior %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

# And how many of these catastrophic/fatal accidents were preceded by an inspection in the prior 10 years?
inspected_prior %>% 
  filter(prior_inspection == T) %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

------------------------------------------------------------
### Calculating figures only using OSHA-inspected workplaces
------------------------------------------------------------

# Create lists of states based on whether OSHA conducts some or all of their inspections of non-federal facilities
osha_states = list("AL", "AR", "AS", "CO", "DC", "DE", "FL", "GA", "GU", "ID", "KS", "LA", "MA", "MO", "MP", "MS", "MT", "ND", "NE", "NH", "OH", "OK", "PA", "RI", "SD", "TX", "UK", "WI", "WV")
non_osha_states = list("AK", "AZ", "CA", "HI", "IN", "IA", "KY", "MD", "MI", "MN", "NV", "NM", "NC", "OR", "PR", "SC", "TN", "UT", "VT", "VA", "WA", "WY")
non_osha_states_gov_workers = list("CT", "IL", "ME", "NJ", "NY", "VI")

# Filter to just OSHA-inspected workplaces
osha_inspections <- inspections %>% 
  filter(owner_type == "D" |
           site_state %in% osha_states |
           site_state %in% non_osha_states_gov_workers & owner_type == "A")

# How many inspections happen each year?
osha_inspections_by_year <- osha_inspections %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_inspections = n()) %>% 
  arrange(desc(year))

# Chart the last six years
osha_inspections_by_year %>%
  filter(year >= 2014) %>% 
  ggplot(aes(x = year, y = num_inspections)) +
  geom_col() +
  labs(title = "Total inspections by year",
       caption = "Source: OSHA",
       x = "Year",
       y = "Inspections")

# How many of these are fatality inspections?
osha_fatality_inspections_by_year <- osha_inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_fatality_inspections = n()) %>% 
  arrange(desc(year))

# Chart the last six years
osha_fatality_inspections_by_year %>%
  filter(year >= 2014) %>% 
  ggplot(aes(x = year, y = num_fatality_inspections)) +
  geom_col() +
  labs(title = "Fatality inspections by year",
       caption = "Source: OSHA",
       x = "Year",
       y = "Inspections")

osha_fatality_inspections_by_year %>% 
  summarize(sum(num_fatality_inspections))

# How many different workplaces received a fatality inspection when grouped by address?
osha_fatality_inspections_by_workplace <- osha_inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           fatality_insp_date = open_date) %>% 
  select(estab_address, fatality_insp_date)

# How many different workplaces received any sort of an inspection when grouped by address?
osha_all_inspections_by_workplace <- osha_inspections %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           all_insp_date = open_date) %>% 
  select(estab_address, all_insp_date)

# Join the data frames to see every inspection prior to each workplace's catastrophic/fatal accidents.
osha_inspected_prior <- inner_join(osha_fatality_inspections_by_workplace, osha_all_inspections_by_workplace, by = "estab_address") %>% 
  mutate(days_between = time_length(fatality_insp_date - all_insp_date, "day"),
         prior_inspection = case_when(
           days_between > 0 & days_between < 3652.5 ~ T,
           days_between == 0 | days_between >= 3652.5 ~ F
           )) %>% 
  filter(days_between >= 0)

# And how many of these catastrophic/fatal accidents were there?
osha_inspected_prior %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

# And how many of these catastrophic/fatal accidents were preceded by an inspection in the prior 10 years?
osha_inspected_prior %>% 
  filter(prior_inspection == T) %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

----------------------------------------------------
### Comparing OSHA inspections under Obama and Trump
----------------------------------------------------

# Obama inspections

# Filter to OSHA inspections just in the last three years of Obama's presidency
# Trump was inaugurated on January 20, 2017. So filter to inspections conducted between January 21, 2014 and January 20, 2017.
obama_osha_inspections <- osha_inspections %>% 
  filter(open_date >= "2014-01-21" & open_date <= "2017-01-20")

# How many different workplaces received a catastrophic/fatality inspection when grouped by address?
obama_osha_fatality_inspections_by_workplace <- obama_osha_inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           fatality_insp_date = open_date) %>% 
  select(estab_address, fatality_insp_date)

# How many different workplaces received any sort of an inspection when grouped by address?
obama_osha_all_inspections_by_workplace <- obama_osha_inspections %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           all_insp_date = open_date) %>% 
  select(estab_address, all_insp_date)

# Join the data frames to see every inspection prior to each workplace's catastrophic/fatal accidents.
obama_osha_inspected_prior <- inner_join(obama_osha_fatality_inspections_by_workplace, obama_osha_all_inspections_by_workplace, by = "estab_address") %>% 
  mutate(days_between = time_length(fatality_insp_date - all_insp_date, "day"),
         prior_inspection = case_when(
           days_between > 0 ~ T,
           days_between == 0 ~ F
         )) %>% 
  filter(days_between >= 0)

# And how many of these catastrophic/fatal accidents were there?
obama_osha_inspected_prior %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

# And how many of these catastrophic/fatal accidents were preceded by an inspection?
obama_osha_inspected_prior %>% 
  filter(prior_inspection == T) %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

# Trump inspections

# Filter to OSHA inspections just in the first three years of Trump's presidency
# Trump was inaugurated on January 20, 2017. So filter to inspections conducted between January 21, 2017 and January 20, 2020.
trump_osha_inspections <- osha_inspections %>% 
  filter(open_date >= "2017-01-21" & open_date <= "2020-01-20")

# How many different workplaces received a catastrophic/fatality inspection when grouped by address?
trump_osha_fatality_inspections_by_workplace <- trump_osha_inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           fatality_insp_date = open_date) %>% 
  select(estab_address, fatality_insp_date)

# How many different workplaces received any sort of an inspection when grouped by address?
trump_osha_all_inspections_by_workplace <- trump_osha_inspections %>% 
  group_by(estab_address = str_replace_all(paste0(site_address, site_city, site_state, site_zip), " ", ""),
           all_insp_date = open_date) %>% 
  select(estab_address, all_insp_date)

# Join the data frames to see every inspection prior to each workplace's catastrophic/fatal accidents.
trump_osha_inspected_prior <- inner_join(trump_osha_fatality_inspections_by_workplace, trump_osha_all_inspections_by_workplace, by = "estab_address") %>% 
  mutate(days_between = time_length(fatality_insp_date - all_insp_date, "day"),
         prior_inspection = case_when(
           days_between > 0 ~ T,
           days_between == 0 ~ F
         )) %>% 
  filter(days_between >= 0)

# And how many of these catastrophic/fatal accidents were there?
trump_osha_inspected_prior %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))

# And how many of these catastrophic/fatal accidents were preceded by an inspection?
trump_osha_inspected_prior %>% 
  filter(prior_inspection == T) %>% 
  distinct(estab_address, fatality_insp_date) %>% 
  summarize(num_accidents = n()) %>% 
  arrange(desc(num_accidents))