# Import packages
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# Set working directly
setwd("/home/joe/Projects/osha-fatality-inspections/")

# Import the inspections data
inspections <- list.files(path = "data/osha_inspections/", pattern = "*.csv", full.names = T) %>% 
  # Convert all columns to character type to minimize data type errors
  lapply(read_csv, col_types = cols(.default = "c")) %>% 
  # Concatenate the CSVs
  bind_rows() %>% 
  # Convert the date columns to date
  mutate_at(c("open_date", "case_mod_date", "close_conf_date", "close_case_date"), funs(ymd))

# How many inspections happen each year?
inspections_by_year <- inspections %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_inspections = n()) %>% 
  arrange(desc(year))

# How many of these are fatality inspections?
fatality_inspections_by_year <- inspections %>% 
  filter(insp_type == "M") %>% 
  group_by(year = year(open_date)) %>% 
  summarize(num_fatality_inspections = n()) %>% 
  arrange(desc(year))

fatality_inspections_by_year %>% 
  summarize(sum(num_fatality_inspections))

# How many different workplaces received a fatality inspection when grouped by address?
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

# How many of these work sites were inspected before their fatal accident?
inspected_prior <- inner_join(fatality_inspections_by_workplace, all_inspections_by_workplace, by = "estab_address") %>% 
  mutate(prior_inspection = case_when(
    all_insp_date < fatality_insp_date ~ T,
    all_insp_date >= fatality_insp_date ~ F
  )) %>% 
  arrange(all_insp_date) %>% 
  distinct(estab_address, fatality_insp_date, .keep_all = T)

inspected_prior %>% 
  group_by(prior_inspection) %>% 
  summarize(n())