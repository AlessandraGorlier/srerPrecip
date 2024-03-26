#Bailie Wynbelt
#wynbeltb@gmail.com
#The purpose of this script is to run once a month and output a csv with up-to-
#date precipitation data from active rain gauges on the Santa Rita Experimental
#Range. This script also will estimate missing values (-9999) based on location
#and modeled values based on month.

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(lubridate)


# GRAB CURRENT SRER DATA--------------------------------------------------
# get current date
current_date <- Sys.Date()

# get previous month
previous_month <- format(as.Date(format(current_date, "%Y-%m-01")), "%Y-%m")

# grab url
url <- sprintf("https://santarita.arizona.edu/sites/santarita.arizona.edu/files/%s/precip%s.xlsx", previous_month, previous_month)

# read to xlsx
precip_data <- read.xlsx(url)

month_mapping <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6, JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)

active <- precip_data |>
  filter(STATION %in% c('GRARI', 'AMADO', 'PAST3','SW','41','ROAD',
                      'WHITE', 'RODEN', 'DESGR', 'FORES', '45',
                      'BOX','IBP','PARKE','RUELA', 'ERIOP', 'MUHLE',
                      'NW','DESST','164','DESRI','HUERF','LIMST',
                      'NE')) |>
  select(STATION:DEC) |>
  pivot_longer(cols = JAN:DEC,
              names_to = "month",
              values_to = "precipitation") |>
  rename(
    station = STATION,
    year = YEAR
  ) |>
  mutate(month_id = match(month, names(month_mapping))) |>
  filter(year >= 1923)


#Write to a csv in data precipitation folder
write.csv(active, "data/precipitation/active_gauges_precip.csv", row.names = FALSE)


# MUTATE ESTIMATED DATA --------------------------------------------------
precip_data <- read_excel("data/precipitation/srer_estimated_monthly_rainfall.xlsx")

month_mapping <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

precip_data <- precip_data |>
  filter(STATION %in% c('GRARI', 'AMADO', 'PAST3','SW','41','ROAD',
                        'WHITE', 'RODEN', 'DESGR', 'FORES', '45',
                        'BOX','IBP','PARKE','RUELA', 'ERIOP', 'MUHLE',
                        'NW','DESST','164','DESRI','HUERF','LIMST',
                        'NE')) |>
  separate(year_month, c("year", "month_id")) |>
  filter(year >= 1923)|>
  mutate(month_id = as.numeric(month_id)) |>
  mutate(month = month.abb[month_id]) |>
  mutate(month = toupper(month)) |>
  select(-extracted_from_n_gauges)|>
  rename("station" = STATION,
         "precipitation" = monthly_rainfall)

merged_data <- rbind(active, precip_data)

merged_data <- merged_data |>
  filter(precipitation != -9999)

for_text_file <- precip_data |>
  select(station, year, month)|>
  filter(station %in% c('GRARI', 'AMADO', 'PAST3','SW','41','ROAD',
                        'WHITE', 'RODEN', 'DESGR', 'FORES', '45',
                        'BOX','IBP','PARKE','RUELA', 'ERIOP', 'MUHLE',
                        'NW','DESST','164','DESRI','HUERF','LIMST',
                        'NE'))

for_text_file <- for_text_file[order(for_text_file$station),]

write.csv(merged_data, "data/precipitation/estimated_precip.csv", row.names = FALSE)

write.csv(for_text_file, "data/precipitation/estimated_gauges", row.names = FALSE)




