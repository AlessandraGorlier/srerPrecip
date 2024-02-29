#Bailie Wynbelt
#wynbeltb@gmail.com
#The purpose of this script is to run one a month and output a csv with up-to-
#date precipitation data from active rain guages on the Santa Rita Experimental
#Range. This script also will estimate missing values (-9999) based on location
#and modeled values based on month.
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

#get current dates
current_year <- format(Sys.Date(), "%Y")
current_month <- format(Sys.Date(), "%m")

#check if it is january, if it is go back a year
if (current_month == 1) {
  current_year <- current_year - 1
}

#decrease month until it matches
while (current_month != as.numeric(format(current_date, "%m"))) {
  current_month <- current_month - 1
  if (current_month == 0) { #handle a zero value with decreasing in for loop
    current_month <- 12
    current_year <- current_year - 1
  }
}


#srerPrecip_Year_Month.xlsx (e.g., srerPrecip_2022_02.xlsx)
url <- "https://cales.arizona.edu/srer/file/srerPrecip_%Y_%m.xlsx"

current_path <- sprintf(url, current_year, current_month)

precip_data <- read_excel("data/precipitation/precipitaion.xlsx")

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

#There is 606 occurrences of -9999 (missing data)
#These need to be estimated


#Write to a csv in data precipitation folder
write.csv(active, "data/precipitation/active_gauges_precip.csv", row.names = FALSE)


# estimated rain ----------------------------------------------------------
precip_data <- read_excel("data/precipitation/srer_estimated_monthly_rainfall.xlsx")

#select stations
#split year month column

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

for_text_file<- for_text_file[order(for_text_file$station),]

write.csv(merged_data, "data/precipitation/estimated_precip.csv", row.names = FALSE)

write.csv(for_text_file, "data/precipitation/estimated_gauges", row.names = FALSE)




