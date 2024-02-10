#Bailie Wynbelt
#wynbeltb@gmail.com
#The purpose of this script is to run one a month and output a csv with up-to-
#date precipitation data from active rain guages on the Santa Rita Experimental
#Range. This script also will estimate missing values (-9999) based on location
#and modeled values based on month.
library(readxl)
library(dplyr)
library(tidyverse)


url <- 'https://cales.arizona.edu/srer/file/1249/download?token=pah6svx1'
download.file(url, "precipitaion.xlsx", mode = "wb")

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
  mutate(month_id = match(month, names(month_mapping)))

#There is 606 occurrences of -9999 (missing data)
#These need to be estimated


#Write to a csv in data precipitation folder
write.csv(active, "data/precipitation/active_gauges_precip.csv", row.names = FALSE)

