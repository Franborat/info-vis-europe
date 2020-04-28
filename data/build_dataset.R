# Install packages if needed

if (!require(eurostat)) install.packages('eurostat')
library(eurostat)

if (!require(rmapshaper)) install.packages('rmapshaper')
library(rmapshaper)

if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

if (!require(sf)) install.packages('sf')
library(sf)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

library(heatmaply)
library(shiny)
library(factoextra)
library(leaflet)
library(spMaps)
library(shinyHeatmaply)
library(plotly)
library(stringi)
library(eurostat)


############## BUILD EUROSTAT DATASET FOR COUNTRIES

##########
gdp                  <- get_eurostat("tec00001", filters = list(unit='CP_MEUR'))
# select data for 2016 and calculate shares
gdp <-
  gdp %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, gdp = 'values') %>%
  drop_na()

##########
gdp_pps                  <- get_eurostat("tec00114", filters = list(unit='CP_MEUR'))
# select data for 2016 and calculate shares
gdp_pps <-
  gdp_pps %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, gdp_pps = 'values') %>%
  drop_na()

##########
population                  <- get_eurostat("tps00001", filters = list(unit='CP_MEUR'))
# select data for 2016 and calculate shares
population <-
  population %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, population = 'values') %>%
  drop_na()

##########
fertility                  <- get_eurostat("tps00199", filters = list(unit='CP_MEUR'))
# select data for 2016 and calculate shares
fertility <-
  fertility %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, fertility = 'values') %>%
  drop_na()

##########
life_expectancy                  <- get_eurostat("tps00205", filters = list(sex='T'))
# select data for 2016 and calculate shares
life_expectancy <-
  life_expectancy %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, life_expectancy = 'values') %>%
  drop_na()

##########
tertiary_education                  <- get_eurostat("sdg_04_20", filters = list(sex='T'))
# select data for 2016 and calculate shares
tertiary_education <-
  tertiary_education %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, tertiary_education = 'values') %>%
  drop_na()

##########
### High- and medium-high technology manufacturing sectors (code C_HTC_MH)
high_tech_employment                  <- get_eurostat("tsc00011", filters = list(nace_r2='C_HTC_MH'))
# select data for 2016 and calculate shares
high_tech_employment <-
  high_tech_employment %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, high_tech_employment = 'values') %>%
  drop_na()

##########
unemployment                  <- get_eurostat("tps00203", filters = list(sex='T', unit='PC_ACT'))
# select data for 2016 and calculate shares
unemployment <-
  unemployment %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, unemployment = 'values') %>%
  drop_na()

##########
cows_milk                  <- get_eurostat("tag00037")
# select data for 2016 and calculate shares
cows_milk <-
  cows_milk %>%
  mutate(year = lubridate::year(time),
         id = as.character(geo)) %>%
  # year 2016, total population
  filter(year == 2016,
         str_length(geo) == 2)%>%
  select(id, cows_milk = 'values') %>%
  drop_na()

indicators_countries <-
  gdp %>%
  left_join(gdp_pps, 'id') %>%
  left_join(population, 'id') %>%
  left_join(fertility, 'id') %>%
  left_join(life_expectancy, 'id') %>%
  left_join(tertiary_education, 'id') %>%
  left_join(high_tech_employment, 'id') %>%
  left_join(unemployment, 'id') %>%
  left_join(cows_milk, 'id')


# Take out UA row
indicators_countries = indicators_countries[-c(11),]
# Take out ID column
indicators_countries = indicators_countries[, -c(1)]

indicators_countries$code = c("ALB", "AUT", "BIH", "BEL", "BGR", "CHE", "CYP", "CZE", "DEU", "DNK", "EST", "GRC","ESP", "FIN", "FRA", "HRV", "HUN", "IRL", "ISL", "ITA", "LIE", "LTU", "LUX", "LVA", "MNE", "MKD", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SWE", "SVN", "SVK", "TUR", "GBR", "KOS")

mapa <- getSpMaps(countries = c("ALB", "AUT", "BIH", "BEL", "BGR", "CHE", "CYP", "CZE", "DEU", "DNK", "EST", "GRC","ESP", "FIN", "FRA", "HRV", "HUN", "IRL", "ISL", "ITA", "LIE", "LTU", "LUX", "LVA", "MNE", "MKD", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SWE", "SVN", "SVK", "TUR", "GBR", "KOS") )

mapa@data <- merge( mapa@data, indicators_countries, by="code")


# Save an object to a file
saveRDS(mapa, file = "data/europeSp.rds")
