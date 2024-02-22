## WEB Du Bois Challenge 1
## Summit Consulting
## 2024-02-21

# Load Packages
library(sf) # Spatial figures? package is most popular in R for mapping/spatial data
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Set working directory (need one per person?)
wd = '../Data Input/'

# Load data
ga <- read_sf(paste0(wd, 'DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp'))

# drop geometry 
# This loads faster than data with the geometry (just to look at variables)
ga_no_geom <- ga %>% 
  st_drop_geometry()

# Clean the variables of Interest
## Set them as factors, cleaned the variable names, cleaned the labels to match DuBois
ga_clean <- ga %>% 
  mutate(data_1870 = factor(`data1870 (`, 
                            levels = c('> 1000', '1000 - 2500', '2500 - 5000', '5000 - 10000', '10000 - 15000', '15000 - 20000', '20000 - 30000'),
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 TO 30,000'))) %>%
  mutate(data_1880 = factor(`data1880_P`, 
                            levels = c('> 1000', '1000 - 2500', '2500 - 5000', '5000 - 10000', '10000 - 15000', '15000 - 20000', '20000 - 30000'),
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 TO 30,000'))) 

# Straighten Maps
ga_clean <- st_transform(ga_clean, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# Map 1870 Data
ga_clean %>% 
  ggplot() +
  geom_sf(aes(fill = data_1870))

# Map 1880 Data
ga_clean %>% 
  ggplot() +
  geom_sf(aes(fill = data_1880))
