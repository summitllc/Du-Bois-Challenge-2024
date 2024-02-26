## WEB Du Bois Challenge 1
## Summit Consulting
## 2024-02-21

rm(list=ls())

# Load Packages
library(sf) # Spatial figures? package is most popular in R for mapping/spatial data
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggforce)
library(grid)
library(extrafont)
library(jpeg)
library(ggpubr)
library(berryFunctions)
library(tigris)

#I think you'll need this code if you haven't updated your fonts, but not positive if we all need it
# font_import()
# loadfonts(device = "win")
# font_import("Bahnschrift Light")

# Set working directory (need one per person?)
# wd = 'C:\\Du-Bois-Challenge-2024\\Data Input'
  
# Load data
# ga <- read_sf('C:\\Du-Bois-Challenge-2024\\Data Input\\DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp')
ga <- read_sf('../Data Input/DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp')

# Load background image
bkg_file <- '../Data Input/background_photo.jpg'

# drop geometry 
# This loads faster than data with the geometry (just to look at variables)
ga_no_geom <- ga %>% 
  st_drop_geometry()

# Clean the variables of Interest
## Set them as factors, cleaned the variable names, cleaned the labels to match DuBois
ga_clean <- ga %>% 
  mutate(data_1870 = factor(`data1870 (`, 
                            levels = c('> 1000', '1000 - 2500', '2500 - 5000', '5000 - 10000', '10000 - 15000', '15000 - 20000', '20000 - 30000'),
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 AND 30,000'))) %>%
  mutate(data_1880 = factor(`data1880_P`, 
                            levels = c('> 1000', '1000 - 2500', '2500 - 5000', '5000 - 10000', '10000 - 15000', '15000 - 20000', '20000 - 30000'),
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 AND 30,000'))) 

# Straighten Maps
ga_clean <- st_transform(ga_clean, "+proj=longlat +ellps=WGS84 +datum=WGS84")

#Add color palette
myColors <- c("#3e5748", 
              "#f2b438", 
              "#e89a96", 
              "#db163c", 
              "#bf9d82", 
              "#3e2518", 
              "#23214f", 
              "#e2cebb")

# Use census tract data to change areas alpha randomly (marker effect)
ga_tracts <- tracts(state = 'Georgia')

#set seed
set.seed(13)
#randomly assign numbers to use for transparency level
ga_tracts <- ga_tracts %>%
  mutate(alpha_level = runif(n=2796, min=1, max=10))


# Map 1870 Data (without legend)
map_1870 <- ga_clean %>% 
  filter(!is.na(data_1870)) %>% 
  ggplot() +
  geom_sf(aes(fill = data_1870), color = "gray10") +
  geom_sf(data = ga_tracts, 
          aes(geometry = geometry, alpha = alpha_level), 
          fill = "white", color = NA) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_fill_manual(values = myColors) +
  ggtitle("I870") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.24, 
                                  vjust = -3.5, 
                                  family = "Malgun Gothic",
                                  size = 11,
                                  face ='bold'))

# Map 1880 Data (without legend)
map_1880 <- ga_clean %>% 
  filter(!is.na(data_1880)) %>% 
  ggplot() +
  geom_sf(aes(fill = data_1880), color = "gray10") +
  geom_sf(data = ga_tracts, 
          aes(geometry = geometry, alpha = alpha_level), 
          fill = "white", color = NA) +
  scale_alpha_continuous(range = c(0, 0.15)) +
  scale_fill_manual(values = myColors) +
  ggtitle("I880") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.24, 
                                  vjust = -3.5, 
                                  family = "Malgun Gothic",
                                  size=11,
                                  face = 'bold'))


#Export png
png("r_challenge.png", height=10000, width=8000, units = "px", res=1200)

#For background
bkg <- jpeg::readJPEG(bkg_file)
grid.raster(bkg)

#Viewports
vp <- viewport(x=0.5, y=0.5, width=.7, height=1)
pushViewport(vp)
grid.text("NEGRO POPULATION OF GEORGIA BY COUNTIES.",
          x=.5, y=.95,
          gp=gpar(fontsize=15, 
                  fontfamily="Bahnschrift", 
                  fontface = "bold", 
                  col="gray10"))


#1870 map
vp1 <- viewport(x=0.18, y=0.70, width=0.7, height=0.52)
pushViewport(vp1)
print(map_1870, newpage=FALSE)
upViewport()

#1880 map
vp4 <- viewport(x=0.81, y=0.24, width=0.7, height=0.52)
pushViewport(vp4)
print(map_1880, newpage=FALSE)
upViewport()

#top right legend
vp2 <- viewport(x=0.8, y=0.75, width=0.5, height=0.5)
pushViewport(vp2)
grid.circle(x=-0.04, y=0.68, r=0.057, gp=gpar(fill="#23214f", col="gray40", alpha=1))
grid.text("BETWEEN 20,000 AND 30,000", 
          x=.51, y=.68, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=-0.04, y=0.54, r=0.057, gp=gpar(fill="#3e2518", col="gray40", alpha=0.9))
grid.text("15,000 TO 20,000", 
          x=.34, y=.54, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=-0.04, y=0.4, r=0.057, gp=gpar(fill="#bf9d82", col="gray40", alpha=0.9))
grid.text("10,000 TO 15,000", 
          x=.34, y=.4, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
upViewport()

#bottom left legend
vp3 <- viewport(x=0.2, y=0.25, width=0.5, height=0.5)
pushViewport(vp3)
grid.circle(x=0.05, y=0.3, r=0.057, gp=gpar(fill="#3e5748", col="gray40", alpha=0.9)) 
grid.text("UNDER 1,000", 
          x=.38, y=.3, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.44, r=0.057, gp=gpar(fill="#f2b438", col="gray40", alpha=0.9))
grid.text("1,000 TO 2,500", 
          x=.4, y=.44, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.58, r=0.057, gp=gpar(fill="#e89a96", col="gray40", alpha=0.9))
grid.text("2,500 TO 5,000", 
          x=.4, y=.58, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.72, r=0.057, gp=gpar(fill="#db163c", col="gray40", alpha=0.9))
grid.text("5,000 TO 10,000", 
          x=.41, y=.72, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
upViewport()



dev.off()

