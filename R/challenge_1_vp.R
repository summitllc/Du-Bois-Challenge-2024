## WEB Du Bois Challenge 1
## Summit Consulting
## 2024-02-21

rm(list=ls())

# Load Packages
library(sf)
library(tigris)
library(tidyverse)
library(ggplot2)
library(grid)
library(extrafont)


# Load data
ga <- read_sf('../Data Input/DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp')

# Load background image
bkg_file <- '../Data Input/background_photo.jpg'

# Clean the variables of interest
## Set them as factors, clean variable names, clean the labels to match Du Bois
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
              "#e5a59e", 
              "#d62b3f",
              "#d0b49c",
              "#3e2518",
              "#201e56",
              "#e2cebb")

myLabels <- c("UNDER 1,000",
              "1,000 TO 2,500",
              "2,500 TO 5,000",
              "5,000 TO 10,000",
              "10,000 TO 15,000",
              "15,000 TO 20,000",
              "BETWEEN 20,000 AND 30,000",
              "NA")

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
png("../Data Output/r_challenge_vp1.png", height=10000, width=8000, units = "px", res=1200)

#For background
bkg <- jpeg::readJPEG(bkg_file)
grid.raster(bkg)

#Viewports
vp <- viewport(x=0.5, y=0.5, width=.7, height=1)
pushViewport(vp)
grid.rect()
grid.text("NEGRO POPULATION OF GEORGIA BY COUNTIES .",
          x=.5, y=.95,
          gp=gpar(fontsize=15, 
                  fontfamily="Bahnschrift", 
                  fontface = "bold", 
                  col="gray10"))


#1870 map
vp1 <- viewport(x=0.18, y=0.70, width=0.7, height=0.52)
pushViewport(vp1)
grid.rect()
print(map_1870, newpage=FALSE)
upViewport()

#1880 map
vp4 <- viewport(x=0.81, y=0.24, width=0.7, height=0.52)
pushViewport(vp4)
grid.rect()
print(map_1880, newpage=FALSE)
upViewport()

#top right legend
vp2 <- viewport(x=0.7, y=0.7, width=0.5, height=0.52)
pushViewport(vp2)
grid.rect()
grid.circle(x=0.1, y=0.78, r=0.057, gp=gpar(fill=myColors[7], col="gray40", alpha=1))
grid.text(myLabels[7], 
          x=.65, y=.78, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.1, y=0.64, r=0.057, gp=gpar(fill=myColors[6], col="gray40", alpha=0.9))
grid.text(myLabels[6], 
          x=.48, y=.64, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.1, y=0.5, r=0.057, gp=gpar(fill=myColors[5], col="gray40", alpha=0.9))
grid.text(myLabels[5], 
          x=.48, y=.5, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
upViewport()

#bottom left legend
vp3 <- viewport(x=0.2, y=0.25, width=0.5, height=0.5)
pushViewport(vp3)
grid.rect()
grid.circle(x=0.05, y=0.3, r=0.057, gp=gpar(fill=myColors[1], col="gray40", alpha=0.9)) 
grid.text(myLabels[1], 
          x=.38, y=.3, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.44, r=0.057, gp=gpar(fill=myColors[2], col="gray40", alpha=0.9))
grid.text(myLabels[2], 
          x=.4, y=.44, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.58, r=0.057, gp=gpar(fill=myColors[3], col="gray40", alpha=0.9))
grid.text(myLabels[3], 
          x=.4, y=.58, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
grid.circle(x=0.05, y=0.72, r=0.057, gp=gpar(fill=myColors[4], col="gray40", alpha=0.9))
grid.text(myLabels[4], 
          x=.41, y=.72, 
          gp=gpar(fontsize=10, 
                  fontfamily="Malgun Gothic Semilight", 
                  col="gray40"))
upViewport()



dev.off()

