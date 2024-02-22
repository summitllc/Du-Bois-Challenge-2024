## WEB Du Bois Challenge 1
## Summit Consulting
## 2024-02-21

# Load Packages
library(sf) # Spatial figures? package is most popular in R for mapping/spatial data
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggforce)
library(grid)
library(extrafont)

#I think you'll need this code if you haven't updated your fonts, but not positive if we all need it
# font_import()
# loadfonts(device = "win")
# font_import("Bahnschrift Light")

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
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 AND 30,000'))) %>%
  mutate(data_1880 = factor(`data1880_P`, 
                            levels = c('> 1000', '1000 - 2500', '2500 - 5000', '5000 - 10000', '10000 - 15000', '15000 - 20000', '20000 - 30000'),
                            labels = c('UNDER 1,000', '1,000 TO 2,500', '2,500 TO 5,000', '5,000 TO 10,000', '10,000 TO 15,000', '15,000 TO 20,000', 'BETWEEN 20,000 AND 30,000'))) 

# Straighten Maps
ga_clean <- st_transform(ga_clean, "+proj=longlat +ellps=WGS84 +datum=WGS84")

#Add color palette
myColors <- c("#536254", "#edb457", "#e89a96", "#dd3454", "#bf9d82", "#78533b", "#352a60", "#e2cebb")

# Map 1870 Data (without legend)
map_1870 <- ga_clean %>% 
  filter(!is.na(data_1870)) %>% 
  ggplot() +
  geom_sf(aes(fill = data_1870), color = "black") +
  scale_fill_manual(values = myColors) +
  ggtitle("1870") +
  # theme(panel.background = element_blank(),
  #       axis.text = element_blank(),
  #       axis.ticks = element_blank(),
  #       legend.position = "none")
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.24, 
                                  vjust = -0.9, 
                                  family = "Bahnschrift",
                                  size = 8))

# Map 1870 Data (with legend)
# ga_clean %>% 
#   filter(!is.na(data_1870)) %>% 
#   ggplot() +
#   geom_sf(aes(fill = data_1870, color = data_1870), size = 16, show.legend = "point") +
#   scale_fill_manual(values = myColors) +
#   scale_color_manual(values = myColors) +
#   labs(fill = NULL, color = NULL, title = "1870") +
#   geom_sf(color = 'black', fill = NA) +
#   theme(panel.background = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())
#   geom_sf(aes(fill = data_1870, color = data_1870), size = 16, show.legend = "point") +
#   geom_sf(color = 'black', fill = NA) +
#   labs(color = NULL, fill = NULL) +
#   theme(legend.text = element_text(size=16))
  # scale_color_manual(
  #   values = c("#9b5fe0", "#16a4d8", "#60dbe8", "#8bd346", '#efdf48', '#f9a52c', '#d64e12'
  #   ),
  #   breaks = c('20000 - 30000', '15000 - 20000','10000 - 15000')
  # )
  # scale_shape_manual(values = c(1,1,1,1,1,1,1,1)) +
  # theme(legend.key.size = unit(1, 'cm'), #change legend key size
  #       legend.key.height = unit(1, 'cm'), #change legend key height
  #       legend.key.width = unit(1, 'cm'), #change legend key width
  #       legend.title = element_text(size=14), #change legend title font size
  #       legend.text = element_text(size=10))

map_1880 <- ga_clean %>% 
  filter(!is.na(data_1880)) %>% 
  ggplot() +
  geom_sf(aes(fill = data_1880), color = "black") +
  scale_fill_manual(values = myColors) +
  ggtitle("1880") +
  # theme(panel.background = element_blank(),
  #       axis.text = element_blank(),
  #       axis.ticks = element_blank(),
  #       legend.position = "none") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.24, 
                                  vjust = -0.9, 
                                  family = "Bahnschrift",
                                  size=8))


## Try at manual legend?
# ggplot() +
#   geom_point() +
#   geom_circle(aes(x0=3, y0=9, r=1), inherit.aes=FALSE) +
#   geom_circle(aes(x0=3, y0=6, r=1), inherit.aes=FALSE) +
#   geom_circle(aes(x0=3, y0=3, r=1), inherit.aes=FALSE) +
#   coord_fixed()


#Viewports
vp <- viewport(x=0.5, y=0.5, width=.7, height=1)
pushViewport(vp)
grid.rect(gp=gpar(fill="#e5d3c1"))
grid.text("NEGRO POPULATION OF GEORGIA BY COUNTIES.", 
          x=.5, y=.97, 
          gp=gpar(fontsize=8, fontfamily="Bahnschrift"))

vp1 <- viewport(x=0.3, y=0.72, width=0.7, height=0.52)
pushViewport(vp1)
grid.rect(gp=gpar(lty="", fill="#e5d3c1"))
print(map_1870, newpage=FALSE)
upViewport()

vp4 <- viewport(x=0.68, y=0.25, width=0.7, height=0.52)
pushViewport(vp4)
grid.rect(gp=gpar(lty="", fill="#e5d3c1"))
print(map_1880, newpage=FALSE)
upViewport()

vp2 <- viewport(x=0.8, y=0.75, width=0.5, height=0.5)
pushViewport(vp2)
grid.rect(gp=gpar(lty="", fill="#e5d3c1"))
grid.circle(x=-0.05, y=0.7, r=0.04, gp=gpar(fill="#352a60"))
grid.text("BETWEEN 20,000 AND 30,000", 
          x=.4, y=.7, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
grid.circle(x=-0.05, y=0.55, r=0.04, gp=gpar(fill="#78533b"))
grid.text("15,000 TO 20,000", 
          x=.26, y=.55, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
grid.circle(x=-0.05, y=0.4, r=0.04, gp=gpar(fill="#bf9d82"))
grid.text("10,000 TO 15,000", 
          x=.26, y=.4, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
upViewport()

vp3 <- viewport(x=0.25, y=0.25, width=0.5, height=0.5)
pushViewport(vp3)
grid.rect(gp=gpar(lty="", fill="#e5d3c1"))
grid.circle(x=0.25, y=0.3, r=0.04, gp=gpar(fill="#536254"))
grid.text("UNDER 1,000", 
          x=.52, y=.3, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
grid.circle(x=0.25, y=0.45, r=0.04, gp=gpar(fill="#edb457"))
grid.text("1,000 TO 2,500", 
          x=.53, y=.45, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
grid.circle(x=0.25, y=0.6, r=0.04, gp=gpar(fill="#e89a96"))
grid.text("2,500 TO 5,000", 
          x=.53, y=.6, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))
grid.circle(x=0.25, y=0.75, r=0.04, gp=gpar(fill="#dd3454"))
grid.text("5,000 TO 10,000", 
          x=.54, y=.75, 
          gp=gpar(fontsize=6, fontfamily="Bahnschrift Light"))




