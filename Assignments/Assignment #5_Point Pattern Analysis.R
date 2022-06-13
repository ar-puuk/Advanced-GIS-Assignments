####################################################
#' CMP 6455 - Advanced GIS
#' Assignment 5 - Point Pattern Analysis
#' 
#' Date: 04/05/2022
#' Name: Pukar Bhandari
#' 
####################################################

# ==================================================
# 1. Setting up the environment
# ==================================================

# -------------------------------------
# Load packages
# -------------------------------------

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("spatstat")
# install.packages("maptools")
# install.packages("ggplot2")

require(tidyverse)
require(sf)
require(spatstat)
require(maptools)
require(ggplot2)

# -------------------------------------
# Reading spatial data (.shp files)
# -------------------------------------

# File paths
# path = "/Users/andyhong/Documents/CMP6455/Assignments/Assignment_5/Data"  # Path for Mac
path = "D:/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Assignment #5/Data" # Path for Windows

# Loading shapefiles
county.sf = st_read(file.path(path, "SLCo_urban.shp"))
crashes.sf = st_read(file.path(path, "SLCo_crashes.shp"))


# ==================================================
# 2. Data wrangling
# ==================================================

# See data
crashes.sf %>% glimpse

# Check yearly crashes
crashes.sf %>% 
  st_drop_geometry() %>% 
  ggplot(aes(YEAR)) + geom_bar()
  
# Filter data for one year 
# And create separate point data for crashes 
# involving pedestrians and cyclists (e.g. PEDESTRIAN == 1)

selected_year = 2018 # Change this for different years
ped.sf = crashes.sf %>% filter(YEAR == selected_year & PEDESTRIAN == 1) # Pedestrian crashes
bike.sf = crashes.sf %>% filter(YEAR == selected_year & BICYCLIST == 1) # Bicycle crashes

# Check
ped.sf %>% st_geometry %>% plot
bike.sf %>% st_geometry %>% plot


# ==================================================
# 3. Setting up PPP objects for spatstat
# ==================================================

# Define window
# Conversion from sf to owin
my.window = county.sf %>% st_simplify %>% as.owin  # SLCo Boundary

# Define points
ped.points = ped.sf %>% st_coordinates
bike.points = bike.sf %>% st_coordinates

# Check
my.window %>% plot
ped.sf %>% st_geometry %>% plot(add=T)
bike.sf %>% st_geometry %>% plot(add=T)

# Create ppp objects
ped.ppp = as.ppp(X = ped.points, W = my.window)
bike.ppp = as.ppp(X = bike.points, W = my.window)

# Note that there are some duplicates.
# Let's jitter the data points a bit so there's no duplicate
ped.ppp = ped.ppp %>%
  rjitter(retry=TRUE, nsim=1, drop=TRUE)

bike.ppp = bike.ppp %>%
  rjitter(retry=TRUE, nsim=1, drop=TRUE)

# Rescale from feet to miles
ped.ppp.mi = rescale(ped.ppp, 5280, "mile") 
bike.ppp.mi = rescale(bike.ppp, 5280, "mile") 


# ==========================================================
# 4. Kernel density estimation for pedestrian crashes
# ==========================================================

# Different bandwidth selection algorithms
b1 = bw.diggle(ped.ppp.mi)
b2 = bw.ppl(ped.ppp.mi)
b3 = bw.scott(ped.ppp.mi)[1]
b4 = bw.scott(ped.ppp.mi)[2]

# Check
b1
b2
b3
b4

##### Using different bandwidths #######

# Set up the panel
# mfrow = c(rows, columns)
# mar = c(bottom, left, top, right) 
par(mfrow=c(2, 2), mar=c(1, 1, 1, 2))

# Using the Diggle method
ped.ppp.mi %>% density(sigma = b1) %>% 
  plot(main=paste("Radius =", round(b1, 2)))

# Using the ppl method
ped.ppp.mi %>% density(sigma = b2) %>% 
  plot(main=paste("Radius =", round(b2, 2)))

# Using the Scott 1 method
ped.ppp.mi %>% density(sigma = b3) %>% 
  plot(main=paste("Radius =", round(b3, 2)))

# Using the Scott 2 method
ped.ppp.mi %>% density(sigma = b4) %>% 
  plot(main=paste("Radius =", round(b4, 2)))

# Reset to default panel
dev.off()


# ==========================================================
# 5. Ripley's K function for pedestrian crashes
# ==========================================================
#
# Create a K object 
K_ped = ped.ppp.mi %>% Kest(correction = "Ripley")

# Plot the K function
plot(K_ped, main="Ripley's K function")


# ==========================================================
# 6. Compare pedestrian and bicycle crashes
# ==========================================================

# Kernel density estimations
#
# Hint: Use ppl method for the selection of bandwidth
# See the line 106 for reference
# Note that you cannot just use sigma = b2. You need to use 
# different ped and bike objects to calculate your sigma. 
# Type in ?bw.ppl in the console for help

d1 = ped.ppp.mi %>% density(sigma = bw.ppl(ped.ppp.mi))
d2 = bike.ppp.mi %>% density(sigma = bw.ppl(bike.ppp.mi))

# Ripley's K functions
#
# Hint: Use Kest function to compute Ripley's K.
# See the line 148 for reference
# Type in ?Kest in the console for help

k1 = ped.ppp.mi %>% Kest(correction = "Ripley") %>% plot(main="Ripley's K function")
k2 = bike.ppp.mi %>% Kest(correction = "Ripley") %>% plot(main="Ripley's K function")

# Running 10 simulations to obtain the 95% confidence interval.
e1 = ped.ppp.mi %>% envelope(fun = Kest, nsim = 10, rank = 1)
e2 = bike.ppp.mi %>% envelope(fun = Kest, nsim = 10, rank = 1)
  
# Plot the final comparison maps
# mfrow = c(rows, columns)
# mar = c(bottom, left, top, right) 
par(mfrow=c(2, 2), mar=c(3, 2, 3, 2))

d1 %>% plot(main="Clustering of Pedestrian Crashes, 2018")
e1 %>% plot(main="Degree of Clustering (K-function)")
d2 %>% plot(main="Clustering of Bicycle Crashes, 2018")
e2 %>% plot(main="Degree of Clustering (K-function)")

dev.off()



# =================================================================
# OPTIONAL: Alternative methods for creating Kernel density plots
# =================================================================

# As an alternative to the spatstat package, you can also use ggplot package
# to create beautiful heatmaps. Spatstat has many statistical tests, 
# such as K-function and L-function. However if you just need heatmaps, 
# ggplot is more versatile and can plot multiple data sets. Also, ggplot is very 
# customizable, so you can create journal-quality maps. If you like, you may 
# tweak this code for your term project.
#
# Obviously, this is an advanced material, but if you are interested in ggplot, 
# check out the stat_density_2d function in the ggplot package: 
# https://ggplot2.tidyverse.org/reference/geom_density_2d.html

install.packages("ggplot2")
install.packages("gridExtra")

require(ggplot2)
require(gridExtra)

# ggplot needs the x, y coordinates to map data
crashes.sf = crashes.sf %>% 
  filter(YEAR != 2019) %>%
  mutate(
    x = st_coordinates(.)[,1], 
    y = st_coordinates(.)[,2]
  ) 

# Heatmap of pedestrian crashes by year
g1 = ggplot() +
  stat_density_2d( # Mapping KDE
    data = crashes.sf %>% filter(PEDESTRIAN == 1),
    aes(x = x, y = y, fill = ..level..), 
    geom = "polygon", contour = TRUE, bins = 10) +
  scale_fill_distiller(palette = "Reds", direction = 1, name="Density\n(low to high)", label=NULL) +
  geom_sf(data = crashes.sf %>% filter(PEDESTRIAN == 1), size = 0.01, alpha=0.1) + # Mapping points
  geom_sf(data = county.sf, fill = NA) + # Mapping county boundary
  theme_void() +
  theme(legend.position = "bottom",  
        plot.title = element_text(vjust = 5), 
        strip.text.x = element_text(size = 11)) +
  ggtitle("(a) Crashes involving pedestrians (2015-2018)") +
  facet_wrap(~YEAR, nrow=1) # Plot maps by year

summary(g1)

# Heatmap of bicycle crashes by year  
g2 = ggplot() +
  stat_density_2d( # Mapping KDE
    data = crashes.sf %>% filter(BICYCLIST == 1),
    aes(x = x, y = y, fill = ..level..), 
    geom = "polygon", contour = TRUE, bins = 10) +
  scale_fill_distiller(palette = "Blues", direction = 1, name="Density\n(low to high)", label=NULL) +
  geom_sf(data = crashes.sf %>% filter(BICYCLIST == 1), size = 0.01, alpha=0.1) + # Mapping points
  geom_sf(data = county.sf, fill = NA) + # Mapping county boundary
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(vjust = 5), 
        strip.text.x = element_text(size = 11)) +
  ggtitle("(b) Crashes involving bicyclists (2015-2018)") +
  facet_wrap(~YEAR, nrow=1) # Plot maps by year

# Combine two maps
grid.arrange(g1, g2, nrow=2)




