####################################################
#' CMP 6455 - Advanced GIS
#' Assignment 4 - Census Data in R
#' 
#' Date: 03/16/2022
#' Name: Pukar Bhandari
#' 
####################################################

# -------------------------------------
# 1. Load packages
# -------------------------------------

install.packages("tidyverse")
install.packages("tidycensus")
install.packages("sf")
install.packages("tmap")

require(tidyverse)
require(tidycensus)
require(sf)
require(tmap)

## Setting the API Key

# Register Census API Key
census_api_key("[CENSUS API]", install = TRUE)

# You need an API key to download data from the Census API. 
# You can sign up for a free key (https://api.census.gov/data/key_signup.html)

census_api_key("[CENSUS API]", install=TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")


# -------------------------------------
# 2. Read data
# -------------------------------------

# Set your main data folder

# For MacOS
# path = "/Users/andyhong/Documents/CMP6455/Assignments/Assignment_4/Data"

# For Windows
path = "D:/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Assignment #4/Backup"

# Get Salt Lake County Census Tracts data
slco_tracts = get_acs(
  year = 2018, 
  survey = "acs5", 
  geography = "tract", 
  state = "UT", 
  county = "Salt Lake",
  variables = "B03002_001", 
  output = "wide", 
  geometry = TRUE
) %>% st_transform(4326) # Re-project map to EPSG 4326


# Read Utah crash csv file and convert the x,y coordinates to spatial data
# Hint: st_as_sf() function takes two parameters
#  1) coords: is where you identify longitude and latitude columns.
#     You may use coords = c("GCS_Long", "GCS_Lat")
#  2) crs: is where you set the original CRS.
#     You may use crs = 4326

utah_crashes = 
  read_csv(file.path(path, "State_of_Utah_Crash_Data_2015-2019.csv")) %>%
  filter(!is.na(GCS_Long) & !is.na(GCS_Lat)) %>% # Remove NA values
  st_as_sf(coords = c("GCS_Long", "GCS_Lat"), crs = 4326 ) # Transform lat/lon to geometry

# Check CRS
slco_tracts %>% st_crs
utah_crashes %>% st_crs


# -----------------------------------------
# 3. Join Utah crash data and SLCo tracts
# -----------------------------------------

# Spatial filter crash data and filter out erronous 2019 data
# Only select crash counts contained by SLCo boundary
# Hint: use function: A %>% st_filter(B, .predicate = st_intersects) 

slco_total_crashes = utah_crashes %>% 
  st_filter( slco_tracts, .predicate = st_intersects ) %>%
  mutate(
    year = substr(CRASH_DATE, 7, 10) # Extract year from CRASH_DATE
  ) %>%
  filter(year != "2019") # Remove year 2019

# Filter to pedestrian crashes and save it as an object
slco_ped_crashes = slco_total_crashes %>%
  filter(PEDESTRIAN == TRUE)

# Check
slco_ped_crashes %>% glimpse
slco_ped_crashes %>% ggplot(aes(x=year)) + geom_bar()
slco_ped_crashes %>% st_geometry %>% plot


# -----------------------------------------------------
# 4. Spatial join (summary) SLCo_tracts and SLCo crashes  
# -----------------------------------------------------

# Spatial join (summary) between SLCo tracts and SLCo crashes
#     Hint: Use function: st_join(A, B, left = F)
#
# And use group by and summarize to create two new variables 
#  1. total_crashes = total number of crash events. 
#     Hint: use n() function
#  2. ped_crashes = number of pedestrian crash events. 
#     Hint: use sum(X, na.rm = T) function
#     Hint: If you sum all the TRUE values, 
#           it will calculate the total pedestrian crashes.

# Examine the pedestrian and bike crash columns
slco_total_crashes$PEDESTRIAN

# Join data
slco_tracts_join = 
  st_join(slco_tracts,  slco_total_crashes) %>%
  group_by(GEOID) %>% # group by unique tract ID
  summarise(
    total_crashes = n(),
    ped_crashes = sum( PEDESTRIAN, na.rm = T )
  ) 

# Check
slco_tracts_join$total_crashes
slco_tracts_join$ped_crashes


# -----------------------------------------------------
# 5. Retrieve and clean ACS data  
# -----------------------------------------------------

# Search for variables in Census 
v18 = load_variables(2018, "acs5", cache = TRUE)
v18 %>% View

# B08301_001
# Estimate!!Total
# MEANS OF TRANSPORTATION TO WORK
# 
# B08301_019
# Estimate!!Total!!Walked
# MEANS OF TRANSPORTATION TO WORK

# Get all the necessary ACS variables
slco_acs = get_acs(
  year = 2018, 
  survey = "acs5", 
  geography = "tract",
  state = "UT", 
  county = "Salt Lake", 
  variables = c(
    total_pop = "B01003_001",      # Total population
    commute_total = "B08301_001",  # Total commute
    walk = "B08301_019"            # Walking commute
  ), 
  output = "wide",
  geometry = FALSE 
) 

slco_acs %>% glimpse


# Calculate % walking commute
# Be careful with the variable names because there will be
# an E postfix and M postfix to indicate whether 
# it's an estimate or a margin of error.

slco_acs_cleaned = slco_acs %>% 
  mutate(
    total_pop = total_popE,           
    commute_total = commute_totalE,   # Total commute
    per_walk = walkE / commute_totalE * 100   # Percent walking commute
  ) %>%
  select(
    GEOID, total_pop, commute_total, per_walk
  )

slco_acs_cleaned %>% glimpse


# -------------------------------------------------
# 6. Join everything: SLCO tracts + crashes + ACS
# -------------------------------------------------

# Left join the previous joined result (slco_tracts_join) 
# and the ACS population data (slco_acs_cleaned)

# And calculate two new variables
#  1. total_crash_rate_1k = total crash events per 1000 people
#     Hint: total_crashes/total_pop*1000
#  2. ped_crash_rate_1k = pedestrian crash events per 1000 people
#     Hint: ped_crashes/total_pop*1000

slco_tracts_acs_join = 
  left_join(slco_tracts_join, slco_acs_cleaned, by = "GEOID") %>%
  mutate(
    total_crash_rate_1k = total_crashes/total_pop*1000,
    ped_crash_rate_1k = ped_crashes/total_pop*1000
  ) 

# Check
slco_tracts_acs_join %>% glimpse


# -----------------------------------------
# 7. Plot the results
# -----------------------------------------

# Install and load additional packages for plotting
install.packages("ggplot2") # Plotting package
install.packages("ggpubr")  # Correlation coefficient package
install.packages("cowplot") # Plot combining package

require(ggplot2) 
require(ggpubr)  
require(cowplot)
require(tmap)


# Pedestrian crash rate map
w1 = tm_shape(slco_tracts_acs_join) + 
  tm_polygons(
    col = "ped_crash_rate_1k", 
    title = "Crashes per 1000",
    palette = "Oranges", 
    style = "jenks"
  ) +
  tm_shape(slco_ped_crashes) + tm_dots(alpha = 0.2) +
  tm_compass(position = c("left", "bottom")) +
  tm_layout(
    title = "Pedestrian Crash Rate",
    title.size = 0.9,
    legend.title.size = 0.85,
    legend.position = c("right", "bottom"),
    legend.frame = TRUE,
    legend.width = -0.3
  ) 

# % Walking commute map
w2 = tm_shape(slco_tracts_acs_join) + 
  tm_polygons(
    col = "per_walk", 
    title = "% Walking Commute",
    palette = "Greens", 
    style = "jenks"
  ) +
  tm_compass(position = c("left", "bottom")) +
  tm_layout(
    title = "% Walking Commute",
    title.size = 0.9,
    legend.title.size = 0.85,
    legend.position = c("right", "bottom"),
    legend.frame = TRUE,
    legend.width = -0.35
  ) 

# Check the maps
w1
w2

# Turn off scientific notation
options(scipen=999)

# Plot walk commute and pedestrian crashes
# Also add Pearson correlation coefficient and p-value
# using stat_cor() function from the ggpubr package

walk_crash_plot = slco_tracts_acs_join %>% 
  ggplot(aes(x = per_walk, y = ped_crash_rate_1k)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Walking Commute", 
    y = "Pedestrian Crash Rate (crashes/1000)"
  )

# Convert tmap objects to ggplot object
p1 = tmap_grob(w1)
p2 = tmap_grob(w2)
maps = plot_grid(p1, p2, labels = c("A","B"), nrow = 2)

# Combine all the maps and chart
plot_grid(
  maps, walk_crash_plot, 
  labels = c("","C"), nrow = 1, rel_widths = c(2, 3)
)


## Additional investigation

# Turn on the interactive map mode
tmap_mode(mode = "view")

# Show two maps side by side and sync them
tmap_arrange(w1, w2, sync = TRUE)

summary(walk_crash_plot)





