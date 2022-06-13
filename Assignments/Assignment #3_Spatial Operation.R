####################################################
#' CMP 6455 - Advanced GIS
#' Assignment 3 - Spatial Operations in R
#' 
#' Date: 02/23/2022
#' Name: Pukar Bhandari
#' 
####################################################

# -------------------------------------
# 1. Load packages
# -------------------------------------

install.packages("tidyverse")
install.packages("sf")
install.packages("tmap")

require(tidyverse)
require(sf)
require(tmap)


# -------------------------------------
# 2. Read data
# -------------------------------------

# Set your main data folder

# For MacOS
# path = "/Users/andyhong/Documents/CMP6455/Assignments/Assignment_3/Data"

# For Windows
path = "D:\\MCMP\\Semester II\\CMP 6455 Advanced GIS Application\\Assignments\\Assignment #3\\Data"


# Download files from these locations and unzip them to your main data folder.
# To avoid confusion, don't change the original folder and files names. Use them as is.
#
# Utah County boundaries: https://opendata.arcgis.com/datasets/90431cac2f9f49f4bcf1505419583753_0.zip
# Utah crash data: https://opendata.utah.gov/api/views/7ihm-46s4/rows.csv?accessType=DOWNLOAD
# TIGER census boundaries: https://www2.census.gov/geo/tiger/TIGER2018/TRACT/tl_2018_49_tract.zip
# ACS 2018 data: https://data.census.gov/cedsci/table?g=0500000US49035%241400000&tid=ACSDT5Y2018.B01001&moe=false

# tidycensus approach (more advanced students)
# If you want to work with tidycensus, you can simply download the tract shapefile and census data all at once. This requires only a few lines of code as shown below.

# slco_acs = get_acs(
#   year = 2018, survey = "acs5", geography = "tract", state = "UT", county = "Salt Lake", 
#   variables = "B03002_001", output = "wide", geometry = TRUE
# )


# Read Utah county boundaries shapefile
# If the data load fails, alternatively you can use: utah_counties_sf = st_read(file.choose())
utah_counties_sf = st_read(file.path(path, "Utah_County_Boundaries", "Counties.shp"))

# Read Utah Census Tracts shapefile
# If the data load fails, alternatively you can use: utah_tracts_sf = st_read(file.choose())
utah_tracts_sf = st_read(file.path(path, "tl_2018_49_tract", "tl_2018_49_tract.shp"))

# Read Utah crash csv file
# If the data load fails, alternatively you can use: utah_crashes = st_read(file.choose())
utah_crashes = read_csv(file.path(path, "State_of_Utah_Crash_Data_2015-2019.csv"))

# Read ACS 2018 5-yr data for SLCo (B01001: population by age)
# Replace the <Folder Name> with the name of the folder that appears when you unzip the file.
# If the data load fails, alternatively you can use: slco_acs = st_read(file.choose())
slco_acs = read.csv(file.path(path, "ACSDT5Y2018.B01001", 
                              "ACSDT5Y2018.B01001_data_with_overlays_2022-02-22T173024.csv"))



# -------------------------------------
# 3. Processing crash and ACS data
# -------------------------------------

# Crash data processing
# Convert the crash data to a spatial data
# Hint: st_as_sf() function takes two parameters
#  1) coords: is where you identify longitude and latitude columns.
#     You may use coords = c("GCS_Long", "GCS_Lat")
#  2) crs: is where you set the original CRS.
#     You may use crs = 4326

utah_crashes_sf = utah_crashes %>%
  filter(!is.na(GCS_Long) & !is.na(GCS_Lat)) %>% # Remove NA values
  st_as_sf(coords = c("GCS_Long", "GCS_Lat"), crs = 4326 ) # Transform lat/lon to geometry

# Plot health care facility data
utah_crashes_sf %>% st_geometry %>% plot

# Examine the GEO_ID column
slco_acs$GEO_ID

# ACS data processing
# Select only the total population column (B01001_001E: total population)
# Hint: to extract FIPS id (GEO_ID), use function substr(x, start, stop). 
# For example, substr("1400000US49035113803", 10, 21) will extract strings "49035113803"

slco_acs_pop = slco_acs %>% 
  slice(-1) %>% # The first row is a header, so remove it
  mutate(
    GEOID = substr( GEO_ID , 10, 21), # Extract 11-digit FIPS code (state + county + tract FIPS) 
    total_pop = as.numeric(B01001_001E)  # Convert B01001_001E to total population (numeric)
  ) %>% 
  select(GEOID, total_pop) 


# Examine the tract FIPS GEOID
slco_acs_pop$GEOID

# ---------------------------------------------------
# 4. Filter to select data only for SLCo county
#    And transform CRS to Utah central: 3566
# ---------------------------------------------------

# epsg: 4326 - WGS 84
# epsg: 3566 - NAD 83 Utah Central

# Examine the NAME column
utah_counties_sf$NAME

slco_boundary = 
  utah_counties_sf %>% 
  filter(NAME == "SALT LAKE") %>% # Filter for SLCo
  st_transform(3566) # Transform the CRS to 3566

# Examine the COUNTYFP column
utah_tracts_sf$COUNTYFP

slco_tracts = 
  utah_tracts_sf %>% 
  filter(COUNTYFP == "035" ) %>% # Hint: use COUNTYFP column and code 035
  st_transform(3566)

# Examine the COUNTY_NAM column
utah_crashes_sf$COUNTY_NAM

slco_crashes = 
  utah_crashes_sf %>% 
  filter(COUNTY_NAM == "SALT LAKE" ) %>% # Hint: use COUNTY_NAM column and SALT LAKE
  st_transform(3566)

# Check data
slco_boundary %>% st_geometry %>% plot
slco_tracts %>% st_geometry %>% plot
slco_crashes %>% st_geometry %>% plot # Still there are erroneous data




# -----------------------------------------
# 5. Clean SLCo crash data
# -----------------------------------------

# Spatial filter crash data
# Only select crash counts contained by SLCo boundary
# Hint: use function: A %>% st_filter(B, .predicate = st_intersects) 

slco_crashes_filtered = slco_crashes %>% 
  st_filter(slco_boundary, .predicate = st_intersects)

# Check
slco_crashes_filtered %>% st_geometry %>% plot

# Extract year variable and remove 2019 data
# Hint: to extract year from CRASH_DATE, use function substr(x, start, stop). 
# For example, substr("123456789", 4, 7) will extract strings "4567"

# Examine the CRASH_DATE column
slco_crashes_filtered$CRASH_DATE

slco_crashes_cleaned = slco_crashes_filtered %>%
  mutate(
    year = substr(CRASH_DATE, 7, 10)
  ) %>%
  filter(year != "2019") # Remove year 2019

# Filter to pedestrian crashes and save it as an object
slco_ped_crashes = slco_crashes_cleaned %>%
  filter(PEDESTRIAN == TRUE)

# Check
slco_crashes_cleaned %>% glimpse
slco_crashes_cleaned %>% ggplot(aes(x=year)) + geom_bar()

slco_ped_crashes %>% glimpse
slco_ped_crashes %>% ggplot(aes(x=year)) + geom_bar()



# -----------------------------------------------------
# 6. Spatial join (summary) SLCo_tracts and SLCo crash  
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

# Examine the pedestrian crash column
slco_crashes_cleaned$PEDESTRIAN

slco_tracts_join = 
  st_join(slco_tracts, slco_crashes, left = F) %>%
  group_by(GEOID) %>% # group by unique tract ID
  summarise(
    total_crashes = n(),
    ped_crashes = sum(PEDESTRIAN, na.rm = T)
  ) 

# Check
slco_tracts_join$total_crashes
slco_tracts_join$ped_crashes



# Left join the previous joined result (slco_tracts_join) 
# with the ACS population data (slco_acs_pop)
#     Hint: Use function: left_join(A, B, by = "GEOID")
#
# And calculate two new variables
#  1. total_crash_rate_1k = total crash events per 1000 people
#     Hint: total_crashes/total_pop*1000
#  2. ped_crash_rate_1k = pedestrian crash events per 1000 people
#     Hint: ped_crashes/total_pop*1000

slco_tracts_acs_join = 
  left_join(slco_tracts_join, slco_acs_pop, by = "GEOID") %>%
  mutate(
    total_crash_rate_1k = total_crashes/total_pop*1000,
    ped_crash_rate_1k = ped_crashes/total_pop*1000
  ) 

# Check
slco_tracts_acs_join


# -----------------------------------------
# 7. Plot the results
# -----------------------------------------

# tmap is a very useful mapping package.
#
# In general, you will need to define your data by using tm_shape(data)
# And then you can add your shape depending on the type of your geometry
# For example, if you just want to map census tract boundaries, you can add tm_borders()
# If you want to map the actual data points, you can use tm_polygons()
# You can also stylize your map using tm_layout() function
#
# Popular geometry functions are: tm_dots(), tm_lines(), tm_borders()
# Popular data mapping functions are: tm_polygons(), tm_fill(), tm_bubbles()

# For more information, 
# see this tutorial: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html

# Population choropleth map
tm_shape(slco_tracts_acs_join) +
  tm_polygons("total_pop", palette="Oranges", style="jenks") 

# Pedestrian crash point map
tm_shape(slco_ped_crashes) + 
  tm_dots(alpha = 0.2) # You can use the alpha option to control opacity

# Pedestrian crash choropleth map
tm_shape(slco_tracts_acs_join) + 
  tm_polygons("ped_crashes", palette="Reds", style="jenks") 

# Pedestrian crash rate choropleth map
tm_shape(slco_tracts_acs_join) + 
  tm_polygons("ped_crash_rate_1k", palette="Blues", style="jenks") 


# -----------------------------------------
# 8. Map the final result
# -----------------------------------------

# Pedestrian crash rate choropleth map with the crash counts
tm_shape(slco_tracts_acs_join) + tm_polygons("ped_crash_rate_1k", palette="Blues", style="jenks") +
  tm_shape(slco_ped_crashes) + tm_dots(alpha = 1) +
  tm_layout(
    title = "Pedestrian Crash Rates in Salt Lake County (2015-2018)"
  )






