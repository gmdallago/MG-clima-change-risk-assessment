# Script to prepare INMET data to be used for bias adjustment of 
# Global Climatic Models from IPCC
# December 30th, 2021


# Climatic data (Temperaturate, humidity, wind speed, and insolation) was obtained 
# from the historical data base of the INMET website. In the past,
# access to the historical data files required a login and information could be downloaded 
# from each station of interest. The system has improved, and it only requires a request and 
# data from all conventional stations are extracted per station in a csv format.

# Data from solar radiation was only available from automatic and hourly. Those stations
# only started operating recently, so they have less data. In addition, a methodogy must
# be used to convert dayly data to a monthly frequency.

# Data was downloaded from MG, SP, RJ, and ES conventional stations on a monthly frequency
# MG, SP, RJ, and ES were all selected because it was easier that way.

# Official map of Minas Gerais was Obtained from the IBGE website (Malah Municipal, 2020)

# Altitude data was obtained from the USGS becasue it was much easier than finding that info
# on IBGE website. The data was obtained on the Shuttle Radar Topography Mission 
# with 30 meters resolution acquired in 2000.


# Loading packages --------------------------------------------------------
require(raster)
require(sf)
require(tidyverse)
require(tidylog)
require(GpGp)



# Reading the data --------------------------------------------------------


## Minas Gerais Map ####
# Getting coordinates boundaries from Minas Gerais based on IBGE (2020) shape file
mg.map <- st_read("./Data/Shape Files/MG_UF_2020/MG_UF_2020.shp")

ggplot() + 
  geom_sf(data = mg.map)


crs.mg.map <- st_crs(mg.map)

## Altitude ####

# Data from MG is expread into 99 GeoTIFF files (~ 25MG each). Reading and merging 25 at the time in hopes to make the 
# process a bit faster 


raster_list <- list.files(path = "./Data/USGS/Bulk Order MG Altitude Data/SRTM 1 Arc-Second Global/", 
                          pattern="tif$", all.files = TRUE,
                          full.names = TRUE)

for(i in 1:25){
  
  # get file name
  file_name <- raster_list[i]
  
  # read raster in
  mg_i <- raster(file_name)
  
  
  if(i == 1){
    
    combined_mg1_25 <- mg_i
    
  } else {
    
    # merge rasters and calc overlap
    combined_mg1_25 <- merge(combined_mg1_25, mg_i)
  }
  
  print(paste(i, " file finished. Only ", 25-i, " missing :)",
              sep = ""))
}




for(i in 26:50){
  
  # get file name
  file_name <- raster_list[i]
  
  # read raster in
  mg_rast_i <- raster(file_name)
  
  if(i == 26){
    
    combined_raster26_50 <- mg_rast_i
    
  } else {
    
    # merge rasters and calc overlap
    combined_raster26_50 <- merge(combined_raster26_50, mg_rast_i)
  }
  
  print(paste(i, " file finished. Only ", 50-i, " missing :)",
              sep = ""))
}



for(i in 51:75){
  
  # get file name
  file_name <- raster_list[i]
  
  # read raster in
  mg_rast_i <- raster(file_name)
  
  if(i == 51){
    
    combined_raster51_75 <- mg_rast_i
    
  } else {
    
    # merge rasters and calc overlap
    combined_raster51_75 <- merge(combined_raster51_75, mg_rast_i)
  }
  
  print(paste(i, " file finished. Only ", 75-i, " missing :)",
              sep = ""))
}


for(i in 76:99){
  
  # get file name
  file_name <- raster_list[i]
  
  # read raster in
  mg_rast_i <- raster(file_name)
  
  if(i == 76){
    
    combined_raster76_99 <- mg_rast_i
    
  } else {
    
    # merge rasters and calc overlap
    combined_raster76_99 <- merge(combined_raster76_99, mg_rast_i)
  }
  
  print(paste(i, " file finished. Only ", 99-i, " missing :)",
              sep = ""))
}

# Final merge of all GeoTiff files

mg.raster.altitude <- merge(merge(merge(combined_mg1_25, combined_raster26_50),
                                  combined_raster51_75), combined_raster76_99)

plot(mg.raster.altitude)


# Removing intermediary raster files
rm(list = c("mg_rast_i", "combined_mg1_25", "combined_raster26_50", 
            "combined_raster51_75", "combined_raster76_99"))


# Saving out a file with the combined raster files so I can read into R using a different function
writeRaster(x = mg.raster.altitude,
            filename = "./Data/USGS/mg_altitude_merged.nc",
            format = "CDF",
            overwrite=TRUE)


# # The resolution of the raster file is way to big!
# # Aggregating it to a 0.10º to 0.10º resolution (factor = 360)
# # Also, changing the projection to match the MG shape file
# res(mg.raster.altitude)
# 
# 
# mg.raster.aggreg <- mg.raster.altitude %>% 
#   aggregate(fact = 0.10/res(.)[1]) %>% 
#   projectRaster(crs = crs(mg.map))
#   
# 
# 
# res(mg.raster.aggreg)
# plot(mg.raster.aggreg)
# 
# 
# 
# # Converting raster object to sf object. The projection is already the same between files.
# st_crs(mg.map)
# st_crs(mg.raster.aggreg)
# 
# 
# mg.altitude <- mg.raster.aggreg %>% 
#   rasterToPoints(rspatial = F) %>% 
#   data.frame() %>% 
#   rename("longitude" = "y",
#          "latitude" = "x",
#          "altitude" = names(mg.raster.aggreg)) %>% 
#   st_as_sf(coords = c("longitude", "latitude"),
#            crs = st_crs(mg.raster.aggreg))
# 
# 
# 
# ggplot() + 
#   geom_sf(data = mg.altitude,
#           aes(color = altitude))
# 
# 
# # Removing big intermediary raster files
# rm(list = c("mg.raster.altitude", "mg.raster.aggreg", "mg_i"))



## Temp, Humidity, and wind ####

# The first step would be reading all the data files, merging them, and attaching
# geolocation information of each station.

# Getting the name of all files
file.names <- dir(path = paste(getwd(), 
                               "/Data/INMET/Convencional - Mensal/", sep = ""))


# Using a for loop to read all the data and add a "file name" column, which 
# has the station ID

clim.data <- data.frame()

for(i in seq_along(file.names)) {
  
  clim.data.read <- read.csv2(paste(getwd(), 
                                    "/Data/INMET/Convencional - Mensal/", file.names[i], 
                                    sep = ""),
                              dec = ",",
                              na.strings = "null",
                              skip = 11,
                              header = FALSE,
                              col.names = c("date", "total_sun_hr", "temp_C", "ur_perct",
                                            "wind_m.s", "empty"))
  
  
  
  clim.data <- clim.data.read %>% 
    mutate(file = file.names[i]) %>% 
    select(-empty) %>% 
    rbind(clim.data)
  
  
}


clim.data <- clim.data %>% 
  mutate(stationID = str_sub(file, start = 7, end = 11))



# Getting additional descriptive information from each station which were at the top
# of the data files


# Station names

station.names <- data.frame()

for(i in seq_along(file.names)) {
  
  name <- read.csv2(paste(getwd(), 
                          "/Data/INMET/Convencional - Mensal/", file.names[i], 
                          sep = ""),
                    dec = ",",
                    na.strings = "null",
                    nrows = 1,
                    header = FALSE,
                    col.names = c("station")) %>% 
    mutate(file = file.names[i],
           station = str_sub(station, start = 7, end = -1L))
  
  
  station.names <- rbind(station.names,
                         name)
  
  
}


# Station latitude

station.lat <- data.frame()

for(i in seq_along(file.names)) {
  
  lat <- read.csv2(paste(getwd(), 
                         "/Data/INMET/Convencional - Mensal/", file.names[i], 
                         sep = ""),
                   dec = ",",
                   na.strings = "null",
                   skip = 2,
                   nrows = 1,
                   header = FALSE,
                   col.names = c("latitude")) %>% 
    mutate(file = file.names[i],
           latitude = str_sub(latitude, start = 11, end = -1L))
  
  
  station.lat <- rbind(station.lat,
                       lat)
  
  
}


# Station longitude

station.long <- data.frame()

for(i in seq_along(file.names)) {
  
  long <- read.csv2(paste(getwd(), 
                          "/Data/INMET/Convencional - Mensal/", file.names[i], 
                          sep = ""),
                    dec = ",",
                    na.strings = "null",
                    skip = 3,
                    nrows = 1,
                    header = FALSE,
                    col.names = c("longitude")) %>% 
    mutate(file = file.names[i],
           longitude = str_sub(longitude, start = 12, end = -1L))
  
  
  station.long <- rbind(station.long,
                        long)
  
  
}


# Station altitude

station.altitude <- data.frame()

for(i in seq_along(file.names)) {
  
  altitude <- read.csv2(paste(getwd(), 
                              "/Data/INMET/Convencional - Mensal/", file.names[i], 
                              sep = ""),
                        dec = ",",
                        na.strings = "null",
                        skip = 4,
                        nrows = 1,
                        header = FALSE,
                        col.names = c("altitude")) %>% 
    mutate(file = file.names[i],
           altitude = str_sub(altitude, start = 11, end = -1L))
  
  
  station.altitude <- rbind(station.altitude,
                            altitude)
  
  
}


# Station situation

station.situation <- data.frame()

for(i in seq_along(file.names)) {
  
  situation <- read.csv2(paste(getwd(), 
                               "/Data/INMET/Convencional - Mensal/", file.names[i], 
                               sep = ""),
                         dec = ",",
                         na.strings = "null",
                         skip = 5,
                         nrows = 1,
                         header = FALSE,
                         col.names = c("situation")) %>% 
    mutate(file = file.names[i],
           situation = str_sub(situation, start = 11, end = -1L))
  
  
  station.situation <- rbind(station.situation,
                             situation)
  
  
}

# There are 280 files. However, only 253 were read. Finding out why.
data.frame(file = file.names) %>% 
  left_join(clim.data %>% 
              select(file, stationID), 
            by = "file") %>% 
  distinct(file, .keep_all = TRUE) %>% 
  filter(is.na(stationID)) %>% 
  left_join(station.names, by = "file") %>% 
  left_join(station.lat, by = "file") %>% 
  left_join(station.long, by = "file") %>% 
  left_join(station.altitude, by = "file") %>% 
  left_join(station.situation, by = "file") %>% 
  View()

# The reason was because there was no data in the files. Additionally, the stations
# were deactivated a long time ago. No data was lost in this step.


# Merging all climatic data together

clim.data1 <- clim.data %>% 
  left_join(station.names, by = "file") %>% 
  left_join(station.lat, by = "file") %>% 
  left_join(station.long, by = "file") %>% 
  left_join(station.altitude, by = "file") %>% 
  left_join(station.situation, by = "file") %>% 
  
  # Insolation variable onlt indicated how many hours of sun, which is of little value
  # for the study. Dropping this variable and the file names
  
  select(-total_sun_hr, -file) %>% 
  
  # Formating variables to the correct format
  mutate(date = lubridate::as_date(date, format = "%Y-%m-%d")) %>% 
  mutate_at(c("latitude", "longitude", "altitude"), as.numeric) %>% 
  mutate_if(is.character, factor) %>% 
  
  
  # IPCC Models in the 6th report covers data from 1850 to 2014 (inclusive).
  # Eyring et al., 2016. DOI: 10.5194/gmd-9-1937-2016
  # Removing data that goes beyond that interval.
  filter(lubridate::year(date) < 2015)


# Creating a time variable (months from origin) and merging it with climatic data
time.month <- clim.data1 %>% 
  distinct(date) %>% 
  arrange(date) %>% 
  mutate(month = 1:n())



clim.data1 <- clim.data1 %>% 
  left_join(time.month, by = "date")



## Solar radiation ####

# Getting the name of all files
file.names <- dir(path = paste(getwd(), 
                               "/Data/INMET/Automatic - Daily/", sep = ""))


# Using a for loop to read all the data and add a "file name" column, which 
# has the station ID

sun.data <- data.frame()

for(i in seq_along(file.names)) {
  
  sun.data.read <- read.csv2(paste(getwd(), 
                                    "/Data/INMET/Automatic - Daily/", file.names[i], 
                                    sep = ""),
                              dec = ",",
                              na.strings = "null",
                              skip = 11,
                              header = FALSE,
                              col.names = c("date", "hour", "radiation_kj.m2", "empty"))
  
  
  
  sun.data <- sun.data.read %>% 
    mutate(file = file.names[i]) %>% 
    select(-empty) %>% 
    rbind(sun.data)
  
  print(i)
  
  
}


sun.data <- sun.data %>% 
  mutate(stationID = str_sub(file, start = 7, end = 10))


# Getting additional descriptive information from each station which were at the top
# of the data files


# Station names

sun.names <- data.frame()

for(i in seq_along(file.names)) {
  
  name <- read.csv2(paste(getwd(), 
                          "/Data/INMET/Automatic - Daily/", file.names[i], 
                          sep = ""),
                    dec = ",",
                    na.strings = "null",
                    nrows = 1,
                    header = FALSE,
                    col.names = c("station")) %>% 
    mutate(file = file.names[i],
           station = str_sub(station, start = 7, end = -1L))
  
  
  sun.names <- rbind(sun.names,
                         name)
  
  
}


# Station latitude

sun.lat <- data.frame()

for(i in seq_along(file.names)) {
  
  lat <- read.csv2(paste(getwd(), 
                         "/Data/INMET/Automatic - Daily/", file.names[i], 
                         sep = ""),
                   dec = ",",
                   na.strings = "null",
                   skip = 2,
                   nrows = 1,
                   header = FALSE,
                   col.names = c("latitude")) %>% 
    mutate(file = file.names[i],
           latitude = str_sub(latitude, start = 11, end = -1L))
  
  
  sun.lat <- rbind(sun.lat,
                       lat)
  
  
}


# Station longitude

sun.long <- data.frame()

for(i in seq_along(file.names)) {
  
  long <- read.csv2(paste(getwd(), 
                          "/Data/INMET/Automatic - Daily/", file.names[i], 
                          sep = ""),
                    dec = ",",
                    na.strings = "null",
                    skip = 3,
                    nrows = 1,
                    header = FALSE,
                    col.names = c("longitude")) %>% 
    mutate(file = file.names[i],
           longitude = str_sub(longitude, start = 12, end = -1L))
  
  
  sun.long <- rbind(sun.long,
                        long)
  
  
}


# Station altitude

sun.altitude <- data.frame()

for(i in seq_along(file.names)) {
  
  altitude <- read.csv2(paste(getwd(), 
                              "/Data/INMET/Automatic - Daily/", file.names[i], 
                              sep = ""),
                        dec = ",",
                        na.strings = "null",
                        skip = 4,
                        nrows = 1,
                        header = FALSE,
                        col.names = c("altitude")) %>% 
    mutate(file = file.names[i],
           altitude = str_sub(altitude, start = 11, end = -1L))
  
  
  sun.altitude <- rbind(sun.altitude,
                            altitude)
  
  
}


# Station situation

sun.situation <- data.frame()

for(i in seq_along(file.names)) {
  
  situation <- read.csv2(paste(getwd(), 
                               "/Data/INMET/Automatic - Daily/", file.names[i], 
                               sep = ""),
                         dec = ",",
                         na.strings = "null",
                         skip = 5,
                         nrows = 1,
                         header = FALSE,
                         col.names = c("situation")) %>% 
    mutate(file = file.names[i],
           situation = str_sub(situation, start = 11, end = -1L))
  
  
  sun.situation <- rbind(sun.situation,
                             situation)
  
  
}

# All files were read OKAY!!


# Merging all climatic data together

sun.data1 <- sun.data %>% 
  left_join(sun.names, by = "file") %>% 
  left_join(sun.lat, by = "file") %>% 
  left_join(sun.long, by = "file") %>% 
  left_join(sun.altitude, by = "file") %>% 
  left_join(sun.situation, by = "file") %>% 
  
  select(-file) %>% 
  

  # Formatting variables to the correct format
  mutate(hour = as.character(hour)) %>% 
  mutate(hour = ifelse(hour == "0", hour, 
                       ifelse(nchar(hour) == 3, str_sub(hour, end = 1),
                              ifelse(nchar(hour) == 4, str_sub(hour, end = 2),
                                     "who knows")))) %>% 
  mutate(hour = as.numeric(hour)) %>% 



  mutate(date = lubridate::as_date(date, format = "%Y-%m-%d")) %>% 
  mutate_at(c("latitude", "longitude", "altitude"), as.numeric) %>% 
  mutate_if(is.character, factor) %>% 
  
  
  # IPCC Models in the 6th report covers data from 1850 to 2014 (inclusive).
  # Eyring et al., 2016. DOI: 10.5194/gmd-9-1937-2016
  # Removing data that goes beyond that interval.
  filter(lubridate::year(date) < 2015) %>% 
  
  # Removing NAs and negative values
  drop_na() %>% 
  filter(radiation_kj.m2 >= 0) %>% 
  
  
  # Calculating monthly average
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  group_by(stationID, station, latitude, longitude, altitude,
           year, month) %>% 
  summarise(radiation_kj.m2 = mean(radiation_kj.m2, na.rm = T)) %>% 
  ungroup %>% 
  
  mutate(date = zoo::as.yearmon(paste(year, month.abb[month], sep = "-"),
                                "%Y-%b")) %>% 
  select(-month, -year)
  


# Data preparation --------------------------------------------------------

# # Filtering out altitude data from outside Minas Gerais delimitation
# 
# # Checking projection
# st_crs(mg.map)
# st_crs(mg.altitude)
# 
# 
# # Same projection!
# 
# 
# mg.altitude.final <- st_filter(x = mg.altitude,
#                                y = mg.map, 
#                                .predicate = st_intersects)
# 
# 
# ggplot() + 
#   geom_sf(data = mg.altitude.final,
#           aes(color = altitude))



# Filtering out stations that are not from Minas Gerais based on their latitude and longitude


# Creating a file which only has coordinates and station IDs
stations <- clim.data1 %>% 
  select(stationID, latitude, longitude) %>% 
  distinct(stationID, .keep_all = TRUE)


stations.sun <- sun.data1 %>% 
  select(stationID, latitude, longitude) %>% 
  distinct(stationID, .keep_all = TRUE)


# Converting station coordinates to a sf object
stations.sf <- st_as_sf(stations,
                        coords = c("longitude", "latitude"),
                        crs = st_crs(mg.map))

stations.sun.sf <- st_as_sf(stations.sun,
                        coords = c("longitude", "latitude"),
                        crs = st_crs(mg.map))


ggplot() + 
  geom_sf(data = mg.map) + 
  geom_sf(data = stations.sf)


ggplot() + 
  geom_sf(data = mg.map) + 
  geom_sf(data = stations.sun.sf)


# Checking projections
st_crs(mg.map)
st_crs(stations.sf)
st_crs(stations.sun.sf)



mg.stations <- st_filter(x = stations.sf,
                         y = mg.map, 
                         .predicate = st_intersects)

mg.sun.stations <- st_filter(x = stations.sun.sf,
                         y = mg.map, 
                         .predicate = st_intersects)


# Keeping only data from Minas Gerais's climatic stations
clim.data2 <- clim.data1 %>% 
  filter(stationID %in% mg.stations$stationID) %>% 
  droplevels()

sun.data2 <- sun.data1 %>% 
  filter(stationID %in% mg.sun.stations$stationID) %>% 
  droplevels()


summary(sun.data2)

# Creating a time variable (months from origin) and merging it with sun data
time.month.sun <- sun.data2 %>% 
  distinct(date) %>% 
  arrange(date) %>% 
  mutate(month = 1:n())



sun.data2 <- sun.data2%>% 
  left_join(time.month.sun, by = "date")


summary(sun.data2)

# How many station left per month/year on average?

sun.data2 %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Min = min(n), Max = max(n))



ggplot() + 
  geom_sf(data = mg.map) + 
  geom_sf(data = sun.data2 %>% 
            group_by(date) %>% 
            mutate(n = n()) %>% 
            ungroup() %>% 
            filter(n == 8) %>% 
            st_as_sf(coords = c("longitude", "latitude"),
                     crs = st_crs(mg.map)))

# There are way too little info on solar radiation on early years because there
# was not enough stations collecting this measure. Setting a minimum of 8 stations
# because they are evenly distributed in the state so to not have a too strong
# impact in the interpolations.

sun.data3 <- sun.data2 %>% 
  group_by(date) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 8) %>% 
  select(-n, -month)


# Recreating the time variable to be used in the interpolation
time.month.sun <- sun.data3 %>% 
  distinct(date) %>% 
  arrange(date) %>% 
  mutate(month = 1:n())



sun.data3 <- sun.data3 %>% 
  left_join(time.month.sun, by = "date")


sun.data3 %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Min = min(n), Max = max(n))
  

# How many months? 
length(unique(sun.data3$date))

# 10 years and 7 months!



# Temp, humidity, and wind

# Creating a time variable (months from origin) and merging it with sun data
time.month <- clim.data2 %>% 
  distinct(date) %>% 
  arrange(date) %>% 
  mutate(month = 1:n())



clim.data2 <- clim.data2%>% 
  left_join(time.month, by = "date")


# creating an individual file for each climatic variable and remove NAs

clim.data.temp <- clim.data2 %>% 
  select(-c(ur_perct, wind_m.s)) %>% 
  drop_na() %>% 
  droplevels()


clim.data.ur <- clim.data2 %>% 
  select(-c(temp_C, wind_m.s)) %>% 
  drop_na() %>% 
  droplevels()


clim.data.wind <- clim.data2 %>% 
  select(-c(temp_C, ur_perct)) %>% 
  drop_na() %>% 
  droplevels()



# How many station left per month/year on average?

clim.data.temp %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Min = min(n), Max = max(n))

# How many months? Should be 648 (from Jan 1961 to December 2014)
length(unique(clim.data.temp$date))



clim.data.ur %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Min = min(n), Max = max(n))

# How many months? Should be 648 (from Jan 1961 to December 2014)
length(unique(clim.data.ur$date))



clim.data.wind %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(Min = min(n), Max = max(n))

# How many months? Should be 648 (from Jan 1961 to December 2014)
length(unique(clim.data.wind$date))



## Prediction grid ####
# Creating a grid of 0.10º by 0.10º of Minas Gerais to use for predictions

st_is_longlat(mg.map) # Needs to be yes!

grid_spacing <- 0.10 # Size of squares. It is in the same unit as the CRS (º for SIRGAS 2000)


mg.grid <- mg.map %>% 
  st_make_grid(square = TRUE, 
               cellsize = c(grid_spacing, grid_spacing),
               what = "centers") %>%
  st_sf() %>% 
  st_filter(y = mg.map, 
            .predicate = st_intersects,
            sparse = T)



ggplot() +
  geom_sf(data = mg.grid)


# Getting grid with longitude and latitude to use in the interpolation
mg.pred1 <- mg.grid %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  rename("longitude" = "X",
         "latitude" = "Y")


# Getting altitude values for each of the grid points

alt.pred <- raster::extract(x = mg.raster.altitude,
                            y = mg.pred1, 
                            fun = mean,
                            na.rm = TRUE)

mg.pred2 <- mg.pred1 %>% 
  mutate(altitude = alt.pred) %>% 
  
  # There is one location with missing altitude. Deleting that location
  drop_na()

summary(mg.pred2)


ggplot() +
  geom_sf(data = st_as_sf(mg.pred2,
                          coords = c("longitude", "latitude"),
                          crs = st_crs(mg.map)),
          aes(colour = altitude))



# Creating prediction grid
mg.pred3 <- data.frame()


for(i in 1:max(time.month$month)) {
  
  pred <- mg.pred2 %>% 
    mutate(month = i)
  
  mg.pred3 <- rbind(mg.pred3, pred)
  
  
  print(paste(i, " month finished. Only ", max(time.month$month)-i, " missing :)",
              sep = ""))
  
}


mg.pred4 <- data.frame(
  
  # Intercept
  intercept = rep(1, length(mg.pred3[,1]))) %>% 
  
  # Coordinates + altitude + months
  cbind(mg.pred3)



# Gaussian Process Interpolation ------------------------------------------


## Temperature ####


# Defining the response variable
ytemp <- clim.data.temp %>% 
  pull(temp_C)


# Defining locations. It is important to have longitude first and then latitude and time
# because the function assumes as much!
loctemp <- clim.data.temp %>% 
  select(longitude, latitude, month)


# Using location ad altitude as covariate
xtemp <- data.frame(
  # Intercept
  intercept = rep(1, length(ytemp))) %>% 
  
  mutate(
    
    # Coordinates
    longitude = loctemp$longitude,
    latitude = loctemp$latitude,
    
    # Altitude
    altitude = clim.data.temp %>% 
      pull(altitude)
  )


# Fitting Gaussian Process model :)
gpfittemp <- fit_model(y = ytemp,
                       locs = loctemp,
                       X = xtemp,
                       
                       # Using exponential_spacetime to account for the time variable
                       covfun_name = "exponential_spacetime")

summary(gpfittemp)




# Running interpolation for the entire grid and months

pred.temp <- predictions(fit = gpfittemp,
                         
                         # Locations to be prodicted + time variable
                         locs_pred = mg.pred4 %>% 
                           select(longitude, latitude, month) %>% 
                           as.matrix(),
                         
                         # Covariates
                         X_pred = mg.pred4 %>% 
                           select(intercept, longitude, latitude, altitude),
                         m = 60)



# Plotting the predictions
ggplot() +
  geom_sf(data = mg.pred4 %>% 
            mutate(predtemp = pred.temp) %>% 
            filter(month == 12) %>% 
            st_as_sf(coords = c("longitude", "latitude"),
                     crs = st_crs(mg.map)),
          aes(colour = predtemp)) +
  scale_color_viridis_c(option = "B") +
  ggtitle("Conditional Expectation (Predictions)")




## Relative humidity ####


# Defining the response variable
yur <- clim.data.ur %>% 
  pull(ur_perct)


# Defining locations. It is important to have longitude first and then latitude and time
# because the function assumes as much!
locur <- clim.data.ur %>% 
  select(longitude, latitude, month)


# Using location ad altitude as covariate
xur <- data.frame(
  # Intercept
  intercept = rep(1, length(yur))) %>% 
  
  mutate(
    
    # Coordinates
    longitude = locur$longitude,
    latitude = locur$latitude,
    
    # Altitude
    altitude = clim.data.ur %>% 
      pull(altitude)
  )


# Fitting Gaussian Process model :)
gpfitur <- fit_model(y = yur,
                     locs = locur,
                     X = xur,
                     
                     # Using exponential_spacetime to account for the time variable
                     covfun_name = "exponential_spacetime")

summary(gpfitur)




# Running interpolation for the entire grid and months

pred.ur <- predictions(fit = gpfitur,
                       
                       # Locations to be prodicted + time variable
                       locs_pred = mg.pred4 %>% 
                         select(longitude, latitude, month) %>% 
                         as.matrix(),
                       
                       # Covariates
                       X_pred = mg.pred4 %>% 
                         select(intercept, longitude, latitude, altitude),
                       m = 60)



# Plotting the predictions
ggplot() +
  geom_sf(data = mg.pred4 %>% 
            mutate(predur = pred.ur) %>% 
            filter(month == 6) %>% 
            st_as_sf(coords = c("longitude", "latitude"),
                     crs = st_crs(mg.map)),
          aes(colour = predur)) +
  scale_color_viridis_c(option = "B") +
  ggtitle("Conditional Expectation (Predictions)")



## Wind speed ####

# Defining the response variable
ywind <- clim.data.wind %>% 
  pull(wind_m.s)


# Defining locations. It is important to have longitude first and then latitude and time
# because the function assumes as much!
locwind <- clim.data.wind %>% 
  select(longitude, latitude, month)


# Using location ad altitude as covariate
xwind <- data.frame(
  # Intercept
  intercept = rep(1, length(ywind))) %>% 
  
  mutate(
    
    # Coordinates
    longitude = locwind$longitude,
    latitude = locwind$latitude,
    
    # Altitude
    altitude = clim.data.wind %>% 
      pull(altitude)
  )


# Fitting Gaussian Process model :)
gpfitwind <- fit_model(y = ywind,
                       locs = locwind,
                       X = xwind,
                       
                       # Using exponential_spacetime to account for the time variable
                       covfun_name = "exponential_spacetime")

summary(gpfitwind)




# Running interpolation for the entire grid and months

pred.wind <- predictions(fit = gpfitwind,
                         
                         # Locations to be prodicted + time variable
                         locs_pred = mg.pred4 %>% 
                           select(longitude, latitude, month) %>% 
                           as.matrix(),
                         
                         # Covariates
                         X_pred = mg.pred4 %>% 
                           select(intercept, longitude, latitude, altitude),
                         m = 60)



# Plotting the predictions
ggplot() +
  geom_sf(data = mg.pred4 %>% 
            mutate(predwind = pred.wind) %>% 
            filter(month == 6) %>% 
            st_as_sf(coords = c("longitude", "latitude"),
                     crs = st_crs(mg.map)),
          aes(colour = predwind)) +
  scale_color_viridis_c(option = "B") +
  ggtitle("Conditional Expectation (Predictions)")



## Solar radiation ####


# Defining the response variable
ysun <- sun.data3 %>% 
  pull(radiation_kj.m2)


# Defining locations. It is important to have longitude first and then latitude and time
# because the function assumes as much!
locsun <- sun.data3 %>% 
  select(longitude, latitude, month)


# Using location ad altitude as covariate
xsun <- data.frame(
  # Intercept
  intercept = rep(1, length(ysun))) %>% 
  
  mutate(
    
    # Coordinates
    longitude = locsun$longitude,
    latitude = locsun$latitude,
    
    # Altitude
    altitude = sun.data3 %>% 
      pull(altitude)
  )


# Fitting Gaussian Process model :)
gpfitsun <- fit_model(y = ysun,
                       locs = locsun,
                       X = xsun,
                       
                       # Using exponential_spacetime to account for the time variable
                       covfun_name = "exponential_spacetime")

summary(gpfitsun)




# Running interpolation for the entire grid and months

# Creating prediction grid
mg.pred.sun1 <- data.frame()


for(i in 1:max(sun.data3$month)) {
  
  pred <- mg.pred2 %>% 
    mutate(month = i)
  
  mg.pred.sun1 <- rbind(mg.pred.sun1, pred)
  
  
  print(paste(i, " month finished. Only ", max(sun.data3$month)-i, " missing :)",
              sep = ""))
  
}


mg.pred.sun2 <- data.frame(
  
  # Intercept
  intercept = rep(1, length(mg.pred.sun1[,1]))) %>% 
  
  # Coordinates + altitude + months
  cbind(mg.pred.sun1)



pred.sun <- predictions(fit = gpfitsun,
                         
                         # Locations to be prodicted + time variable
                         locs_pred = mg.pred.sun2 %>% 
                           select(longitude, latitude, month) %>% 
                           as.matrix(),
                         
                         # Covariates
                         X_pred = mg.pred.sun2 %>% 
                           select(intercept, longitude, latitude, altitude),
                         m = 60)



# Plotting the predictions
ggplot() +
  geom_sf(data = mg.pred.sun2 %>% 
            mutate(predsun = pred.sun) %>% 
            filter(month == 57) %>% 
            st_as_sf(coords = c("longitude", "latitude"),
                     crs = st_crs(mg.map)),
          aes(colour = predsun)) +
  scale_color_viridis_c(option = "B") +
  ggtitle("Conditional Expectation (Predictions)")




# Exporting ---------------------------------------------------------------

# Creating a final file with all the climatic data to be used as reference for 
# bias correction


final.interpolations <- mg.pred4 %>% 
  mutate(temp_C = pred.temp,
         ur_percent = pred.ur,
         wind_m.s = pred.wind) %>% 
  
  # Getting the Month-Year date to match with the solar predictions
  left_join(time.month, by = "month") %>% 
  mutate(date = zoo::as.yearmon(paste(lubridate::year(date),
                                      month.abb[lubridate::month(date)], sep = "-"),
                                "%Y-%b")) %>% 
  select(-intercept, -month) %>% 
  left_join(
    
    mg.pred.sun2 %>% 
      mutate(radiation_kj.m2 = pred.sun) %>% 
      
      # Getting the Month-Year date
      left_join(time.month.sun, by = "month") %>% 
      select(-intercept, -month),
    by = c("longitude", "latitude", "altitude", "date")) %>% 
  select(date, setdiff(names(.), "date"))




# Creating a raster file for each variable to export as nc file

layers <- final.interpolations %>% 
  distinct(date) %>% 
  pull()



# Creating a empty raster layer to rasterize my sf object
empty.raster <- raster(crs = crs(mg.map), 
                       vals = 0, 
                       resolution = c(0.10, 0.10), 
                       ext = extent(mg.map))


## Temperature ####

for(i in seq_along(layers)) {
  
  # Get band data
  band <- final.interpolations %>% 
    filter(date == layers[i]) %>% 
    select(-date) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(mg.map))
  
  
  # rasterizing
  
  raster.band.i <- rasterize(
    band,
    y = empty.raster,

    field = c("temp_C"),
    fun = mean)
  
  names(raster.band.i) <- layers[i]
  
  if(i == 1){
    
    final.interpolations.raster.temp_C <- raster.band.i
    
    
  } else {
    
    final.interpolations.raster.temp_C <- stack(final.interpolations.raster.temp_C,
                                         raster.band.i)
    
  }
  
  print(paste(i, " file finished. Only ", length(layers)-i, " missing :)",
              sep = ""))
}



writeRaster(final.interpolations.raster.temp_C,
            "./Data/INMET/final.interpolations.raster.temp_C.nc",
            format = "CDF")




## Humidity ####

for(i in seq_along(layers)) {
  
  # Get band data
  band <- final.interpolations %>% 
    filter(date == layers[i]) %>% 
    select(-date) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(mg.map))
  
  
  # rasterizing
  
  raster.band.i <- rasterize(
    band,
    y = empty.raster,
    
    field = c("ur_percent"),
    fun = mean)
  
  names(raster.band.i) <- layers[i]
  
  if(i == 1){
    
    final.interpolations.raster.ur_percent <- raster.band.i
    
    
  } else {
    
    final.interpolations.raster.ur_percent <- stack(final.interpolations.raster.ur_percent,
                                                raster.band.i)
    
  }
  
  print(paste(i, " file finished. Only ", length(layers)-i, " missing :)",
              sep = ""))
}


writeRaster(final.interpolations.raster.ur_percent,
            "./Data/INMET/final.interpolations.raster.ur_percent.nc",
            format = "CDF")




## Wind ####

for(i in seq_along(layers)) {
  
  # Get band data
  band <- final.interpolations %>% 
    filter(date == layers[i]) %>% 
    select(-date) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(mg.map))
  
  
  # rasterizing
  
  raster.band.i <- rasterize(
    band,
    y = empty.raster,
    
    field = c("wind_m.s"),
    fun = mean)
  
  names(raster.band.i) <- layers[i]
  
  if(i == 1){
    
    final.interpolations.raster.wind_m.s <- raster.band.i
    
    
  } else {
    
    final.interpolations.raster.wind_m.s <- stack(final.interpolations.raster.wind_m.s,
                                                raster.band.i)
    
  }
  
  print(paste(i, " file finished. Only ", length(layers)-i, " missing :)",
              sep = ""))
}


writeRaster(final.interpolations.raster.wind_m.s,
            "./Data/INMET/final.interpolations.raster.wind_m.s.nc",
            format = "CDF")



## Solar radiation ####

layers.sun <- final.interpolations %>% 
  select(date, radiation_kj.m2) %>% 
  drop_na() %>% 
  distinct(date) %>% 
  pull()

for(i in seq_along(layers.sun)) {
  
  # Get band data
  band <- final.interpolations %>% 
    filter(date == layers.sun[i]) %>% 
    select(-date) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(mg.map))
  
  
  # rasterizing
  
  raster.band.i <- rasterize(
    band,
    y = empty.raster,
    
    field = c("radiation_kj.m2"),
    fun = mean)
  
  names(raster.band.i) <- layers.sun[i]
  
  if(i == 1){
    
    final.interpolations.raster.radiation_kj.m2 <- raster.band.i
    
    
  } else {
    
    final.interpolations.raster.radiation_kj.m2 <- stack(final.interpolations.raster.radiation_kj.m2,
                                                raster.band.i)
    
  }
  
  print(paste(i, " file finished. Only ", length(layers.sun)-i, " missing :)",
              sep = ""))
}



writeRaster(final.interpolations.raster.radiation_kj.m2,
            "./Data/INMET/final.interpolations.raster.radiation_kj.m2.nc",
            format = "CDF")


save.image("INMET data prep.RData")
