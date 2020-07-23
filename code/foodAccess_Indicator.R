setwd("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon")

library(dplyr)
library(tidyverse)
library(ggmap)
library(leaflet)
library(rgdal)
library(sf)
library(rgeos)
library(stringr)
library(geosphere)
library(pracma)
library(tidycensus)

foodRetailersVA <- read.csv("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data/MarketMaker_Food Retailers_Virginia.csv", 
                            header=T, stringsAsFactors=F)
CenterPop_tract <- read.csv("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data/CenPop2010_Mean_TR51.txt", header =T)
map  <- st_read("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data/VirginiaShapeFiles/tl_2019_51_tract.shp",
                stringsAsFactors = FALSE)

nova_counties  <- c("Arlington",
                    "Fairfax",
                    "Loudoun",
                    "Prince William",
                    "Alexandria",
                    "Falls Church",
                    "Fairfax City",
                    "Manassas",
                    "Manassas Park",
                    "Fauquier")


foodRetailersNOVA <- foodRetailersVA %>%
  filter(County %in% nova_counties)
  
foodRetailersNOVA <- foodRetailersNOVA %>%
  filter(Profiles == "Food Retailer")


foodRetailersNOVA$fullAddress <- paste(foodRetailersNOVA$Address1, foodRetailersNOVA$City, 
         foodRetailersNOVA$State, foodRetailersNOVA$Zip)

# Remove the hashtag from the addresses (causes error when geocoding)
foodRetailersNOVA$fullAddress <- str_replace_all(foodRetailersNOVA$fullAddress, "[[:punct:]]", "")

foodRetailersNOVA$fullAddress[foodRetailersNOVA$Business == "Warrenton Farmers Market (Saturday)"] <- "96 E Lee St Warrenton, VA 20186"
foodRetailersNOVA$fullAddress[foodRetailersNOVA$Business == "Talk-The Town Gourmet Gifts-Mr"] <- "7420 Fullerton Rd Springfield Virginia 22153"
foodRetailersNOVA$fullAddress[foodRetailersNOVA$BusinessId == 4309133] <- "1521 N Quaker Ln Alexandria, VA 22302"

myACSkey <- ""
# Get County Outlines
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Arlington county",
                        "Fairfax county",
                        "Loudoun county",
                        "Prince William county",
                        "Alexandria city",
                        "Falls Church city",
                        "Fairfax city",
                        "Manassas city",
                        "Manassas Park city",
                        "Fauquier County"),
               variables = "B19058_002",
               survey = "acs5",
               key = myACSkey,
               year=2018,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)


# Get Loudoun County Outline only
loudoun_outline<-get_acs(geography = "county",
                         state="VA",
                         county=c("Loudoun county"),
                         variables = "B19058_002",
                         survey = "acs5",
                         key = myACSkey,
                         year=2018,
                         output = "wide",
                         show_call = T,
                         geometry = T,
                         keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)


register_google(key = "")

# Geocode
for(i in 1:nrow(foodRetailersNOVA)){
  result <- geocode(foodRetailersNOVA$fullAddress[i], output="latlona", source="google")
  foodRetailersNOVA$lon[i] <- as.numeric(result[1])
  foodRetailersNOVA$lat[i] <- as.numeric(result[2])
  Sys.sleep(time = .15)
}

# Fix geocode errors on NA data
for(i in 1:nrow(foodRetailersNOVA)){
  if(is.na(foodRetailersNOVA$lat[i]) | is.na(foodRetailersNOVA$lon[i])){
    result <- geocode(foodRetailersNOVA$fullAddress[i], output="latlona", source="google")
    foodRetailersNOVA$lon[i] <- as.numeric(result[1])
    foodRetailersNOVA$lat[i] <- as.numeric(result[2])
    Sys.sleep(time = .15)
  }
}

write.csv(foodRetailersNOVA, 
          "C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data/FoodAccessGeoLocData\\FoodRetailers_NOVA_geoloc.csv",
          row.names = FALSE)


# Rename columns in Census Tract Population Center data set
  colnames(CenterPop_tract)[5] <- "CENTER.LATITUDE"
  colnames(CenterPop_tract)[6] <- "CENTER.LONGITUDE"

for(i in 1:nrow(CenterPop_tract)){
  CenterPop_tract$TRACTCE[i] <- str_pad(CenterPop_tract$TRACTCE[i], 6, pad="0")
}

map$TRACTCE <- as.numeric(map$TRACTCE)
map$COUNTYFP <- as.numeric(map$COUNTYFP)

# VA only
CenterPopTR_map <- inner_join(CenterPop_tract, map, by=c("TRACTCE", "COUNTYFP"))
# anti_join(CenterPop_tract, map, by=c("TRACTCE", "COUNTYFP"))

CenterPopTR_mapNOVA <- CenterPopTR_map %>%
  filter(NAMELSAD %in% NOVA_TRid)

# Helper function: calculates distance in meters given 2 sets of coordinates (long, lat)
# longitude first, then latitude
calculate_distance <- function(long1, lat1, long2, lat2){
  ans <- distm(c(long1, lat1), c(long2, lat2), fun=distHaversine)
  return(ans)
}


# Pass in location points dataset
# radius defaults are 5, 10, 20 miles (in meters)
# uses Tract Population center
loc_within_radius <- function(loc_data, radius_1 = 8046.72, radius_2 = 16093.4, radius_3 = 32186.9){
  centerData_copy <- CenterPopTR_mapNOVA
  
  centerData_copy$COUNT_5mile <- NA
  centerData_copy$INVERSE_DIST_5mile <- NA
  centerData_copy$COUNT_10mile <- NA
  centerData_copy$INVERSE_DIST_10mile <- NA
  centerData_copy$COUNT_20mile <- NA
  centerData_copy$INVERSE_DIST_20mile <- NA
  
  for(i in 1:nrow(centerData_copy)){
    count_1 <- 0
    inverse_dist_1 <- 0
    count_2 <- 0
    inverse_dist_2 <- 0
    count_3 <- 0
    inverse_dist_3 <- 0
    centerData_copy$COUNT_5mile[i] <- 0
    centerData_copy$INVERSE_DIST_5mile[i] <- 0
    centerData_copy$COUNT_10mile[i] <- 0
    centerData_copy$INVERSE_DIST_10mile[i] <- 0
    centerData_copy$COUNT_20mile[i] <- 0
    centerData_copy$INVERSE_DIST_20mile[i] <- 0
    
    for(j in 1:nrow(loc_data)){
      distance <- calculate_distance(centerData_copy$CENTER.LONGITUDE[i], centerData_copy$CENTER.LATITUDE[i],
                                     loc_data$lon[j], loc_data$lat[j])
      
      if(distance <= radius_1){
        count_1 <- count_1 + 1
        inverse_dist_1 <- (1/distance) + inverse_dist_1
      }
      if(distance <= radius_2){
        count_2 <- count_2 + 1
        inverse_dist_2 <- (1/distance) + inverse_dist_2
      }
      if(distance <= radius_3){
        count_3 <- count_3 + 1
        inverse_dist_3 <- (1/distance) + inverse_dist_3
      }
      
    }
    centerData_copy$COUNT_5mile[i] <- count_1
    centerData_copy$INVERSE_DIST_5mile[i] <- inverse_dist_1 / centerData_copy$POPULATION[i]
    centerData_copy$COUNT_10mile[i] <- count_2
    centerData_copy$INVERSE_DIST_10mile[i] <- inverse_dist_2 / centerData_copy$POPULATION[i]
    centerData_copy$COUNT_20mile[i] <- count_3
    centerData_copy$INVERSE_DIST_20mile[i] <- inverse_dist_3 / centerData_copy$POPULATION[i]
  }
  return(centerData_copy)
}


# Output columns are COUNT_5mile... COUNT_20mile, INVERSE_DIST_5mile...INVERSE_DIST_20mile
output <- loc_within_radius(foodRetailersNOVA)
  

# Map of count of retailers within radius
ggplot(output) + 
  geom_sf(aes(fill= INVERSE_DIST_10mile, geometry=geometry)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  scale_fill_viridis_c() + scale_color_viridis_c() +
  theme_bw() +theme(legend.title = element_blank())



# Map of count of retailers within radius
ggplot(output) + 
  geom_sf(aes(fill= COUNT_10mile, geometry=geometry)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  # scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
  #                     breaks = c(0, 1, 3, 5, 10, 30, 100), labels=c(0, 1, 3, 5, 10, 30, 100),
  #                     na.value="grey") +
  theme_bw() +theme(legend.title = element_blank()) +
  ggtitle("Food Retailer Count within 10 Miles of Tract Center")
  
# Map of inverse distance of retailers within radius
ggplot(output) + 
  geom_sf(aes(fill= INVERSE_DIST_20mile, geometry=geometry)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
                       breaks = c(0, 0.000001, 0.00001, 0.0001, 0.001, 0.01), labels=c(0, 0.000001, 0.00001, 0.0001, 0.001, .01),
                      na.value="grey") +
  theme_bw() +theme(legend.title = element_blank()) + 
  ggtitle("Inverse Distance for Food Retailer within 20 Miles of Tract Center")



tmp <- output %>%
  select(GEOID, INVERSE_DIST_10mile, INVERSE_DIST_20mile, COUNT_5mile)
tmp$INVERSE_DIST_20mile[is.nan(tmp$INVERSE_DIST_20mile)] <- 0 

write.csv(tmp, 
          "C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data\\foodRetailer_TRscore.csv",
          row.names = FALSE)


