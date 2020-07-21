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
# radius default is 5 miles (in km)
# uses Tract Population center
loc_within_radius <- function(loc_data, radius = 8046.72){
  centerData_copy <- CenterPopTR_mapNOVA
  centerData_copy$COUNT.IN.RADIUS <- NA
  for(i in 1:nrow(centerData_copy)){
    count <- 0
    centerData_copy$COUNT.IN.RADIUS[i] <- 0
    for(j in 1:nrow(loc_data)){
      distance <- calculate_distance(centerData_copy$CENTER.LONGITUDE[i], centerData_copy$CENTER.LATITUDE[i],
                                     loc_data$lon[j], loc_data$lat[j])
      if(distance <= radius)
        count <- count + 1
    }
    centerData_copy$COUNT.IN.RADIUS[i] <- count
  }
  return(centerData_copy)
}


output <- loc_within_radius(foodRetailersNOVA)
  

# Map of count of retailers within radius
ggplot(output) + 
  geom_sf(aes(fill= COUNT.IN.RADIUS, geometry=geometry)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  scale_fill_viridis_c() + scale_color_viridis_c() +
  theme_bw() +theme(legend.title = element_blank())


