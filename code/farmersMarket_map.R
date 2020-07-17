
setwd("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon")

library(dplyr)
library(tidyverse)
library(ggmap)
library(leaflet)
library(rgdal)
library(sf)

library(rgeos)

# Project Scope Def
nova_counties  <- c("Arlington",
                    "Fairfax",
                    "Loudoun",
                    "Prince William",
                    "Alexandria City",
                    "Falls Church City",
                    "Fairfax City",
                    "Manassas City",
                    "Manassas Park City",
                    "Fauquier")


farmersMarket_data <- read.csv("data/MarketMaker_Farmers Markets_Virginia.csv", stringsAsFactors = FALSE)
  
farmersMarket_data_filtered <- farmersMarket_data %>%
  filter(County %in% nova_counties)

# Separate farmers market locations with unclean address1 field.
unclean_farmersMarket_data <- farmersMarket_data_filtered %>%
  filter(Business %in% c("Archwood Green Barns", "Archwood Green Barns Farmers Market",
                         "Field to Table, Inc.", "Warrenton Saturday Farmers Market" ))

# Add extra entry since Field to Table Inc manages multiple Farmers Markets
unclean_farmersMarket_data$Address2 <- unclean_farmersMarket_data$Address1
unclean_farmersMarket_data$Address1 <- ""
unclean_farmersMarket_data <- rbind(unclean_farmersMarket_data, 
                                    subset(unclean_farmersMarket_data, Business == "Field to Table, Inc."))


unclean_farmersMarket_data[3, 2] <- "Field to Table, Inc.; Fairlington Farmers Market"
unclean_farmersMarket_data[3, 3] <- "3308 S Stafford St, Arlington, VA 22206"

unclean_farmersMarket_data[5, 2] <- "Field to Table, Inc.; Marymount Farmers Market"
unclean_farmersMarket_data[5, 3] <- "2807 N Glebe Rd, Arlington, VA 22207"

unclean_farmersMarket_data[2, 3] <- "4557 Old Tavern Rd, The Plains, VA 20198"
unclean_farmersMarket_data[4, 3] <- "S 5th St & E Lee St, Warrenton, VA 20186"
unclean_farmersMarket_data[1, 3] <- NA
unclean_farmersMarket_data[2, 4] <- "P.O. Box 269"

unclean_farmersMarket_data <- unclean_farmersMarket_data %>%
  mutate(fullAddress = Address1)
  

farmersMarket_data_filtered <- farmersMarket_data_filtered %>%
  mutate(fullAddress = paste(Address1, City, State, Zip, sep=" "))

# Remove duplicates with uncleaned to be merge
farmersMarket_data_filtered <- farmersMarket_data_filtered %>%
  filter(Business != "Archwood Green Barns") %>%
  filter(Business != "Archwood Green Barns Farmers Market") %>%
  filter(Business != "Field to Table, Inc.") %>%
  filter(Business != "Warrenton Farmers Market (Saturday)") %>%
  filter(Business != "Warrenton Saturday Farmers Market")

# Merge
cleaned_farmersMarket_data <- rbind(farmersMarket_data_filtered, 
                                    unclean_farmersMarket_data[2:nrow(unclean_farmersMarket_data),])


# Rename column as misc address data (keep extra data)
names(cleaned_farmersMarket_data)[names(cleaned_farmersMarket_data) == "Address2"] <- "Misc Address Info"

# Correcting addresses 
cleaned_farmersMarket_data$fullAddress[cleaned_farmersMarket_data$Business == "Columbia Pike Farmers Market"] <- "2705 Columbia Pike Arlington, Virginia 22204"
cleaned_farmersMarket_data$fullAddress[cleaned_farmersMarket_data$Business == "Warrenton Saturday Farmers Market"] <- "96 E Lee St Warrenton, VA 20186"

write.csv(cleaned_farmersMarket_data, 
          "C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data\\cleaned_MarketMaker_FarmersMarket_Virginia.csv",
          row.names = FALSE)


register_google(key = "")



# Geocoding
# FarmersMarket_locations <- mutate_geocode(cleaned_farmersMarket_data, fullAddress)


write.csv(FarmersMarket_locations, 
          "C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data\\FarmersMarket_locations.csv",
          row.names = FALSE)
NOVA_foodpantry_loc <- read.csv("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon/data/FoodPantries_NOVA_Locations.csv", 
                                header=T, stringsAsFactors = FALSE)
         
# NOVA_foodpantry_loc[1] <- mutate_geocode(NOVA_foodpantry_loc[1], Address)

# Get Country Outlines
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


ggplot(map_and_data) + 
  geom_point(data=FarmersMarket_locations, aes(x=lon, y=lat), size=1.5) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  theme_bw() +
  theme(legend.title = element_blank()) 


