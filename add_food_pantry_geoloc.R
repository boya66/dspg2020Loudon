## Add food pantries of Loudon to the map
source('google_maps_api.R')

## load food pantry address
library(readxl)
data <- read_excel("data/food_pantry_loudon.xlsx", sheet = 1)[1:17,]
data <- transform(data, lon = rep(NA, nrow(data)), lat = rep(NA, nrow(data)))

for(i in 1:nrow(data)){
  geoloc <- geoCode(data$Address[i])
  data$lat[i] <- geoloc[1]
  data$lon[i] <- geoloc[2]
}

write.csv(data, file = 'data/loudon_food_pantry_geoloc.csv')
