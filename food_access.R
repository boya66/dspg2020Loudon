library("readxl")
dat <- read_excel("data/DataDownload2015.xlsx", sheet = 3)
dat <- dat[dat$State == "Virginia",]

## extract the rows from NoVa tracts
dat <- dat[dat$CensusTract %in% NoVaReducedGeom$GEOID,]
names(dat)[1] <- "GEOID"
load('ACSLoudonEnvironment.RData')
dat_geo <- merge(NoVaReducedGeom, dat, by = "GEOID")

library(ggplot2)
library(ggrepel)

## read in the food pantry geolocations
fp_geoloc <- read.csv('data/loudon_food_pantry_geoloc.csv')
fp_geoloc <- fp_geoloc[1:13,]

## log count of housing units receiving SNAP benefits in tract 
ggplot(dat_geo) +
  geom_sf(aes(geometry=geometry, fill = log(TractSNAP), color = log(TractSNAP))) +
  geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
  ggtitle("Housing units receiving SNAP benefits in year 2015", 
          subtitle = "with Food banks marked")+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c() +
#  geom_text_repel(data = fp_geoloc, aes(x = lon, y = lat, label = X))
  geom_point(data = fp_geoloc, aes(x = lon, y = lat), size = 0.6, color = 'red') +
  theme(plot.title = element_text(hjust = 0.5))

## Flag for food desert when considering low accessibilty at 1 and 20 miles
ggplot(dat_geo) +
  geom_sf(aes(geometry=geometry, fill = LA1and20, color = LA1and20)) +
  geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
  ggtitle("Flag for food desert in year 2015")+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  theme(plot.title = element_text(hjust = 0.5))


## Low income population count beyond 1 mile for urban areas 
## or 10 miles for rural areas from supermarket
## set to 0 if the count==0
ggplot(dat_geo) +
  geom_sf(aes(geometry=geometry, fill = log(LALOWI1_10 + 1), color = log(LALOWI1_10 + 1))) +
  geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
  ggtitle("low income population count with \n low food access in year 2015")+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  theme(plot.title = element_text(hjust = 0.5))
