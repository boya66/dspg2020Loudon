library(sf)
library(tidyverse)
library(plotly)
library(tigris)
library(viridis)
library(ggthemes)
library(osrm)
library(leaflet)
library(sp)
library(vroom)
library(rmapzen)
library(ggmap)
library(ggthemes)
library(openrouteservice)
library(rgdal)
library(viridis)
library(mapview)


#importing relevant data files
FoodPantry_NOVA_geoloc <- read_csv("~/dspg2020Loudon/data/FoodAccessGeoLocData/FoodPantry_NOVA_geoloc.csv")
FoodPantry_NOVA_geoloc$Name[28]<-"Messiahs Market Community Church"
FoodPantry_NOVA_geoloc$Name[31]<-"Sterling United Methodist - Grace Ministry"

FarmersMarket_NOVA_geoloc <- read_csv("~/dspg2020Loudon/data/FoodAccessGeoLocData/FarmersMarket_NOVA_geoloc.csv")
FoodRetailers_NOVA_geoloc <- read_csv("~/dspg2020Loudon/data/FoodAccessGeoLocData/FoodRetailers_NOVA_geoloc.csv")


#Geometry for NoVa
VAcounties<-counties(state="51",cb=T,class="sf")%>%
  filter(COUNTYFP%in%c('51','013','059','107','153','510','610','600','683','685','061'))

#FOOD PANTRY ISOCHRONES

coordinates <- data.frame(FoodPantry_NOVA_geoloc%>%select(lon,lat))

## 10 minute driving range.  ors_isochrones allows a maximun of five locations.
res1<-ors_isochrones(coordinates[1:5,], profile = "driving-car", range = 600, output = "sf")
res2<-ors_isochrones(coordinates[6:10,], profile = "driving-car", range = 600,output= "sf")
res3<-ors_isochrones(coordinates[11:15,], profile = "driving-car", range = 600, output= "sf")
res4<-ors_isochrones(coordinates[16:20,], profile = "driving-car", range = 600,output="sf")
res5<-ors_isochrones(coordinates[21:25,], profile = "driving-car", range = 600, output = "sf")
res6<-ors_isochrones(coordinates[26:30,], profile = "driving-car", range = 600,output= "sf")
res7<-ors_isochrones(coordinates[31:35,], profile = "driving-car", range = 600, output= "sf")
res8<-ors_isochrones(coordinates[36:40,], profile = "driving-car", range = 600,output="sf")
res9<-ors_isochrones(coordinates[41:45,], profile = "driving-car", range = 600, output = "sf")
res10<-ors_isochrones(coordinates[46:50,], profile = "driving-car", range = 600,output= "sf")
res11<-ors_isochrones(coordinates[51,], profile = "driving-car", range = 600, output= "sf")


res<-rbind(res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11)
res$group_index<-c(1:51)

#mapping
res%>%
leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(color = "blue" ,
              fillOpacity = 0.2, smoothFactor = 0.2, weight = 2,stroke = F)%>%
  setView(lng=-77.535747,lat=38.852920,zoom=9)%>%
  addCircles(lng=coordinates$lon,lat=coordinates$lat,color="black",radius=15,label = FoodPantry_NOVA_geoloc$Name)%>%
  addPolylines(data = VAcounties, color = "black", opacity = 0.6, weight = 1)%>%
  addControl("Ten minute drive to NoVa food pantries", position = "bottomleft", className="map-title")


#FARMERS MARKETS ISOCHRONES

coordinates <- data.frame(FarmersMarket_NOVA_geoloc%>%select(lon,lat))

## 10 minute driving range.  ors_isochrones allows a maximun of five locations.
res1<-ors_isochrones(coordinates[1:5,], profile = "driving-car", range = 600, output = "sf")
res2<-ors_isochrones(coordinates[6:10,], profile = "driving-car", range = 600,output= "sf")
res3<-ors_isochrones(coordinates[11:15,], profile = "driving-car", range = 600, output= "sf")
res4<-ors_isochrones(coordinates[16:20,], profile = "driving-car", range = 600,output="sf")
res5<-ors_isochrones(coordinates[21:25,], profile = "driving-car", range = 600, output = "sf")
res6<-ors_isochrones(coordinates[26:30,], profile = "driving-car", range = 600,output= "sf")
res7<-ors_isochrones(coordinates[31:35,], profile = "driving-car", range = 600, output= "sf")
res8<-ors_isochrones(coordinates[36:40,], profile = "driving-car", range = 600,output="sf")
res9<-ors_isochrones(coordinates[41:45,], profile = "driving-car", range = 600, output = "sf")
res10<-ors_isochrones(coordinates[46:48,], profile = "driving-car", range = 600,output= "sf")




#mapping
res%>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(color = "red" ,
              fillOpacity = 0.2, smoothFactor = 0.2, weight = 2,stroke = F)%>%
  setView(lng=-77.535747,lat=38.852920,zoom=9)%>%
  addCircles(lng=coordinates$lon,lat=coordinates$lat,color="black",radius=15,label = FarmersMarket_NOVA_geoloc$Business)%>%
  addPolylines(data = VAcounties, color = "black", opacity = 0.6, weight = 1)%>%
  addControl("Ten minute drive to NoVa farmers markets", position = "bottomleft", className="map-title")

#FOOD RETAILERS ISOCHRONES

coordinates <- data.frame(FoodRetailers_NOVA_geoloc%>%select(lon,lat))

## 10 minute driving range.  ors_isochrones allows a maximun of five locations.

ors_isochrones_large<-function(coordinates,profile,range,output,...)
  {
  res<-data.frame(NULL)
  for (i in seq(1,nrow(coordinates),by=5))
    {
    if(nrow(coordinates)%in%c(i:(i+4)))
      {
      p<-ors_isochrones(coordinates[i:nrow(coordinates),], profile = profile, range = range, output = output,...)
      }
    else
      {
      p<-ors_isochrones(coordinates[i:(i+4),], profile = profile, range = range, output = output,...)
      }
    res<-rbind(res,p)
    Sys.sleep(3.25)
    }
  res$group_index<-c(1:nrow(coordinates))
  return(res)
}

res<-ors_isochrones_large(coordinates=coordinates,profile="driving-car",range=600,output="sf")

#mapping
res%>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(color = "green" ,
              fillOpacity = 0.2, smoothFactor = 0.2, weight = 2,stroke = F)%>%
  setView(lng=-77.535747,lat=38.852920,zoom=9)%>%
  addCircles(lng=coordinates$lon,lat=coordinates$lat,color="black",radius=15,label = FoodRetailers_NOVA_geoloc$Business)%>%
  addPolylines(data = VAcounties, color = "black", opacity = 0.6, weight = 1)%>%
  addControl("Ten minute drive to NoVa food retailers", position = "bottomleft", className="map-title")




