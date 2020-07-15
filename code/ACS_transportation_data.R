#NOVA TRANSPORTATION

setwd("~/dspg2020Loudon/code")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(olsrr)
library(stats)
library(psych)
library(viridis)
library(ggthemes)
library(ggmap)
library(ggspatial)
library(sf)
library(leaflet)
library(tigris)
library(readr)



#show available variables in a particular ACS survey
acs5<-load_variables(2009, "acs5", cache=T)
View(acs5)

acs5_subject <- load_variables(2018, "acs5/subject", cache=T)
View(acs5_subject)

acs5_profile<- load_variables(2018, "acs5/profile", cache=T)
View(acs5_profile)

#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of 
# all the tables bound together.  The function requires a vector of table names, 
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically, 
# it separates the variable column into separate variables, and it separates "NAME" into 
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple 
# variable tables across multiple years in one single tibble.  A couple of notes: the way that 
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have 
# not included "geometry" in the function.  If the user includes geometry, he/she may need 
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}

#NOVA TRANSPORTATION DATA

tables<-c("B08103","B08301","DP04")
years<-c(2018)
colnames=c("Census_tract","County","State")

acs_NoVa_transportation<-acs_years_tables(tables=tables,
                           years=years,
                           key=.key,
                           geography="tract",
                           state="VA",
                           geometry=T,
                           county=c("Arlington county",
                                    "Fairfax county",
                                    "Loudoun county",
                                    "Prince William county",
                                    "Alexandria city",
                                    "Falls Church city",
                                    "Fairfax city",
                                    "Manassas city",
                                    "Manassas Park city",
                                    "Fauquier county"),
                           NAME_col_names = colnames)

#DCleaning the dataset
acs_NoVa_trans_reduced<-acs_NoVa_transportation%>%
  mutate(Percent_DroveAlone=round(((B08301_003/B08301_001)*100),1),
         Percent_Carpooled=round(((B08301_004/B08301_001)*100),1),
         Percent_PublicTrans=round(((B08301_010/B08301_001)*100),1),
         Percent_Bicycle=round(((B08301_018/B08301_001)*100),1),
         Percent_Walk=round(((B08301_019/B08301_001)*100),1),
         Percent_WorkFrHome=round(((B08301_020/B08301_001)*100),1))%>%
  rename(MedianAge=B08103_001,
         MedianAge_DroveAlone=B08103_002,
         MedianAge_Carpooled=B08103_003,
         MedianAge_PublicTransport=B08103_004,
         MedianAge_Walk=B08103_005,
         MedianAge_Other=B08103_006,
         MedianAge_WorkFrHome=B08103_007,
         PercentNoVehicle=DP04_0058P,
         Percent1Vehicle=DP04_0059P,
         Percent2Vehicle=DP04_0060P,
         Percent3plusVehicle=DP04_0061P)%>%
  select(GEOID,Census_tract,County,State,Percent_DroveAlone,Percent_Carpooled,
         Percent_PublicTrans,Percent_Bicycle,Percent_Walk,Percent_WorkFrHome,
         MedianAge,MedianAge_Carpooled,MedianAge_PublicTransport,MedianAge_Walk,
         MedianAge_Other,MedianAge_WorkFrHome,PercentNoVehicle,Percent1Vehicle,
         Percent2Vehicle,Percent3plusVehicle)

#Save a csv
write_csv(acs_NoVa_transportation,"/Users/adamwells/dspg2020Loudon/data/ACS_NoVa_transportation")


 #PLOTTING IN LEAFLET

#Pulls geometry data from tigris, transforms geometry to the proper type for leaflet maps
VAtracts<-tracts(state="51",cb=T,class="sf")
VAcounties<-counties(state="51",cb=T,class="sf")%>%
  filter(COUNTYFP%in%c('51','013','059','107','153','510','610','600','683','685','061'))
VAcounties<-st_transform(VAcounties,crs="WGS84")
VAdata<-inner_join(VAtracts,acs_NoVa_trans_reduced, by="GEOID")%>%
  select(-Census_tract)
VAdata<-st_transform(VAdata,crs="WGS84")

#set palette
mypalette <- colorQuantile(palette="viridis", VAdata$PercentNoVehicle,n=5)

#construct map
leaflet() %>%
  addTiles() %>%
  addPolygons(data=VAdata,color = ~mypalette(VAdata$PercentNoVehicle),
              fillOpacity = 0.6, smoothFactor = 0.2, weight = 2,stroke = F, label=paste("County: ",VAdata$County,", Tract: ",VAdata$NAME, ", Value: ",VAdata$PercentNoVehicle))%>%
  addLegend(pal=mypalette,position = "topright",values = VAdata$PercentNoVehicle,
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              paste0(cuts[-n], " &ndash; ", cuts[-1])})%>%
  addPolylines(data = VAcounties, color = "black", opacity = 0.6, weight = 1)








