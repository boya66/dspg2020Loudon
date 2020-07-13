library(tidycensus)
library(dplyr)
#Get geometry data for NoVa census tracts
NoVaGeometry<-get_acs(geography = "tract",
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
                               "Fauquier county"),
                      variables = "B19058_002",
                      survey = "acs5",
                      key = "f69e67b9edaba057db9eb6c475030af08cf1adb9",
                      year=2018,
                      output = "wide",
                      show_call = T,
                      geometry = T,
                      keep_geo_vars = T)%>% select(-c(11:12))


## Get county outlines for Loudoun
loudoun_sf<-get_acs(geography = "county",
                    state="VA",
                    county=c("Loudoun county"),
                    variables = "B19058_002",
                    survey = "acs5",
                    key = "f69e67b9edaba057db9eb6c475030af08cf1adb9",
                    year=2018,
                    output = "wide",
                    show_call = T,
                    geometry = T,
                    keep_geo_vars = T)

## get food access data
library("readxl")
dat <- read_excel("data/DataDownload2015.xlsx", sheet = 3)
dat <- dat[dat$State == "Virginia",]

## extract the rows from NoVa tracts
dat <- dat[dat$CensusTract %in% NoVaGeometry$GEOID,]
# load('ACSLoudonEnvironment.RData')
dat_geo <- merge(dat, NoVaGeometry, by.y = "GEOID", 
                 by.x = 'CensusTract', all.x = T, all.y = F)
## read in the food pantry geolocations
fp_geoloc <- read.csv('data/loudon_food_pantry_geoloc.csv')
fp_geoloc <- fp_geoloc[1:16,]


## function that bin the metric
f_bin <- function(x, bins = NULL){
  if(is.null(bins)){
    qts <- quantile(x)
    bins <- cut(x, breaks = qts, include.lowest = T)
    return(as.factor(bins))
  }else{
    bins <- cut(x, breaks = bins, include.lowest = T)
    return(as.factor(bins))
  }
 
}


library(ggplot2)
library(ggrepel)
library(viridis)
library(ggthemes)

dat_geo <- cbind(dat_geo, TractSNAP_q  = f_bin(dat_geo$TractSNAP))
ggplotly(ggplot(dat_geo) +
  geom_sf(aes(geometry=geometry, fill = factor(TractSNAP_q), color = factor(TractSNAP_q))) +
  geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
  geom_sf(data=loudoun_sf,fill="transparent",color="red",size=0.5)+
  # labs(title="NoVA, 2015 \nHousing units receiving SNAP benefits",subtitle="")+
  labs(title="NoVA, 2015 \nSNAP map with Loudoun food pantry locations",subtitle="")+
  scale_fill_viridis(discrete=T,name = "Number of \n units", guide = guide_legend(reverse=TRUE))+
  scale_color_viridis(discrete=T,name = "Number of \n units", guide = guide_legend(reverse=TRUE))+
  theme_map()+
  theme(legend.position=c(0.905,0.79))  +
  geom_point(data = fp_geoloc, aes(x = lon, y = lat), size = 0.6, color = 'red') )


## Flag for food desert when considering low accessibilty at 1 and 20 miles
dat_geo$LA1and20 <- as.factor(dat_geo$LA1and20)
levels(dat_geo$LA1and20) <- c('No', 'Yes')

ggplotly(ggplot(dat_geo) +
      geom_sf(aes(geometry=geometry, fill = LA1and20, color = LA1and20)) +
      geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
      geom_sf(data=loudoun_sf,fill="transparent",color="red",size=0.5)+
      labs(title="NoVA, 2015 \n Food desert flag",subtitle="")+
      scale_fill_viridis(discrete=T,name = "Flag",  guide = guide_legend(reverse=TRUE))+
      scale_color_viridis(discrete=T,name = "Flag",  guide = guide_legend(reverse=TRUE))+
      theme_map()+
      theme(legend.position=c(0.905,0.79)) )

## Low income population count beyond 1 mile for urban areas 
## or 20 miles for rural areas from supermarket
dat_geo <- cbind(dat_geo, LALOWI1_10_q  = f_bin(dat_geo$LALOWI1_10, bins = c(0,1,61,500, 1700)))
ggplotly(ggplot(dat_geo) +
           geom_sf(aes(geometry=geometry, fill = LALOWI1_10_q, color = LALOWI1_10_q)) +
           geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
           geom_sf(data=loudoun_sf,fill="transparent",color="red",size=0.5)+
           labs(title="NoVA, 2015 \n low income population count with low food access")+
           scale_fill_viridis(discrete=T,name = "count",  guide = guide_legend(reverse=TRUE))+
           scale_color_viridis(discrete=T,name = "count",  guide = guide_legend(reverse=TRUE))+
           theme_map()+
           theme(legend.position=c(0.905,0.79)) )


