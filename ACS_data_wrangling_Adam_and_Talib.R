setwd("~/dspg2020Loudon")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(olsrr)
library(stats)
library(psych)

# Potential variable tables for predicting food insecurity (from FeedingAmerica):
# B14006 (non- undergraduate student poverty rate), 
# C17002 (ratio of income to poverty level), 
# B19013 (median income), 
# DP04 (homeownership rate), 
# DP05 (percent African American and percent Hispanic)
# S1810 (disability rate)
# S2301 (Unemployment)

#show available variables in a particular ACS survey
acs5<-load_variables(2009, "acs5", cache=T)
View(acs5)

acs5_subject <- load_variables(2018, "acs5/subject", cache=T)
View(acs5_subject)

acs5_profile<- load_variables(2018, "acs5/profile", cache=T)
View(acs5_profile)


#The "get_acs" function cannot return multiple variable tables at once.  To get around this, the following function
#calls "get_acs" on a vector of table names. It returns a dataframe of all the tables bound 
#together.  Note: the function requires a vector of table numbers, a census API key, and 
#a geographical unit.  The user can add other parameters as well.

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

#This function cleans the data returned from a census API call.  Specifically, 
#it separates the variable column into seperate variables, and it separates "NAME" into 
#different columns with pre-defined column names (NAME_col_names). Note: the function also
#drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#acs_years retrieves individual variables (or a list) across a series of years.
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


#This function uses two previously defined functions (acs_tables and acs_wide) to return multiple 
#variable tables across multiple years in one single tibble.  A couple of notes: the way that 
#get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
#For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have 
#not included "geometry" in the function.  If the user includes geometry, he/she may need 
#to modify the call to acs_wide.


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


tables<-c("B14006","C17002","B19013","DP04","DP05","S1810","S2301")
years<-c(2013,2014,2015,2016,2017,2018)
colnames=c("Census_tract","County","State")

acs_Loudon<-acs_years_tables(tables=tables,
                             years=years,
                             key=.key,
                             geography="tract",
                             state="VA",
                             county="Loudoun",
                             NAME_col_names = colnames)

#To request specific variables (rather than tables of variables), use the following code:

#create a vector of variables to return
vars<-c(SNAP_="B19058_002","S1701_C02_001")



#"get_acs" creates a census api call using the vector of variables specified above
acs<-get_acs(geography = "tract",
             state="VA",
             county = "Loudoun county",
             variables = "B19058_002",
             survey = "acs5",
             key = .key,
             year=2009,
             output = "wide",
             show_call = T,
             geometry = T,
             keep_geo_vars = T)

#Separate the NAME column into Census_tract, County, and State
colnames=c("Census_tract","County","State")
acs<-separate(acs,NAME.y, into=colnames, sep = ", ")

#To make a map:
ggplot(acs, aes(fill = SNAP_E/households_E, color = SNAP_E/households_E)) +
  geom_sf() +
  coord_sf(crs = 26914)+
  labs(title="Loudoun County",subtitle="Households receiving SNAP")+
  theme(legend.title = element_blank())



#To request a table or list of tables, use the following code, which returns variables for
#all census tracts in VA:
acs_tract<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is tract
                      geography = "tract",
                      #data restricted to the state of VA
                      state = "VA",
                      county="Loudoun",
                      geometry=T)


#Changes the resulting data frame from long to wide format and drops MOE
colnames=c("Census_tract","County","State")
acs_tract_wide<-acs_wide(data=acs_tract,NAME_col_names = colnames)

#This code returns a list of tables for all states in the US across years.
acs_state<-acs_years_tables(tables = tables,
                            key = .key,
                            #geographic entity is "state."  Data includes all states.
                            geography = "state",
                            years=years,
                            NAME_col_names = colnames)


#To save csv files, uncomment the relevant line:
# write_csv(acs_state,"~/dspg2020Loudon/ACS_datasets/acs_state")
# write_csv(acs_state_wide,"~/dspg2020Loudon/ACS_datasets/acs_state_wide")
# write_csv(acs_tract,"~/dspg2020Loudon/ACS_datasets/acs_VA_tract")
# write_csv(acs_tract_wide,"~/dspg2020Loudon/ACS_datasets/acs_VA_tract_wide")
# write_csv(acs5,"~/dspg2020Loudon/ACS_datasets/acs5_variables")
# write_csv(acs5,"~/dspg2020Loudon/ACS_datasets/acs5_subject_variables")
# write_csv(acs5_subject,"~/dspg2020Loudon/ACS_datasets/acs5_subject_variables")
# write_csv(acs5_profile,"~/dspg2020Loudon/ACS_datasets/acs5_profile_variables")


#The following code pulls in CPS food security data from IPUMS
cps_00002 <- read.csv("~/Desktop/cps_00002.csv")
cps<-cps_00002%>%
  select(YEAR,CPSID,STATEFIP,FSSTATUS,FSSTATUSD)%>%
  filter(!is.na(FSSTATUSD))%>%
  filter(!FSSTATUSD %in% c(99,98))

#A state's food insecurity level is estimated by look at the ratio of households
#that score 3 or 4 on the food survey to all households surveyed.  The following 
#code groups food insecurity estimates by state and year.
FSbyState<-cps%>%
  mutate(LowSecurity=FSSTATUSD%in%c(3,4))%>%
  group_by(STATEFIP,YEAR)%>%
  summarize(InsecurityRate=mean(LowSecurity)*100)


# The following code is still in draft form

# FSbyState$STATEFIP<-as.integer(FSbyState$STATEFIP)
# 
# acs_tract_wide<-acs_tract_wide%>%
#   rename(STATEFIP=GEOID)
# acs_tract_wide$STATEFIP<-as.integer(acs_tract_wide$STATEFIP)
# 
# StateFoodInsecurity<-inner_join(FSbyState,acs_state_wide,by="STATEFIP")%>%
#   mutate(PovertyRate=(S1701_C02_001/S1701_C01_001)*100)%>%
#   mutate(MedianIncome=B19013_001)%>%
#   mutate(OwnRate=(DP04_0046/DP04_0045)*100)%>%
#   mutate(PerAfAm=(DP05_0038/DP05_0033)*100)%>%
#   mutate(PerHisp=(DP05_0071/DP05_0070)*100)%>%
#   mutate(DisRate=(S1810_C02_001/S1810_C01_001)*100)%>%
#   mutate(Unemployment=S2301_C04_021)%>%
#   mutate(SNAPRate=S2201_C04_001)
#   
# StateFoodInsecurity_loudon<-acs_tract_wide%>%
#   mutate(PovertyRate=(S1701_C02_001/S1701_C01_001)*100)%>%
#   mutate(MedianIncome=B19013_001)%>%
#   mutate(OwnRate=(DP04_0046/DP04_0045)*100)%>%
#   mutate(PerAfAm=(DP05_0038/DP05_0033)*100)%>%
#   mutate(PerHisp=(DP05_0071/DP05_0070)*100)%>%
#   mutate(DisRate=(S1810_C02_001/S1810_C01_001)*100)%>%
#   mutate(Unemployment=S2301_C04_021)%>%
#   mutate(SNAPRate=S2201_C04_001)
#  
# LoudonReduced<-StateFoodInsecurity_loudon%>%
#   select(GEOID, Census_tract, County, State, geometry,State,PovertyRate,MedianIncome,OwnRate,PerAfAm,PerHisp,DisRate,SNAPRate,Unemployment)
# 
# 
# LoudonReducedPred<-StateFoodInsecurity_loudon%>%
#   select(PovertyRate,MedianIncome,OwnRate,PerAfAm,PerHisp,DisRate,SNAPRate,Unemployment)
# LoudonReducedPred<-as.data.frame(LoudonReducedPred)
# 
# FoodInsecurity<-c()
# for(i in 1:length(LoudonReducedPred$PovertyRate)){
#   p<-predict(Model,newdata = LoudonReducedPred[i,])
#   FoodInsecurity[i]<-p
# }
# 
# LoudonReduced<-cbind(LoudonReduced,FoodInsecurity)
# ggplot(LoudonReduced, aes(fill = FoodInsecurity, color = FoodInsecurity,geometry=geometry)) +
#   geom_sf() +
#   coord_sf(crs = 26914)+
#   labs(title="Loudoun County",subtitle="Households receiving SNAP")+
#   theme(legend.title = element_blank())+
#   scale_fill_viridis_c()+
#   scale_color_viridis_c()
# 
# 
# 
# StateInsecurity_Reduced<-StateFoodInsecurity%>%
#   select(STATEFIP,InsecurityRate,State,PovertyRate,MedianIncome,OwnRate,PerAfAm,PerHisp,DisRate,SNAPRate,Unemployment)
# 
# Model<-lm(InsecurityRate~PovertyRate+MedianIncome+OwnRate+PerAfAm+PerHisp+DisRate+SNAPRate+Unemployment,data=StateInsecurity_Reduced)
# summary(Model)
# ols_plot_resid_fit(Model)
# pairs.panels(StateInsecurity_Reduced[,-c(1,3)],method = "pearson", density = T, hist.col="blue", ellipses=T)

