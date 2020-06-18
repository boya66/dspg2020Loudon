setwd("~/dspg2020Loudon")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)

# Potential variable tables for predicting food insecurity (from FeedingAmerica):
# B14006 (non- undergraduate student poverty rate), 
# C17002 (ratio of income to poverty level), 
# B19013 (median income), 
# DP04 (homeownership rate), 
# DP05 (percent African American and percent Hispanic)
# S1810 (disability rate)
# S2301 (Unemployment)

#show available variables in a particular ACS survey
acs5<-load_variables(2018, "acs5", cache=T)
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


#To request specific variables (rather than tables of variables), use the following code:

#create a vector of variables to return
vars<-c(households_= "B19058_001",SNAP_="B19058_002")

#"get_acs" creates a census api call using the vector of variables specified above
acs<-get_acs(geography = "tract",
             state="VA",
             county = "Loudoun county",
             variables = vars,
             survey = "acs5",
             key = .key,
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
tables<-c("B14006","C17002","B19013","DP04","DP05","S1810","S2301")
acs_tract<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is tract
                      geography = "tract",
                      #data restricted to the state of VA
                      state = "VA")


#Changes the resulting data frame from long to wide format and drops MOE
colnames=c("Census_tract","County","State")
acs_tract_wide<-acs_wide(data=acs_tract,NAME_col_names = colnames)

#This code returns a list of variables for all states in the US.
acs_state<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is "state."  Data includes all states.
                      geography = "state")
colnames="State"
acs_state_wide<-acs_wide(data=acs_state,NAME_col_names = colnames)

#To save csv files, uncomment the relevant line:
# write_csv(acs_state,"~/dspg2020Loudon/ACS_datasets/acs_state")
# write_csv(acs_state_wide,"~/dspg2020Loudon/ACS_datasets/acs_state_wide")
# write_csv(acs_tract,"~/dspg2020Loudon/ACS_datasets/acs_VA_tract")
# write_csv(acs_tract_wide,"~/dspg2020Loudon/ACS_datasets/acs_VA_tract_wide")
# write_csv(acs5,"~/dspg2020Loudon/ACS_datasets/acs5_variables")
# write_csv(acs5,"~/dspg2020Loudon/ACS_datasets/acs5_subject_variables")
# write_csv(acs5_subject,"~/dspg2020Loudon/ACS_datasets/acs5_subject_variables")
# write_csv(acs5_profile,"~/dspg2020Loudon/ACS_datasets/acs5_profile_variables")


