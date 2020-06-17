setwd("~/dspg2020Loudon")

library(tidycensus)
library(tidyverse)
library (stringr)

# Potential Variable Tables (from FeedingAmerica)
# B14006 (non- undergraduate student poverty rate), 
# C17002 (ratio of income to poverty level), 
# B19013 (median income), 
# DP04 (homeownership rate), 
# DP05 (percent African American and percent Hispanic)
# S1810 (disability rate)
# S2301 (Unemployment)


#show available variables
v17 <- load_variables(2018,"acs5",cache=T)
View(v17)

#create a vector of variables to return
vars<-c(population="B01003_001", SNAP="B19123_002")

#"get_acs" creates a census api call
acs<-get_acs(geography = "tract",
             state="VA",
             variables = vars,
             #table = "S2301",
             survey = "acs5",
             key = key,
             show_call = T)
head(acs)

#separate the variable column into seperate variables and separate "NAME" into census tract, county, and state
acs_pivot<-acs%>%
  select (-moe)%>%
  pivot_wider(names_from = variable,values_from=estimate)%>%
  separate(NAME, into=c("Census_tract","County","State"), sep = ", ")
head(acs_pivot)
