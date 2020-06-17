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

#show available variables in a particular ACS survey
v17 <- load_variables(2018,"acs5",cache=T)
View(v17)

#"get_acs" cannot return multiple variable tables at once.  To get around this, the following function
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
#different columns with pre-defined column names (NAME_col_names).

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#To request specific variables (rather than tables of variables), use the following code:

#create a vector of variables to return
vars<-c(population="B01003_001", SNAP="B19123_002")

#"get_acs" creates a census api call using the vector of variables specified above
acs<-get_acs(geography = "tract",
             state="VA",
             variables = vars,
             survey = "acs5",
             key = key,
             show_call = T)


#To request a table or list of tables, use the following code:

tables<-c("B14006","C17002","B19013","DP04","DP05","S1810","S2301")
acs_tract<-acs_tables(tables = tables,
                      key = key,
                      geography = "tract",
                      state = "VA")

colnames=c("Census_tract","County","State")
acs_tract_wide<-acs_wide(data=acs_tract,NAME_col_names = colnames)

acs_state<-acs_tables(tables = tables,
                      key = key,
                      geography = "state")
colnames="State"
acs_state_wide<-acs_wide(data=acs_state,NAME_col_names = colnames)


