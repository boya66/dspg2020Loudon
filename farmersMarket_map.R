
setwd("C:/Users/Admin/Documents/DSPG/Loudoun/GitHub/dspg2020Loudon")

library(dplyr)
library(tidyverse)

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


farmersMarket_data <- read.csv("data/MarketMaker_Farmers Markets_Virginia.csv")
  
farmersMarket_data_filtered <- farmersMarket_data %>%
  filter(County %in% nova_counties)
