#######################################################################
# see page 89 secion 6.1 of ``Constructing_composite_indicators.pdf''
#######################################################################

## The following code is from
## https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html#:~:text=In%20the%20R%20software%20factor,specified%20by%20the%20argument%20factors%20.

library(dplyr)
data_farmer <- read.csv('../data/TRscoreData/farmersMarket_TRscore.csv', header = T)
data_farmer <- data_farmer[,c(8, 20,22)]
names(data_farmer)[2:3] <- paste0('farmer', names(data_farmer)[2:3])

data_retailer <- read.csv('../data/TRscoreData/foodRetailer_TRscore.csv', header = T)
data_retailer <- data_retailer[, c(8, 20, 22)]
names(data_retailer)[2:3] <- paste0('retailer', names(data_retailer)[2:3])

data_pantry <- read.csv('../data/TRscoreData/foodPantry_TRscore.csv', header = T)
data_pantry <- data_pantry[, c(8, 20, 22)]
names(data_pantry)[2:3] <- paste0('pantry', names(data_pantry)[2:3])

data_svi <- read.csv('../data/Virginia_CDC_SVI.csv', header  =T)
data_svi <- rename(data_svi, GEOID = FIPS)
data_svi <- data_svi %>% select(GEOID, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES)

# will not use the transportation data that Adam upladed, it doesn't have the same length of 
# transportation <- read.csv('../data/ACS_NoVa_transportation', header = T)
data <- data_farmer
data <- data %>% left_join(data_retailer, by = 'GEOID') %>% 
  left_join(data_pantry, by = 'GEOID')%>% 
  left_join(data_svi, by = 'GEOID')

data$RPL_THEME1[data$RPL_THEME1 ==-999] <- rep(NA, sum(data$RPL_THEME1 ==-999))
data$RPL_THEME2[data$RPL_THEME2 ==-999] <- rep(NA, sum(data$RPL_THEME2 ==-999))
data$RPL_THEME3[data$RPL_THEME3 ==-999] <- rep(NA, sum(data$RPL_THEME3 ==-999))
data$RPL_THEME4[data$RPL_THEME4 ==-999] <- rep(NA, sum(data$RPL_THEME4 ==-999))
data$RPL_THEMES[data$RPL_THEMES ==-999] <- rep(NA, sum(data$RPL_THEMES ==-999))

data$RPL_THEME1[data$fa ==-999] <- rep(NA, sum(data$RPL_THEME1 ==-999))
## There are two ways to deal with missing values
# ## 1. impute the NAs with mean
# data_im <- data
# for(i in 1:ncol(data_im)){
#   data_im[is.na(data_im[,i]), i] <- mean(data_im[,i], na.rm = TRUE)
# }

## 2. remove all the NAs
library(tidyr)
data <- as.data.frame(data)
data_rm <- data %>% drop_na()

## take the second one
# data <- as.data.frame(data_im)
data <- data_rm
data_geocode <- data[,1]
data <- scale(data)[,-c(1)]

## factor analysis with rotation type varimax
## the number of factors should be defined by some theoretical model/pre-assumptions
data.fa <- factanal(data, factors = 3, rotation = 'varimax')
data.fa
## calculate the composite index 
# Comp_index <- food %*% food.fa$loadings 
# Comp_index <- data %*% data.fa$loadings
## loadings are the coefficients of linear combination of two underlying factors
## this step transforms the design matrix from 5d to 2d

## the next step takes the top three principle componentes and calculate the 
## average with weights of their loadings
SS_loadings <- colSums(data.fa$loadings^2)/sum(colSums(data.fa$loadings^2))
Comp_index <- as.matrix(data) %*% data.fa$loadings %*% SS_loadings

## write out the composite indicator
write.csv(data.frame(comp_index = as.numeric(Comp_index), GEOID = data_geocode), '../data/composite_indicator.csv')
