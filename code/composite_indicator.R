#######################################################################
# see page 89 secion 6.1 of ``Constructing_composite_indicators.pdf''
#######################################################################

## The following code is from
## https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html#:~:text=In%20the%20R%20software%20factor,specified%20by%20the%20argument%20factors%20.

food <- read.csv("https://userpage.fu-berlin.de/soga/300/30100_data_sets/food-texture.csv",
                 row.names = "X")
## food is a dataset with 5 colums/factors
str(food)
food <- scale(food) ## standardize the data

## factor analysis with rotation type varimax
## the number of factors should be defined by some theoretical model/pre-assumptions
food.fa <- factanal(food, factors = 2, rotation = 'varimax')

## calculate the composite index 
Comp_index <- food %*% food.fa$loadings 
## loadings are the coefficients of linear combination of two underlying factors
## this step transforms the design matrix from 5d to 2d

## the next step doesn't make sense to me
SS_loadings <- colSums(food.fa$loadings^2)/sum(colSums(food.fa$loadings^2))
Comp_index <- as.matrix(food) %*% food.fa$loadings %*% SS_loadings

