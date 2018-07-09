# ============================= Packages required =============================


library(tidyverse)


# ============================ Load in data ===================================


# growth.rates.csv is a data file containing interannual changes in species 
# abundance.


growth.rate <- read.csv("Data/species.data.csv", header = T)

# ../ = go back a folder
# / go forward a folder


# ============================ Format data ====================================