
# ============================= Packages required =============================


library(tidyverse)


# ============================ Load in data ===================================


# growth.rates.csv is a data file containing interannual changes in sepcies 
# abundance.


growth.rate <- read.csv("Data/growth.rates.csv", header = T)

# ../ = go back a folder
# / go forward a folder


# ============================ Format data ====================================


# Remove rows that are not needed (tr0obs, col.ind.prev.year)

growth.rate <- select(growth.rate, species, year, growth.rate.dif)


# ========== Plot all species interannual changes in abundance ================


growth.rate.plot <-ggplot(
  growth.rate, aes(
    x = year,
    y = growth.rate.dif,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5))

growth.rate.plot


# ======== Plot mean of all species interannual changes in abundance ==========


# create table of mean growth rate differenctials grouped by year

growth.rate.mean <- growth.rate %>% 
  group_by(year) %>%
  summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))


# Plot mean growth rate differentials

mean.growth.rate.plot <- ggplot(
  growth.rate.mean, aes(
    x = year,
    y = mean.growth.rate)) + 
  geom_line() + 
  scale_x_continuous(breaks=seq(1975,2015,5))

mean.growth.rate.plot


# 


growth.rate <- read.csv("C://Users//dp005352//Dropbox//PhD//R_Projects//Synchrony_Analysis//Outputs//growth.rates.csv", header = T)


# ============================ Format data ====================================


# Remove rows that are not needed (tr0obs, col.ind.prev.year)

growth.rate <- select(growth.rate, species, year, growth.rate.dif)



species.list <- unique(growth.rate$species)
species.list

head(growth.rate)

tail(growth.rate)

subset(growth.rate, species != i)

for(i in species.list){
  
  #sp.data <- growth.rate[growth.rate$species == i,]
  
  a <- subset(growth.rate, species != i) 
  
  growth.rate.mean <- a %>% 
    group_by(year) %>%
    summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))
  
  mean.growth.rate.plot <- ggplot(
    growth.rate.mean, aes(
      x = year,
      y = mean.growth.rate)) + 
    geom_line() + 
    scale_x_continuous(breaks=seq(1975,2015,5)) +
    ggtitle(i)
  
  plot(mean.growth.rate.plot)
  
}

warnings()
tail(a)
sp.data <- filter(species == i)}

head(growth.rate)
sp.data
growth.rate.mean <- growth.rate %>% 
  filter(species != 123) %>%
  group_by(year) %>%
  summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))

mean.growth.rate.plot <- ggplot(
  growth.rate.mean, aes(
    x = year,
    y = mean.growth.rate)) + 
  geom_line() + 
  scale_x_continuous(breaks=seq(1975,2015,5))

mean.growth.rate.plot
}
