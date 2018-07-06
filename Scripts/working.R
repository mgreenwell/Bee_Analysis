
# ============================= Packages required =============================


library(tidyverse)


# ============================ Load in data ===================================


# growth.rates.csv is a data file containing interannual changes in species 
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


# === Plot mean of all species interannual changes in abundance - 1 species ===


# Create list of species from growth.rate data

species.list <- unique(growth.rate$species)


# Write for loop
  # loop creates a subset of data removing one species each time (species i)
  # Plots yearly mean growth rates minus species i each time
  # Creates n plots where n is number of species in list

for(i in species.list){  # Open loop
  
  
  # a gets a subset of growth.rate data minus species i.
  
    removed_species <- subset(growth.rate, species != i) 
  
    
  # Growth.rate.mean gets removed_species 
    # removed_species is grouped by year
    # yearly mean growthrates of all species are calulated
    
  growth.rate.mean <- removed_species %>% 
    group_by(year) %>%
    summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))
  
  
  # Create plot of mean yearly growth rates across all species using subset of data
  mean.growth.rate.plot <- ggplot(
    growth.rate.mean, aes(
      x = year,
      y = mean.growth.rate)) + 
    geom_line() + 
    scale_x_continuous(breaks=seq(1975,2015,5)) +
    ggtitle(i)
  
  
  # Plot graph
  
  plot(mean.growth.rate.plot)
  
} # Close loop

warnings()

