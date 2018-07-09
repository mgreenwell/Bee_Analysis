
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


# Set NA values to 0
  # NA values are for the 1st year of data therefore no differential.

growth.rate[is.na(growth.rate)] <- 0


# Scaling data around mean of 0 and sd of 1

# Create empty dataframe to put scaled data into

scaled <- NULL


# Create list of species names

species.list <- unique(growth.rate$species)


# Open for loop
# Loop creates new df each time of scaled values. 
# Binds scaled values onto scaled dataset

for (i in species.list){
  
  # subset gets growth.rate data for one species (species i)
  
  subset <- filter(growth.rate, species == i)
  
  
  # Scale growth.rate.dif for species i
  
  subset$growth.rate.dif <- scale(subset$growth.rate.dif, center = T, scale = T)
  
  
  # Bind subset onto scaled dataset
  
  scaled = rbind(scaled, subset)
  
} # Close loop


# for some reason r is creating a dataframe within the dataframe
# to remove this use
# solution found at https://stackoverflow.com/questions/30896605/dataframe-within-dataframe

scaled <- do.call(data.frame, scaled)




# ========== Plot all species interannual changes in abundance ================


growth.rate.plot <-ggplot(
  scaled, aes(
    x = year,
    y = growth.rate.dif,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5))

growth.rate.plot


# ======== Plot mean of all species interannual changes in abundance ==========

# create table of mean growth rate differenctials grouped by year

growth.rate.mean <- scaled %>% 
  group_by(year) %>%
  summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))

growth.rate.mean <- as.data.frame(growth.rate.mean)

head(growth.rate.mean)
is.numeric(growth.rate.mean$year)
is.numeric(growth.rate.mean$mean.growth.rate)
# Plot mean growth rate differentials

mean.growth.rate.plot <- ggplot(
  growth.rate.mean, aes(
    x = year,
    y = mean.growth.rate)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1975, 2015), ylim = c(-2, 2)) +
  scale_x_continuous(breaks = seq(1975, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(-2, 2, 0.1)) +
  geom_line() +
  theme_classic() 
  

mean.growth.rate.plot



# === Plot mean of all species interannual changes in abundance - 1 species ===


# Create list of species from growth.rate data

species.list <- unique(scaled$species)


# Write for loop
  # loop creates a subset of data removing one species each time (species i)
  # Plots yearly mean growth rates minus species i each time
  # Creates n plots where n is number of species in list

for(i in species.list){  # Open loop
  
  
  # a gets a subset of growth.rate data minus species i.
  
    removed_species <- subset(scaled, species != i) 
  
    
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
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1976, 2015), ylim = c(-2, 2)) +
    scale_x_continuous(breaks = seq(1976, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-2, 2, 0.1)) +
    geom_line() +
    theme_classic() +
    ggtitle(i)
  
  

  # Plot graph
  
  plot(mean.growth.rate.plot)
  
} # Close loop
