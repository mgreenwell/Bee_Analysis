# ============================= Packages required =============================


library(tidyverse)


# ============================ Load in data ===================================


# growth.rates.csv is a data file containing interannual changes in species 
# abundance.


species_data <- read.csv("Data/species.data.csv", header = T, strip.white=TRUE)

# ../ = go back a folder
# / go forward a folder


# ============================ Format data ====================================


species_data$species <- gsub(" ", "_", species_data$species)


# ===================== Calculating Growth Rates ==============================


# create a list of species codes that is the same as those in species_data

species.list <- unique(species_data$species)


# Create empty data frame in which to put loop output

growth.rate=NULL          


# Open loop
# for i in species.list = everytime i occurs within species list

for(i in species.list){ 
  
  
  # create dataframe sp.data. where sp.data is the same as species_data with
  # specie set to i
  
  sp.data <- species_data[species_data$species == i,]
  
  
  # create 2 new columns in sp.data.
  # New column 1 = tr0obs column offset by one 
  # i.e. the value of the year  previous
  # New column 2 = tr0obs column - tr0obs offset by one column
  
  sp.data <- sp.data %>%
    mutate(col.ind.prev.year = lag(occupancy)) %>% 
    mutate(growth.rate.dif = occupancy - col.ind.prev.year)
  
  
  # Bind the two data frames growth.rate and sp.data together
  growth.rate = rbind(growth.rate,sp.data)
  
}  # Close loop


# ============================ Format data ====================================


# Remove unnecessary columns from data

bee_species_data <- growth.rate %>% select(-(col.ind.prev.year), -(occupancy))

head(bee_species_data)
tail(bee_species_data)

# Set NA values to 0
# NA values are for the 1st year of data therefore no differential.

bee_species_data[is.na(bee_species_data)] <- 0


# Scaling data around mean of 0 and sd of 1

# Create empty dataframe to put scaled data into

scaled <- NULL


# Create list of species names

species.list <- unique(bee_species_data$species)


# Open for loop
# Loop creates new df each time of scaled values. 
# Binds scaled values onto scaled dataset

for (i in species.list){
  
  # subset gets growth.rate data for one species (species i)
  
  subset <- filter(bee_species_data, species == i)
  
  
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


# Plot mean growth rate differentials

mean.growth.rate.plot <- ggplot(
  growth.rate.mean, aes(
    x = year,
    y = mean.growth.rate)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1985, 2015), ylim = c(-1, 1.5)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(-1, 1.5, 0.1)) +
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
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1, 1.5)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1, 1.5, 0.1)) +
    geom_line() +
    theme_classic() +
    ggtitle(i)
  
  
  # Plot graph
  
  plot(mean.growth.rate.plot)
  
} # Close loop



# =========================== Bean pollinators ================================


bean_species <- scaled %>% filter(Field.Bean == 1)


# Create list of species from growth.rate data

bean_species_list <- unique(bean_species$species)


# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in bean_species_list){  # Open loop
  
  
  # a gets a subset of growth.rate data minus species i.
  
  bean_removed_species <- subset(bean_species, species != i) 
  
  
  # Growth.rate.mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  bean_growth_rate_mean <- bean_removed_species %>% 
    group_by(year) %>%
    summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))
  
  
  # Create plot of mean yearly growth rates across all species using subset of data
  
  
  bean_mean_growth_rate_plot <- ggplot(
    bean_growth_rate_mean, aes(
      x = year,
      y = mean.growth.rate)) + 
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1.5, 2)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1.5, 2, 0.1)) +
    geom_line() +
    theme_classic() +
    ggtitle(i, "Removed") 
  
  # Plot graph
  
  plot(bean_mean_growth_rate_plot)
  
} # Close loop



# Plot and calculate deficits


# Create list of species from growth.rate data

bean_species_list <- unique(bean_species$species)

deficit_table <- NULL
# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in bean_species_list){  # Open loop
  
  
  # a gets a subset of growth.rate data minus species i.
  
  bean_removed_species <- subset(bean_species, species != i) 
  
  
  # Growth.rate.mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  bean_growth_rate_mean <- bean_removed_species %>% 
    group_by(year) %>%
    summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))
  bean_growth_rate_mean
  
  # Create plot of mean yearly growth rates across all species using subset of data
  
  
  bean_mean_growth_rate_plot <- ggplot(
    bean_growth_rate_mean, aes(
      x = year,
      y = mean.growth.rate)) +
    geom_hline(aes(yintercept= -0.1) , colour="red") +
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1.5, 2)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1.5, 2, 0.1)) +
    geom_col() +
    theme_classic() +
    ggtitle(i, "Removed") 
  
  
  # Plot graph
  
  plot(bean_mean_growth_rate_plot)
  
  
  deficit <- bean_growth_rate_mean %>% mutate( theta = -0.1, deficit = mean.growth.rate - theta)
  deficit <- deficit %>% filter(deficit < 0)
  
  
  a <- sum(deficit$deficit)
  b <- i
  deficit.all <- cbind(a, b)
  
  deficit_table <- rbind(deficit_table, deficit.all)
  
} # Close loop
deficit_table