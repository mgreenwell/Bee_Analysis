# ============================= Packages required =============================


library(tidyverse)


# ============================ Load in data ===================================


# species.data.csv is a data file containing interannual changes in species 
# abundance.


species_data <- read.csv("Data/species.data.csv", header = T, strip.white=TRUE)

# ../ = go back a folder
# / go forward a folder


# ============================ Format data ====================================


species_data$species <- gsub(" ", "_", species_data$species)


# ============================ Format data ====================================


# Scaling data around mean of 0 and sd of 1

# Create empty dataframe to put scaled data into

scaled <- NULL


# Create list of species names

species_list <- unique(species_data$species)


# Open for loop
# Loop creates new df each time of scaled values. 
# Binds scaled values onto scaled dataset

for (i in species_list){
  
  # subset gets species_data data for one species (species i)
  
  subset <- filter(species_data, species == i)
  
  
  # Scale occupancy for species i
  
  subset$occupancy <- scale(subset$occupancy, center = T, scale = T)
  
  
  # Bind subset onto scaled dataset
  
  scaled = rbind(scaled, subset)
  
} # Close loop



# for some reason r is creating a dataframe within the dataframe
# to remove this use
# solution found at https://stackoverflow.com/questions/30896605/dataframe-within-dataframe

scaled <- do.call(data.frame, scaled)

write.csv(scaled, "Outputs/scaled_species_data.csv")


# ========== Plot all species interannual changes in abundance ================


occupancy_plot <-ggplot(
  scaled, aes(
    x = year,
    y = occupancy,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5))

occupancy_plot


# ======== Plot mean of all species interannual changes in abundance ==========


# create table of mean growth rate differenctials grouped by year

occupancy_mean <- scaled %>% 
  group_by(year) %>%
  summarise(mean_occupancy = mean(occupancy, na.rm = TRUE))

occupancy_mean <- as.data.frame(occupancy_mean)


# Plot mean growth rate differentials

mean_occupancy_plot <- ggplot(
  occupancy_mean, aes(
    x = year,
    y = mean_occupancy)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1985, 2015), ylim = c(-1, 1.5)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(-1, 1.5, 0.1)) +
  geom_line() +
  theme_classic() 


mean_occupancy_plot



# === Plot mean of all species interannual changes in abundance - 1 species ===


# Create list of species from scaled data

species_list <- unique(scaled$species)


# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in species_list){  # Open loop
  
  
  # a gets a subset of scaled data minus species i.
  
  removed_species <- subset(scaled, species != i) 
  
  
  # occupancy_mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  occupancy_mean <- removed_species %>% 
    group_by(year) %>%
    summarise(mean_occupancy = mean(occupancy, na.rm = TRUE))
  
  
  # Create plot of mean yearly growth rates across all species using subset of data
  

  mean_occupancy_plot <- ggplot(
    occupancy_mean, aes(
      x = year,
      y = mean_occupancy)) + 
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1, 1.5)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1, 1.5, 0.1)) +
    geom_line() +
    theme_classic() +
    ggtitle(i)
  
  
  # Plot graph
  
  plot(mean_occupancy_plot)
  
} # Close loop



# =========================== Bean pollinators ================================


bean_species <- scaled %>% filter(Field.Bean == 1)


# Create list of species from scaled data

bean_species_list <- unique(bean_species$species)


# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in bean_species_list){  # Open loop
  
  
  # a gets a subset of scaled data minus species i.
  
  bean_removed_species <- subset(bean_species, species != i) 
  
  
  # occupancy_mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  bean_occupancy_mean <- bean_removed_species %>% 
    group_by(year) %>%
    summarise(mean_occupancy = mean(occupancy, na.rm = TRUE))
  
  
  # Create plot of mean yearly growth rates across all species using subset of data
  
  
  bean_mean_occupancy_plot <- ggplot(
    bean_occupancy_mean, aes(
      x = year,
      y = mean_occupancy)) + 
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1.5, 2)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1.5, 2, 0.1)) +
    geom_line() +
    theme_classic() +
    ggtitle(i, "Removed") 
  
  # Plot graph
  
  plot(bean_mean_occupancy_plot)
  
} # Close loop



# Plot and calculate deficits


# Create list of species from scaled data

bean_species_list <- unique(bean_species$species)

deficit_table <- NULL
# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in bean_species_list){  # Open loop
  
  
  # a gets a subset of scaled data minus species i.
  
  bean_removed_species <- subset(bean_species, species != i) 
  
  
  # occupancy_mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  bean_occupancy_mean <- bean_removed_species %>% 
    group_by(year) %>%
    summarise(mean_occupancy = mean(occupancy, na.rm = TRUE))
  bean_occupancy_mean
  
  # Create plot of mean yearly growth rates across all species using subset of data
  
  
  bean_mean_occupancy_plot <- ggplot(
    bean_occupancy_mean, aes(
      x = year,
      y = mean_occupancy)) +
    geom_hline(aes(yintercept= -0.1) , colour="red") +
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(1985, 2015), ylim = c(-1.5, 2)) +
    scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
    scale_y_continuous(breaks=seq(-1.5, 2, 0.1)) +
    geom_col() +
    theme_classic() +
    ggtitle(i, "Removed") 
  
  
  # Plot graph
  
  plot(bean_mean_occupancy_plot)
  
  
  deficit <- bean_occupancy_mean %>% 
    mutate( theta = -0.1, deficit = mean_occupancy - theta)
  
  deficit <- deficit %>% filter(deficit < 0)
  
  
  a <- sum(deficit$deficit)
  b <- i
  deficit_all <- cbind(a, b)
  
  deficit_table <- rbind(deficit_table, deficit_all)
  
} # Close loop
deficit_table
