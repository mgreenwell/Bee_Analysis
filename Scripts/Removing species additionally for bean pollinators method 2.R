# ============================ Required Packages ==============================


library(tidyverse)
library(zoo)


# ============================= Load in data ==================================


# scaled-species_data is a datfile containing population dynamics
# of x bee species scaled witha mean on 0 and sd of 1

species_data <- read.csv(
  "Outputs/scaled_species_data.csv", 
  header = T, strip.white=TRUE)


# ============================= Format data ===================================


# Remove unwanted species
# Only interested in bees that pollinate beans
# Remoe unnecceary columns
#Only need species, year & growth.rate.dif

data <- species_data %>% 
  filter(Field.Bean == 1) %>%
  select(species, year, growth.rate.dif)


# ========================== Writing loops ====================================

# Create list of species from growth.rate data

bean_species_list <- unique(data$species)

deficit <- NULL
# Write for loop
# loop creates a subset of data removing one species each time (species i)
# Plots yearly mean growth rates minus species i each time
# Creates n plots where n is number of species in list

for(i in bean_species_list){  # Open loop
  
  
  # a gets a subset of growth.rate data minus species i.
  
  bean_removed_species <- subset(data, species != i) 
  
  
  # Growth.rate.mean gets removed_species 
  # removed_species is grouped by year
  # yearly mean growthrates of all species are calulated
  
  bean_growth_rate_mean <- bean_removed_species %>% 
    group_by(year) %>%
    summarise(mean.growth.rate = mean(growth.rate.dif, na.rm = TRUE))
  
  
  # Set all values above theta (-0.1 in this example) to equal theta
  
  bean_growth_rate_mean$mean.growth.rate[bean_growth_rate_mean$mean.growth.rate > -0.1] <- -0.1
  
  
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
  
  
  # Create a new df of the order of years in the data
  
  id <- order(bean_growth_rate_mean$year)
  
  # Cacluate area under the curve i.e. area between line and zero
  
  AUC <-
    sum(
      diff(bean_growth_rate_mean$year[id]) * 
        rollmean(bean_growth_rate_mean$mean.growth.rate[id], 2)
    )
  
  
  # create new df where all points have the value of theta
  
  y2 <- rep(-0.1, 31)
  df<- cbind(bean_growth_rate_mean$year,y2)
  df <- as.data.frame(df)
  
  # Plot
  
  # ggplot(df, aes(growth.rate.mean$year, y2)) +geom_line() 
  
  
  # Calculate area between theta and zero
  
  AUC2 <- sum(diff(bean_growth_rate_mean$year[id])*rollmean(y2[id],2))
  
  
  # Subtract area between theta and zero from original calculation of AUC to get only area
  # between the line and theta.
  
  deficit_value <- AUC-AUC2
  
  
  # Set as datafram
  
  deficit_value <- as.data.frame(cbind(deficit_value, i))
  
  
  # Bind to deficit dataframe. Each loop adds new rows with a different species removed
  
  deficit  <- rbind(deficit_value, deficit)
  
  
} # Close loop


# See calculated deficits
deficit


# Only works if theta less than zero 
# need to work out how to calc if theta greater than zero.


