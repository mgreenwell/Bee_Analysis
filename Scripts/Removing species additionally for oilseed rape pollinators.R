# ============================ Required Packages ==============================


library(tidyverse)


# ============================= Load in data ==================================


# scaled-species_data is a datfile containing population dynamics
# of x bee species scaled witha mean on 0 and sd of 1

species_data <- read.csv(
  "Outputs/scaled_species_data.csv", 
  header = T, strip.white=TRUE)

head(species_data)
# ============================= Format data ===================================


# Remove unwanted species
# Only interested in bees that pollinate oilseed rape
# Remoe unnecceary columns
#Only need species, year & growth.rate.dif

data <- species_data %>% 
  filter(Oilseed.Rape == 1) %>%
  select(species, year, growth.rate.dif)


# ========================== Writing loops ====================================


# create list of unique species names that occur in data

species_list<-unique(data$species)
species_list <- as.data.frame(species_list)


# Create an empty dataframe

bean_species_removal <- NULL


# Create list on numbers from 1 to 30 (number of species in dataframe)

num_list <- 1:30


# Loops create a table of mean deficit below a value of theta, sd and n
# First loop sets the number of species to be removed
# Second loop repeats the code inside loop 100 times
# Second loop removes a number of species from the dataframe depending on the 
# value of x in first loop
# Calculates mean amount of deficit when x species are removed at random x 100
# After looping 100 times values of x changes and loop repeats 100 times
# Loops continue until x has been all values between 1 and 6


# Open for loop 1
# For each number in 1 to 30

for (x in num_list){
  
  
  # Create three empty dataframes
  
  mysample <- NULL
  data_mean <-NULL
  filtered_data <- NULL
  
  
  # open loop 2
  # loop repeats code 100 times with a value of x
  
  
  for (i in 1:100){
    
    
    # mysample gets a random selection of x species,
    # where x is a number from 1:30
    
    mysample <-(species_list[sample(1:nrow(species_list), x, replace=FALSE),])
    mysample <- as.data.frame(mysample)
    
    
    # Create new dataframe where only species from random sample are included
    
    filtered_data <- filter(data, species %in% mysample$mysample)
    
    
    # create new dataframe where mean growth rate differentials are calculated
    # for each year
    # Also create column showing how many species have been removed in the loop
    
    data_mean <- filtered_data %>% 
      group_by(year) %>%
      summarise(mean_gr = mean(growth.rate.dif, na.rm = TRUE),
                nremoved = 30-x)
    
    data_mean <- as.data.frame(data_mean)
    
    
    # Create new dataframe whereby a value for theta is set
    # Using theta the total amount where growth rate differential is lower 
    # than theta is calculated
    
    deficit <- data_mean %>% 
      mutate( theta = -0.1, deficit = mean_gr - theta)
    
    
    # Only interested in negative deficit, all values above zero are credit
    
    deficit <- deficit %>% filter(deficit < 0)
    
    
    # Calculate the total amount of deficit
    
    total_deficit <- sum(deficit$deficit)
    total_deficit<-as.data.frame(total_deficit)
    
    # Add new column indicating how many species have been removed from analysis
    
    total_deficit$species_removed <- 30-x
    
    
    # Bind total deficit dataframe to bean_species_removal
    
    bean_species_removal <- rbind(bean_species_removal, total_deficit)
    
    
  } # close loop 1 
} # close loop 2


# ========================== Plotting line plot ===============================

# bean_species_removal gets bean_species_removal that has been grouped by the 
# number of species removed then mean deficits calculated, plus sd and n

bean_species_removal <- bean_species_removal %>% 
  group_by(species_removed) %>%
  summarise(mean_total_deficit = mean(total_deficit),
            sd_total_deficit = sd(total_deficit),
            n = n())

# calculate standard error 

bean_species_removal <- mutate(bean_species_removal, 
                               s.e. = sd_total_deficit / sqrt(n))


# Create plot to show effect of additional species removal on levels of deficit

ggplot(bean_species_removal,
       aes(x = species_removed, 
           y = mean_total_deficit)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_total_deficit - s.e.,
                    ymax = mean_total_deficit + s.e.),
                width = .1) +
  theme_classic() +
  labs(x = "Number of species removed from system",
       y = "Deficit (units below theta)")
