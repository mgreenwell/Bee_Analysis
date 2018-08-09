# ============================= Packages required =============================


library(tidyverse)


# ============================= Functions required =============================


# Calculate coefficient of variance
# https://www.r-bloggers.com/measure-of-relative-variability/

CV <- function(mean, sd){
  (sd/mean)*100
}


# ============================ Load in data ===================================


species_data <- read.csv("Data//species.data.csv", header = T, strip.white=TRUE)

# ../ = go back a folder
# / go forward a folder


# ============================ Format data ====================================


# Scale the data so that all species have the same mean and standard deviation
# Scales data with mean 0 and sd 1
# Scale data so that all values are positive by adding the most negative 
# value to all values
# Scale data between zero and one by dividing all values by highest value


# Create empty dataframe

scaled <- NULL


# Create list of species names

species_list <- unique(species_data$species)

# Open for loop
# Loop creates new df each time of scaled values. 
# Binds scaled values onto scaled dataset

for (i in species_list){
  
  # subset gets occupancy data (species_data) for one species (species i)
  
  subset <- filter(species_data, species == i)
  
  
  # Scale occuapncy data for species i around a mean of 0 and sd of 1
  
  subset$occupancy <- scale(subset$occupancy, center = T, scale = T)
  
  
  # Bind subset onto scaled dataset
  
  scaled = rbind(scaled, subset)
  
} # Close loop


# for some reason r is creating a dataframe within the dataframe
# to remove this use
# solution found at https://stackoverflow.com/questions/30896605/dataframe-within-dataframe

scaled <- do.call(data.frame, scaled)


# Find minimum  and maximum values of occupancy

min_occ <- min(scaled$occupancy, na.rm = T)
max_occ <- max(scaled$occupancy, na.rm = T)


# Add minimum value to all values then divide by highest value, all have same
# mean and variance but values between 0 and 1

scaled <- scaled %>%
  mutate(occupancy_added = (occupancy - min_occ)/(max_occ - min_occ)) %>%
  select(species, year, occupancy_added, Apple, Field.Bean, Oilseed.Rape) %>%
  rename(occupancy = occupancy_added)

summary(scaled$occupancy)

write.csv(scaled, "Outputs//scaled_positive_species_data_between_0_and_1.csv")


# ======================== ALL SPECIES / OILSEED RAPE =========================


# =================================* Format Data ==============================


oilseed_species_data <- scaled %>% filter(Oilseed.Rape == 1)


# Calcualte summed occupancy scores per year

oilseed_occupancy_sum <- oilseed_species_data %>% 
  group_by(year) %>%
  summarise(sum_occupancy = sum(occupancy, na.rm = TRUE))


# Convert from tibble to dataframe

oilseed_occupancy_sum <- as.data.frame(oilseed_occupancy_sum)


# Find maximum value of occupancy score

all_species_max_occupancy <- max(oilseed_occupancy_sum$sum_occupancy)


# Divide occipancy scores by maximum value to scale between zero and one

oilseed_occupancy_sum <- mutate(
  oilseed_occupancy_sum, 
  scaled_summed_occupancy = sum_occupancy / all_species_max_occupancy)


# ==================* Plot summed occupancy and mean summed ===================


oilseed_occupancy_plot <-ggplot(
  oilseed_species_data, aes(
    x = year,
    y = occupancy,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5)) +
  theme(legend.title=element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Species Occupancy")


oilseed_occupancy_plot

# Missing 16 rows error due to B. hypnorum having no data from 1985-2001



# Plot summed occupancy over time

oilseed_summed_occupancy_plot <- ggplot(
  oilseed_occupancy_sum, aes(
    x = year,
    y = scaled_summed_occupancy)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1985, 2015), ylim = c(0, 1.1)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1), expand = c(0,0)) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept=c(0.1, 0.25, 0.5, 0.75, 0.9), 
             linetype="dashed", color = "red") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Summed Species Occupancy")

oilseed_summed_occupancy_plot


# =========================* Calculate mean, sd and cv ========================


# Calcuate mean, sd and CV for oilseed data
# Bind into table with number of pollinators and plant type

sd_summed_occupancy <- sd(oilseed_occupancy_sum$scaled_summed_occupancy)
mean_summed_occupancy <- mean(oilseed_occupancy_sum$scaled_summed_occupancy)
CV_summed_occupancy <- CV(mean = mean_summed_occupancy, 
                          sd = sd_summed_occupancy)
n_pollinators <- 30
plant <- "Oilseed Rape"


oilseed_table <- as.data.frame(
  cbind(
    n_pollinators, 
    plant, 
    mean_summed_occupancy, 
    sd_summed_occupancy, 
    CV_summed_occupancy))

oilseed_table


# =================* Calculating deficits for variable theta ================


# Create list of theta options
# Theta at 10% of total summed to 90% of total summed

theta_list <- c(0.1,0.25,0.5,0.75,0.9)


# Create empty dataframe for loop output

oilseed_deficit_table <- NULL


# Open loop

for (i in theta_list){
  
  
  # oilseed_deficit gets oilseed_occupancy_sum
  oilseed_deficit <- oilseed_occupancy_sum %>% 
    
    # creates a new column based on theta  
    mutate(theta = i, 
           
           # Creates a column of deficits using theta and occupancy
           # Multiplies by -1 so that higher deficit is larger number
           deficit = (scaled_summed_occupancy - theta) *-1) %>%
    
    # Removes all values that are negative as are not deficits
    filter(deficit > 0) %>%
    
    # Adds all deficit values together
    summarise(summed_deficit =sum(deficit))
  
  # Creates vector based on i
  theta <- i
  
  # binds theta value (i) to value of oilseed deficit
  oilseed_theta_table <- cbind(theta, oilseed_deficit)
  
  #binds new table to the empty dataframe created previously
  oilseed_deficit_table <- rbind(oilseed_deficit_table, oilseed_theta_table)
  
} # Close loop

oilseed_deficit_table


# =========================== BEAN POLLINATORS ================================


# =================================* Format Data ==============================


bean_species_data <- scaled %>% filter(Field.Bean == 1)


# Calcualte summed occupancy scores per year

bean_occupancy_sum <- bean_species_data %>% 
  group_by(year) %>%
  summarise(sum_occupancy = sum(occupancy, na.rm = TRUE))


# Convert from tibble to dataframe

bean_occupancy_sum <- as.data.frame(bean_occupancy_sum)


# Find maximum value of occupancy score

all_species_max_occupancy <- max(bean_occupancy_sum$sum_occupancy)


# Divide occipancy scores by maximum value to scale between zero and one

bean_occupancy_sum <- mutate(
  bean_occupancy_sum, 
  scaled_summed_occupancy = sum_occupancy / all_species_max_occupancy)


# ==================* Plot summed occupancy and mean summed ===================


bean_occupancy_plot <-ggplot(
  bean_species_data, aes(
    x = year,
    y = occupancy,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5)) +
  theme(legend.title=element_blank(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Species Occupancy")



bean_occupancy_plot

# Missing 16 rows error due to B. hypnorum having no data from 1985-2001



# Plot summed occupancy over time

bean_summed_occupancy_plot <- ggplot(
  bean_occupancy_sum, aes(
    x = year,
    y = scaled_summed_occupancy)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1985, 2015), ylim = c(0, 1.1)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1), expand = c(0,0)) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept=c(0.1, 0.25, 0.5, 0.75, 0.9), 
             linetype="dashed", color = "red")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Summed Species Occupancy")

bean_summed_occupancy_plot


# =========================* Calculate mean, sd and cv ========================


# Calcuate mean, sdand CV for bean data
# Bind into table with number of pollinators and plant type

sd_summed_occupancy <- sd(bean_occupancy_sum$scaled_summed_occupancy)
mean_summed_occupancy <- mean(bean_occupancy_sum$scaled_summed_occupancy)
CV_summed_occupancy <- CV(mean = mean_summed_occupancy, 
                          sd = sd_summed_occupancy)
n_pollinators <- 6
plant <- "Field Bean"


bean_table <- as.data.frame(
  cbind(
    n_pollinators, 
    plant, 
    mean_summed_occupancy, 
    sd_summed_occupancy, 
    CV_summed_occupancy))

bean_table



# =================* Calculating deficits for variable theta ==================


# Create list of theta options
# Theta at 10% of total summed to 90% of total summed

theta_list <- c(0.1,0.25,0.5,0.75,0.9)


# Create empty dataframe for loop output

bean_deficit_table <- NULL


# Open loop

for (i in theta_list){
  
  
  # bean_deficit gets bean_occupancy_sum
  bean_deficit <- bean_occupancy_sum %>% 
    
    # creates a new column based on theta  
    mutate(theta = i, 
           
           # Creates a column of deficits using theta and occupancy
           # Multiplies by -1 so that higher deficit is larger number
           deficit = (scaled_summed_occupancy - theta) *-1) %>%
    
    # Removes all values that are negative as are not deficits
    filter(deficit > 0) %>%
    
    # Adds all deficit values together
    summarise(summed_deficit =sum(deficit))
  
  # Creates vector based on i
  theta <- i
  
  # binds theta value (i) to value of bean deficit
  bean_theta_table <- cbind(theta, bean_deficit)
  
  #binds new table to the empty dataframe created previously
  bean_deficit_table <- rbind(bean_deficit_table, bean_theta_table)
  
} # Close loop

bean_deficit_table


# =========================== APPLE POLLINATORS ===============================


# =================================* Format Data ==============================


apple_species_data <- scaled %>% filter(Apple == 1)


# Calcualte summed occupancy scores per year

apple_occupancy_sum <- apple_species_data %>% 
  group_by(year) %>%
  summarise(sum_occupancy = sum(occupancy, na.rm = TRUE))


# Convert from tibble to dataframe

apple_occupancy_sum <- as.data.frame(apple_occupancy_sum)


# Find maximum value of occupancy score

all_species_max_occupancy <- max(apple_occupancy_sum$sum_occupancy)


# Divide occipancy scores by maximum value to scale between zero and one

apple_occupancy_sum <- mutate(
  apple_occupancy_sum, 
  scaled_summed_occupancy = sum_occupancy / all_species_max_occupancy)


# ==================* Plot summed occupancy and mean summed ===================


apple_occupancy_plot <-ggplot(
  apple_species_data, aes(
    x = year,
    y = occupancy,
    colour = factor(species))) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1975, 2015, 5)) +
  theme(legend.title=element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Species Occupancy")


apple_occupancy_plot

# Missing 16 rows error due to B. hypnorum having no data from 1985-2001



# Plot summed occupancy over time

apple_summed_occupancy_plot <- ggplot(
  apple_occupancy_sum, aes(
    x = year,
    y = scaled_summed_occupancy)) + 
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(1985, 2015), ylim = c(0, 1.1)) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 2)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1), expand = c(0,0)) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept=c(0.1, 0.25, 0.5, 0.75, 0.9), 
             linetype="dashed", color = "red") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  labs(x = "Year",
       y = "Summed Species Occupancy")


apple_summed_occupancy_plot


# =========================* Calculate mean, sd and cv ========================


# Calcuate mean, sdand CV for apple data
# Bind into table with number of pollinators and plant type

sd_summed_occupancy <- sd(apple_occupancy_sum$scaled_summed_occupancy)
mean_summed_occupancy <- mean(apple_occupancy_sum$scaled_summed_occupancy)
CV_summed_occupancy <- CV(mean = mean_summed_occupancy, 
                          sd = sd_summed_occupancy)
n_pollinators <- 19
plant <- "Apple"


apple_table <- as.data.frame(
  cbind(
    n_pollinators, 
    plant, 
    mean_summed_occupancy, 
    sd_summed_occupancy, 
    CV_summed_occupancy))

apple_table


# =================* Calculating deficits for variable theta ==================


# Create list of theta options
# Theta at 10% of total summed to 90% of total summed

theta_list <- c(0.1,0.25,0.5,0.75,0.9)


# Create empty dataframe for loop output

apple_deficit_table <- NULL


# Open loop

for (i in theta_list){
  
  
  # apple_deficit gets apple_occupancy_sum
  apple_deficit <- apple_occupancy_sum %>% 
    
    # creates a new column based on theta  
    mutate(theta = i, 
           
           # Creates a column of deficits using theta and occupancy
           # Multiplies by -1 so that higher deficit is larger number
           deficit = (scaled_summed_occupancy - theta) *-1) %>%
    
    # Removes all values that are negative as are not deficits
    filter(deficit > 0) %>%
    
    # Adds all deficit values together
    summarise(summed_deficit = sum(deficit))
  
  # Creates vector based on i
  theta <- i
  
  # binds theta value (i) to value of apple deficit
  apple_theta_table <- cbind(theta, apple_deficit)
  apple_theta_table
  #binds new table to the empty dataframe created previously
  apple_deficit_table <- rbind(apple_deficit_table, apple_theta_table)
  
} # Close loop


# ====================== COMPLETE TABLES AND PLOTS =============================


# ==========================* Format theta table ==============================


# Add collumn with values matching those in pollination index table plant column

oilseed_deficit_table <- mutate(oilseed_deficit_table, species = "Oilseed Rape")
apple_deficit_table <- mutate(apple_deficit_table, species = "Apple")
bean_deficit_table <- mutate(bean_deficit_table, species = "Field Bean")


# Bind all rows together

new_table <-rbind(oilseed_deficit_table,
                  apple_deficit_table,
                  bean_deficit_table)


# Spread table to match pollination index table

data_table <- spread(new_table, theta, summed_deficit)


# Rename column headings

data_table <- rename(data_table, 
                     "plant" = "species")


# ====================* Format polliantion index table=========================


# Bind all dfs together as rows

pollination_index_table <- rbind(oilseed_table, apple_table, bean_table)

pollination_index_table


# ============================* Combine both tables============================ 


complete_table <- merge(pollination_index_table, data_table, by = "plant")

write.csv(complete_table, "Outputs//deficit_summary_table.csv")


# ================================* plots =====================================


oilseed_occupancy_plot
oilseed_summed_occupancy_plot

bean_occupancy_plot
bean_summed_occupancy_plot

apple_occupancy_plot
apple_summed_occupancy_plot


# Gather table together to make table that can be plotted

plotting_complete_table <- gather(
  complete_table, theta, deficit, "0.1", "0.25", "0.5", "0.75", "0.9")


# Plot total deficits split by theta value

total_deficit_plot <- ggplot(
  plotting_complete_table, 
  aes(plant, deficit, shape = theta)) + 
  geom_point(size = 4) +
  labs(x = "Pollinator Assemblage", 
       y = "Total Deficit") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))

total_deficit_plot
