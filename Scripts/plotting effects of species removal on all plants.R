# ========================= Required packages =================================


library(tidyverse)


# ========================= Read in data ======================================


# All three datasets created from scripts
# Data are the effects of sequential species removal on the level of deficit 
  # below a defined value of theta

apple <- read.csv("Outputs/apple_pollinators_additonal_removal.csv", row.names = 1)
bean <- read.csv("Outputs/bean_pollinators_additonal_removal.csv", row.names = 1)
oilseed_rape <- read.csv("Outputs/oilseed_rape_pollinators_additonal_removal.csv", row.names = 1)


# =============================== Format data =================================


# Final column of each data set needs to be the same to add datasets together

apple <- rename(apple, plant = apple)
bean <- rename(bean, plant = bean)
oilseed_rape <- rename(oilseed_rape, plant = oilseed_rape)


# Bind datasets together

all_species <- rbind(apple, bean, oilseed_rape)


# ==== Plot line plot to show effects of sequential species loss on each system


ggplot(all_species,
       aes(x = species_removed, 
           y = mean_total_deficit,
           colour=plant)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_total_deficit - s.e.,
                    ymax = mean_total_deficit + s.e.),
                width = .4) +
  theme_classic() +
  labs(x = "Number of species removed from system",
       y = "Deficit (units below theta)")
