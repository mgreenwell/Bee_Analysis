library(tidyverse)

species_data <- read.csv("Outputs/scaled_species_data.csv", header = T, strip.white=TRUE)

data <- species_data %>% 
  filter(Field.Bean == 1) %>%
  select(species, year, growth.rate.dif)

species_list<-unique(data$species)
species_list <- as.data.frame(species_list)

oo_thing <- NULL

num_list <- 1:6
for (x in num_list){
  
  mysample <- NULL
  data_mean <-NULL
  filtered_data <- NULL
  for (i in 1:100){
    
    mysample <-(species_list[sample(1:nrow(species_list), x, replace=FALSE),])
    mysample <- as.data.frame(mysample)
    
    filtered_data <- filter(data, species %in% mysample$mysample)
    
    data_mean <- filtered_data %>% 
      group_by(year) %>%
      summarise(mean_gr = mean(growth.rate.dif, na.rm = TRUE),
                nremoved = 6-x)
    data_mean <- as.data.frame(data_mean)
    data_mean
    
    deficit <- data_mean %>% 
      mutate( theta = -0.1, deficit = mean_gr - theta)
    deficit

    deficit <- deficit %>% filter(deficit < 0)

    total_deficit <- sum(deficit$deficit)
    total_deficit<-as.data.frame(total_deficit)
    total_deficit$species_removed <- 6-x
    oo_thing <- rbind(oo_thing, total_deficit)
  }
}
oo_thing
oo_thing <- oo_thing %>% 
  group_by(species_removed) %>%
  summarise(mean_total_deficit = mean(total_deficit),
            sd_total_deficit = sd(total_deficit),
            n = n())

oo_thing <- mutate(oo_thing, s.e. = sd_total_deficit / sqrt(n))

oo_thing
ggplot(oo_thing, aes(x = species_removed, y = mean_total_deficit)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_total_deficit - s.e.,
                    ymax = mean_total_deficit + s.e.),
                width = .1) +
  theme_classic() + 
  labs(x ="Number of species removed from system", y = "Deficit (units below theta)")
