# Make figure for number of species in my dataset for each family coloured by superfamily grouping
# Sep 2020

## Load libraries
library(tidyverse)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Get the number of species for each family 
## in the dataset
##------------------------------------------------

number_species <- 
  max_bf_overall %>% 
  count(Family)


##------------------------------------------------
## Subset the superfamily/infraoder for each 
## family and then join this with the number of 
## species
##------------------------------------------------

superfamily <- max_bf_overall %>% select(Family, HigherTaxonomy) %>% distinct()

superfamily <- full_join(sp_number, superfamily, by = "Family")


##------------------------------------------------
## Make the plot
##------------------------------------------------


ggplot(superfamily, aes( y = n, x=Family, fill = HigherTaxonomy, colour = HigherTaxonomy)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  scale_fill_manual(values = c("red","goldenrod4","forestgreen","darkorange1","deepskyblue1", "deeppink1")) +
  scale_colour_manual(values = c("red","goldenrod4","forestgreen","darkorange1","deepskyblue1", "deeppink1")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,45,5)) +
  xlab("Family") +
  ylab("Number of species in my dataset") +
  labs(fill = "Superfamily/Infraorder") +
  labs(colour = "Superfamily/Infraorder") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = "right")

# Save the plot
# ggsave("figures/family-coverage-by-superfamily.png", dpi = 300, height = 6, width = 10)


