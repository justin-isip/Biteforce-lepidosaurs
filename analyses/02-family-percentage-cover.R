# Creating stacked bar charts to show the % coverage of each family based on the total species numbers in the family
# Justin Isip
# July 2020
#----------------------------------------------------------------------------------------------------------------
library(scales)
library(tidyverse)
library(forestmangr)

#---------------------------------------------------------------  
# Total number of species for each of the families in my dataset
# Inferred from Reptile Database
#---------------------------------------------------------------    
# Family          | Total species number
#---------------------------------------------------------------
# Agamidae        | 526 
# Chameleonidae   | 217 
# Crotaphytidae   | 12 
# Dactyloidae     | 436
# Iguanidae       | 44
# Liolaemidae     | 320
# Phrynosomatidae | 162 
# Tropiduridae    | 136
# Lacertidae      | 347
# Teiidae         | 162
# Trogonophidae   | 6
# Cordylidae      | 70 
# Scincidae       | 1685
# Anguidae        | 85 
# Varanidae       | 81
# Xenosauridae    | 12
# Gekkonidae      | 1331
# Phyllodactylidae| 150
# Shenodontidae   | 1 
# Leiosauridae    | 34







# Build our data frame

family_coverage <- 
  
  # Our max_bf_overall dataset has the 164 species we have available data for
    max_bf_overall %>% 
  
  # Count how many species are in each family
        count(Family) %>% 
  
  # Add a column based on the total number of species for each family inferred from Reptile DB
  mutate(TotalSpNumber = c(526, 85, 217, 70, 12, 436, 1331, 44, 347, 34, 320, 162, 150, 1685, 1, 162, 6, 136, 81, 12)) %>%
  
  # Rename 'n' column to OurSpNumber (how many species we have for each family)
  rename(OurSpNumber = n) %>%
  
  # Create a column to work out the proportion of each family we have data for
  mutate(PercentCoverage = OurSpNumber / TotalSpNumber) %>%
  
  # Round our percentage cover to 2 decimal places
  round_df(digits = 3, rf = "round") 




# Lets create something pretty-ish!
ggplot(family_coverage, aes(fill=PercentCoverage, y = OurSpNumber, x=reorder(Family, OurSpNumber))) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  theme(text = element_text(size=8)) +
  coord_flip() +
  theme_minimal() +
  ggtitle("% coverage of each family based on total species numbers") +
  geom_text(aes(label = scales::percent(PercentCoverage)), nudge_y = 2, color = "black")+ 
  scale_y_continuous(breaks=seq(0,45,5)) +
  labs(y = "Number of species in our study", x = "Family")





