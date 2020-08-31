# Scatterplot for biteforce vs all morphological variables based on diet
# July 2020
#----------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(viridis)
library(patchwork)

# Add rphylopic
#install.packages("remotes")
#remotes::install_github("sckott/rphylopic")
library(rphylopic)


#------------------------
# Read in the data
#------------------------
max_bf_overall <- read_csv("../data/max_bf_overall.csv")
#glimpse(max_bf_overall)

#------------------------
# Create the plots
#------------------------

# Snout-vent length
p_svl <- max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_svl), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

# Body mass
p_bm <-  max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_bm), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

# Head length
p_hl <- max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_hl), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

# Head height
p_hh <- max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_hh), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

# Head width
p_hw <-  max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_hw), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

# Lower-jaw length
p_ljl <- max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=log(max_ljl), y =log(max_bf), shape = MieriDiet, colour = MieriDiet)) + 
  geom_point() 

ggarrange(p_svl, p_bm ,p_hl ,p_hh ,p_hw ,p_ljl ,ncol = 3, nrow = 2, common.legend = TRUE, legend ="bottom")  +  
  plot_annotation(title = "Relationship between BF and all morphological variables")


