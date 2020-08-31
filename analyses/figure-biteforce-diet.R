# Figure for distribution of bite force by diet based on Mieiri (2018)
# Justin Isip
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


max_bf_overall %>% 
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x=MieriDiet,y=log(max_bf), colour=MieriDiet, label = BinomialReptileDatabase)) +
  geom_boxplot(show.legend = F, fill = "white", alpha = 0.2) + 
  geom_jitter(show.legend = F, width=0.1,alpha=0.2) +
  ggtitle("Distribution of max bf based on diet") +
  theme(axis.title.x = element_blank()) 
