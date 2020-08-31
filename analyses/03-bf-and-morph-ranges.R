# Ranges for bite-force and morphological variables
# Justin Isip
# July 2020
#----------------------------------------------------------------

# Load libraries
library(tidyverse)


#------------------------
# Read in the data
#------------------------
max_bf_overall <- read_csv("../data/max_bf_overall.csv")
#glimpse(max_bf_overall)

# Minimum and maximum for each variable based on species
variable_range_species <- max_bf_overall %>% 
  group_by(BinomialReptileDatabase) %>%
  summarise(min_bm = min(max_bm), max_bm = max(max_bm), min_hl = min(max_hl),max_hl = max(max_hl),min_hw = min(max_hw), max_hw = max(max_hw), min_hh = min(max_hh ), min_bf = min(max_bf), max_bf = max(max_bf), max_hh = max(max_hh), min_ljl = min(max_ljl), max_ljl = max(max_ljl))


# Minimum and maximum for each variable based on family
variable_range_family <- max_bf_overall %>% 
  group_by(Family) %>%
  summarise(min_svl = min(max_svl, na.rm = T), max_svl = max(max_svl, na.rm = T), min_bm = min(max_bm, na.rm = TRUE), max_bm = max(max_bm, na.rm = TRUE), min_hl = min(max_hl,na.rm = TRUE),max_hl = max(max_hl, na.rm= TRUE),min_hw = min(max_hw, na.rm = TRUE), max_hw = max(max_hw, na.rm = TRUE), min_hh = min(max_hh ,na.rm = TRUE), min_bf = min(max_bf, na.rm = T), max_bf = max(max_bf, na.rm = T), max_hh = max(max_hh, na.rm = TRUE), min_ljl = min(max_ljl, na.rm = TRUE), max_ljl = max(max_ljl, na.rm = TRUE))

