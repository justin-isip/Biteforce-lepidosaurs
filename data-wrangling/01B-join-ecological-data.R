# Joining ecological dataset to tidied datasets
# Justin Isip
# July 2020
#---------------------------------------------------------------

# Load libraries
library(tidyverse)


#---------------------------------------------------------------
# Read in my data
#---------------------------------------------------------------

# Ecological data
ecologicaldata <- read_csv("./data/ecologicaldata.csv")

# Tidied data arrangements
max_bf_overall <- read_csv("./data/max_bf_overall.csv")
max_bf_males <- read_csv("./data/max_bf_males.csv")
max_bf_females <- read_csv("./data/max_bf_females.csv", col_types =  cols(Sex = col_factor()))
avg_bf_plus_se_overall <- read_csv("./data/avg_bf_plus_se_overall.csv")
avg_bf_plus_se_males <- read_csv("./data/avg_bf_plus_se_males.csv")
avg_bf_plus_se_females <- read_csv("./data/avg_bf_plus_se_females.csv", col_types =  cols(Sex = col_factor()))
avg_bf_plus_sd_overall <- read_csv("./data/avg_bf_plus_sd_overall.csv")
avg_bf_plus_sd_males <- read_csv("./data/avg_bf_plus_sd_males.csv")
avg_bf_plus_sd_females <- read_csv("./data/avg_bf_plus_sd_females.csv", col_types =  cols(Sex = col_factor()))

#---------------------------------------------------------------
# Join ecological data to tidied data
#---------------------------------------------------------------

max_bf_overall <- ecologicaldata %>% 
  
  # Use inner join which joins the ecological to the tidied data and retains only rows in both sets
  inner_join(max_bf_overall, ecologicaldata, by = "BinomialReptileDatabase") %>%
  
  # Select the columns we want to keep
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, max_bf, max_bm, max_svl, max_hl, max_hw, max_hh, max_ljl, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)
  
# Do the same for all other data sets

max_bf_males <- ecologicaldata %>% 
inner_join(max_bf_males, ecologicaldata, by = "BinomialReptileDatabase") %>%
select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, max_bf, max_bm, max_svl, max_hl, max_hw, max_hh, max_ljl, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

max_bf_females <- ecologicaldata %>% 
  inner_join(max_bf_females, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, max_bf, max_bm, max_svl, max_hl, max_hw, max_hh, max_ljl, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_sd_overall <- ecologicaldata %>% 
  inner_join(avg_bf_plus_sd_overall, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_sd, bm_plus_sd, svl_plus_sd, hl_plus_sd, hw_plus_sd, hh_plus_sd, ljl_plus_sd, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_sd_males <- ecologicaldata %>% 
  inner_join(avg_bf_plus_sd_males, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_sd, bm_plus_sd, svl_plus_sd, hl_plus_sd, hw_plus_sd, hh_plus_sd, ljl_plus_sd, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_sd_females <- ecologicaldata %>% 
  inner_join(avg_bf_plus_sd_females, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_sd, bm_plus_sd, svl_plus_sd, hl_plus_sd, hw_plus_sd, hh_plus_sd, ljl_plus_sd, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_se_overall <- ecologicaldata %>% 
  inner_join(avg_bf_plus_se_overall, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_se, bm_plus_se, svl_plus_se, hl_plus_se, hw_plus_se, hh_plus_se, ljl_plus_se, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_se_males <- ecologicaldata %>% 
  inner_join(avg_bf_plus_se_males, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_se, bm_plus_se, svl_plus_se, hl_plus_se, hw_plus_se, hh_plus_se, ljl_plus_se, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)

avg_bf_plus_se_females <- ecologicaldata %>% 
  inner_join(avg_bf_plus_se_females, ecologicaldata, by = "BinomialReptileDatabase") %>%
  select(BinomialReptileDatabase, Sex, Family, HigherTaxonomy, SSM, SSBF, bf_plus_se, bm_plus_se, svl_plus_se, hl_plus_se, hw_plus_se, hh_plus_se, ljl_plus_se, MainBiogeographicRealm, Lifestyle, MieriDiet, MetzgerHerrelDiet, CooperDiet, PercentPlantMatterEaten)


# Now that I've merged the ecological data with the tidied data for all datasets, rewrite them all to csv 
write_csv(max_bf_overall, path = "data/max_bf_overall.csv")
write_csv(max_bf_males, path = "data/max_bf_males.csv")
write_csv(max_bf_females, path = "data/max_bf_females.csv")
write_csv(avg_bf_plus_sd_overall, path = "data/avg_bf_plus_sd_overall.csv")
write_csv(avg_bf_plus_sd_males, path = "data/avg_bf_plus_sd_males.csv")
write_csv(avg_bf_plus_sd_females, path = "data/avg_bf_plus_sd_females.csv")
write_csv(avg_bf_plus_se_overall, path = "data/avg_bf_plus_se_overall.csv")
write_csv(avg_bf_plus_se_males, path = "data/avg_bf_plus_se_males.csv")
write_csv(avg_bf_plus_se_females, path = "data/avg_bf_plus_se_females.csv")

