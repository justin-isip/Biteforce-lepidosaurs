# Tidying the data from my dataset and getting it ready for analysis
# Justin Isip
# July 2020
#--------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Write some functions

# Convert SD to SE
sd2se <- function(x, n) {
  x / sqrt(n)
}

# Convert SE to SD

se2sd <- function(x, n) {
  x * sqrt(n)
}

# Create the max column for each variable

get_max <- function(var.max, var.mean) {
  ifelse(is.na(var.max), var.mean, var.max)
}


#---------------------------------------------------------------
# Read in my data
#---------------------------------------------------------------

rawdata <- read_csv("data/rawdata.csv")

#---------------------------------------------------------------
# Housekeeping
#---------------------------------------------------------------                                                                     

rawdata <- rawdata %>% 
  
  # Remove rows that have morphometric data from museums and BF data from in the field
  filter(DataQuality != "Remove") %>%
  
  # Remove juveniles and sub-adults
  filter(!Age %in% c("JV", "SA")) %>% 
  
  # Remove unneeded columns
  select(-c(Journal, Year, BinomialStudy, SC, Notes, BM.MIN, SVL.MIN, HL.MIN, HW.MIN, HH.MIN, LJL.MIN, BF.MIN, Age)) %>%

  # Replace NA values with "unknown" in sex column
  mutate(Sex = replace_na(Sex, "unknown")) %>% 
  
  
  # Convert SE to SD where we have SE but no SD 
  # Note that for BF I put SSBF (sample size for bite force)
  # For morphological variables I put SSM (sample size morphometrics)
  
  mutate(BF.SD = case_when(is.na(BF.SD) & !is.na(BF.SE) ~ se2sd(BF.SE, SSBF),
                           TRUE ~ BF.SD)) %>%
 
  mutate(SVL.SD = case_when(is.na(SVL.SD) & !is.na(SVL.SE) ~ se2sd(SVL.SE, SSM),
                           TRUE ~ SVL.SD)) %>%
  
  mutate(BM.SD = case_when(is.na(BM.SD) & !is.na(BM.SE) ~ se2sd(BM.SE, SSM),
                           TRUE ~ BM.SD)) %>%
  
  mutate(HL.SD = case_when(is.na(HL.SD) & !is.na(HL.SE) ~ se2sd(HL.SE, SSM),
                           TRUE ~ HL.SD)) %>%
  
  mutate(HW.SD = case_when(is.na(HW.SD) & !is.na(HW.SE) ~ se2sd(HW.SE, SSM),
                           TRUE ~ HW.SD)) %>%
  
  mutate(HH.SD = case_when(is.na(HH.SD) & !is.na(HH.SE) ~ se2sd(HH.SE, SSM),
                           TRUE ~ HH.SD)) %>%
  
  mutate(LJL.SD = case_when(is.na(LJL.SD) & !is.na(LJL.SE) ~ se2sd(LJL.SE, SSM),
                           TRUE ~ LJL.SD)) %>%
  
  
  # Convert SD to SE where we have SD but no SE
  # Note that for BF I used SSBF (sample size for bite force)
  # For morphological variables I used SSM (sample size morphometrics)
  
  mutate(BF.SE = case_when(is.na(BF.SE) & !is.na(BF.SD) ~ sd2se(BF.SD, SSBF),
                           TRUE ~ BF.SE)) %>%
  
  mutate(SVL.SE = case_when(is.na(SVL.SE) & !is.na(SVL.SD) ~ sd2se(SVL.SD, SSM),
                           TRUE ~ SVL.SE)) %>%
  
  mutate(BM.SE = case_when(is.na(BM.SE) & !is.na(BM.SD) ~ sd2se(BM.SD, SSM),
                           TRUE ~ BM.SE)) %>%
  
  mutate(HL.SE = case_when(is.na(HL.SE) & !is.na(HL.SD) ~ sd2se(HL.SD, SSM),
                           TRUE ~ HL.SE)) %>%
  
  mutate(HW.SE = case_when(is.na(HW.SE) & !is.na(HW.SD) ~ sd2se(HW.SD, SSM),
                           TRUE ~ HW.SE)) %>%
  
  mutate(HH.SE = case_when(is.na(HH.SE) & !is.na(HH.SD) ~ sd2se(HH.SD, SSM),
                           TRUE ~ HH.SE)) %>%
  
  mutate(LJL.SE = case_when(is.na(LJL.SE) & !is.na(LJL.SD) ~ sd2se(LJL.SD, SSM),
                           TRUE ~ LJL.SE)) %>%

  # Create columns of BF and each morphological variable + the SD (as a proxy for the max)
  # Note: In the raw dataset, this will also input BF + SD values for species with "bad" quality data
  # however these species/individuals should not be used for SD analysis 
  
  mutate(bf_plus_sd = BF + BF.SD) %>%
  
  mutate(svl_plus_sd = SVL + SVL.SD) %>%
  
  mutate(bm_plus_sd = BM + BM.SD) %>%
  
  mutate(hl_plus_sd = HL + HL.SD) %>%
  
  mutate(hw_plus_sd = HW + HW.SD) %>%
  
  mutate(hh_plus_sd = HH + HH.SD) %>%
  
  mutate(ljl_plus_sd = LJL + LJL.SD) %>%
  
  
  
  # Create columns of BF and each morphological variable + the SE (as a proxy for the max)
  # Note: same as above ^
  
  mutate(bf_plus_se = BF + BF.SE) %>%
  
  mutate(svl_plus_se = SVL + SVL.SE) %>%
  
  mutate(bm_plus_se = BM + BM.SE) %>%
  
  mutate(hl_plus_se = HL + HL.SE) %>%
  
  mutate(hw_plus_se = HW + HW.SE) %>%
  
  mutate(hh_plus_se = HH + HH.SE) %>%
  
  mutate(ljl_plus_se = LJL + LJL.SE) %>%
  
  
  # Add a column that says how many NAs each row has
  mutate(number_NAs = rowSums(is.na(.)))
           
  # Count how many NAs each column has too
  na_count <- rawdata %>% summarise_all(~ sum(is.na(.)))
  
#---------------------------------------------------------------------------------------------------------------------------
# Here, I am going to create 9 datasets for the different analyses:
# 1. max_bf_overall:  maximum bite force for each species (overall = males and females combined)
# 2. max_bf_males: maximum bite force for each species (males only)
# 3. max_bf_females: maximum bite force for each species (females only)
# 4. avg_bf_withsd_overall:  average bite force + standard deviation for each species (overall = males and females combined)
# 5. avg_bf_withsd_males: average bite force + standard deviation for each species (males only)
# 6. avg_bf_withsd_females: average bite force + standard deviation for each species (females only)
# 7. avg_bf_withse_overall:  average bite force + standard error for each species (overall = males and females combined)
# 8. avg_bf_withse_males: average bite force + standard error for each species (males only)
# 9. Avg_bf_withse_females: average bite force + standard error for each species (females only)
#---------------------------------------------------------------------------------------------------------------------------  
# 1. max_bf_overall 
#  
# Here I've created new columns for BF and all the morph measurements (BM,SVL,HH,HW,HL LJL)
# and a function obtain the max for each variable for each species:
#
#  get_max <- function(var.max, var.mean) {
#    ifelse(is.na(var.max), var.mean, var.max)
#  }
#
# The code is, If var.max is an NA, give me var.mean, else give me var.max
# This gives me a column of either the var.max (max value recorded for that species) or the  var.mean (avg value for that species)
# Note: the max will always be higher than the average
# Therefore, if var.max is available input that into the column, else input the average
#---------------------------------------------------------------------------------------------------------------------------
# Do this for each variable and create new columns: max_bf_overall, max_bm_overall etc.. 
#--------------------------------------------------------------------------------------------------------------------------
  
   max_bf_overall <- rawdata %>% 
   mutate(max_bf_overall = get_max(BF.MAX, BF))  %>%
   mutate(max_bm_overall = get_max(BM.MAX, BM))  %>%
   mutate(max_svl_overall = get_max(SVL.MAX, SVL))  %>%  
   mutate(max_hl_overall = get_max(HL.MAX, HL))  %>%
   mutate(max_hw_overall = get_max(HW.MAX, HW))  %>%
   mutate(max_hh_overall = get_max(HH.MAX, HH))  %>%
   mutate(max_ljl_overall = get_max(LJL.MAX, LJL))  %>%
   
   # We want one row for each species, but we have instances of multiple rows for the same species  
   # Filter the max bite force for each species!
   group_by(BinomialReptileDatabase) %>%
   filter(max_bf_overall == max(max_bf_overall)) %>%
   
   # For instances where two individuals of the same species have the same max BF (duplicates)
   # I have chosen to keep the individual with the fewest NAs (i.e. the better quality data)
   # arrange() sorts by the number of NAs with the first row for each species having the fewest NAs
   # distinct() takes the first species from the duplicate rows based on only the species names
   # being duplicated and keeps all other values. This automates the choices for duplicated species!
   # Cool eh? (Thanks Natalie)  
    
   arrange(number_NAs) %>% 
   distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
  
   # Select columns we want
   select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, max_bf_overall, max_bm_overall, max_svl_overall, max_hl_overall, max_hw_overall, max_hh_overall, max_ljl_overall)
  

    #---------------------------------------------------------------------------------    
    # max_bf_males (MAXBF without SD or SE) - MALES
    # Exact same steps as max_bf_overall, except filtering for males and removing
    # species with unknown sex
    #---------------------------------------------------------------------------------  
  
    max_bf_males <- rawdata %>% 
    
    # Filter only males and remove species with unknown sex
    filter(Sex == "M" & Sex != "Unknown") %>%
    
    #As before, create new columns
    mutate(max_bf_males = get_max(BF.MAX, BF))  %>%
    mutate(max_bm_males = get_max(BM.MAX, BM))  %>%
    mutate(max_svl_males = get_max(SVL.MAX, SVL))  %>%  
    mutate(max_hl_males = get_max(HL.MAX, HL))  %>%
    mutate(max_hw_males = get_max(HW.MAX, HW))  %>%
    mutate(max_hh_males = get_max(HH.MAX, HH))  %>%
    mutate(max_ljl_males = get_max(LJL.MAX, LJL))  %>%
    
    # Filter the max bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(max_bf_males == max(max_bf_males)) %>%
  
    arrange(number_NAs) %>% 
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%

    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, max_bf_males, max_bm_males, max_svl_males, max_hl_males, max_hw_males, max_hh_males, max_ljl_males)
    

    #---------------------------------------------------------------  
    # max_bf_females (MAXBF without SD or SE) - FEMALES
    # Exact same steps as max_bf_overall, except filtering for females only and removing
    # unknown sex species
    #---------------------------------------------------------------    
    
    max_bf_females <- rawdata %>% 
      
    # Filter only females and remove species with unknown sex:
    filter(Sex == "F" & Sex != "Unknown") %>%
    
    #As before, create new columns
    mutate(max_bf_females = get_max(BF.MAX, BF))  %>%
    mutate(max_bm_females = get_max(BM.MAX, BM))  %>%
    mutate(max_svl_females = get_max(SVL.MAX, SVL))  %>%  
    mutate(max_hl_females = get_max(HL.MAX, HL))  %>%
    mutate(max_hw_females = get_max(HW.MAX, HW))  %>%
    mutate(max_hh_females = get_max(HH.MAX, HH))  %>%
    mutate(max_ljl_females = get_max(LJL.MAX, LJL))  %>%
    
    # Filter the max bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(max_bf_females == max(max_bf_females)) %>%
    
    arrange(number_NAs) %>% 
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, max_bf_females, max_bm_females, max_svl_females, max_hl_females, max_hw_females, max_hh_females, max_ljl_females)


    #---------------------------------------------------------------------------------------------------------------------  
    # avg_bf_withsd_overall - average bite force + SD for each species (males and females combined)
    # The new columns of BF and all of the morphological variables + the SD were created earlier to make the script tidier
    # The code is very similar to max_bf_overall, the only difference is we are subsetting the average of BF and each of
    # the morphological variables + the SD (as a proxy for the maximum)
    #---------------------------------------------------------------------------------------------------------------------  
    
    avg_bf_withsd_overall <- rawdata %>%
  
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis    
      
    filter(DataQuality != "Bad") %>%
      
    mutate(bf_withsd_overall = bf_plus_sd) %>%
      
    mutate(svl_withsd_overall = svl_plus_sd) %>%
      
    mutate(bm_withsd_overall = bm_plus_sd) %>%
      
    mutate(hl_withsd_overall = hl_plus_sd) %>%
      
    mutate(hw_withsd_overall = hw_plus_sd ) %>%
      
    mutate(hh_withsd_overall = hh_plus_sd) %>%
      
    mutate(ljl_withsd_overall = ljl_plus_sd) %>%
    
    group_by(BinomialReptileDatabase) %>%
      
    filter(bf_withsd_overall == max(bf_withsd_overall)) %>%
      
    arrange(number_NAs) %>% 
      
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
        
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withsd_overall, bm_withsd_overall, svl_withsd_overall, hl_withsd_overall, hw_withsd_overall, hh_withsd_overall, ljl_withsd_overall)
      
      
      
      
    #------------------------------------------------------------------------------------  
    # avg_bf_withsd_males  (average BF + SD) - MALES
    # Exact same steps as avg_bf_withsd_overall, except filtering for males and removing
    # unknown sex species
    #------------------------------------------------------------------------------------

  
    avg_bf_withsd_males <- rawdata %>%
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis    
    filter(DataQuality != "Bad") %>%
    
    # Filter for males only and remove species with unknown sex 
    filter(Sex == "M" & Sex != "Unknown") %>%
    
    mutate(bf_withsd_males = bf_plus_sd) %>%
    
    mutate(svl_withsd_males = svl_plus_sd) %>%
    
    mutate(bm_withsd_males = bm_plus_sd) %>%
    
    mutate(hl_withsd_males = hl_plus_sd) %>%
    
    mutate(hw_withsd_males = hw_plus_sd ) %>%
    
    mutate(hh_withsd_males = hh_plus_sd) %>%
    
    mutate(ljl_withsd_males = ljl_plus_sd) %>%
    
    group_by(BinomialReptileDatabase) %>%
    
    filter(bf_withsd_males == max(bf_withsd_males)) %>%
    
    arrange(number_NAs) %>% 
    
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withsd_males, bm_withsd_males, svl_withsd_males, hl_withsd_males, hw_withsd_males, hh_withsd_males, ljl_withsd_males)
  
  
  
  
    #--------------------------------------------------------------------------------------  
    # avg_bf_withsd_females  (average BF + SD) - FEMALES
    # Exact same steps as avg_bf_withsd_overall, except filtering for females and removing
    # unknown sex species
    #--------------------------------------------------------------------------------------  
  
  
    avg_bf_withsd_females <- rawdata %>%
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis    
    filter(DataQuality != "Bad") %>%
    
    # Filter for females only and remove species with unknown sex 
    filter(Sex == "F" & Sex != "Unknown") %>%
    
    mutate(bf_withsd_females = bf_plus_sd) %>%
    
    mutate(svl_withsd_females = svl_plus_sd) %>%
    
    mutate(bm_withsd_females = bm_plus_sd) %>%
    
    mutate(hl_withsd_females = hl_plus_sd) %>%
    
    mutate(hw_withsd_females = hw_plus_sd ) %>%
    
    mutate(hh_withsd_females = hh_plus_sd) %>%
    
    mutate(ljl_withsd_females = ljl_plus_sd) %>%
    
    group_by(BinomialReptileDatabase) %>%
    
    filter(bf_withsd_females == max(bf_withsd_females)) %>%
    
    arrange(number_NAs) %>% 
    
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withsd_females, bm_withsd_females, svl_withsd_females, hl_withsd_females, hw_withsd_females, hh_withsd_females, ljl_withsd_females)
  
  
    #-----------------------------------------------------------------------------------------------------------------------  
    # avg_bf_withse_overall - average bite force + standard error for each species (males and females combined)
    # The new columns of BF and all of the morphological variables + the SE were created earlier to make the script tidier
    # The code is very similar to max_bf_overall, the only difference is we are subsetting the average of BF and each of
    # the morphological variables + the SE (as a proxy for the maximum)
    #-----------------------------------------------------------------------------------------------------------------------
  
    avg_bf_withse_overall <- rawdata %>%
    
    # First, remove species with "bad" data quality that can't be used for SE analysis    
    filter(DataQuality != "Bad") %>%
    
    mutate(bf_withse_overall = bf_plus_se) %>%
    
    mutate(svl_withse_overall = svl_plus_se) %>%
    
    mutate(bm_withse_overall = bm_plus_se) %>%
    
    mutate(hl_withse_overall = hl_plus_se) %>%
    
    mutate(hw_withse_overall = hw_plus_se ) %>%
    
    mutate(hh_withse_overall = hh_plus_se) %>%
    
    mutate(ljl_withse_overall = ljl_plus_se) %>%
    
    group_by(BinomialReptileDatabase) %>%
    
    filter(bf_withse_overall == max(bf_withse_overall)) %>%
    
    arrange(number_NAs) %>% 
    
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withse_overall, bm_withse_overall, svl_withse_overall, hl_withse_overall, hw_withse_overall, hh_withse_overall, ljl_withse_overall)
  
  
    #-------------------------------------------------------------------------------------
    # avg_bf_withse_males (average BF + SE) - MALES
    # Exact same steps as avg_bf_withse_overall, except filtering for males and removing
    # species of unknown sex
    #-------------------------------------------------------------------------------------      
  
    avg_bf_withse_males <- rawdata %>%
    
    # First, remove species with "bad" data quality that can't be used for SE analysis    
    filter(DataQuality != "Bad") %>%
    
    # Filter for only males and remove species with unknown sex 
    filter(Sex == "M" & Sex != "Unknown") %>%
    
    mutate(bf_withse_males = bf_plus_se) %>%
    
    mutate(svl_withse_males = svl_plus_se) %>%
    
    mutate(bm_withse_males = bm_plus_se) %>%
    
    mutate(hl_withse_males = hl_plus_se) %>%
    
    mutate(hw_withse_males = hw_plus_se ) %>%
    
    mutate(hh_withse_males = hh_plus_se) %>%
    
    mutate(ljl_withse_males = ljl_plus_se) %>%
    
    group_by(BinomialReptileDatabase) %>%
    
    filter(bf_withse_males == max(bf_withse_males)) %>%
    
    arrange(number_NAs) %>% 
    
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withse_males, bm_withse_males, svl_withse_males, hl_withse_males, hw_withse_males, hh_withse_males, ljl_withse_males)
  

    #---------------------------------------------------------------  
    # avg_bf_withse_females (average BF + SE) - FEMALES
    # Exact same steps as avg_bf_withse_overall, except filtering for females and removing
    # species of unknown sex
    #---------------------------------------------------------------    
  
    avg_bf_withse_females <- rawdata %>%
    
    # First, remove species with "bad" data quality that can't be used for SE analysis    
    filter(DataQuality != "Bad") %>%
    
    # Filter for only females and remove species with unknown sex 
    filter(Sex == "F" & Sex != "Unknown") %>%
    
    mutate(bf_withse_females = bf_plus_se) %>%
    
    mutate(svl_withse_females = svl_plus_se) %>%
    
    mutate(bm_withse_females = bm_plus_se) %>%
    
    mutate(hl_withse_females = hl_plus_se) %>%
    
    mutate(hw_withse_females = hw_plus_se ) %>%
    
    mutate(hh_withse_females = hh_plus_se) %>%
    
    mutate(ljl_withse_females = ljl_plus_se) %>%
    
    group_by(BinomialReptileDatabase) %>%
    
    filter(bf_withse_females == max(bf_withse_females)) %>%
    
    arrange(number_NAs) %>% 
    
    distinct(BinomialReptileDatabase, .keep_all = TRUE) %>%
    
    select(BinomialReptileDatabase, Sex, Family, SSM, SSBF, bf_withse_females, bm_withse_females, svl_withse_females, hl_withse_females, hw_withse_females, hh_withse_females, ljl_withse_females)
  


  
    # write each dataset to csv
    write_csv(rawdata, path = "data/tidydata.csv")
    write_csv(max_bf_overall, path = "data/max_bf_overall.csv")
    write_csv(max_bf_males, path = "data/max_bf_males.csv")
    write_csv(max_bf_females, path = "data/max_bf_females.csv")
    write_csv(avg_bf_withsd_overall, path = "data/avg_bf_withsd_overall.csv")
    write_csv(avg_bf_withsd_males, path = "data/avg_bf_withsd_males.csv")
    write_csv(avg_bf_withsd_females, path = "data/avg_bf_withsd_females.csv")
    write_csv(avg_bf_withse_overall, path = "data/avg_bf_withse_overall.csv")
    write_csv(avg_bf_withse_males, path = "data/avg_bf_withse_males.csv")
    write_csv(avg_bf_withse_females, path = "data/avg_bf_withse_females.csv")




