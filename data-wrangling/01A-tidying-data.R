# Tidying the data from my dataset
# Justin Isip
# July 2020
#---------------------------------
# Load libraries
library(tidyverse)

#---------------------------------------------------------------
# Read in my data
#---------------------------------------------------------------

d <- read.csv("original-data/11-07-2020-data.csv", na.strings=c("","NA", "N", "NA "), colClasses = c("Citation" = "factor", "Family" = "factor", "BinomialReptileDatabase" = "factor", "Sex" = "factor"))


#---------------------------------------------------------------
# Housekeeping
#---------------------------------------------------------------                                                                     

d <- d %>% 
  
  # Remove rows that have morphometric data from museums and BF data from in the field
  filter(DataQuality != "Remove") %>%
  
  # Remove unneeded columns
  select(-c(Journal, Year, BinomialStudy, SC, Notes)) %>%
  
  # Remove juveniles and sub-adults
  filter(!Age %in% c("JV", "SA"))
  
  # Convert sex to character
  d$Sex <- as.character(d$Sex)

  # Replace NA values with "unknown" in sex column
  d$Sex <- d$Sex %>% replace_na("unknown")
  
  # Convert back to factor
  d$Sex <- as.factor(d$Sex) 
  
  # How many NAs in each column 
  na_count <-sapply(d, function(y) sum(length(which(is.na(y))))) 
  
  # Convert to DF
  na_count <- data.frame(na_count) 
  
  # Total rows: 331 - NA's to tell me how many rows we have data for :)
  na_count <- 331 - na_count 
  
#---------------------------------------------------------------
# Here, I am going to create 9 datasets for 9 different analyses
# 1. A1:  maximum bite force for each species (overall, males and females)
# 2. A1M: maximum bite force for each species (males only)
# 3. A1F: maximum bite force for each species (females only)
# 4. A2:  average bite force + standard deviation for each species (overall, males and females)
# 5. A2M: average bite force + standard deviation for each species (males only)
# 6. A2F: average bite force + standard deviation for each species (females only)
# 7. A3:  average bite force + standard error for each species (overall, males and females)
# 8. A3M: average bite force + standard error for each species (males only)
# 9. A3F: average bite force + standard error for each species (females only)
#---------------------------------------------------------------  
# A1  (MAXBF without SD or SE)
# Here I've created new columns for BF and all the morph measurements (BM,SVL,HH,HW,HL LJL)
# The new column is the max for each variable
# The code is, If BF.MAX is an NA, print BF, else print BF.MAX
# This gives me a column of either the BF.MAX or the BF value (average)
# BF = average bite force for that sample
# BF.MAX = maximum bite recorded for that sample
# This chooses between BF and BF.MAX (BF.MAX will always be higher than the average)
# Therefore, if BF.MAX is available input that into the column, else input the average
#---------------------------------------------------------------
# Do this for each variable and create new columns: MAX.BM, MAX.SVL, MAX.HH, MAX.HW, MAX.HL, MAX.LJL
#---------------------------------------------------------------
  
   A1 <- d %>% 
   mutate(MAX.BF=ifelse(is.na(BF.MAX),BF,BF.MAX))%>%
   mutate(MAX.BM=ifelse(is.na(BM.MAX),BM,BM.MAX))%>%
   mutate(MAX.SVL=ifelse(is.na(SVL.MAX),SVL,SVL.MAX))%>%
   mutate(MAX.HL=ifelse(is.na(HL.MAX),HL,HL.MAX))%>%
   mutate(MAX.HW=ifelse(is.na(HW.MAX),HW,HW.MAX))%>%
   mutate(MAX.HH=ifelse(is.na(HH.MAX),HH,HH.MAX))%>%
   mutate(MAX.LJL=ifelse(is.na(LJL.MAX),LJL,LJL.MAX)) %>%
   
   # We have multiple rows for the same species
   # Filter the max bite force for each species!
   group_by(BinomialReptileDatabase) %>%
   filter(MAX.BF == max(MAX.BF)) 
  
   # Check for duplicate rows in case two rows have the same max BF  
   A1[duplicated(A1$BinomialReptileDatabase),]
  
  # Whoops, for two species, two individuals have the same max BF
  # Bradypodion kentanicum - I've chosen to filter out the individual with the smaller SVL
  # Bradypodion melanocephalum - I've chosen to filter out the individual with less morph data available
  # Filter these two species out based on their SVL
  # I checked before to make sure they were the only species with this exact SVL
  A1 <- filter(A1, MAX.SVL != 51.02,) %>%
        filter( MAX.SVL != 57.02,) 
  
  # Double check to make sure I haven't messed this up
  # There are 164 distinct species in my original dataset
  d %>% distinct(BinomialReptileDatabase)
  
  # 164 distinct species in my filtered MAX.BF dataset (males + females)
  A1 %>% distinct(BinomialReptileDatabase)

#---------------------------------------------------------------  
# A1M  (MAXBF without SD or SE) - MALES
# Exact same steps as A1, except filtering for males and removing
# unknown sex species
#---------------------------------------------------------------    
  
  A1M <- d %>% 
    # Filter only males and remove species with unknown sex
    filter(Sex == "M" & Sex != "Unknown") %>%
    
    #As before create new MAX.BF column
    mutate(MAX.BF=ifelse(is.na(BF.MAX),BF,BF.MAX))%>%
    mutate(MAX.BM=ifelse(is.na(BM.MAX),BM,BM.MAX))%>%
    mutate(MAX.SVL=ifelse(is.na(SVL.MAX),SVL,SVL.MAX))%>%
    mutate(MAX.HL=ifelse(is.na(HL.MAX),HL,HL.MAX))%>%
    mutate(MAX.HW=ifelse(is.na(HW.MAX),HW,HW.MAX))%>%
    mutate(MAX.HH=ifelse(is.na(HH.MAX),HH,HH.MAX))%>%
    mutate(MAX.LJL=ifelse(is.na(LJL.MAX),LJL,LJL.MAX)) %>%
    
    # Filter the max bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(MAX.BF == max(MAX.BF)) 
  
    # Check for duplicate rows in case two individuals have the same BF values 
    A1M[duplicated(A1M$BinomialReptileDatabase),]
    
    # Duplicate rows:
    # Anolis sheplani - kept the individual with a larger SVL
    # Bradypodion melanocephalum - kept the individual with better quality data
    # Filter these two individuals out based on their SVL
    # I checked before to make sure they were the only individuals with this exact SVL
    #  A1M[A1M$SVL == 48.63,]
    # A1M[A1M$SVL == 38.61,]
    
    A1M <- filter(A1M, MAX.SVL != 48.63,) %>%
      filter( MAX.SVL != 38.61,) 
    
    # 132 distinct species in my filtered MAX.BF dataset (MALES only)
    A1M %>% distinct(BinomialReptileDatabase)

    #---------------------------------------------------------------  
    # A1F (MAXBF without SD or SE) - FEMALES
    # Exact same steps as A1 and A1M, except filtering for females and removing
    # unknown sex species
    #---------------------------------------------------------------    
    
    A1F <- d %>% 
      # Filter only males and remove species with unknown sex
      filter(Sex == "F" & Sex != "Unknown") %>%
      
      #As before create new MAX.BF column
      mutate(MAX.BF=ifelse(is.na(BF.MAX),BF,BF.MAX))%>%
      mutate(MAX.BM=ifelse(is.na(BM.MAX),BM,BM.MAX))%>%
      mutate(MAX.SVL=ifelse(is.na(SVL.MAX),SVL,SVL.MAX))%>%
      mutate(MAX.HL=ifelse(is.na(HL.MAX),HL,HL.MAX))%>%
      mutate(MAX.HW=ifelse(is.na(HW.MAX),HW,HW.MAX))%>%
      mutate(MAX.HH=ifelse(is.na(HH.MAX),HH,HH.MAX))%>%
      mutate(MAX.LJL=ifelse(is.na(LJL.MAX),LJL,LJL.MAX)) %>%
      
      # Filter the max bite force for each species!
      group_by(BinomialReptileDatabase) %>%
      filter(MAX.BF == max(MAX.BF)) 
    
    # Check for duplicate rows in case two individuals have the same BF values 
    A1F[duplicated(A1F$BinomialReptileDatabase),]
    
    # Duplicate rows:
    # Bradypodion melanocephalum - kept the individual with better quality data
    # Filter this individual based on their SVL
    # I checked before to make sure they were the only individuals with this exact SVL
    # A1F[A1F$SVL == 57.02,]
    
    A1F <- filter(A1F, MAX.SVL != 57.02)
  
    # 112 distinct species in my filtered MAX.BF dataset (females only)
    A1F %>% distinct(BinomialReptileDatabase)

#---------------------------------------------------------------  
# Analysis 2  (Average BF + SD)
# Formula: SD <- SE * sqrt(n)  
# Here I've created new columns for BF and all the morph measurements (BM,SVL,HH,HW,HL LJL)
# The new column is the average + the SD for each variable (as a proxy for the maximum)
# The code is if BF.SE is an NA then print BF + BF.SD, else print BF + (BF.SE * sqrt(SSM))
# SSM = sample size morphometrics  
# Gives me a column of bite force + the standard deviation
#---------------------------------------------------------------
# Create new columns: SD.BM, SD.SVL, SD.HH, SD.HW, SD.HL, SD.LJL
# Note that for BF I used SSBF (sample size for bite force)
#---------------------------------------------------------------  

  A2 <- d %>% 
  
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
  
    mutate(SD.BM=ifelse(is.na(BM.SE),BM+BM.SD,BM+(BM.SE*sqrt(SSM))))%>%
    mutate(SD.SVL=ifelse(is.na(SVL.SE),SVL+SVL.SD,SVL+(SVL.SE*sqrt(SSM))))%>%
    mutate(SD.HL=ifelse(is.na(HL.SE),HL+HL.SD,HL+(HL.SE*sqrt(SSM))))%>%
    mutate(SD.HW=ifelse(is.na(HW.SE),HW+HW.SD,HW+(HW.SE*sqrt(SSM))))%>%
    mutate(SD.HH=ifelse(is.na(HH.SE),HH+HH.SD,HH+(HH.SE*sqrt(SSM))))%>%
    mutate(SD.LJL=ifelse(is.na(LJL.SE),LJL+LJL.SD,LJL+(LJL.SE*sqrt(SSM))))%>%
    mutate(SD.BF=ifelse(is.na(BF.SE),BF+BF.SD,BF+(BF.SE*sqrt(SSBF)))) %>% 
  
  # Filter the max bite force for each species!
      group_by(BinomialReptileDatabase) %>%
      filter(SD.BF == max(SD.BF)) 
  
  # Check for duplicate rows
  A2[duplicated(A2$BinomialReptileDatabase),]

  # 128 distinct species in my filtered SD.BF dataset (males + females)
  A2 %>% distinct(BinomialReptileDatabase)
  
  
  #---------------------------------------------------------------  
  # A2M  (average BF + SD) - MALES
  # Exact same steps as A2, except filtering for males and removing
  # unknown sex species
  #---------------------------------------------------------------    
  A2M <- d %>% 
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
    
    # Filter for only males and remove species with unknown sex 
    filter(Sex == "M" & Sex != "Unknown") %>%
  
    mutate(SD.BM=ifelse(is.na(BM.SE),BM+BM.SD,BM+(BM.SE*sqrt(SSM))))%>%
    mutate(SD.SVL=ifelse(is.na(SVL.SE),SVL+SVL.SD,SVL+(SVL.SE*sqrt(SSM))))%>%
    mutate(SD.HL=ifelse(is.na(HL.SE),HL+HL.SD,HL+(HL.SE*sqrt(SSM))))%>%
    mutate(SD.HW=ifelse(is.na(HW.SE),HW+HW.SD,HW+(HW.SE*sqrt(SSM))))%>%
    mutate(SD.HH=ifelse(is.na(HH.SE),HH+HH.SD,HH+(HH.SE*sqrt(SSM))))%>%
    mutate(SD.LJL=ifelse(is.na(LJL.SE),LJL+LJL.SD,LJL+(LJL.SE*sqrt(SSM))))%>%
    mutate(SD.BF=ifelse(is.na(BF.SE),BF+BF.SD,BF+(BF.SE*sqrt(SSBF)))) %>% 
    
    # Filter the max bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(SD.BF == max(SD.BF)) 
  
  # Check for duplicate rows
  A2M[duplicated(A2M$BinomialReptileDatabase),]
  
  # 94 distinct species in my filtered SD.BF dataset (males only)
  A2M %>% distinct(BinomialReptileDatabase)
  
  
  #---------------------------------------------------------------  
  # A2F (average BF + SD) - FEMALES
  # Exact same steps as A2 and A2M, except filtering for females and removing
  # unknown sex species
  #---------------------------------------------------------------    
  A2F <- d %>% 
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
    
    # Filter for only females and remove species with unknown sex 
    filter(Sex == "F" & Sex != "Unknown") %>%
    
    mutate(SD.BM=ifelse(is.na(BM.SE),BM+BM.SD,BM+(BM.SE*sqrt(SSM))))%>%
    mutate(SD.SVL=ifelse(is.na(SVL.SE),SVL+SVL.SD,SVL+(SVL.SE*sqrt(SSM))))%>%
    mutate(SD.HL=ifelse(is.na(HL.SE),HL+HL.SD,HL+(HL.SE*sqrt(SSM))))%>%
    mutate(SD.HW=ifelse(is.na(HW.SE),HW+HW.SD,HW+(HW.SE*sqrt(SSM))))%>%
    mutate(SD.HH=ifelse(is.na(HH.SE),HH+HH.SD,HH+(HH.SE*sqrt(SSM))))%>%
    mutate(SD.LJL=ifelse(is.na(LJL.SE),LJL+LJL.SD,LJL+(LJL.SE*sqrt(SSM))))%>%
    mutate(SD.BF=ifelse(is.na(BF.SE),BF+BF.SD,BF+(BF.SE*sqrt(SSBF)))) %>% 
    
    # Filter the max bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(SD.BF == max(SD.BF)) 
  
  # Check for duplicate rows
  A2F[duplicated(A2F$BinomialReptileDatabase),]
  
  # 92 distinct species in my filtered SD.BF dataset (females only)
  A2F %>% distinct(BinomialReptileDatabase)
  
  

#---------------------------------------------------------------  
# Analysis 3  (Average BF + SE) - (overall, males + females)
# Formula: SE <- SD / sqrt(n) 
# Here I've created new columns for BF and all the morph measurements (BM,SVL,HH,HW,HL LJL)
# The new column is the average + the SE for each variable (as a proxy for the maximum)
# The code is if BF.SD is an NA then print BF + BF.SE, else print BF + (BF.SD / sqrt(SSM))
# SSM = sample size morphometrics
# Gives me a column of bite force + the standard error
#---------------------------------------------------------------
# Create new columns: SD.BM, SD.SVL, SD.HH, SD.HW, SD.HL, SD.LJL
# Note that for BF I used SSBF (sample size for bite force)
#---------------------------------------------------------------  
  
  A3 <- d %>% 
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
    
    mutate(SE.BM=ifelse(is.na(BM.SD),BM+BM.SE,BM+(BM.SD/sqrt(SSM))))%>%
    mutate(SE.SVL=ifelse(is.na(SVL.SD),SVL+SVL.SE,SVL+(SVL.SD/sqrt(SSM))))%>%
    mutate(SE.HL=ifelse(is.na(HL.SD),HL+HL.SE,HL+(HL.SD/sqrt(SSM))))%>%
    mutate(SE.HW=ifelse(is.na(HW.SD),HW+HW.SE,HW+(HW.SD/sqrt(SSM))))%>%
    mutate(SE.HH=ifelse(is.na(HH.SD),HH+HH.SE,HH+(HH.SD/sqrt(SSM))))%>%
    mutate(SE.LJL=ifelse(is.na(LJL.SD),LJL+LJL.SE,LJL+(LJL.SD/sqrt(SSM))))%>%
    mutate(SE.BF=ifelse(is.na(BF.SD),BF+BF.SE,BF+(BF.SD/sqrt(SSBF))))
  
  # We have multiple rows for the same species
  # Filter the max bite force for each species!
  A3 <- A3 %>% 
    group_by(BinomialReptileDatabase) %>%
    filter(SE.BF == max(SE.BF)) 
  
  # 128 distinct species in my filtered SD.BF dataset 
  A3 %>% distinct(BinomialReptileDatabase)

  #---------------------------------------------------------------  
  # A3M (average BF + SE) - MALES
  # Exact same steps as A3, except filtering for males and removing
  # unknown sex species
  #---------------------------------------------------------------    
  
  A3M <- d %>% 
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
    
    # Filter for only males and remove species with unknown sex 
    filter(Sex == "M" & Sex != "Unknown") %>%
    
    # Create the columns 
    mutate(SE.BM=ifelse(is.na(BM.SD),BM+BM.SE,BM+(BM.SD/sqrt(SSM))))%>%
    mutate(SE.SVL=ifelse(is.na(SVL.SD),SVL+SVL.SE,SVL+(SVL.SD/sqrt(SSM))))%>%
    mutate(SE.HL=ifelse(is.na(HL.SD),HL+HL.SE,HL+(HL.SD/sqrt(SSM))))%>%
    mutate(SE.HW=ifelse(is.na(HW.SD),HW+HW.SE,HW+(HW.SD/sqrt(SSM))))%>%
    mutate(SE.HH=ifelse(is.na(HH.SD),HH+HH.SE,HH+(HH.SD/sqrt(SSM))))%>%
    mutate(SE.LJL=ifelse(is.na(LJL.SD),LJL+LJL.SE,LJL+(LJL.SD/sqrt(SSM))))%>%
    mutate(SE.BF=ifelse(is.na(BF.SD),BF+BF.SE,BF+(BF.SD/sqrt(SSBF))))%>%
  
  # Filter the max SE bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(SE.BF == max(SE.BF)) 
  
  # 94 distinct species in my filtered SD.BF dataset (males only)
  A3M %>% distinct(BinomialReptileDatabase)
  
  
  #---------------------------------------------------------------  
  # A3F (average BF + SE) - FEMALES
  # Exact same steps as A3, except filtering for females and removing
  # unknown sex species
  #---------------------------------------------------------------    
  
  A3F <- d %>% 
    
    # First, remove species with "bad" data quality that can't be used for SD/SE analysis
    filter(DataQuality != "Bad") %>%
    
    # Filter for only males and remove species with unknown sex 
    filter(Sex == "F" & Sex != "Unknown") %>%
    
    # Create the columns 
    mutate(SE.BM=ifelse(is.na(BM.SD),BM+BM.SE,BM+(BM.SD/sqrt(SSM))))%>%
    mutate(SE.SVL=ifelse(is.na(SVL.SD),SVL+SVL.SE,SVL+(SVL.SD/sqrt(SSM))))%>%
    mutate(SE.HL=ifelse(is.na(HL.SD),HL+HL.SE,HL+(HL.SD/sqrt(SSM))))%>%
    mutate(SE.HW=ifelse(is.na(HW.SD),HW+HW.SE,HW+(HW.SD/sqrt(SSM))))%>%
    mutate(SE.HH=ifelse(is.na(HH.SD),HH+HH.SE,HH+(HH.SD/sqrt(SSM))))%>%
    mutate(SE.LJL=ifelse(is.na(LJL.SD),LJL+LJL.SE,LJL+(LJL.SD/sqrt(SSM))))%>%
    mutate(SE.BF=ifelse(is.na(BF.SD),BF+BF.SE,BF+(BF.SD/sqrt(SSBF))))%>%
    
    # Filter the max SE bite force for each species!
    group_by(BinomialReptileDatabase) %>%
    filter(SE.BF == max(SE.BF)) 
  
  # 92 distinct species in my filtered SD.BF dataset (females only)
  A3F %>% distinct(BinomialReptileDatabase)
  
  
  
  
  # Remove all unneeded columns
  # We can get rid of age column as all rows are adults!
  A1 <- select(A1, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A1M <- select(A1M, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A1F <- select(A1F, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A2 <- select(A2, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A2M <- select(A2M, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A2F <- select(A2F, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A3 <- select(A3, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A3M <- select(A3M, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  A3F <- select(A3F, -c(Citation, Age, BM, BM.SD, BM.SE, BM.MIN, BM.MAX, SVL, SVL.SD, SVL.SE, SVL.MIN, SVL.MAX, HL, HL.SD, HL.SE, HL.MIN, HL.MAX, HW, HW.SD, HW.SE, HW.MIN, HW.MAX, HH, HH.SD, HH.SE, HH.MIN, HH.MAX, BF, BF.SD, BF.SE, BF.MIN, BF.MAX, LJL, LJL.MAX, LJL.SD, LJL.SE, LJL.MIN, LJL.MAX, DataQuality))
  
  # Reorder the columns
  A1 <- A1[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "MAX.BM", "MAX.SVL", "MAX.HL", "MAX.HW", "MAX.HH", "MAX.LJL", "MAX.BF")]
  A1M <- A1M[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "MAX.BM", "MAX.SVL", "MAX.HL", "MAX.HW", "MAX.HH", "MAX.LJL", "MAX.BF")]
  A1F <- A1F[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "MAX.BM", "MAX.SVL", "MAX.HL", "MAX.HW", "MAX.HH", "MAX.LJL", "MAX.BF")]
  A2 <- A2[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SD.BM", "SD.SVL", "SD.HL", "SD.HW", "SD.HH", "SD.LJL", "SD.BF")]
  A2M <- A2M[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SD.BM", "SD.SVL", "SD.HL", "SD.HW", "SD.HH", "SD.LJL", "SD.BF")]
  A2F <- A2F[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SD.BM", "SD.SVL", "SD.HL", "SD.HW", "SD.HH", "SD.LJL", "SD.BF")]
  A3 <- A3[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SE.BM", "SE.SVL", "SE.HL", "SE.HW", "SE.HH", "SE.LJL", "SE.BF")]
  A3M <- A3M[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SE.BM", "SE.SVL", "SE.HL", "SE.HW", "SE.HH", "SE.LJL", "SE.BF")]
  A3F <- A3F[c("BinomialReptileDatabase", "Sex", "Family", "SSM", "SSBF", "SE.BM", "SE.SVL", "SE.HL", "SE.HW", "SE.HH", "SE.LJL", "SE.BF")]
  
  
  # write each dataset to csv
  write.csv(A1,"C:/Users/Justin Isip/Documents/MSc Thesis/A1.csv", row.names = FALSE)
  write.csv(A1M,"C:/Users/Justin Isip/Documents/MSc Thesis/A1M.csv", row.names = FALSE)
  write.csv(A1F,"C:/Users/Justin Isip/Documents/MSc Thesis/A1F.csv", row.names = FALSE)
  write.csv(A2,"C:/Users/Justin Isip/Documents/MSc Thesis/A2.csv", row.names = FALSE)
  write.csv(A2M,"C:/Users/Justin Isip/Documents/MSc Thesis/A2M.csv", row.names = FALSE)
  write.csv(A2F,"C:/Users/Justin Isip/Documents/MSc Thesis/A2F.csv", row.names = FALSE)
  write.csv(A3,"C:/Users/Justin Isip/Documents/MSc Thesis/A3.csv", row.names = FALSE)
  write.csv(A3M,"C:/Users/Justin Isip/Documents/MSc Thesis/A3M.csv", row.names = FALSE)
  write.csv(A3F,"C:/Users/Justin Isip/Documents/MSc Thesis/A3F.csv", row.names = FALSE)

  
  
  






