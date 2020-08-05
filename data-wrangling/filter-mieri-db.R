#Extracting habitat and diet data from Mieri paper
meiri <- read.csv(file.choose())
str(meiri)
filter <- meiri[,c(1,7,22,23,24,25)] #filtered relevant rows
str(filter)
filter[filter$Binomial == "Cryptoblepharus metallicus",] 


test <- max_bf_overall %>%
  select(BinomialReptileDatabase, MainBiogeographicRealm, Lifestyle, MieriDiet)
