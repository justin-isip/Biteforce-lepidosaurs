# Code to edit the data for Lacerta trilineata, I accidentally recorded it as cm rather than mm, so multiplied each morph measurement by 10



max_bf_overall[max_bf_overall$BinomialReptileDatabase == "Lacerta trilineata",] <- max_bf_overall %>%
  filter(BinomialReptileDatabase == "Lacerta trilineata") %>%
  mutate(max_svl = max_svl * 10) %>%
  mutate(max_hw = max_hw * 10) %>%
  mutate(max_hl = max_hl * 10) %>%
  mutate(max_hh = max_hh * 10) %>%
  mutate(max_ljl = max_ljl *10) %>%
  mutate(max_bm = max_bm *10)


max_bf_females[max_bf_females$BinomialReptileDatabase == "Lacerta trilineata",] <- max_bf_females %>%
  filter(BinomialReptileDatabase == "Lacerta trilineata") %>%
  mutate(max_svl = max_svl * 10) %>%
  mutate(max_hw = max_hw * 10) %>%
  mutate(max_hl = max_hl * 10) %>%
  mutate(max_hh = max_hh * 10) %>%
  mutate(max_ljl = max_ljl *10) %>%
  mutate(max_bm = max_bm *10)

max_bf_males[max_bf_males$BinomialReptileDatabase == "Lacerta trilineata",] <- max_bf_males %>%
  filter(BinomialReptileDatabase == "Lacerta trilineata") %>%
  mutate(max_svl = max_svl * 10) %>%
  mutate(max_hw = max_hw * 10) %>%
  mutate(max_hl = max_hl * 10) %>%
  mutate(max_hh = max_hh * 10) %>%
  mutate(max_ljl = max_ljl *10) %>%
  mutate(max_bm = max_bm *10)

write_csv(max_bf_overall, path = "data/max_bf_overall.csv")
write_csv(max_bf_males, path = "data/max_bf_males.csv")
write_csv(max_bf_females, path = "data/max_bf_females.csv")