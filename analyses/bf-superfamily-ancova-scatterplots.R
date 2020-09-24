# Superfamily/infraorder analysis
# Fit PGLS models (ANCOVA) and create scatterplots of relationships between bite-force and each morphological variables based on superfamily/infraorder grouping
# Sep 2020

## Load libraries
library(tidyverse)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Fit PGLS models (ANCOVA) to test for relationships between bite-force and morphological variables based on superfamily
## Note that I have used the comparative data that excludes Sphenodontia and Gekkota as there were limited data for these two superfamilies
## and that for body mass analysis I had to remove Anguimorpha too
##------------------------------------------------

bf_svl_by_superfamily <- pgls(log(max_bf) ~ log(max_svl) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota, 
                              lambda = "ML")


# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_svl_by_superfamily)
anova(bf_svl_by_superfamily)
summary(bf_svl_by_superfamily)


# Note the different data used which excludes Anguimorpha
bf_bm_by_superfamily <- pgls(log(max_bf) ~ log(max_bm) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota_anguimorpha, 
                             lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_bm_by_superfamily)
anova(bf_bm_by_superfamily)
summary(bf_bm_by_superfamily)


bf_hl_by_superfamily <- pgls(log(max_bf) ~ log(max_hl) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota, 
                             lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hl_by_superfamily)
anova(bf_hl_by_superfamily)
summary(bf_hl_by_superfamily)


bf_hw_by_superfamily <- pgls(log(max_bf) ~ log(max_hw) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota, 
                             lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hw_by_superfamily)
anova(bf_hw_by_superfamily)
summary(bf_hw_by_superfamily)

bf_hh_by_superfamily <- pgls(log(max_bf) ~ log(max_hh) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota, 
                             lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hh_by_superfamily)
anova(bf_hh_by_superfamily)
summary(bf_hh_by_superfamily)

bf_ljl_by_superfamily <- pgls(log(max_bf) ~ log(max_ljl) * HigherTaxonomy, data = lepidosaur_no_sphenodontia_gekkota, 
                              lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_ljl_by_superfamily)
anova(bf_ljl_by_superfamily)
summary(bf_ljl_by_superfamily)


##------------------------------------------------
## Create scatterplots of bite-force against each morphological variables with separate regression lines for each superfamily
## Regression lines come from PGLS models
##------------------------------------------------

# First add the consistent colours used for each superfamily
scale_colour_manual(values = c("red","goldenrod4","forestgreen","darkorange1","deepskyblue1", "deeppink1"))

# Now lets create the plots!
bf_svl_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes(y =log(max_bf), x = log(max_svl), group=HigherTaxonomy, colour=HigherTaxonomy, na.rm = T)) +
  geom_point() +
  scale_color_manual(values=c("red","forestgreen","darkorange1","deepskyblue1")) +
  xlab("log(SVL) (mm)") +
  ylab("log(BF)(N)")  +
  geom_abline(intercept = coefficients(bf_svl_by_superfamily)[1], 
              slope = coefficients(bf_svl_by_superfamily)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_svl_by_superfamily)[1] - coefficients(bf_svl_by_superfamily)[3], 
              slope = coefficients(bf_svl_by_superfamily)[2] + coefficients(bf_svl_by_superfamily)[6], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_svl_by_superfamily)[1] - coefficients(bf_svl_by_superfamily)[4], 
              slope = coefficients(bf_svl_by_superfamily)[2] - coefficients(bf_svl_by_superfamily)[7], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_svl_by_superfamily)[1] + coefficients(bf_svl_by_superfamily)[5], 
              slope = coefficients(bf_svl_by_superfamily)[2] + coefficients(bf_svl_by_superfamily)[8], colour = "deepskyblue1") +
  labs(colour = "Superfamily/Infraorder")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

bf_bm_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Anguimorpha" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes( y =log(max_bf), x = log(max_bm), group=HigherTaxonomy, colour=HigherTaxonomy, na.rm = T)) +
  geom_point()  +
  scale_color_manual(values=c("forestgreen","darkorange1","deepskyblue1")) +
  xlab("log(BM) (g)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_bm_by_superfamily)[1], 
              slope = coefficients(bf_bm_by_superfamily)[2], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_bm_by_superfamily)[1] + coefficients(bf_bm_by_superfamily)[3], 
              slope = coefficients(bf_bm_by_superfamily)[2] + coefficients(bf_bm_by_superfamily)[5], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_bm_by_superfamily)[1] + coefficients(bf_bm_by_superfamily)[4], 
              slope = coefficients(bf_bm_by_superfamily)[2] + coefficients(bf_bm_by_superfamily)[6], colour = "deepskyblue1") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

bf_hl_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes(y = log(max_bf), x = log(max_hl), group=HigherTaxonomy, colour=HigherTaxonomy)) +
  scale_color_manual(values=c("red","forestgreen","darkorange1","deepskyblue1")) +
  geom_point()  +
  xlab("log(HL) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hl_by_superfamily)[1], 
              slope = coefficients(bf_hl_by_superfamily)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hl_by_superfamily)[1] + coefficients(bf_hl_by_superfamily)[3], 
              slope = coefficients(bf_hl_by_superfamily)[2] + coefficients(bf_hl_by_superfamily)[6], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_hl_by_superfamily)[1] + coefficients(bf_hl_by_superfamily)[4], 
              slope = coefficients(bf_hl_by_superfamily)[2] + coefficients(bf_hl_by_superfamily)[7], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_hl_by_superfamily)[1] + coefficients(bf_hl_by_superfamily)[5], 
              slope = coefficients(bf_hl_by_superfamily)[2] + coefficients(bf_hl_by_superfamily)[8], colour = "deepskyblue1") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

bf_hw_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes(y = log(max_bf), x = log(max_hw), group=HigherTaxonomy, colour=HigherTaxonomy, na.rm = T)) +
  scale_color_manual(values=c("red","forestgreen","darkorange1","deepskyblue1")) +
  geom_point()  +
  xlab("log(HW) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hw_by_superfamily)[1], 
              slope = coefficients(bf_hw_by_superfamily)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hw_by_superfamily)[1] + coefficients(bf_hw_by_superfamily)[3], 
              slope = coefficients(bf_hw_by_superfamily)[2] + coefficients(bf_hw_by_superfamily)[6], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_hw_by_superfamily)[1] + coefficients(bf_hw_by_superfamily)[4], 
              slope = coefficients(bf_hw_by_superfamily)[2] + coefficients(bf_hw_by_superfamily)[7], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_hw_by_superfamily)[1] + coefficients(bf_hw_by_superfamily)[5], 
              slope = coefficients(bf_hw_by_superfamily)[2] + coefficients(bf_hw_by_superfamily)[8], colour = "deepskyblue1") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

bf_hh_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes(y=log(max_bf), x = log(max_hh), group=HigherTaxonomy, colour=HigherTaxonomy, na.rm = T)) +
  scale_color_manual(values=c("red","forestgreen","darkorange1","deepskyblue1")) +
  geom_point()  +
  xlab("log(HH) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hh_by_superfamily)[1], 
              slope = coefficients(bf_hh_by_superfamily)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hh_by_superfamily)[1] + coefficients(bf_hh_by_superfamily)[3], 
              slope = coefficients(bf_hh_by_superfamily)[2] + coefficients(bf_hh_by_superfamily)[6], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_hh_by_superfamily)[1] + coefficients(bf_hh_by_superfamily)[4], 
              slope = coefficients(bf_hh_by_superfamily)[2] + coefficients(bf_hh_by_superfamily)[7], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_hh_by_superfamily)[1] + coefficients(bf_hh_by_superfamily)[5], 
              slope = coefficients(bf_hh_by_superfamily)[2] + coefficients(bf_hh_by_superfamily)[8], colour = "deepskyblue1") +
  theme_bw() 

bf_ljl_by_superfamily_plot <- max_bf_overall %>%
  filter(HigherTaxonomy != "Sphenodontia" & HigherTaxonomy != "Gekkota") %>%
  ggplot(aes(y=log(max_bf), x = log(max_ljl), group=HigherTaxonomy, colour=HigherTaxonomy, na.rm = T)) +
  scale_color_manual(values=c("red","forestgreen","darkorange1","deepskyblue1")) +
  geom_point()  +
  xlab("log(LJL) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_ljl_by_superfamily)[1], 
              slope = coefficients(bf_ljl_by_superfamily)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_ljl_by_superfamily)[1] + coefficients(bf_ljl_by_superfamily)[3], 
              slope = coefficients(bf_ljl_by_superfamily)[2] + coefficients(bf_ljl_by_superfamily)[6], colour = "forestgreen") +
  geom_abline(intercept = coefficients(bf_ljl_by_superfamily)[1] + coefficients(bf_ljl_by_superfamily)[4], 
              slope = coefficients(bf_ljl_by_superfamily)[2] + coefficients(bf_ljl_by_superfamily)[7], colour = "darkorange1") +
  geom_abline(intercept = coefficients(bf_ljl_by_superfamily)[1] + coefficients(bf_ljl_by_superfamily)[5], 
              slope = coefficients(bf_ljl_by_superfamily)[2] + coefficients(bf_ljl_by_superfamily)[8], colour = "deepskyblue1") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

# Save the plot
# ggarrange( bf_svl_by_superfamily_plot ,bf_bm_by_superfamily_plot, bf_hl_by_superfamily_plot, bf_hw_by_superfamily_plot, bf_hh_by_superfamily_plot, bf_ljl_by_superfamily_plot,  ncol = 3, nrow = 2,  common.legend = TRUE, legend ="bottom")
# ggsave("biteforce-lepidosaurs/figures/bf-superfamily-scatterplot.png", dpi = 300, height = 6, width = 8)


