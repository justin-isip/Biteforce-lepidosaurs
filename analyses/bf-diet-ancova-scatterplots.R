# Diet analysis
# Fit PGLS models (ANCOVA) and create scatterplots of relationships between bite-force and each morphological variables based on diet
# Sep 2020

## Load libraries
library(tidyverse)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Fit PGLS models (ANCOVA) to test for relationships between bite-force and morphological variables based on diet
##------------------------------------------------

bf_svl_by_diet <- pgls(log(max_bf) ~ log(max_svl) * MieriDiet, data = lepidosaur, 
                       lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_svl_by_diet)
anova(bf_svl_by_diet)
summary(bf_svl_by_diet)


bf_bm_by_diet <- pgls(log(max_bf) ~ log(max_bm) * MieriDiet, data = lepidosaur, 
                      lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_bm_by_diet)
anova(bf_bm_by_diet)
summary(bf_bm_by_diet)


bf_hl_by_diet <- pgls(log(max_bf) ~ log(max_hl) * MieriDiet, data = lepidosaur, 
                      lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hl_by_diet)
anova(bf_hl_by_diet)
summary(bf_hl_by_diet)


bf_hw_by_diet <- pgls(log(max_bf) ~ log(max_hw) * MieriDiet, data = lepidosaur, 
                      lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hw_by_diet)
anova(bf_hw_by_diet)
summary(bf_hw_by_diet)

bf_hh_by_diet <- pgls(log(max_bf) ~ log(max_hh) * MieriDiet, data = lepidosaur, 
                      lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hh_by_diet)
anova(bf_hh_by_diet)
summary(bf_hh_by_diet)

bf_ljl_by_diet <- pgls(log(max_bf) ~ log(max_ljl) * MieriDiet, data = lepidosaur, 
                       lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_ljl_by_diet)
anova(bf_ljl_by_diet)
summary(bf_ljl_by_diet)


##------------------------------------------------
## For comparison with previous studies that did not account for phylogeny, fit linear models (ANCOVA)
##------------------------------------------------


bf_svl_by_diet_lm <- lm(log(max_bf) ~ log(max_svl)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_svl_by_diet_lm)
anova(bf_svl_by_diet_lm)
summary(bf_svl_by_diet_lm)

bf_bm_by_diet_lm <- lm(log(max_bf) ~ log(max_bm)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_bm_by_diet_lm, smooth.colour = NA)
anova(bf_bm_by_diet_lm)
summary(bf_bm_by_diet_lm)


bf_hl_by_diet_lm <- lm(log(max_bf) ~ log(max_hl)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_hl_by_diet_lm, smooth.colour = NA)
anova(bf_hl_by_diet_lm)
summary(bf_hl_by_diet_lm)


bf_hw_by_diet_lm <- lm(log(max_bf) ~ log(max_hw)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_hw_by_diet_lm, smooth.colour = NA)
anova(bf_hw_by_diet_lm)
summary(bf_hw_by_diet_lm)

bf_hh_by_diet_lm <- lm(log(max_bf) ~ log(max_hh)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_hh_by_diet_lm, smooth.colour = NA)
anova(bf_hh_by_diet_lm)
summary(bf_hh_by_diet_lm)

bf_ljl_by_diet_lm <- lm(log(max_bf) ~ log(max_ljl)* MieriDiet, data = max_bf_overall)

# Check model diagnostics to make sure everything is ok
autoplot(bf_ljl_by_diet_lm, smooth.colour = NA)
anova(bf_ljl_by_diet_lm)
summary(bf_ljl_by_diet_lm)


##------------------------------------------------
## Create scatterplots of bite-force against each morphological variables with separate regression lines for each diet
## Regression lines come from PGLS models
##------------------------------------------------


bf_svl_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(y =log(max_bf), x = log(max_svl), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point() +
  xlab("log(SVL) (mm)") +
  ylab("log(BF)(N)")  +
  geom_abline(intercept = coefficients(bf_svl_by_diet)[1], 
              slope = coefficients(bf_svl_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_svl_by_diet)[1] + coefficients(bf_svl_by_diet)[3], 
              slope = coefficients(bf_svl_by_diet)[2] + coefficients(bf_svl_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_svl_by_diet)[1] + coefficients(bf_svl_by_diet)[4], 
              slope = coefficients(bf_svl_by_diet)[2] + coefficients(bf_svl_by_diet)[6], colour = "blue") +
  labs(colour = "Diet")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

bf_bm_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes( y =log(max_bf), x = log(max_bm), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point()  +
  xlab("log(BM) (g)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_bm_by_diet)[1], 
              slope = coefficients(bf_bm_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_bm_by_diet)[1] + coefficients(bf_bm_by_diet)[3], 
              slope = coefficients(bf_bm_by_diet)[2] + coefficients(bf_bm_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_bm_by_diet)[1] + coefficients(bf_bm_by_diet)[4], 
              slope = coefficients(bf_bm_by_diet)[2] + coefficients(bf_bm_by_diet)[6], colour = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")

bf_hl_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(y = log(max_bf), x = log(max_hl), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point()  +
  xlab("log(HL) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hl_by_diet)[1], 
              slope = coefficients(bf_hl_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hl_by_diet)[1] + coefficients(bf_hl_by_diet)[3], 
              slope = coefficients(bf_hl_by_diet)[2] + coefficients(bf_hl_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_hl_by_diet)[1] + coefficients(bf_hl_by_diet)[4], 
              slope = coefficients(bf_hl_by_diet)[2] + coefficients(bf_hl_by_diet)[6], colour = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")

bf_hw_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(y = log(max_bf), x = log(max_hw), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point()  +
  xlab("log(HW) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hw_by_diet)[1], 
              slope = coefficients(bf_hw_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hw_by_diet)[1] + coefficients(bf_hw_by_diet)[3], 
              slope = coefficients(bf_hw_by_diet)[2] + coefficients(bf_hw_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_hw_by_diet)[1] + coefficients(bf_hw_by_diet)[4], 
              slope = coefficients(bf_hw_by_diet)[2] + coefficients(bf_hw_by_diet)[6], colour = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")

bf_hh_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(y=log(max_bf), x = log(max_hh), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point()  +
  xlab("log(HH) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_hh_by_diet)[1], 
              slope = coefficients(bf_hh_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_hh_by_diet)[1] + coefficients(bf_hh_by_diet)[3], 
              slope = coefficients(bf_hh_by_diet)[2] + coefficients(bf_hh_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_hh_by_diet)[1] + coefficients(bf_hh_by_diet)[4], 
              slope = coefficients(bf_hh_by_diet)[2] + coefficients(bf_hh_by_diet)[6], colour = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")

bf_ljl_by_diet_plot <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(y=log(max_bf), x = log(max_ljl), group=MieriDiet, colour=MieriDiet, na.rm = T)) +
  geom_point()  +
  xlab("log(LJL) (mm)") +
  ylab("log(BF)(N)") +
  geom_abline(intercept = coefficients(bf_ljl_by_diet)[1], 
              slope = coefficients(bf_ljl_by_diet)[2], colour = "red") +
  geom_abline(intercept = coefficients(bf_ljl_by_diet)[1] + coefficients(bf_ljl_by_diet)[3], 
              slope = coefficients(bf_ljl_by_diet)[2] + coefficients(bf_ljl_by_diet)[5], colour = "green") +
  geom_abline(intercept = coefficients(bf_ljl_by_diet)[1] + coefficients(bf_ljl_by_diet)[4], 
              slope = coefficients(bf_ljl_by_diet)[2] + coefficients(bf_ljl_by_diet)[6], colour = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# Save the plot
# ggarrange( bf_svl_by_diet_plot , bf_bm_by_diet_plot, bf_hl_by_diet_plot, bf_hw_by_diet_plot, bf_hh_by_diet_plot, bf_ljl_by_diet_plot,  ncol = 3, nrow = 2,  common.legend = TRUE, legend ="bottom")  
# ggsave("figures/bf-diet-scatterplot.png", dpi = 300, width = 10)
