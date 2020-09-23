# Fit PGLS models (linear regression) and create scatterplots of relationships between bite-force and each morphological variables
# using regression lines from PGLS models
# Sep 2020

## Load libraries
library(tidyverse)
library(patchwork)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Fit PGLS models (linear regression) to test for relationships between bite-force and morphological variables.
##------------------------------------------------

bf_svl <- pgls(log(max_bf) ~ log(max_svl), data = lepidosaur,
               lambda = "ML")

# Check model diagnostics to make sure everything is ok
# The first two plots show the fit of the phylogenetic residuals from the model to a normal distribution: 
# a density plot of the residuals and a normal Q-Q plot. 
# The second two plots scatterplots show pattern in the distribution of the fitted values against the observed and residual values.
# PGLS also has an additional diagnostic that looks at phylogenetic residuals in a density plot.


par(mfrow=c(2,2))
plot(bf_svl)
anova(bf_svl)
summary(bf_svl)


bf_bm <- pgls(log(max_bf) ~ log(max_bm), data = lepidosaur,
              lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_bm)
anova(bf_bm)
summary(bf_bm)

bf_hl <- pgls(log(max_bf) ~ log(max_hl), data = lepidosaur,
              lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hl)
anova(bf_hl)
summary(bf_hl)

bf_hw <- pgls(log(max_bf) ~ log(max_hw), data = lepidosaur,
              lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hw)
anova(bf_hw)
summary(bf_hw)

bf_hh <- pgls(log(max_bf) ~ log(max_hh), data = lepidosaur,
              lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_hh)
anova(bf_hh)
summary(bf_hh)

bf_ljl <- pgls(log(max_bf) ~ log(max_ljl), data = lepidosaur,
               lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_ljl)
anova(bf_ljl)
summary(bf_ljl)


##------------------------------------------------
## Create scatterplots of bite-force against each morphological variable
## Regression lines come from PGLS models
##------------------------------------------------


plot_svl <- max_bf_overall %>% 
  ggplot(aes(x=log(max_svl), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE) +
  theme_bw() +
  xlab("log(SVL) (mm)") +
  ylab("log(BF)(N)") + 
  geom_abline(slope = coefficients(bf_svl)[2], 
              intercept = coefficients(bf_svl)[1]) +
  ylim(-2.5, 7.5) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")

plot_bm <-  max_bf_overall %>% 
  ggplot(aes(x=log(max_bm), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE) +
  theme_bw() +
  xlab("log(BM) (g)") +
  ylab("log(BF)(N)")+ 
  geom_abline(slope = coefficients(bf_bm)[2], 
              intercept = coefficients(bf_bm)[1]) +
  ylim(-2.5, 7.5)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")


plot_hl <- max_bf_overall %>% 
  ggplot(aes(x=log(max_hl), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE) +
  theme_bw() +
  xlab("log(HL) (mm)") +
  ylab("log(BF)(N)")+ 
  geom_abline(slope = coefficients(bf_hl)[2], 
              intercept = coefficients(bf_hl)[1]) +
  ylim(-2.5, 7.5)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")


plot_hh <- max_bf_overall %>% 
  ggplot(aes(x=log(max_hh), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE) +
  theme_bw() +
  xlab("log(HH) (mm)") +
  ylab("log(BF)(N)")+ 
  geom_abline(slope = coefficients(bf_hh)[2], 
              intercept = coefficients(bf_hh)[1]) +
  ylim(-2.5, 7.5)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")


plot_hw <-  max_bf_overall %>% 
  ggplot(aes(x=log(max_hw), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE) +
  theme_bw() +
  xlab("log(HW) (mm)") +
  ylab("log(BF)(N)")+ 
  geom_abline(slope = coefficients(bf_hw)[2], 
              intercept = coefficients(bf_hw)[1]) +
  ylim(-2.5, 7.5)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")


plot_ljl <- max_bf_overall %>% 
  ggplot(aes(x=log(max_ljl), y =log(max_bf))) + 
  geom_point(alpha = 0.8, na.rm = TRUE)+
  theme_bw() +
  xlab("log(LJL) (mm)") +
  ylab("log(BF)(N)")+ 
  geom_abline(slope = coefficients(bf_ljl)[2], 
              intercept = coefficients(bf_ljl)[1]) +
  ylim(-2.5, 7.5)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none")


##----------------------------------------------
## Create patchwork grid of all morphological
## variables
##----------------------------------------------

# Patchwork grid plot
# plot_svl + plot_bm + plot_hl + plot_hh + plot_hw + plot_ljl + plot_layout(ncol = 3, nrow = 4)

# GGarrange grid plot
# ggarrange( plot_svl , plot_bm, plot_hl, plot_hh, plot_hw, plot_ljl,  ncol = 3, nrow = 2)

# Save the plot
ggsave("figures/bf-morph-regression-scatterplot.png", dpi = 300, width = 8, height = 6)


