# Create density plot of distribution of bite-force and morphological variables
# Sep 2020

## Load libraries
library(tidyverse)
library(patchwork)
library(grid)

##----------------------------------------------
## Add colour blind palette for plot
##----------------------------------------------

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##----------------------------------------------
## Create a density plot for each morphological
## variable using colour blind palette and
## add sample size annotation (n = x)
##----------------------------------------------


bf_distribution <-  ggplot(max_bf_overall, aes(x=log(max_bf))) +
  geom_density(color=cbPalette[2], fill=cbPalette[2], alpha=0.3) +
  theme_bw() +
  xlab("log(BF) (N)") +
  annotate(geom = "text",x = 5, y = 0.3,label = "n = 161", size = 6) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

svl_distribution <-  ggplot(max_bf_overall, aes(x=log(max_svl))) +
  geom_density(colour = cbPalette[1], fill = cbPalette[1], alpha=0.3) +
  theme_bw() +
  xlab("log(SVL) (mm)") +
  annotate(geom = "text", x = 5.47, y = 1, label = "n = 161", size = 6) +
  ylim(0,1.2)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

bm_distribution <-  ggplot(max_bf_overall, aes(x=log(max_bm))) +
  geom_density(colour = cbPalette[3], fill = cbPalette[3],  alpha=0.3) +
  theme_bw() +
  xlab("log(BM) (g)") +
  annotate(geom = "text", x = 5.5, y = 0.26, label = "n = 42", size = 6) +
  ylim(0,0.3)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


hl_distribution <-  ggplot(max_bf_overall, aes(x=log(max_hl))) +
  geom_density(colour = cbPalette[4], fill = cbPalette[4],  alpha=0.3) +
  theme_bw() +
  xlab("log(HL) (mm)") +
  annotate(geom = "text", x = 4, y = 1, label = "n = 136", size = 6)+
  ylim(0,1.2)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

hh_distribution <-  ggplot(max_bf_overall, aes(x=log(max_hh))) +
  geom_density(colour = cbPalette[5], fill = cbPalette[5],  alpha=0.3) +
  theme_bw() +
  xlab("log(HH) (mm)") +
  annotate(geom = "text", x = 3.5, y = 0.8, label = "n = 136", size = 6)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


hw_distribution <-  ggplot(max_bf_overall, aes(x=log(max_hw))) +
  geom_density(colour = cbPalette[6], fill = cbPalette[6],  alpha=0.3) +
  theme_bw() +
  xlab("log(HW) (mm)") +
  annotate(geom = "text", x = 3.55, y = 0.8, label = "n = 142", size = 6)+
  ylim(0, 1)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ljl_distribution <-  ggplot(max_bf_overall, aes(x=log(max_ljl))) +
  geom_density(colour = cbPalette[7], fill = cbPalette[7],  alpha=0.3) +
  theme_bw() +
  xlab("log(LJL) (mm)") +
  annotate(geom = "text", x = 4, y = 0.8, label = "n = 93", size = 6)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

##----------------------------------------------
## Create patchwork grid of all morphological
## variables
##----------------------------------------------

bf_distribution + svl_distribution + bm_distribution+ hl_distribution + hw_distribution + hh_distribution + ljl_distribution + plot_layout(ncol = 2, nrow = 4, byrow = T)

# Save the plot
ggsave("figures/bf-morph-density.png", dpi = 300, height = 10, width = 8)

