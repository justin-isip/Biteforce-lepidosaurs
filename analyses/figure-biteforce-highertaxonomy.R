# Figure for distribution of bite force by infraorder/superfamily
# Justin Isip
# July 2020
#----------------------------------------------------------------

# Load libraries
library(tidyverse)
library(viridis)
library(patchwork)

# Add rphylopic
#install.packages("remotes")
#remotes::install_github("sckott/rphylopic")
library(rphylopic)


#------------------------
# Read in the data
#------------------------
max_bf_overall <- read_csv("./data/max_bf_overall.csv")
#glimpse(max_bf_overall)


#------------------------
# Create the plot
#------------------------
ggplot(max_bf_overall,aes(x=HigherTaxonomy,y=log(max_bf),color=HigherTaxonomy))+
  geom_boxplot(show.legend=F,fill="white")+
  geom_jitter(show.legend=F,alpha=0.4) +
  xlab("Infraorder/Superfamily") + ylab("log(max-biteforce)") +
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing = unit(0, "lines")
  )
