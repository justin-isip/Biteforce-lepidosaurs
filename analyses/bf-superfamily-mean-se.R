# Create mean bite-force ± 1 SE plot for each of the lepidosaur families coloured by superfamily/infraorder group
# Sep 2020

## Load libraries
library(tidyverse)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Create plot showing the distribution of bite-forces among the 20 lepidosaur families in my dataset. 
## Colours represent the superfamily/infraorder groups. 
## Larger points represent the mean bite-force for the family, error bars represent ± 1 SE.
##------------------------------------------------

# First, relevel families so they are in alphabetical order
max_bf_overall <- 
  max_bf_overall %>%
  mutate(Family = factor(Family, levels = c("Agamidae", "Chamaeleonidae", "Crotaphytidae", "Dactyloidae", "Iguanidae", "Leiosauridae", "Liolaemidae", "Phrynosomatidae", "Tropiduridae", "Lacertidae", "Teiidae", "Trogonophidae", "Cordylidae", "Scincidae", "Anguidae", "Varanidae", "Xenosauridae","Gekkonidae", "Phyllodactylidae", "Sphenodontidae")))

# Create a summary of the mean bite-force and SE for each family
family.summary <- max_bf_overall %>%
  group_by(Family) %>%
  summarise(
    MeanBF = mean(log(max_bf)),
    SeBF = sd(log(max_bf))/sqrt(n()),
    HigherTaxonomy = HigherTaxonomy # had to add a higher taxonomy column as wouldn't work without!
  ) %>%
  distinct()

# Create the plot 

ggplot(max_bf_overall, aes(x = Family, y = log(max_bf), colour = HigherTaxonomy)) +
  geom_point(alpha = 0.5) +
  geom_point(data= family.summary, aes(y = MeanBF, size = 1), show.legend =  FALSE) +
  geom_errorbar(data = family.summary, aes(y = MeanBF, ymin = MeanBF - SeBF, ymax = MeanBF + SeBF)) +
  scale_colour_manual(values = c("red","goldenrod4","forestgreen","darkorange1","deepskyblue1", "deeppink1")) +
  theme_bw() +
  xlab("") +
  ylab("log(BF) (N)") +
  labs(colour=("Superfamily/Infraorder")) +
  coord_flip() +
  ylim(-1, 6)  +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title =  element_text(size = 15),
        legend.position = "bottom")


# Save the plot
# ggsave("figures/bf-superfamily-mean-se.png", dpi = 300,width = 9, height = 10)

##------------------------------------------------
## Fit PGLS ANOVA model testing whether
## bite-force differs between superfamily/infraorder group
## Remember you need to combine the data with 
## the phylogenyfirst to run PGLS models
##------------------------------------------------

# Run the model
bf_highertax <- pgls(log(max_bf) ~ HigherTaxonomy, data = lepidosaur,
                     lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_highertax)
anova(bf_highertax)
summary(bf_highertax)

