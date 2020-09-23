# Create mean bite-force ± 1 SE plot for each of the lepidosaur families coloured by diet
# Sep 2020

## Load libraries
library(tidyverse)

##------------------------------------------------
## Read in the data 
##------------------------------------------------

max_bf_overall <- read.csv("data/max_bf_overall.csv", stringsAsFactors = T)

##------------------------------------------------
## Create plot showing the distribution of bite-forces among the 20 lepidosaur families in my dataset based on diet.
## Colours represent the three diet categories (herbivores, omnivores and carnivores).
## Larger points represent the mean bite-force for the family, error bars represent ± 1 SE.
##------------------------------------------------

# First, lets summarise the mean bite-force and standard errors for each diet category
diet.summary <- max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  group_by(MieriDiet) %>%
  summarise(
    MeanBF = mean(log(max_bf)),
    SeBF = sd(log(max_bf))/sqrt(n())
  )

# Plot the data
max_bf_overall %>%
  filter(!is.na(MieriDiet)) %>%
  ggplot(aes(x = MieriDiet, y = log(max_bf), colour = MieriDiet)) +
  geom_point(alpha = 0.6) +
  geom_point(data = diet.summary, aes(y = MeanBF, size = 2), show.legend =  FALSE) +
  geom_errorbar(data = diet.summary, aes(y = MeanBF, ymin = MeanBF - SeBF, ymax = MeanBF + SeBF, width = 0.1)) +
  theme_bw() +
  xlab("") +
  ylab("log(BF) (N)") +
  labs(colour=("Diet")) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none")

# Save the plot
# ggsave("biteforce-lepidosaurs/figures/bf-diet-mean-se.png", dpi = 300, height = 6)


##------------------------------------------------
## Fit PGLS ANOVA model testing whether
## bite-force differs between diet category
## Remember you need to combine the data with 
## the phylogenyfirst to run PGLS models
##------------------------------------------------

bf_diet <- pgls(log(max_bf) ~ MieriDiet, data = lepidosaur,
                lambda = "ML")

# Check model diagnostics to make sure everything is ok
par(mfrow=c(2,2))
plot(bf_diet)
anova(bf_diet)
summary(bf_diet)

