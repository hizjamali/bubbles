# project started on 08/08/2018 as part of AFDS excercise for data visualization
# data includes first few measurements from nano bubble glasshouse trial at ACRI

library(tidyverse)
library(ggplot2)

bubbles <- read_csv(("data/bubbles.csv"))            #upload data

bubbles


#Effect of bubbles on plant height
nanobubbles <- as_tibble(bubbles)  
  
nanobubbles %>% 
  #filter(datetime %in% c("23/07/2013")) %>%
  ggplot(aes(x = datetime, y = height_cm, colour = treatment)) +
  geom_boxplot(size = 1) +
  scale_y_continuous(limits =c(0,110)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Effect of Bubbles on Plant Height",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 45, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))
  
#Effect of bubbles on number of leaves
nanobubbles <- as_tibble(bubbles)  

nanobubbles %>% 
  #filter(datetime %in% c("23/07/2013")) %>%
  ggplot(aes(x = datetime, y = leaves, colour = treatment)) +
  geom_boxplot(size = 1) +
  scale_y_continuous(limits =c(0,10)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Number of leaves",
    title = "Effect of Bubbles on Number of Leaves",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 45, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))


