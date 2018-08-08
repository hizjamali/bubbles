# project started on 08/08/2018 as part of AFDS excercise for data visualization
# data includes first few measurements from nano bubble glasshouse trial at ACRI

library(tidyverse)
library(ggplot2)

bubbles <- read_csv(("data/bubbles.csv"))            #upload data

bubbles

#
nanobubbles <- as_tibble(bubbles)
bubbles_plot1 <- ggplot(data = nanobubbles,
                       aes(x = date(date), y = height_cm, colour = treatment)) +
  geom_point(size = 2) +
  scale_y_continuous(limits =c(0,110)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Corn",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 75, size=7),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))
