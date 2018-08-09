# project started on 08/08/2018 as part of AFDS excercise for data visualization
# data includes first few measurements from nano bubble glasshouse trial at ACRI

library(tidyverse)
library(ggplot2)

bubbles <- read_csv(("data/bubbles.csv"))            #upload data

bubbles


# Scatter plot: Effect of bubbles on height
nanobubbles <- as_tibble(bubbles)  

nanobubbles %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  ggplot(aes(x = as.Date(datetime), y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  #geom_point(size = 2) +
  geom_smooth(method = 'lm') + #to see if slope differs
  scale_y_continuous(limits =c(0,110)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Effect of Bubbles on Plant Height",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 90, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

ggsave("figures/Smooth_height.png", width = 6, height = 6)



#Effect of bubbles on plant height
nanobubbles <- as_tibble(bubbles)  
  
nanobubbles %>% 
  #filter(datetime %in% c("23/07/2013")) %>%
  ggplot(aes(x = datetime, y = height_cm, colour = treatment)) +
  geom_boxplot(size = 0.5) +
  geom_smooth(method = 'lm') +
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

ggsave("figures/boxplot_height.png", width = 6, height = 6)
  
#Boxplot# Effect of bubbles on number of leaves
nanobubbles <- as_tibble(bubbles)  

nanobubbles %>% 
  filter(species == "corn")%>%
  ggplot(aes(x = datetime, y = leaves, colour = treatment)) +
  geom_boxplot(size = 0.7) +
  scale_y_continuous(limits =c(0,10)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x = "Date",
    y = "Number of leaves",
    title = "Effect of Bubbles on Number of Leaves",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 45, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

ggsave("figures/boxplot_leaves_corn.png", width = 6, height = 6)



#Scatter height vs leaves

# Scatter plot: Effect of bubbles on height
nanobubbles <- as_tibble(bubbles)  

nanobubbles %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  ggplot(aes(x = leaves, y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_point(size = 2) +
  geom_smooth(method = 'lm') + #to see if slope differs
  scale_y_continuous(limits =c(0,110)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Number of leaves",
    y = "Height (cm)",
    title = "Relationship between Number of leaves and Plant Height",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle =, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

ggsave("figures/Scatter_Smooth_leavesVSheight.png", width = 6, height = 6)


# Calculate means Scatter plot: Effect of bubbles on height
nanobubbles <- as_tibble(bubbles)  

summary(nanobubbles)

nanobubbles %>% 
  filter(species == "cotton")%>%
  group_by(datetime) %>% 
  ggplot(aes(x = as.Date(datetime), y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_point(size = 2) +
  geom_smooth(method = 'lm') + #to see if slope differs
  scale_y_continuous(limits =c(0,15)) +
  #facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "COTTON",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle =, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

ggsave("figures/Scatter_Smooth_Cotton_Height.png", width = 6, height = 6)



