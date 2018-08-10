# project started on 08/08/2018 as part of AFDS excercise for data visualization
# data includes first few measurements from nano bubble glasshouse trial at ACRI

library(tidyverse)
library(ggplot2)

bubbles <- read_csv(("data/bubbles.csv"))            #upload data

bubbles

#Create unique ID by uniting two columns but keep original columns
bubbles1 <- as.tibble(bubbles) %>% 
  unite(date_pot, datetime, pot, sep = "_", remove = FALSE)



# Scatter plot: Effect of bubbles on height

bubbles1 %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  ggplot(aes(x = datetime, y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_jitter(size = 4, alpha = 0.5, width = 0.3) +
  #geom_smooth(method = 'lm') + #to see if slope differs
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


# Treatment vs height
#geom_smooth can't run as x-axis is chr

bubbles1 %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  #filter((datetime == "30/07/2018")) %>% 
  filter(species=="corn") %>% 
  ggplot(aes(x = treatment, y = height_cm, colour = datetime)) +  
  geom_jitter(size = 4, alpha = 0.5) +
  #geom_smooth(method = 'lm') + #to see if slope differs
  scale_y_continuous(limits =c(0,110)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Effect of Bubbles on Plant Height",
    colour = "Date") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 90, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

#Effect of bubbles on plant height

bubbles1 %>% 
  #filter(datetime %in% c("30/07/2013")) %>%
  filter((datetime == "30/07/2018")) %>%
  filter(species=="cotton") %>% 
  ggplot(aes(x = treatment, y = height_cm, colour = datetime)) +
  geom_boxplot(size = 0.5) +
  geom_smooth(method = 'lm') +
  scale_y_continuous(limits =c(0,20)) +
  facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Effect of Bubbles on Plant Height",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 0, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))

ggsave("figures/boxplot_height.png", width = 6, height = 6)
  
#Boxplot# Effect of bubbles on number of leaves
 

bubbles1 %>% 
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
  

bubbles1 %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  ggplot(aes(x = leaves, y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_jitter(size = 3, alpha = 0.5) +
  #geom_smooth(method = 'lm') + #to see if slope differs
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

summary(nanobubbles)

bubbles1 %>% 
  filter(species == "corn")%>%
  #group_by() %>% 
  ggplot(aes(x = as.Date(datetime), y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_point(size = 2) +
  geom_smooth(method = 'loess') + #to see if slope differs
  ylim(40,90)+
  #scale_y_continuous(limits =c(40,100)) +
  facet_wrap(~species) +
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



#Show average and standard errors in bars

bubbles1 %>% 
  #filter(datetime %in% c("30/07/2018")) %>%
  filter(species=="corn)") %>% 
  ggplot(aes(x = treatment, y = height_cm, colour = treatment)) + #as.Date convert date from chr to a factor for geom_smooth 
  geom_col() +
  
  geom_smooth(method = 'lm') + #to see if slope differs
  scale_y_continuous(limits =c(0,110)) +
  #facet_wrap(~species) +
  labs(                                      #labels 
    x= "Date",
    y = "Height (cm)",
    title = "Effect of Bubbles on Plant Height",
    colour = "Treatment") +
  theme(panel.grid = element_blank(), #remove panel lines
        axis.text.x = element_text(angle = 90, size=10),
        panel.background = element_rect(fill = "white",
                                        colour = "black"))





