# load the data
load(url("https://github.com/bariscr/data/raw/master/name_data_turkstat_2019.Rdata"))
################################################################################
library(tidyverse)
library(gganimate)
library(ggdark)
# transform data
name_data <- name_data_turkstat_2019 %>% 
  mutate(name_order = 
           case_when(is.na(name_order) == TRUE ~ 999,
                     TRUE ~ name_order)) %>% 
  filter(sex == 1) 

top_names <- name_data %>% filter(name_order <= 10) %>%
  select(name, year, name_order) %>% distinct()

p <- ggplot(top_names, aes(name_order, group = name, fill = as.factor(name))) +
  geom_text(aes(y = 0, label = paste0(name_order, ". ", name, " ")),
            hjust = "center", fontface = "bold", size = 8) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  dark_theme_minimal() +
  labs(title='{closest_state}', caption="Data: Turkstat", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')
  anim <- animate(p, fps = 25, duration = 30, width = 800, height = 600)

anim_save("output.gif", anim)
