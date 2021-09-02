
# load packages -----
library(googlesheets4)
library(tidyverse)
library(gganimate)


# data -----
df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1l3dfi1_8yHRYTYZODdDH5Eu7T0c5S4zwsU27_eTVW-A/edit?resourcekey#gid=1833286313"
) %>% 
  janitor::clean_names()



# eda -----
p <- 
  df %>% 
  group_by(names) %>% 
  
  summarise(n = n()) %>% 
  ggplot(aes(y = reorder(names, n), x = n, fill = names)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = n), fill = "white") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(
    title = "Jordan's New Dog's Name",
    y = NULL,
    x = "Count (#)",
    caption = "Data Source | Google Sheets"
  )

# save plot 
ggsave(
  filename = here::here("images","2021-09-02_jordans-pet.png"),
  plot = p,
  width = 10,
  height = 5
)

