
# load packages -----
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(gganimate)


# data -----
df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1l3dfi1_8yHRYTYZODdDH5Eu7T0c5S4zwsU27_eTVW-A/edit?resourcekey#gid=1833286313"
) %>% 
  janitor::clean_names() %>% 
  mutate(timestamp = round_date(as_datetime(timestamp), "1 mins"))

# eda -----
# create static plot
p <- 
  df %>% 
  filter(clean_email == 1) %>% 
  group_by(names) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(y = reorder(names, n), x = n, fill = names)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = n), fill = "white") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(
    title = "Jordan's New Dog's Name",
    subtitle = glue::glue("Out of a total of {sum(df$clean_email, na.rm = TRUE)} responses"),
    y = NULL,
    x = "Count (#)",
    caption = "Data Source | Google Sheets"
  )

# check plot 
p


# save plot 
ggsave(
  filename = here::here("images","2021-09-02_jordans-pet.png"),
  plot = p,
  width = 10,
  height = 5
)


# create gif
g <- 
  df %>% 
  filter(clean_email == 1) %>%
  group_by(names, timestamp) %>% 
  summarise(n = n()) %>% 
  mutate(n = cumsum(n)) %>% 
  group_by(names) %>% 
  mutate(time = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(y = reorder(names, n), x = n, fill = names)) + 
  geom_col(color = "black") + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  geom_label(aes(label = as.character(n)), fill = "white") + 
  transition_reveal(time) + 
  labs(
    title = "Jordan's New Dog's Name",
    subtitle = glue::glue("Out of a total of {sum(df$clean_email, na.rm = TRUE)} responses"),
    y = NULL,
    x = "Count (#)",
    caption = "Data Source | Google Sheets"
  )

# animate 
animate(
  g,
  fps = 30,
  duration = 15,
  end_pause = 70
)

# save animation
gganimate::anim_save(
  filename =  "2021-09-02_jordans-pet.gif",
  path = here::here("images"),
  animation = last_animation()
)
