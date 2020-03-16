library(tidyverse)
library(hexbin)
library(jpeg)
library(raster)
library(grid)
library(RCurl)
library(plotly)


source("scripts/court_plot.R")




shots_data <- read_csv("data.csv")
train <- shots_data %>%
  filter(!is.na(shot_made_flag))



shots_data %>% 
  group_by(opponent) %>% 
  summarise(
    fgp = mean(shot_made_flag, na.rm = TRUE) * 100
  ) %>% 
  mutate(
    opponent = fct_reorder(opponent, fgp)
  ) %>% 
  ggcharts::lollipop_chart(opponent, fgp)

  ggplot(aes(x = opponent, y = fgp)) +
   +
  coord_flip()


shots_data %>% 
  ggplot(aes(y = lat, x = lon)) +
  geom_point(size = 0.2, alpha = 0.4) +
  #geom_raster(interpolate = TRUE) +
  scale_y_reverse() +
  theme_minimal()

plot_court(court, court_theme = court_themes$light)

plot_court(court_theme = court_themes$light) +
  geom_point(data = shots_data,
             aes(x = loc_x/10,
                 y = ((loc_y/10)) + 5),
             alpha = 0.11) 

# shots density plots  
plot_court(court_theme = court_themes$light) +
  stat_density_2d(
    data = filter(train, action_type == "Jump Shot"),
    aes(x = loc_x/10,
        y = ((loc_y/10)) + 5,
        color = factor(shot_made_flag),
        fill = ..level..,
        alpha = ..level..
        ),
    geom = "polygon"
  ) +
  guides(color = FALSE, alpha = FALSE) +
  facet_wrap(~shot_made_flag) +
  scale_fill_viridis_c() +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 8)
  )

# hexbin 

plot_court(court_theme = court_themes$light) +
  geom_hex(
    data = filter(train, action_type == "Jump Shot", season == "2015-16"),
    aes(x = loc_x/10,
               y = ((loc_y/10)) + 5),
    bins = 40) +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 8)
  )


kd_shots <- with(subset(train, action_type == "Jump Shot" & shot_made_flag == 1), MASS::kde2d(loc_x, loc_y, n = 30))
plot_ly(x = kd_shots$x, y = kd_shots$y, z = kd_shots$z) %>% add_surface()





