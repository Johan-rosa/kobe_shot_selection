# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(hexbin)
library(plotly)
library(GGally)
library(tidymodels)
library(caret)

# Importando objetos relevantes ----------------------------------------------------
# objeto con las líneas de la cancha y el tema para lo gráficos
source("scripts/court_plot.R")

# importando la data 
shots_data <- read_csv("data.csv")

action_type_top15 <- shots_data %>%
  count(action_type) %>% 
  top_n(15) %>% 
  select(action_type) %>% 
  unlist()

# Agregando nuevas variables y modificando algunas de las existentes
shots_data <- shots_data %>% 
  mutate(
    time_remaning  = minutes_remaining * 60 + seconds_remaining,
    shot_distance = ifelse(shot_distance > 45, 45, shot_distance),
    action_type = ifelse(action_type %in% action_type_top15, action_type, "other"),
    shot_made_flag = factor(shot_made_flag, levels = c(0, 1), labels = c("Fallado", "Encestado"))
    ) %>% 
  select(-game_event_id, -game_id, -lat, -lon,
         -minutes_remaining, -seconds_remaining, 
         -team_name, -opponent, -shot_id) %>% 
  mutate_if(is.character, factor)


complete_cases <- shots_data %>% 
  filter(!is.na(shot_made_flag))



# dividiendo la data en set de entrenamiento y de prueba

set.seed(123)
shots_split <- initial_split(complete_cases)
shots_train <- training(shots_split)
shots_test <- testing(shots_split)





# agregando algunas 
shot_rec <- recipe(shot_made_flag ~ ., data = shots_train) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

test_proc <- bake(shot_rec, new_data = shots_test)

# KNN
knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>%
  set_mode("classification")

knn_fit <- knn_spec %>% 
  fit(shot_made_flag ~ . , data = juice(shot_rec))

# Random Forest
rforest_spc <- rand_forest(mode = "classification", mtry = 10, trees = 800) %>% 
  set_engine("ranger")

rforest_fit <- rforest_spc %>% 
  fit(shot_made_flag ~ ., data = juice(shot_rec))


#load("kb_modeling_ws")

### Visualizaciones ----------------------------------------------------------------

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


kd_shots <- with(
  subset(shots_data, action_type == "Jump Shot" & shot_made_flag == 1),
  MASS::kde2d(loc_x, loc_y, n = 30)
  )

plotly::plot_ly(x = kd_shots$x, y = kd_shots$y, z = kd_shots$z) %>% plotly::add_surface()


train %>% 
  select(minutes_remaining, seconds_remaining, playoffs,
         shot_distance, shot_made_flag, shot_zone_range) %>% 
  ggpairs(aes(color = factor(shot_made_flag)))



