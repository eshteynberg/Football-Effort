library(qgam)
library(tidyverse)
?qgam


# Player test -------------------------------------------------------------
set.seed(1)
# Choosing player name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# 5 folds
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- player_runs |> 
  distinct(playId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Making the modeling data frame
player_runs_modeling <- player_runs |> 
  select(s_mph, a_mpsh, playId) |> 
  left_join(plays_folds_expected) |> 
  select(-playId)

player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold == x)
  train_data <- player_runs_modeling |> 
    filter(fold != x)
  
  # Modeling
  qgam_fit <- qgam(a_mpsh ~ s(s_mph),
                   data = hi)
}
  