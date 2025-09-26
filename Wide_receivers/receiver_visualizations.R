library(tidyverse)
library(gt)

# Average DOT -------------------------------------------------------------

wrs_dot <- wrs_play |> 
  left_join(select(plays, passLength, gameId, playId))

# Relationship of dis_score and passlength by play level
wrs_dot |> 
  ggplot(aes(x = passLength, y = dis_score)) + 
  geom_point()

dot <- wrs_dot |> 
  group_by(nflId, displayName) |> 
  summarize(avgPassLength = mean(passLength)) |> 
  ungroup()

wrs_targeted_vs_nottargeted <- wrs_targeted_vs_nottargeted |> 
  left_join(dot)

# Relationship of dis_score and passlength by player level
wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = avgPassLength, y = targeted_dis_score)) + 
  geom_point()

wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = avgPassLength, y = not_targeted_dis_score)) + 
  geom_point()

# No real relationship between the two


# 20+ yard passes --------------------------------------------------------------
wrs_dot_perc <- wrs_dot |> 
  group_by(nflId, displayName) |> 
  summarize(long_passes = sum(passLength >= 20) / n()) |> 
  ungroup()

wrs_targeted_vs_nottargeted <- wrs_targeted_vs_nottargeted |> 
  left_join(wrs_dot_perc)

# Relationship of dis_score and long_passes by player level
wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = long_passes, y = targeted_dis_score)) + 
  geom_point()

wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = long_passes, y = not_targeted_dis_score)) + 
  geom_point()

# Targets ----------------------------------------------------------
targets <- dis_score_wrs |> 
  group_by(gameId, playId, nflId, displayName) |> 
  summarize(targeted = max(wasTargettedReceiver)) |> 
  ungroup() |> 
  group_by(nflId, displayName) |> 
  summarize(targets = sum(targeted)) |> 
  ungroup()

wrs_targeted_vs_nottargeted <- wrs_targeted_vs_nottargeted |> 
  left_join(targets)

# Relationship of dis_score and number of targets by player level
wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = targets, y = targeted_dis_score)) + 
  geom_point()

wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = targets, y = not_targeted_dis_score)) + 
  geom_point()


# Tables ------------------------------------------------------------------


