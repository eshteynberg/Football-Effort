
# Player Level ------------------------------------------------------------

# Combining the effort metrics obtained from Ellipse.R and Nonlinear_AccAndSpeed_qgam.R
validating_effort_metrics <- rb_stats_total_filtered |> 
  left_join(ellipse_above) |> 
  left_join(adj_dis_scores_players_k15) |> 
  left_join(dis_scores_players_k15)

## EPA Analysis
validating_effort_metrics |> 
  ggplot(aes(x = ellipse_score, y = avg_EPA)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(validating_effort_metrics$ellipse_score, y = validating_effort_metrics$avg_EPA)

validating_effort_metrics |> 
  ggplot(aes(x = adj_dis_score_below, y = avg_EPA)) +
  geom_point()

validating_effort_metrics |> 
  ggplot(aes(x = prop_between, y = avg_EPA)) +
  geom_point()

validating_effort_metrics |> 
  ggplot(aes(x = dis_score_below, y = avg_EPA)) +
  geom_point()
# Not good :(

## Average distance after catch
validating_effort_metrics |> 
  ggplot(aes(x = ellipse_score, y = avg_dis_gained_ac)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(validating_effort_metrics$ellipse_score, y = validating_effort_metrics$avg_dis_gained_ac)

validating_effort_metrics |> 
  ggplot(aes(x = adj_dis_score_below, y = avg_dis_gained_ac)) +
  geom_point()

validating_effort_metrics |> 
  ggplot(aes(x = prop_between, y = avg_dis_gained_ac)) +
  geom_point()

validating_effort_metrics |> 
  ggplot(aes(x = dis_score_below, y = avg_dis_gained_ac)) +
  geom_point()

## Average yards gained
validating_effort_metrics |> 
  ggplot(aes(x = ellipse_score, y = avg_yards_gained)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(validating_effort_metrics$ellipse_score, y = validating_effort_metrics$avg_yards_gained)

validating_effort_metrics |> 
  ggplot(aes(x = adj_dis_score_below, y = avg_yards_gained)) +
  geom_point() 

validating_effort_metrics |> 
  ggplot(aes(x = dis_score_below, y = avg_dis_gained_ac)) +
  geom_point()

## Number of rushes (rather this not be correlated)
validating_effort_metrics |> 
  ggplot(aes(x = ellipse_score, y = num_of_rushes)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(validating_effort_metrics$ellipse_score, y = validating_effort_metrics$num_of_rushes)

validating_effort_metrics |> 
  ggplot(aes(x = adj_dis_score_below, y = num_of_rushes)) +
  geom_point() 

validating_effort_metrics |> 
  ggplot(aes(x = dis_score_below, y = num_of_rushes)) +
  geom_point()

## Avg Acceleration
validating_effort_metrics |> 
  ggplot(aes(x = ellipse_score, y = avg_accel)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(validating_effort_metrics$ellipse_score, y = validating_effort_metrics$avg_accel)

validating_effort_metrics |> 
  ggplot(aes(x = adj_dis_score_below, y = avg_accel)) +
  geom_point() 

validating_effort_metrics |> 
  ggplot(aes(x = dis_score_below, y = avg_accel)) +
  geom_point() +
  geom_smooth(method = "lm")


# Play level -------------------------------------------------------------

# Combining the effort metrics obtained from Ellipse.R and Nonlinear_AccAndSpeed_qgam.R
# Utilizing tracking_bc_play_stats from LoadingTrackingBcCombined.R
effort_play <- tracking_bc_play_stats |> 
  left_join(ellipse_play) |> 
  left_join(dis_scores_plays) |> 
  left_join(ellipse_stats_play)

effort_play_ac <- effort_play |> 
  na.omit()

## WHOLE PLAY
# EPA
effort_play |> 
  ggplot(aes(x = ellipse_score, y = expectedPointsAdded)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play$expectedPointsAdded, effort_play$ellipse_score)

effort_play |> 
  ggplot(aes(x = dis_score_mix, y = expectedPointsAdded)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play$expectedPointsAdded, effort_play$dis_score_mix)

effort_play |> 
  ggplot(aes(x = final_ellipse_score, y = expectedPointsAdded)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play$expectedPointsAdded, effort_play$final_ellipse_score)

# Rushing Yards
effort_play |> 
  ggplot(aes(x = ellipse_score, y = rushingYards)) +
  geom_point() +
  geom_smooth(method = "loess")
cor(effort_play$rushingYards, effort_play$ellipse_score)

effort_play |> 
  ggplot(aes(x = dis_score_mix, y = rushingYards)) +
  geom_point() +
  geom_smooth(method = "loess")
cor(effort_play$rushingYards, effort_play$ellipse_score)

effort_play |> 
  ggplot(aes(x = final_ellipse_score, y = rushingYards)) +
  geom_point() +
  geom_smooth(method = "loess")
cor(effort_play$rushingYards, effort_play$final_ellipse_score)

# KE
effort_play |> 
  ggplot(aes(x = ellipse_score, y = mean_ke)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play$mean_ke, effort_play$ellipse_score)

## AFTER CONTACT
# Acceleration change into contact
effort_play_ac |> 
  ggplot(aes(x = ellipse_score, y = acc_change)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play_ac$acc_change, effort_play_ac$ellipse_score)

# dis gained after contact in x direction
effort_play_ac |> 
  ggplot(aes(x = ellipse_score, y = dis_gained_x_ac)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play_ac$dis_gained_x_ac, effort_play_ac$ellipse_score)

# Time after contact
effort_play_ac |> 
  ggplot(aes(x = ellipse_score, y = time_ac)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play_ac$time_ac, effort_play_ac$ellipse_score)

# COD
effort_play_ac |> 
  ggplot(aes(x = ellipse_score, y = avg_COD)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play_ac$time_ac, effort_play_ac$avg_COD)

# Linear effort metric
effort_play_ac |> 
  ggplot(aes(x = ellipse_score, y = num_of_effort_move)) +
  geom_point() +
  geom_smooth(method = "lm")
cor(effort_play_ac$time_ac, effort_play_ac$num_of_effort_move)
