theme_set(theme_bw())
library(tidyverse)

# Data pre-processing -----------------------------------------------------

# Making sure tracking is same orientation
tracking <- tracking |>
  mutate(
    # Plays will always go from left to right
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    # flip player direction and orientation
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

# Don't care about pre-snap plays
tracking <- tracking |> 
  filter(frameType != "BEFORE_SNAP")

# Getting velocity in the x (endzone) and y (sideline) direction
tracking <- tracking |>
  mutate(
    # Converting degrees into radians
    dir_rad = pi * (dir / 180),
    # get angle of motion in x (endzone) and y (sideline) direction
    # NA checks are for the ball
    dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
    dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
    # directional speed (also known as velocity)
    s_x = dir_x * s,
    s_y = dir_y * s,
    # directional acceleration
    a_x = dir_x * a,
    a_y = dir_y * a
  )  

# Filtering for all running back plays
# bc_id = ball carrier id, bc_club = team id
plays_rb_runs <- player_play |> 
  # Make sure it's a running play
  filter(hadRushAttempt == 1) |> 
  left_join(select(players, nflId, position)) |> 
  filter(position == "RB") |> 
  select(gameId, playId, bc_id = nflId, bc_club = teamAbbr)

# Tracking data will now show only plays with running backs
tracking_rb_runs <- tracking |> 
  inner_join(plays_rb_runs)

## TRACKING FOR ALL RUNNING BACK PLAYS
# Keeping only frames between handoff and end of play event (out of bounds, tackle, TD)
tracking_rb_runs <- tracking_rb_runs |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_handoff = frameId[which(event == "handoff")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_handoff), !is.na(frame_end)) |> 
  filter(frameId >= frame_handoff & frameId <= frame_end)

## TRACKING FOR RUNNING BACKS ONLY FOR RUNNING BACK PLAYS
# Tracking data for ball carriers only
tracking_bc <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  left_join(select(players, nflId, weight)) |> 
  mutate(ke = 0.5 * weight * s^2,
         m_x = weight * s_x, # momentum in the x
         dis_x = ifelse(gameId == lag(gameId) & playId == lag(playId), x - lag(x), NA),
         work = ifelse(gameId==lag(gameId) & playId == lag(playId), ke-lag(ke), NA),
         positive_work = ifelse(gameId==lag(gameId) & playId == lag(playId), pmax(ke-lag(ke),0), NA),
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA),
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA))



# Running back stats per play
rb_stats_per_play <- tracking_bc |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained = sum(dis),
            mean_ke = mean(ke),
            mean_m_x = mean(m_x),
            sd_ke=sd(ke),
            dis_gained_x = sum(dis_x, na.rm = TRUE),
            mean_pos_work = mean(positive_work, na.rm=TRUE),
            sd_pos_work = sd(positive_work, na.rm=TRUE),
            effort_consistency = mean_ke/sd_ke,
            total_pos_work=sum(positive_work, na.rm=TRUE),
            avg_accel = mean(a),
            avg_COD = mean(COD, na.rm = TRUE) / n(),
            avg_jerk = mean(jerk, na.rm=TRUE)) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded))

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    avg_dis_gained_x = mean(dis_gained_x, na.rm = TRUE) / n(),
    mean_ke = mean(mean_ke),
    avg_sd_ke=mean(sd_ke, na.rm=TRUE),
    avg_sd_work= mean(sd_pos_work, na.rm=TRUE),
    avg_effort_consistency =mean(effort_consistency, na.rm=TRUE),
    mean_m_x = mean(mean_m_x),
    mean_pos_work = mean(mean_pos_work, na.rm=TRUE),
    total_pos_work = sum(total_pos_work, na.rm=TRUE),
    total_yards_gained = sum(yardsGained),
    avg_yards_gained = mean(yardsGained),
    avg_EPA = mean(expectedPointsAdded),
    num_of_rushes = n(),
    avg_accel = mean(avg_accel),
    avg_COD = mean(avg_COD),
    avg_jerk = mean(avg_jerk)
  ) |> 
  ungroup()

summary(rb_stats_total$num_of_rushes) # Do a minimum of 20 rushes to eliminate players with low rushes

rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)


# GMM ---------------------------------------------------------------------
# Variables used:
# Jerk, KE, avg_dis_gained_x
library(mclust)
library(broom)

# Making the ideal "effortful player"
ideal_player <- data.frame(displayName = "Ideal Player", 
                           mean_ke = max(rb_stats_total_filtered$mean_ke),
                           # avg_COD = max(rb_stats_total_filtered$avg_COD),
                           avg_jerk = max(rb_stats_total_filtered$avg_jerk),
                           avg_EPA = max(rb_stats_total_filtered$avg_EPA)
                           # avg_dis_gained_x = max(rb_stats_total_filtered$avg_dis_gained_x)
)

# Adding the ideal player to the data set
rb_gmm_players <- rb_stats_total_filtered |> 
  select(displayName, mean_ke, avg_jerk, avg_EPA) |> 
  rbind(ideal_player)

# Scaling the data
rb_mclust <- rb_gmm_players |> 
  select(mean_ke, avg_jerk, avg_EPA) |> 
  Mclust() 

# Viewing results
rb_mclust |> 
  tidy() 
summary(rb_mclust)

rb_gmm_clustered <- rb_mclust |> 
  augment() |> 
  mutate(displayName = (rb_gmm_players$displayName))

rb_gmm_probs <- as.tibble(rb_mclust$z)
colnames(rb_gmm_probs) <- c("cluster1", "cluster2")

rb_gmm_clustered <- rb_gmm_clustered |> 
  mutate(cluster1 = rb_gmm_probs$cluster1,
         cluster2 = rb_gmm_probs$cluster2) |> 
  mutate(effort_score = round(cluster2 * 100, 3))


# k-means -----------------------------------------------------------------
library(factoextra)
# Clustering plays
# PCA
play_features_pca <- rb_stats_per_play |> 
  select(mean_ke, rushingYards, avg_jerk, dis_gained_x_ac, expectedPointsAdded) |> 
  prcomp(center = TRUE, scale. = TRUE)

summary(play_features_pca)

play_features_matrix <- play_features_pca$x

play_features_pca |> 
  fviz_pca_biplot(label = "var",
                  alpha.ind = .25,
                  alpha.var = .75,
                  label.size = 5,
                  col.var = "darkblue",
                  repel = TRUE)

## k-means
# Scaling data
play_features <- rb_stats_per_play |> 
  select(mean_ke, rushingYards, avg_jerk, dis_gained_x_ac, expectedPointsAdded)

play_features_clean <- rb_stats_per_play |> 
  select(mean_ke, rushingYards, avg_jerk, dis_gained_x_ac, expectedPointsAdded) |> 
  scale()

# elbow plot
play_features_clean |> 
  fviz_nbclust(FUNcluster = kmeans, method = "wss")

# 5 clusters
kmeans_features <- play_features_clean |> 
  kmeans(centers = 5, nstart = 100)

# Plotting the clusters
kmeans_features |> 
  fviz_cluster(data = play_features,
               geom = "point",
               ellipse = FALSE)

# Adding cluster numbers
plays_clustered <- play_features |> 
  mutate(cluster = kmeans_features$cluster)

# Summarizing clusters
cluster_stats <- plays_clustered |> 
  group_by(cluster) |> 
  summarize(avg_ke = mean(mean_ke),
            avg_rushingYards = mean(rushingYards),
            avg_jerk = mean(avg_jerk),
            avg_dis_gained_x_ac = mean(dis_gained_x_ac),
            avg_EPA = mean(expectedPointsAdded))
# Cluster 2 has the most effortful plays
table(plays_clustered$cluster)
