library(tidyverse)


# Loading in the data -----------------------------------------------------

# Original data frames
tracking <- arrow::read_parquet("data/tracking.parquet")
games <- read_csv("data/games.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
plays <- read_csv("data/plays.csv")

# Putting plays in the correct direction
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
    a_y = dir_y * a,
    a_x2 = ((s_x - lag(s_x)) / 0.1),
    a_y2 = ((s_y - lag(s_y)) / 0.1)
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

# Keeping only frames between handoff and end of play event (out of bounds, tackle, TD)
tracking_rb_runs <- tracking_rb_runs |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_handoff = frameId[which(event == "handoff")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_handoff), !is.na(frame_end)) |> 
  filter(frameId >= frame_handoff & frameId <= frame_end) |> 
  arrange(gameId, playId, nflId, frameId)

tracking_bc <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  left_join(select(players, nflId, weight)) |> 
  mutate(ke = 0.5 * weight * s^2,
         m_x = weight * s_x, # momentum in the x
         dis_x = ifelse(gameId == lag(gameId) & playId == lag(playId), x - lag(x), NA),
         work = ifelse(gameId==lag(gameId) & playId == lag(playId), ke-lag(ke), NA),
         positive_work = ifelse(gameId==lag(gameId) & playId == lag(playId), pmax(ke-lag(ke),0), NA),
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA),
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA),
         s_mph = s * (3600 / 1760),
         a_mpsh = a * (3600 / 1760),
         dir_a = a*cos(dir_rad),
         dir_a_mpsh = dir_a*(3600/1760),
         dir_a_right = (s_x * a_x2 + s_y * a_y2) / sqrt(s_x ^ 2 + s_y^2),
         dir_a_right_mpsh = dir_a_right * (3600/1760))

tracking_bc_after_contact <- tracking_bc |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_contact = frameId[which(event == "first_contact")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_contact), !is.na(frame_end)) |> 
  filter(frameId >= frame_contact & frameId <= frame_end)

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
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded)) |> 
  left_join(select(player_play, playId, bc_id = nflId, gameId, rushingYards))

# Adding after contact stats
after_contact <- tracking_bc_after_contact |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained_x_ac = sum(dis_x),
            avg_accel_ac = mean(a),
            time_ac = n() / 10,
            mean_ke_ac = mean(ke)) |> 
  ungroup() |> 
  mutate(weighted_ke_ac = (1 + sqrt(time_ac)) * mean_ke_ac)

# Looking at statistics per rush
rb_stats_per_play <- rb_stats_per_play |> 
  left_join(after_contact) |> 
  mutate(dis_gained_x_ac = ifelse(is.na(dis_gained_x_ac), 0, dis_gained_x_ac),
         avg_accel_ac = ifelse(is.na(avg_accel_ac), 0, avg_accel_ac)) 

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
    avg_jerk = mean(avg_jerk),
    avg_dis_gained_ac = mean(dis_gained_x_ac),
    avg_acc_ac = mean(avg_accel_ac)
  ) |> 
  ungroup()

# Filtering for at least 20 rushes
rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20) 

# USED TO FILTER RBs
rbs_names <- rb_stats_total_filtered$displayName

tracking_bc_filtered <- tracking_bc |> 
  filter(displayName %in% rbs_names)


# Rolling avg every 5 frames ----------------------------------------------

#Example
#first 4 rows are NA
#frame 5's value= avg of frames 1-5
#frame 6's value= avg of frames 2-6
#frame 7's value=avg of frames 3-7

library(zoo) #for rollmean() function

tracking_rb_avg <- tracking_bc |>
  arrange(gameId, playId, nflId, frameId) |> 
  group_by(gameId, playId, nflId, displayName) |> 
  mutate(s_5=rollmean(s_mph,k=5,fill=NA, align="right"),
         dir_a_mpsh_5=rollmean(dir_a_right_mpsh, k=5, fill=NA, align="right")) |> 
  ungroup() |> 
  filter(displayName %in% rbs_names) |> 
  filter(!is.na(s_5), !is.na(dir_a_mpsh_5))


# QGAM with Rolling Avg ---------------------------------------------------

library(qgam)

# QGAM Function -----------------------------------------------------------

eff_function_qgam <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_rb_avg |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_5, dir_a_mpsh_5, gameId, displayName, bc_id, playId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mpsh_5 >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mpsh_5 < 0)
  
  # Models
  # speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
  
  qgam_fit_a_top <- qgam(dir_a_mpsh_5 ~ s(s_5, k = 10, bs = "ad"),
                         data = data_pos,
                         qu = .95,
                         multicore = TRUE,
                         ncores = 7)
  
  qgam_fit_a_bottom <- qgam(dir_a_mpsh_5 ~ s(s_5, k = 10, bs = "ad"),
                            data = data_neg,
                            qu = .05,
                            multicore = TRUE,
                            ncores = 7)
  
  data_pos_final <- data_pos |> 
    mutate(qgam_pred = qgam_fit_a_top$fitted.values,
           diff_a = qgam_pred - dir_a_mpsh_5)
  
  data_neg_final <- data_neg |> 
    mutate(qgam_pred = qgam_fit_a_bottom$fitted.values,
           diff_a = dir_a_mpsh_5 - qgam_pred)
  
  
  # Combining positive and negative vals
  player_runs_test_preds <- rbind(data_pos_final, data_neg_final)

  return(player_runs_test_preds)
}

# Player Tests
eff_function_qgam("Saquon Barkley", graph = TRUE)
eff_function_qgam("Rex Burkhead", graph = TRUE)
eff_function_qgam("Christian McCaffrey", graph = TRUE)

#test for Saquon
test <- eff_function_qgam("Saquon Barkley") |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mpsh_5 < 0, dis_score / 2, dis_score)) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  mutate(dis_score = round(dis_score, 4) *100)


# Running function for all players
# qgam_combined <- purrr::map(rbs_names, eff_function_qgam) |>
#   bind_rows()

# write.csv(qgam_combined, "RollingVelocityQGAM.csv")
qgam_combined <- read.csv("created_data/RollingVelocityQGAM.csv")

qgam_dis <- qgam_combined |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mpsh_5 < 0, dis_score / 2, dis_score))

# Player level
qgam_dis_player <- qgam_dis |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score)) |> 
  mutate(dis_score = round(dis_score, 4) *100,
         rank = 1:n())

# Play level
qgam_dis_play <- qgam_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score_qgam = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score_qgam)) |> 
  mutate(dis_score_qgam = round(dis_score_qgam, 4) *100,
         rank = 1:n())

# Graph QGAM-------------------------------------------------------------------
qgam_graph <- function(name) {
  qgam_dis2 <- qgam_dis |> 
    filter(displayName == name)
  
  qgam_pos <- qgam_dis |> 
    filter(displayName == name,
           qgam_pred >= 0)
  
  qgam_neg <- qgam_dis |> 
    filter(displayName == name,
           qgam_pred < 0)
  
  graph <- qgam_dis2 |> 
    ggplot(aes(x = s_5, y = dir_a_mpsh_5)) +
    geom_point(alpha=.6, aes(fill = dis_score_adj), pch = 21, size = 3) +
    geom_line(data = qgam_pos, aes(y = qgam_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
    geom_line(data = qgam_neg, aes(y = qgam_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
    geom_hline(aes(yintercept = 0), color = "black", lwd = 1.3, lty = 2) +
    scale_color_manual("Line", values = c("#D50A0A", "#0072B2")) +
    scale_fill_gradient(name = "Effort Score", low = "#a4f5ef", high = "goldenrod") +
    labs(x = "Speed (mph)",
         y = "Acceleration (mph/s)",
         title = paste0(name)) +
    theme_minimal(base_size=16) +
    theme(plot.title = element_text(face = "bold.italic",
                                    size = 18, 
                                    hjust = .5),
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.text=element_text(size=15),
          plot.caption = element_text(face = "italic", size = 8),
          legend.key.height = unit(1.4, "cm")) +
    guides(
      fill = guide_colorbar(
        title.theme = element_text(margin = margin(b = 20)),  # add space below title
        barwidth = unit(0.5, "cm"),
        barheight = unit(5, "cm")
      )
    ) +
    xlim(0, 22.5) +
    ylim(-17.5, 17.5)
  
  return(graph)
}

qgam_graph("Christian McCaffrey")
qgam_graph("Khalil Herbert")



# GT Table ----------------------------------------------------------------

library(gt)
library(gtExtras)
library(nflplotR)
library(nflreadr)

# Getting player heads
rosters <- nflreadr::load_rosters(2022) |> 
  mutate(gsis_it_id = as.numeric(gsis_it_id))

qgam_dis_player |>
  slice_max(dis_score, n = 10) |> 
  left_join(select(rosters, bc_id = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top RBs for Effort Score**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")

qgam_dis_player |>
  slice_min(dis_score, n = 10) |> 
  left_join(select(rosters, bc_id = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Bottom RBs for Effort Score**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")


# Quadratic Function ------------------------------------------------------

eff_function_nlrq <- function(name) {
  # Choosing player name
  player_runs <- tracking_rb_avg |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_5, dir_a_mpsh_5, gameId, displayName, bc_id, playId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mpsh_5 >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mpsh_5 < 0)
  
  # Models
  nlrq_pos <- nlrq(dir_a_mpsh_5 ~ x * s_5^2 + y * s_5 + z,
                   data = data_pos,
                   tau = .95,
                   start = list(x = 10, y = 2, z = 5))
  
  nlrq_neg <- nlrq(dir_a_mpsh_5 ~ x * s_5^2 + y * s_5 + z,
                   data = data_neg,
                   tau = .05,
                   start = list(x = 10, y = 2, z = 5))
  
  data_pos_final <- data_pos |> 
    mutate(nlrq_pred = nlrq_pos$m$fitted(),
           diff_a = nlrq_pred - dir_a_mpsh_5)
  
  data_neg_final <- data_neg |> 
    mutate(nlrq_pred = nlrq_neg$m$fitted(),
           diff_a = dir_a_mpsh_5 - nlrq_pred)
  
  
  # Combining positive and negative vals
  player_runs_test_preds <- rbind(data_pos_final, data_neg_final)

  return(player_runs_test_preds)
}

# Mapping the function 
nlrq_combined <- purrr::map(rbs_names, eff_function_nlrq) |>
  bind_rows()

nlrq_dis <- nlrq_combined |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mpsh_5 < 0, dis_score / 2, dis_score))

# Player level
nlrq_dis_player <- nlrq_dis |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score)) |> 
  mutate(dis_score = round(dis_score, 4) *100,
         rank = 1:n())

# Play level
nlrq_dis_play <- nlrq_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score_nlrq = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score_nlrq)) |> 
  mutate(dis_score_nlrq = round(dis_score_nlrq, 4) *100,
         rank = 1:n())

# Graph nlrq-------------------------------------------------------------------
nlrq_graph <- function(name) {
  nlrq_dis2 <- nlrq_dis |> 
    filter(displayName == name)
  
  nlrq_pos <- nlrq_dis |> 
    filter(displayName == name,
           nlrq_pred >= 0)
  
  nlrq_neg <- nlrq_dis |> 
    filter(displayName == name,
           nlrq_pred < 0)
  
  graph <- nlrq_dis2 |> 
    ggplot(aes(x = s_5, y = dir_a_mpsh_5)) +
    geom_point(alpha=.6, aes(fill = dis_score_adj), pch = 21, size = 3) +
    geom_line(data = nlrq_pos, aes(y = nlrq_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
    geom_line(data = nlrq_neg, aes(y = nlrq_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
    geom_hline(aes(yintercept = 0), color = "black", lwd = 1.3, lty = 2) +
    scale_color_manual("Line", values = c("#D50A0A", "#0072B2")) +
    scale_fill_gradient(name = "Effort Score", low = "#a4f5ef", high = "goldenrod") +
    labs(x = "Speed (mph)",
         y = "Acceleration (mph/s)",
         title = paste0(name)) +
    theme_minimal(base_size=16) +
    theme(plot.title = element_text(face = "bold.italic",
                                    size = 18, 
                                    hjust = .5),
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.text=element_text(size=15),
          plot.caption = element_text(face = "italic", size = 8),
          legend.key.height = unit(1.4, "cm")) +
    guides(
      fill = guide_colorbar(
        title.theme = element_text(margin = margin(b = 20)),  # add space below title
        barwidth = unit(0.5, "cm"),
        barheight = unit(5, "cm")
      )
    ) + 
    xlim(0, 22.5) +
    ylim(-17.5, 17.5)
  
  return(graph)
}

nlrq_graph("Christian McCaffrey")
nlrq_graph("Khalil Herbert")


# GT Tables ---------------------------------------------------------------

library(gt)
library(gtExtras)
library(nflplotR)
library(nflreadr)

# Getting player heads
rosters <- nflreadr::load_rosters(2022) |> 
  mutate(gsis_it_id = as.numeric(gsis_it_id))

nlrq_dis_player |>
  slice_max(dis_score, n = 10) |> 
  left_join(select(rosters, bc_id = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top RBs for Effort Score**"),
             subtitle = md("*Utilized Quadratic Function Method to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")

nlrq_dis_player |>
  slice_min(dis_score, n = 10) |> 
  left_join(select(rosters, bc_id = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Bottom RBs for Effort Score**"),
             subtitle = md("*Utilized Quadratic Function Method to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")

# Combining graphs with the Magick package --------------------------------
library(magick)

#qgam
cmc_qgam <- image_read("cmsac_presentation/images/CMC_qgam.png")
kh_qgam <- image_read("cmsac_presentation/images/Herbert_qgam.png")
cmc_qgam <- image_scale(cmc_qgam, "x500") 
kh_qgam <- image_scale(kh_qgam, "x500") 

Christian_Khalil_qgam <- image_append(c(cmc_qgam, kh_qgam)) 
image_write(Christian_Khalil_qgam, "Christian_Khalil_qgam.png")

#nlrq
cmc_nlrq <- image_read("cmsac_presentation/images/CMC_nlrq.png")
kh_nlrq <- image_read("cmsac_presentation/images/Herbert_nlrq.png")
cmc_nlrq <- image_scale(cmc_nlrq, "x500") 
kh_nlrq <- image_scale(kh_nlrq, "x500") 

Christian_Khalil_nlrq <- image_append(c(cmc_nlrq, kh_nlrq)) 
image_write(Christian_Khalil_nlrq, "Christian_Khalil_nlrq.png")


# Correlation Table -------------------------------------------------------
play_stats_effort <- nlrq_dis_play |> 
  select(-rank) |> 
  left_join(select(qgam_dis_play, gameId, playId, bc_id, displayName, dis_score_qgam)) |> 
  left_join(tracking_bc_play_stats)

correlations <- data.frame(type = c("QGAM", "Quadratic"), 
                           dis_gained_ac = round(c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$dis_gained_x_ac),
                                                   cor(play_stats_effort$dis_score_nlrq, play_stats_effort$dis_gained_x_ac)), 3),
                           EPA = round(c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$expectedPointsAdded),
                                         cor(play_stats_effort$dis_score_nlrq, play_stats_effort$expectedPointsAdded)), 3),
                           rushingYards = round(c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$rushingYards),
                                                  cor(play_stats_effort$dis_score_nlrq, play_stats_effort$rushingYards)), 3))

correlations |>
  gt() |>
  tab_header(title = md("**Effort metrics do not show a strong correlation with play outcomes**"))|>
  cols_label(type = "Effort metric type", dis_gained_ac = "Yards gained after contact", EPA = "Expected points added",
             rushingYards = "Rushing Yards") |>
  gtExtras::gt_theme_espn() |> 
  gtsave(file = "EffortCorrelations.png",
         vwidth = 800,
         vheight = 200)
