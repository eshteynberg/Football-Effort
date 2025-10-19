library(tidyverse)
library(gt)
# install.packages("nflplotR")
library(nflplotR)

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
  geom_point() |> 
  labs(x = "Pass Lenght")

wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = avgPassLength, y = not_targeted_dis_score)) + 
  geom_point()

# No real relationship between the two for overall effort


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
  geom_point() +
  geom_smooth(method = "lm")


# Boxplot -----------------------------------------------------------------

# Play level overall
wrs_play_t_v_nt <- dis_score_wrs |> 
  group_by(gameId, playId, nflId, displayName, wasTargettedReceiver) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  mutate(wasTargettedReceiver2 = ifelse(wasTargettedReceiver == 1, "Targeted", "Not Targeted"),
         dis_score = dis_score * 100)

str(wrs_play_t_v_nt)

wrs_play_t_v_nt |> 
  ggplot(aes(x = dis_score)) + 
  geom_boxplot(fill = "lightgrey") +
  facet_wrap(~wasTargettedReceiver2, labeller = labeller(category = labels), nrow = 2) +
  scale_x_continuous(breaks = c(seq(0, 100, by = 10))) +
  labs(x = "Effort Score (%)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#0072B2", 
                                        color = "black", 
                                        size = 1),
        strip.text = element_text(color = "white", 
                                  size = 14, 
                                  face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 14,
                                    face = "bold"),
        axis.text.x = element_text(size = 12))
  


# Avg. DOT Targeted vs. Not -----------------------------------------------

wrs_play_t_v_nt <- wrs_play_t_v_nt |> 
  left_join(select(plays, passLength, gameId, playId))

wrs_play_t_v_nt |> 
  ggplot(aes(x = dis_score, y = passLength)) + 
  geom_point(alpha = .75) + 
  facet_wrap(~ wasTargettedReceiver2) + 
  labs(y = "Pass Length (Yards)",
       x = "Effort Score (%)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  theme(strip.background = element_rect(fill = "#0072B2", 
                                        color = "black", 
                                        size = 1),
        strip.text = element_text(color = "white", 
                                  size = 14, 
                                  face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,
                                  face = "bold"))

# Scatterplot of Correlation Between Scores -------------------------------------------------------------

wrs_targeted_vs_nottargeted |> 
  ggplot(aes(x = targeted_dis_score, y = not_targeted_dis_score)) + 
  geom_point() +
  xlim(20,40) +
  ylim(20,40)

cor(wrs_targeted_vs_nottargeted$targeted_dis_score, wrs_targeted_vs_nottargeted$not_targeted_dis_score)

# Tables ------------------------------------------------------------------
library(gt)
library(gtExtras)
library(nflplotR)
library(nflreadr)


rosters <- nflreadr::load_rosters(2022) |> 
  mutate(gsis_it_id = as.numeric(gsis_it_id))

# Overall Top
wrs_player |> 
  slice_max(dis_score, n = 10) |> 
  # Get rid of the tie
  filter(displayName != "Brandin Cooks") |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top Effortful WRs Overall**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsTopOverall.png",
         vwidth = 600,
         vheight = 800)

# Overall Bottom
wrs_player |> 
  slice_min(dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Bottom Effortful WRs Overall**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", dis_score = "Effort Score %", rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsBottomOverall.png",
         vwidth = 600,
         vheight = 800)

# Top targeted
wrs_targeted_vs_nottargeted |>
  slice_max(targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Top Effortful WRs when Targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Effort Score %", targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(targeted_dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsTopTargeted.png",
         vwidth = 600,
         vheight = 800)

# Bottom targeted
wrs_targeted_vs_nottargeted |>
  slice_min(targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Bottom Effortful WRs when Targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Effort Score %", targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(targeted_dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsBottomTargeted.png",
         vwidth = 600,
         vheight = 800)

# Top not targeted
wrs_targeted_vs_nottargeted |>
  slice_max(not_targeted_dis_score, with_ties = FALSE, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, not_targeted_dis_score, not_targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Top Effortful WRs when Not Targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", not_targeted_dis_score = "Effort Score %", not_targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(not_targeted_dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |> 
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsTopNotTargeted.png",
         vwidth = 600,
         vheight = 800)

# Bottom not targeted
wrs_targeted_vs_nottargeted |>
  slice_min(not_targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, not_targeted_dis_score, not_targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Bottom Effortful WRs when Not Targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", not_targeted_dis_score = "Effort Score %", not_targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(not_targeted_dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |> 
  opt_align_table_header(align = "center") |> 
  gtsave(file = "WRsBottomNotTargeted.png",
         vwidth = 600,
         vheight = 800)

# Biggest difference
wrs_targeted_vs_nottargeted |>
  slice_max(score_diff, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank, 
         not_targeted_dis_score, not_targeted_rank, score_diff, rank_diff) |> 
  gt() |>
  tab_header(title = md("**These WRs gave more effort when not targeted vs. targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Targeted Effort Score %", targeted_rank = "Targeted Rank",
             not_targeted_dis_score = "Not Targeted Effort Score %", not_targeted_rank = "Not Targeted Rank",
             score_diff = "Effort Difference %", rank_diff = "Rank Difference") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(score_diff),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "BiggestDiff1.png",
         vwidth = 1000,
         vheight = 800)
  

# Smallest difference
wrs_targeted_vs_nottargeted |>
  slice_min(score_diff, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank, 
         not_targeted_dis_score, not_targeted_rank, score_diff, rank_diff) |> 
  gt() |>
  tab_header(title = md("**These WRs gave more effort when targeted vs. not targeted**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Targeted Effort Score %", targeted_rank = "Targeted Rank",
             not_targeted_dis_score = "Not Targeted Effort Score %", not_targeted_rank = "Not Targeted Rank",
             score_diff = "Effort Difference %", rank_diff = "Rank Difference") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(score_diff),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "BiggestDiff2.png",
         vwidth = 1000,
         vheight = 800)
 