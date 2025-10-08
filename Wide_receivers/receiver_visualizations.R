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
library(gt)
library(gtExtras)
library(nflplotR)
library(nflreadr)


rosters <- nflreadr::load_rosters(2022) |> 
  mutate(gsis_it_id = as.numeric(gsis_it_id))

# Top targeted
wrs_targeted_vs_nottargeted |>
  slice_max(targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Top Effortful WRs when Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Effort Score %", targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(targeted_dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  tab_options(
    table.width = pct(40) 
  ) |> 
  opt_align_table_header(align = "center")

# Bottom targeted
wrs_targeted_vs_nottargeted |>
  slice_min(targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Bottom Effortful WRs when Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Effort Score %", targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(targeted_dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  tab_options(
    table.width = pct(40) 
  ) |> 
  opt_align_table_header(align = "center")

# Top not targeted
wrs_targeted_vs_nottargeted |>
  slice_max(not_targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, not_targeted_dis_score, not_targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Top Effortful WRs when not Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", not_targeted_dis_score = "Effort Score %", not_targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(not_targeted_dis_score),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  tab_options(
    table.width = pct(40) 
  ) |> 
  opt_align_table_header(align = "center")

# Bottom not targeted
wrs_targeted_vs_nottargeted |>
  slice_min(not_targeted_dis_score, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, not_targeted_dis_score, not_targeted_rank) |> 
  gt() |>
  tab_header(title = md("**Bottom Effortful WRs when not Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", not_targeted_dis_score = "Effort Score %", not_targeted_rank = "Effort Rank") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(not_targeted_dis_score),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  tab_options(
    table.width = pct(40) 
  ) |> 
  opt_align_table_header(align = "center")

# Biggest difference
wrs_targeted_vs_nottargeted |>
  slice_max(score_diff, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank, 
         not_targeted_dis_score, not_targeted_rank, score_diff, rank_diff) |> 
  gt() |>
  tab_header(title = md("**Biggest Difference of WRs' Effort When Not Targeted vs. Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Targeted Effort Score %", targeted_rank = "Targeted Rank",
             not_targeted_dis_score = "Not Targeted Effort Score %", not_targeted_rank = "Not Targeted Rank",
             score_diff = "Effort Difference %", rank_diff = "Rank Difference") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(score_diff),
             fn = scales::col_numeric(palette = c("white", "#D50A0A"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")

# Smallest difference
wrs_targeted_vs_nottargeted |>
  slice_min(score_diff, n = 10) |> 
  left_join(select(rosters, nflId = gsis_it_id, gsis_id)) |> 
  select(displayName, gsis_id, targeted_dis_score, targeted_rank, 
         not_targeted_dis_score, not_targeted_rank, score_diff, rank_diff) |> 
  gt() |>
  tab_header(title = md("**Biggest Difference of WRs' Effort When Not Targeted vs. Targeted for the NFL 2022 season**"),
             subtitle = md("*Utilized QGAM to calculate score*")) |>
  cols_label(displayName = "Player", gsis_id = "", targeted_dis_score = "Targeted Effort Score %", targeted_rank = "Targeted Rank",
             not_targeted_dis_score = "Not Targeted Effort Score %", not_targeted_rank = "Not Targeted Rank",
             score_diff = "Effort Difference %", rank_diff = "Rank Difference") |>
  nflplotR::gt_nfl_headshots(columns = gsis_id, height = 60) |> 
  data_color(columns = c(score_diff),
             fn = scales::col_numeric(palette = c("#0072B2", "white"), domain = NULL))  |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center")
