library(tidyverse)
library(qgam)

# QGAM Function -----------------------------------------------------------

eff_function_qgam <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_rb_avg |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_5, dir_a_mpsh_5, gameId, displayName, bc_id, playId, frame_group)
  
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
}

# Player Tests
eff_function_qgam("Saquon Barkley", graph = TRUE)
eff_function_qgam("Rex Burkhead", graph = TRUE)
eff_function_qgam("Christian McCaffrey", graph = TRUE)

# Running function for all players
# qgam_combined <- purrr::map(rbs_names, eff_function_qgam) |>
#   bind_rows()

# write.csv(qgam_combined, "AvgVelocityQGAM.csv")
qgam_combined <- read.csv("created_data/AvgVelocityQGAM.csv") |> 
  select(-X)

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

# Graph -------------------------------------------------------------------
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
    scale_fill_gradient(name = "Effort Score", low = "#88CCEE", high = "#CC6677") +
    scale_size_continuous(name = "Effort Score", range = c(2, 6)) +
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
      fill = guide_legend(order = 1),
      size = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    xlim(0, 25) +
    ylim(-20, 20)
  
  return(graph)
}

qgam_graph("Derrick Henry")



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
