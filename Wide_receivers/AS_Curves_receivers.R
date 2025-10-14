library(tidyverse)
library(qgam)


# qgam model --------------------------------------------------------------
receivers_eff <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_wrs_avg |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_5, dir_a_mphs_5, hadPassReception, wasTargettedReceiver, gameId, displayName, nflId, playId, frameId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mphs_5 >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mphs_5 < 0)
  
  qgam_fit_a_top <- qgam(dir_a_mphs_5 ~ s(s_5, k = 10, bs = "ad"),
                         data = data_pos,
                         qu = .95,
                         multicore = TRUE,
                         ncores = 7)
  
  qgam_fit_a_bottom <- qgam(dir_a_mphs_5 ~ s(s_5, k = 10, bs = "ad"),
                            data = data_neg,
                            qu = .05,
                            multicore = TRUE,
                            ncores = 7)
  
  data_pos_final <- data_pos |> 
    mutate(qgam_pred = qgam_fit_a_top$fitted.values,
           diff_a = qgam_pred - dir_a_mphs_5)
  
  data_neg_final <- data_neg |> 
    mutate(qgam_pred = qgam_fit_a_bottom$fitted.values,
           diff_a = dir_a_mphs_5 - qgam_pred)
  
  
  # Combining positive and negative vals
  player_runs_test_preds <- rbind(data_pos_final, data_neg_final)
  
  if (graph == TRUE) {
    out_line <- player_runs_test_preds |> 
      filter(diff_a <= 0)
    
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = s_5, y = dir_a_mphs_5, fill = as.factor(wasTargettedReceiver))) +
      geom_point(alpha=.6, shape = 21)+
      geom_line(data = data_pos_final, aes(y = qgam_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
      geom_line(data = data_neg_final, aes(y = qgam_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.3, lty = 2) +
      scale_color_manual("Line", values = c("#D50A0A", "#0072B2")) +
      scale_fill_manual("Was Targetted Receiver?", values = c("#b3b3b3", "gold2")) +
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
      xlim(0, 25) +
      ylim(-20, 20)
    return(player_graph)
  }
  return(player_runs_test_preds)
}

# Test players
receivers_eff("D.J. Moore", graph = TRUE)


# Calculating Effort ------------------------------------------------------
# Running the function for all qualified wrs
receivers_qgam_avg <- purrr::map(wrs_names$displayName, receivers_eff) |>
  bind_rows()

# Writing data into new file
# write.csv(receivers_qgam_avg, "receivers_effort_rollingavg.csv")

receivers_qgam <- read.csv("created_data/receivers_effort_rollingavg.csv") |> 
  select(-X)

dis_score_wrs <- receivers_qgam |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mphs_5 < 0, dis_score / 2, dis_score))

# Play level overall
wrs_play <- dis_score_wrs |> 
  group_by(gameId, playId, nflId, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() 

# Player level
wrs_player <- dis_score_wrs |> 
  group_by(nflId, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score)) |> 
  mutate(dis_score = round(dis_score, 4) *100,
         rank = 1:n())

# Player level (was targeted vs. didn't)
wrs_play_targeted <- dis_score_wrs |> 
  filter(wasTargettedReceiver == 1) |> 
  group_by(nflId, displayName) |> 
  summarize(targeted_dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(targeted_dis_score)) |> 
  mutate(targeted_dis_score = round(targeted_dis_score, 4) *100,
         targeted_rank = 1:n())

wrs_play_not_targeted <- dis_score_wrs |> 
  filter(wasTargettedReceiver == 0) |> 
  group_by(nflId, displayName) |> 
  summarize(not_targeted_dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(not_targeted_dis_score)) |> 
  mutate(not_targeted_dis_score = round(not_targeted_dis_score, 4) *100,
         not_targeted_rank = 1:n())

wrs_targeted_vs_nottargeted <- wrs_play_targeted |> 
  left_join(wrs_play_not_targeted, by = c("nflId", "displayName")) |> 
  mutate(score_diff = not_targeted_dis_score - targeted_dis_score,
         rank_diff = not_targeted_rank- targeted_rank)

# Positive diff means more effort on non-targeted plays
# Negative diff means more effort on targeted plays


# Graph -------------------------------------------------------------------

wrs_qgam_graph <- function(name) {
  dis_score_wrs_2 <- dis_score_wrs |> 
    filter(displayName == name)
  
  qgam_pos <- dis_score_wrs |> 
    filter(displayName == name,
           qgam_pred >= 0)
  
  qgam_neg <- dis_score_wrs |> 
    filter(displayName == name,
           qgam_pred < 0)
  
  graph <- dis_score_wrs_2 |> 
    ggplot(aes(x = s_5, y = dir_a_mphs_5)) +
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

wrs_qgam_graph("D.J. Chark")
wrs_qgam_graph("Tyreek Hill")
