library(tidyverse)
library(qgam)


# qgam model --------------------------------------------------------------
receivers_eff <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_wrs |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, dir_a_mphs, hadPassReception, gameId, displayName, nflId, playId, frameId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mphs >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mphs < 0)
  
  qgam_fit_a_top <- qgam(dir_a_mphs ~ s(s_mph, k = 10, bs = "ad"),
                         data = data_pos,
                         qu = .95,
                         multicore = TRUE,
                         ncores = 7)
  
  qgam_fit_a_bottom <- qgam(dir_a_mphs ~ s(s_mph, k = 10, bs = "ad"),
                            data = data_neg,
                            qu = .05,
                            multicore = TRUE,
                            ncores = 7)
  
  data_pos_final <- data_pos |> 
    mutate(qgam_pred = qgam_fit_a_top$fitted.values,
           diff_a = qgam_pred - dir_a_mphs)
  
  data_neg_final <- data_neg |> 
    mutate(qgam_pred = qgam_fit_a_bottom$fitted.values,
           diff_a = dir_a_mphs - qgam_pred)
  
  
  # Combining positive and negative vals
  player_runs_test_preds <- rbind(data_pos_final, data_neg_final)
  
  if (graph == TRUE) {
    out_line <- player_runs_test_preds |> 
      filter(diff_a <= 0)
    
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = s_mph, y = dir_a_mphs)) +
      geom_point(alpha=.6, color="grey2")+
      geom_point(data = out_line, aes(x = s_mph, y = dir_a_mphs, fill = "Adjusted distance = 0"), 
                 stroke = 1.2, color="black", shape = 21) +
      geom_line(data = data_pos_final, aes(y = qgam_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
      geom_line(data = data_neg_final, aes(y = qgam_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.3, lty = 2) +
      scale_color_manual("Line", values = c("#D50A0A", "#0072B2")) +
      scale_fill_manual("Point", values = c("#b3b3b3")) +
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
receivers_qgam <- purrr::map(wrs_names$displayName, receivers_eff) |>
  bind_rows()

# Writing data into new file
# write.csv(receivers_qgam, "receivers_effort.csv")

dis_score_wrs <- receivers_qgam |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mphs < 0, dis_score / 2, dis_score))

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

# Player level (got ball vs. didn't)
wrs_play_offball <- dis_score_wrs |> 
  group_by(gameId, playId, as.factor(hadPassReception), nflId, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup()

