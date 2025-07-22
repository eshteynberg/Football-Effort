library(tidyverse)
library(qgam)

# Player Test -------------------------------------------------------------
set.seed(1)
# Choosing player name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# 5 folds
N_FOLDS <- 5

# # Making sure plays are in the same fold
plays_folds <- player_runs |>
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Making the modeling data frame
player_runs_modeling <- player_runs |> 
  select(s_mph, dir_a_mpsh, gameId) |> 
  left_join(plays_folds) |>
  select(-gameId)

train_data <- player_runs_modeling |> 
  filter(dir_a_mpsh > 0)

qgam_fit <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                 data = train_data,
                 qu = .99)

plot(train_data$s_mph, qgam_fit$fitted.values)

player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold == 1) |> 
    filter(dir_a_mpsh > 0)
  train_data <- player_runs_modeling |> 
    filter(fold != 1) |> 
    filter(dir_a_mpsh > 0)
  
  nlrqs <- nlrq(dir_a_mpsh ~ a * s_mph^2 + b * s_mph + c, data = train_data, start = list(a = 2, b = 2, c = 0), tau = .95)

  predict(nlrqs, newdata = test_data)
  # Modeling
  qgam_fit <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                   data = train_data,
                   qu = .99)
  qgam_fit_s <- qgam(s_mph ~ s(dir_a_mpsh, k = 10, bs = "ad"),
                     data = train_data,
                     qu = .99)
  
  out <- tibble(
    displayName = "Saquon Barkley",
    qgam_pred = predict(qgam_fit_s, newdata = test_data),
    actual_acc = test_data$dir_a_mpsh,
    actual_speed = test_data$s_mph,
    res = abs(actual_acc - qgam_pred),
    test_fold = x
  )
  return(out)
}

# Saquon
player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
  bind_rows()

# nlrq model --------------------------------------------------------------

eff_function_nlrq <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, dir_a_mpsh, gameId, displayName, bc_id, playId, frameId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mpsh >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mpsh < 0)
  
  # Models
  nlrq_pos <- nlrq(dir_a_mpsh ~ x * s_mph^2 + y * s_mph + z,
                   data = data_pos,
                   tau = .95,
                   start = list(x = 10, y = 2, z = 5))

  nlrq_neg <- nlrq(dir_a_mpsh ~ x * s_mph^2 + y * s_mph + z,
                   data = data_neg,
                   tau = .05,
                   start = list(x = 10, y = 2, z = 5))
  
  data_pos_final <- data_pos |> 
    mutate(nlrq_pred = nlrq_pos$m$fitted(),
           diff_a = nlrq_pred - dir_a_mpsh)
  
  data_neg_final <- data_neg |> 
    mutate(nlrq_pred = nlrq_neg$m$fitted(),
           diff_a = dir_a_mpsh - nlrq_pred)
  
  
  # Combining positive and negative vals
  player_runs_test_preds <- rbind(data_pos_final, data_neg_final)
  
  if (graph == TRUE) {
    out_line <- player_runs_test_preds |> 
      filter(diff_a <= 0)
    
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = s_mph, y = dir_a_mpsh)) +
      geom_point(alpha=.6, color="grey2")+
      geom_point(data = out_line, aes(x = s_mph, y = dir_a_mpsh, fill = "Adjusted distance = 0"), size = 4, 
                 stroke = 1.2, color="black", shape = 21) +
      geom_line(data = data_pos_final, aes(y = nlrq_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
      geom_line(data = data_neg_final, aes(y = nlrq_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.3, lty = 2) +
      scale_color_manual("Line", values = c("#D50A0A", "#0072B2", "goldenrod")) +
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
            legend.key.height = unit(1.4, "cm"),
            legend.position = "right") +
      xlim(0, 25) +
      ylim(-15, 15)
    return(player_graph)
  }
  return(player_runs_test_preds)
}

eff_function_nlrq("James Cook", graph = TRUE)
eff_function_nlrq("Saquon Barkley", graph = TRUE)
nlrq_combined <- purrr::map(rbs_names, eff_function_nlrq) |>
  bind_rows()

nlrq_dis <- nlrq_combined |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mpsh < 0, dis_score / 2, dis_score))

# Play level
nlrq_dis_play <- nlrq_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup()

# Player level
nlrq_dis_player <- nlrq_dis |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score)) |> 
  mutate(dis_score = round(dis_score, 4) *100,
         rank = 1:n())

## GT TABLE FOR NLRQ
nlrq_dis_player_gt <- nlrq_dis_player |> 
  filter(rank %in% c(1:5, 9, 63, 65:79))

library(gt)
library(gtExtras)
nlrq_dis_player_gt |>
  select(displayName, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top and Bottom RBs Ranked \nby Effort Metric #1**")) |>
  cols_label(displayName = "Name", dis_score = "Effort Score (%)", rank = "Rank") |>
  data_color(columns = c(rank),
             fn = scales::col_numeric(palette = c("#D50A0A","white", "#0072B2"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "Effort1Rank.png",
         vwidth = 380,
         vheight = 600)

# qgam right --------------------------------------------------------------

eff_function_qgam <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, dir_a_mpsh, gameId, displayName, bc_id, playId, frameId)
  
  # Data to be in model
  data_pos <- player_runs_modeling |> 
    filter(dir_a_mpsh >= 0)
  
  data_neg <- player_runs_modeling |> 
    filter(dir_a_mpsh < 0)
    
  # Models
  # speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
  
  qgam_fit_a_top <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                         data = data_pos,
                         qu = .95,
                         multicore = TRUE,
                         ncores = 7)
  
  qgam_fit_a_bottom <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                            data = data_neg,
                            qu = .05,
                            multicore = TRUE,
                            ncores = 7)
  
  data_pos_final <- data_pos |> 
    mutate(qgam_pred = qgam_fit_a_top$fitted.values,
           diff_a = qgam_pred - dir_a_mpsh)
  
  data_neg_final <- data_neg |> 
    mutate(qgam_pred = qgam_fit_a_bottom$fitted.values,
           diff_a = dir_a_mpsh - qgam_pred)

    
    # Combining positive and negative vals
    player_runs_test_preds <- rbind(data_pos_final, data_neg_final)
  
  if (graph == TRUE) {
    out_line <- player_runs_test_preds |> 
      filter(diff_a <= 0)
    
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = s_mph, y = dir_a_mpsh)) +
      geom_point(alpha=.6, color="grey2")+
      geom_point(data = out_line, aes(x = s_mph, y = dir_a_mpsh, fill = "Adjusted distance = 0"), size = 4, 
                 stroke = 1.2, color="black", shape = 21) +
      geom_line(data = data_pos_final, aes(y = qgam_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.3) +
      geom_line(data = data_neg_final, aes(y = qgam_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.3) +
      # geom_vline(aes(color = "Speed 0.99 quantile line", 
      #                xintercept = quantile(s_mph, probs = c(.99))), 
      #            lty = 2, lwd = 1.5) +
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
      ylim(-15, 15)
    return(player_graph)
  }
  return(player_runs_test_preds)
}

eff_function_qgam("James Cook", graph = TRUE)
eff_function_qgam("Saquon Barkley", graph = TRUE)

qgam_combined <- purrr::map(rbs_names, eff_function_qgam) |>
  bind_rows()

qgam_dis <- qgam_combined |> 
  mutate(diff_adj = ifelse(diff_a <= 0, 0, diff_a),
         dis_score = 1 / (1 + diff_adj),
         dis_score_adj = ifelse(dir_a_mpsh < 0, dis_score / 2, dis_score))

# Play level
qgam_dis_play <- qgam_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup()

# Player level
qgam_dis_player <- qgam_dis |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score)) |> 
  mutate(dis_score = round(dis_score, 4) *100,
         rank = 1:n())

# Only looking at positive acceleration
qgam_dis_player_pos <- qgam_dis |> 
  filter(dir_a_mpsh >= 0) |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score_pos_qgam = mean(dis_score_adj)) |> 
  ungroup() |> 
  arrange(desc(dis_score_pos_qgam)) |> 
  mutate(dis_score = round(dis_score_pos_qgam, 4) *100,
         rank = 1:n())

qgam_dis_play_pos <- qgam_dis |> 
  filter(dir_a_mpsh >= 0) |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score = mean(dis_score_adj)) |> 
  ungroup()

## GT TABLE FOR QGAM
qgam_dis_player_gt <- qgam_dis_player |> 
  filter(rank %in% c(1:5, 8, 58, 65:79))

library(gt)
library(gtExtras)
qgam_dis_player_gt |>
  select(displayName, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top and Bottom RBs Ranked \nby Effort Metric #2**")) |>
  cols_label(displayName = "Name", dis_score = "Effort Score (%)", rank = "Rank") |>
  data_color(columns = c(rank),
             fn = scales::col_numeric(palette = c("#D55E00","white", "#0072B2"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "Effort2Rank.png",
         vwidth = 380,
         vheight = 600)


# Looking at two metrics together -----------------------------------------
## Scatterplot
dis_scatter <- qgam_dis_player |> 
  mutate(dis_score_qgam = dis_score,
         rank_qgam = rank) |> 
  select(-dis_score, -rank) |> 
  left_join(nlrq_dis_player, by = c("bc_id", "displayName"))

label_names <- dis_scatter |> 
  filter(rank_qgam <= 5 | rank <= 5 | rank_qgam >=65 
         | rank >= 65 | displayName %in% c("Saquon Barkley", "James Cook"))

Barkley_and_cook <- dis_scatter |> 
  filter(displayName %in% c("Saquon Barkley", "James Cook"))

dis_scatter |> 
  ggplot(aes(x = dis_score, y = dis_score_qgam)) +
  geom_hline(aes(yintercept = mean(dis_score_qgam)), lwd = 1.2, lty = 2, color = "black", alpha = .7) +
  geom_vline(aes(xintercept = mean(dis_score)), lwd = 1.2, lty = 2, color = "black", alpha = .7) +
  geom_point(size = 3, alpha = .8, color = "#0072B2") +
  geom_point(data = label_names, size = 3, alpha = .8, color = "#D50A0A") +
  geom_point(data = Barkley_and_cook, size = 4, alpha = .8, shape = 21, stroke = 1.3, fill = "#D50A0A", col = "black") +
  labs(title = "Player effort metrics are positively correlated",
       x = "Effort metric #1 (Quadratic quantile regression)",
       y = "Effort metric #2 (QGAM)")+
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold")) +
  ggrepel::geom_text_repel(data = label_names, aes(label = displayName), 
                           size = 5, max.overlaps = 15,
                           fontface = "italic")
  

# Combining play sets
qgam_dis_play <- qgam_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score_qgam = mean(dis_score_adj)) |> 
  ungroup()

nlrq_dis_play <- nlrq_dis |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(dis_score_nlrq = mean(dis_score_adj)) |> 
  ungroup()


effort_plays_combined <- nlrq_dis_play |> 
  left_join(qgam_dis_play)

# Joining with play stats
play_stats_effort <- effort_plays_combined |> 
  left_join(tracking_bc_play_stats) |>
  left_join(select(plays, gameId, playId, down, quarter))

correlations <- data.frame(type = rep(c("QGAM", "Quadratic", "Mean Acceleration"), 3), 
                           dis_gained_ac = c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$dis_gained_x_ac),
                                             cor(play_stats_effort$dis_score_nlrq, play_stats_effort$dis_gained_x_ac),
                                             cor(play_stats_effort$mean_acc, play_stats_effort$dis_gained_x_ac)),
                           EPA = c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$expectedPointsAdded),
                                   cor(play_stats_effort$dis_score_nlrq, play_stats_effort$expectedPointsAdded),
                                   cor(play_stats_effort$mean_acc, play_stats_effort$expectedPointsAdded)),
                           rushingYards = c(cor(play_stats_effort$dis_score_qgam, play_stats_effort$rushingYards),
                                            cor(play_stats_effort$dis_score_nlrq, play_stats_effort$rushingYards),
                                            cor(play_stats_effort$mean_acc, play_stats_effort$rushingYards)))
# GT table of correlations
qgam_dis_player_gt |>
  select(displayName, dis_score, rank) |> 
  gt() |>
  tab_header(title = md("**Top and Bottom RBs Ranked \nby Effort Metric #2**")) |>
  cols_label(displayName = "Name", dis_score = "Effort Score (%)", rank = "Rank") |>
  data_color(columns = c(rank),
             fn = scales::col_numeric(palette = c("#D55E00","white", "#0072B2"), domain = NULL)) |>
  gtExtras::gt_theme_espn() |>
  opt_align_table_header(align = "center") |> 
  gtsave(file = "Effort2Rank.png",
         vwidth = 380,
         vheight = 600)
                                            
