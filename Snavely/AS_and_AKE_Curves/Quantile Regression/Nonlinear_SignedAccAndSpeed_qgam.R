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



# Function ----------------------------------------------------------------

eff_function_qgam_mix <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # 5 folds
  N_FOLDS <- 5
  
  # # Making sure plays are in the same fold
  plays_folds <- player_runs |>
    distinct(gameId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, dir_a_mpsh, gameId, bc_id, playId, frameId) |> 
    left_join(plays_folds)

  
  player_runs_cv_pos <- function(x){
    # Finding 99th percentile of speed
    speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
    
    test_data_pos <- player_runs_modeling |> 
      filter(fold == x) |> 
      filter(dir_a_mpsh >= 0)
    train_data_pos <- player_runs_modeling |> 
      filter(fold != x) |> 
      filter(dir_a_mpsh >= 0)
    
    
    # Modeling
    qgam_fit_a_top <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                       data = train_data_pos,
                       qu = .95,
                       multicore = TRUE,
                       ncores = 7)
    
    pos <- tibble(
      displayName = name,
      bc_id = test_data_pos$bc_id,
      gameId = test_data_pos$gameId,
      playId = test_data_pos$playId,
      frameId = test_data_pos$frameId,
      qgam_pred = predict(qgam_fit_a_top, newdata = test_data_pos),
      actual_acc = test_data_pos$dir_a_mpsh,
      actual_speed = test_data_pos$s_mph,
      diff_a = qgam_pred - actual_acc, # negative indicates above line
      diff_speed = speed_99 - actual_speed, # negative indicates right of line
      test_fold = x
    )

    return(pos)
  }
  
  player_runs_cv_neg <- function(x){
    # Finding 99th percentile of speed
    speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
    
    test_data_neg <- player_runs_modeling |> 
      filter(fold == x) |> 
      filter(dir_a_mpsh < 0)
    train_data_neg <- player_runs_modeling |> 
      filter(fold != x) |> 
      filter(dir_a_mpsh < 0)


    # Modeling
    qgam_fit_a_bottom <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                              data = train_data_neg,
                              qu = .05,
                              multicore = TRUE,
                              ncores = 7)
    
    neg <- tibble(
      displayName = name,
      bc_id = test_data_neg$bc_id,
      gameId = test_data_neg$gameId,
      playId = test_data_neg$playId,
      frameId = test_data_neg$frameId,
      qgam_pred = predict(qgam_fit_a_bottom, newdata = test_data_neg),
      actual_acc = test_data_neg$dir_a_mpsh,
      actual_speed = test_data_neg$s_mph,
      diff_a = actual_acc - qgam_pred, # negative indicates below line
      diff_speed = speed_99 - actual_speed, # negative indicates right of line
      test_fold = x
    )

    return(neg)
  }
  
  
  # Doing cross fold validation
    player_runs_test_preds_pos <- map(1:N_FOLDS, player_runs_cv_pos) |> 
      bind_rows()

    player_runs_test_preds_neg <- map(1:N_FOLDS, player_runs_cv_neg) |> 
      bind_rows()
    
    player_runs_test_preds <- rbind(player_runs_test_preds_pos, player_runs_test_preds_neg)
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = actual_acc)) +
      geom_point(alpha=.3, color="grey2")+
      stat_smooth(data = player_runs_test_preds_pos,
                  method="gam",
                  formula=y~s(x),
                  se=FALSE,
                  lwd = 1.5,
                  aes(y = qgam_pred, color="0.98 quantile regression line"), size=1.2) +
      stat_smooth(data = player_runs_test_preds_neg,
                  method="gam",
                  formula=y~s(x),
                  se=FALSE,
                  lwd = 1.5,
                  aes(y = qgam_pred, color="0.02 quantile regression line"), size=1.2) +
      geom_vline(aes(color = "Speed 0.99 quantile line", 
                     xintercept = quantile(actual_speed, probs = c(.99))), 
                 lty = 2, lwd = 1.5) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.2, lty = 2) +
      scale_color_manual("Line", values = c("#D55E00", "#0072B2", "goldenrod")) +
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
            plot.caption = element_text(face = "italic", size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

# Player test
eff_function_qgam_mix("Derrick Henry", graph = TRUE)
eff_function_qgam_mix("Craig Reynolds", graph = TRUE)
eff_function_qgam_mix("Rex Burkhead", graph = TRUE)
Rex <- eff_function_qgam_mix("Saquon Barkley")

Rex |> 
  ggplot(aes(x = actual_speed, y = qgam_pred)) +
  geom_point()
# qgam_mixed <- purrr::map(rbs_names, eff_function_qgam_mix) |>
#   bind_rows()
# write.csv(qgam_mixed, "SignedAccPercentiles.csv")

# Loading in the data
signedPercentiles <- read.csv("created_data/SignedAccPercentiles.csv") |> 
  select(-X) |> 
  rowwise() |> 
  mutate(minimum_diff = min(diff_a, diff_speed), # Finding minimum distance
         adj_minimum_diff = ifelse(minimum_diff <= 0, 0, minimum_diff), # Giving value of 0 for points past lines
         dis_stat = (1 / (1 + adj_minimum_diff)),
         dis_stat_adj = ifelse(actual_acc < 0, dis_stat / 2, dis_stat)) # Penalty for deccelerating

## Mean interpretation
# Final effort score for players
dis_scores_players <- signedPercentiles |> 
  group_by(displayName) |> 
  summarize(dis_score_mix = mean(dis_stat_adj)) |> 
  ungroup()

# For plays
dis_scores_plays <- signedPercentiles |> 
  group_by(gameId, playId, displayName) |> 
  summarize(dis_score_mix = mean(dis_stat_adj)) |> 
  ungroup()

cor(tracking_bc_play_stats$expectedPointsAdded, dis_scores_plays$dis_score_mix)

## Max effortful second interpretation
sec_dis_scores_plays <- signedPercentiles |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  slice_max(order_by = dis_stat_adj, n = 10) |> 
  summarize(dis_score_mix = mean(dis_stat_adj)) |> 
  ungroup()

sec_dis_scores_players <- sec_dis_scores_plays |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score_mix = mean(dis_score_mix)) |> 
  ungroup()

cor(tracking_bc_play_stats$expectedPointsAdded, sec_dis_scores_plays$dis_score_mix)


# Factoring in rushing yards
scores_and_rushingYards <- tracking_bc_play_stats |> 
  left_join(dis_scores_plays) |> 
  mutate(dis_score_rush = dis_score_mix * rushingYards)

players_scoresRushed <- scores_and_rushingYards |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score_rush = mean(dis_score_rush))

cor(scores_and_rushingYards$expectedPointsAdded, scores_and_rushingYards$dis_score_rush)



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
      geom_line(data = data_pos_final, aes(y = nlrq_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.2) +
      geom_line(data = data_neg_final, aes(y = nlrq_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.2) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.2, lty = 2) +
      scale_color_manual("Line", values = c("#D55E00", "#0072B2", "goldenrod")) +
      scale_fill_manual("Point", values = c("#b3b3b3")) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name, "'s effort score: 18.65%")) +
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
             fn = scales::col_numeric(palette = c("#D55E00","white", "#0072B2"), domain = NULL)) |>
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
      geom_line(data = data_pos_final, aes(y = qgam_pred, color = "0.95 quantile accel. \nregression line"), lwd = 1.2) +
      geom_line(data = data_neg_final, aes(y = qgam_pred, color = "0.95 quantile decel. \nregression line"), lwd = 1.2) +
      # geom_vline(aes(color = "Speed 0.99 quantile line", 
      #                xintercept = quantile(s_mph, probs = c(.99))), 
      #            lty = 2, lwd = 1.5) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.2, lty = 2) +
      scale_color_manual("Line", values = c("#D55E00", "#0072B2")) +
      scale_fill_manual("Point", values = c("#b3b3b3")) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name, "'s effort score: 22.60%")) +
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


## Scatterplot
dis_scatter <- qgam_dis_player |> 
  mutate(dis_score_qgam = dis_score,
         rank_qgam = rank) |> 
  select(-dis_score, -rank) |> 
  left_join(nlrq_dis_player, by = c("bc_id", "displayName"))

label_names <- dis_scatter |> 
  filter(rank_qgam <= 6 | rank <= 6 | rank_qgam >=64 
         | rank >= 64 | displayName %in% c("Saquon Barkley", "James Cook"))

dis_scatter |> 
  ggplot(aes(x = dis_score, y = dis_score_qgam)) +
  geom_hline(aes(yintercept = mean(dis_score_qgam)), lwd = 1.2, lty = 2, color = "#D50A0A") +
  geom_vline(aes(xintercept = mean(dis_score)), lwd = 1.2, lty = 2, color = "#D50A0A") +
  geom_point(aes(color = dis_score), size = 3, alpha = .8) +
  scale_color_gradient(low = "#0072B2", high = "#D55E00") +
  labs(title = "Player effort metrics positively correlate with each other",
       x = "Effort metric #1 (non-linear quantile regression)",
       y = "Effort metric #2 (GAM quantile regression)")+
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold")) +
  ggrepel::geom_text_repel(data = label_names, aes(label = displayName), 
                           size = 5, max.overlaps = 15,
                           fontface = "italic")
  
