library(quantreg)
library(tidyverse)
library(mgcv)
library(ranger)

tracking_bc_expected |> 
  na.omit() |> 
  nrow()

# Player test -------------------------------------------------------------

set.seed(1)
player_runs <- tracking_bc_expected |> 
  filter(displayName == "Saquon Barkley")

N_FOLDS <- 5

plays_folds_expected <- player_runs |> 
  distinct(playId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

player_runs_modeling <- player_runs |> 
  select(expected_speed, expected_acceleration, playId) |> 
  left_join(plays_folds_expected) |> 
  select(-playId) |> 
  na.omit()

# Function will only perform for Saquon
player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold==x)
  train_data <- player_runs_modeling |> 
    filter(fold != x)
  
  s_range <- range(train_data$expected_speed)
  test_data <- test_data |>
    filter(expected_speed >= max(s_range[1], 1),  #>= max of training lower bound and 1
           expected_speed <= min(s_range[2], 9))  #<= min of training upper bound and 9
  #nonparametric quantile reg using smooth spline
  #fit a smooth spline of acc as a function of speed
  #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
  rq_fit_95 <- rqss(expected_acceleration~qss(expected_speed, lambda=3), tau=.95, data=train_data)
  rq_fit_50 <- rqss(expected_acceleration~qss(expected_speed, lambda=3), tau=.5, data=train_data)
  
  out <- tibble(
    rq_pred_95 = predict(rq_fit_95, newdata=test_data),
    rq_pred_50 = predict(rq_fit_50, newdata=test_data),
    test_actual = test_data$expected_acceleration,
    x_values = test_data$expected_speed,
    res_rq_95 = test_actual - rq_pred_95,
    res_rq_50 = test_actual - rq_pred_50,
    test_fold=x
  )
  return(out)
}

player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
  list_rbind()

spline_fn_95 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_95)
spline_fn_50 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_50)

auc_95 <- integrate(spline_fn_95, lower=min(player_runs_test_preds$x_values), 
                    upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value
auc_50 <- integrate(spline_fn_50, lower=min(player_runs_test_preds$x_values), 
                    upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value

# Saquon's difference AUC
auc_95 - auc_50



# Function (acceleration expected) ----------------------------------------------------------------
eff_function_expected <- function(name, graph = FALSE, area = FALSE, dis = FALSE){
  player_runs <- tracking_bc_expected |> 
    filter(displayName == name)
  
  N_FOLDS <- 5
  
  plays_folds_expected <- player_runs |> 
    distinct(playId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_modeling <- player_runs |> 
    select(expected_speed, expected_acceleration, playId) |> 
    left_join(plays_folds_expected) |> 
    select(-playId) |> 
    na.omit()
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$expected_speed)
    test_data <- test_data |>
      filter(expected_speed >= max(s_range[1], 1),  #>= max of training lower bound and 1
             expected_speed <= min(s_range[2], 9))  #<= min of training upper bound and 9
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rq_fit_95 <- rqss(expected_acceleration~qss(expected_speed, lambda=3), tau=.95, data = train_data)
    rq_fit_50 <- rqss(expected_acceleration~qss(expected_speed, lambda=3), tau=.5, data = train_data)
    
    # GAM fit
    gam_fit <- gam(expected_acceleration ~ s(expected_speed), 
                   data = train_data, 
                   family = gaussian(),
                   method = "REML")
    
    out <- tibble(
      displayName = name,
      rq_pred_95 = predict(rq_fit_95, newdata=test_data),
      rq_pred_50 = predict(rq_fit_50, newdata=test_data),
      gam_pred = predict(gam_fit, newdata=test_data),
      test_actual = test_data$expected_acceleration,
      x_values = test_data$expected_speed,
      res_rq_95 = test_actual - rq_pred_95,
      res_rq_50 = test_actual - rq_pred_50,
      res_gam = test_actual - gam_pred,
      test_fold=x
    )
    return(out)
  }
  
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    list_rbind()
  
  if (area == TRUE) {
    spline_fn_95 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_95)
    spline_fn_50 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_50)
    
    auc_95 <- integrate(spline_fn_95, lower=min(player_runs_test_preds$x_values), 
                        upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value
    auc_50 <- integrate(spline_fn_50, lower=min(player_runs_test_preds$x_values), 
                        upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value
    
    return(tibble(displayName = name, area = auc_95 - auc_50))
  }
  
  if (dis == TRUE) {
    dis_table <- player_runs_test_preds |> 
      filter(test_actual >= rq_pred_95) |> 
      select(displayName, dis_metric_median = res_rq_50, dis_metric_mean = res_gam)
    
    return(dis_table)
  }
  
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = expected_speed, y = expected_acceleration)) +
      geom_point(alpha=.3, color="grey2")+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.95), se=FALSE, aes(color="95th Quartile Line"), size=1.2)+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.5), se=FALSE, aes(color="Median Line"), size=1.2)+
      stat_smooth(method="gam", formula=y~s(x),
                  se=FALSE, aes(color="Expected acceleration line"), size=1.2) + 
      scale_color_manual("Line", values = c("darkblue", "hotpink", "#FFB612")) +
      labs(x = "Adjusted Speed",
           y = "Adjusted Acceleration",
           title = paste0(name, "'s effort is defined as the mean distance of points \npast the 95th quartile line to the expected acceleration line"),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme(plot.title = element_text(face = "bold",
                                      size = 20, 
                                      hjust = .5),
            legend.title = element_text(face = "bold",
                                        size = 15),
            axis.title = element_text(face = "bold",
                                      size = 15),
            axis.text = element_text(size = 13),
            plot.caption = element_text(face = "italic",
                                        size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

eff_function_expected("Saquon Barkley", graph = TRUE)
eff_function_expected("Rex Burkhead", graph = TRUE)
eff_function_expected("Jaylen Warren", graph = TRUE)

# Function (acceleration normal) ----------------------------------------------------------------
eff_function_normal <- function(name, graph = FALSE, area = FALSE, dis = FALSE){
  player_runs <- tracking_bc_expected |> 
    filter(displayName == name)
  
  N_FOLDS <- 5
  
  plays_folds_expected <- player_runs |> 
    distinct(playId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_modeling <- player_runs |> 
    select(expected_speed, a, playId) |> 
    left_join(plays_folds_expected) |> 
    select(-playId) |> 
    na.omit()
  
  player_runs_normal_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$expected_speed)
    test_data <- test_data |>
      filter(expected_speed >= max(s_range[1], 1),  #>= max of training lower bound and 1
             expected_speed <= min(s_range[2], 9))  #<= min of training upper bound and 9
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rq_fit_95 <- rqss(a~qss(expected_speed, lambda=3), tau=.95, data = train_data)
    rq_fit_50 <- rqss(a~qss(expected_speed, lambda=3), tau=.5, data = train_data)
    
    # GAM fit
    gam_fit <- gam(a ~ s(expected_speed), 
                   data = train_data, 
                   family = gaussian(),
                   method = "REML")
    
    out <- tibble(
      displayName = name,
      rq_pred_95 = predict(rq_fit_95, newdata=test_data),
      rq_pred_50 = predict(rq_fit_50, newdata=test_data),
      gam_pred = predict(gam_fit, newdata=test_data),
      test_actual = test_data$a,
      x_values = test_data$expected_speed,
      res_rq_95 = test_actual - rq_pred_95,
      res_rq_50 = test_actual - rq_pred_50,
      res_gam = test_actual - gam_pred,
      test_fold=x
    )
    return(out)
  }
  
  player_runs_test_preds_normal <- map(1:N_FOLDS, player_runs_normal_cv) |> 
    list_rbind()
  
  if (area == TRUE) {
    spline_fn_95 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_95)
    spline_fn_50 <- splinefun(player_runs_test_preds$x_values, player_runs_test_preds$rq_pred_50)
    
    auc_95 <- integrate(spline_fn_95, lower=min(player_runs_test_preds$x_values), 
                        upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value
    auc_50 <- integrate(spline_fn_50, lower=min(player_runs_test_preds$x_values), 
                        upper=max(player_runs_test_preds$x_values), subdivisions = 2000)$value
    
    return(tibble(displayName = name, area = auc_95 - auc_50))
  }
  
  if (dis == TRUE) {
    dis_table <- player_runs_test_preds |> 
      filter(test_actual >= rq_pred_95) |> 
      select(displayName, dis_metric_median = res_rq_50, dis_metric_mean = res_gam)
    
    return(dis_table)
  }
  
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = expected_speed, y = a)) +
      geom_point(alpha=.3, color="grey2")+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.95), se=FALSE, aes(color="95th Quartile Line"), size=1.2)+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.5), se=FALSE, aes(color="Median Line"), size=1.2)+
      stat_smooth(method="gam", formula=y~s(x),
                  se=FALSE, aes(color="Expected acceleration line"), size=1.2, lty = 2) + 
      scale_color_manual("Line", values = c("darkblue", "darkgreen", "#FFB612")) +
      labs(x = "Adjusted Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the mean distance of points \npast the 95th quartile line to the expected acceleration line"),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme(plot.title = element_text(face = "bold",
                                      size = 20, 
                                      hjust = .5),
            legend.title = element_text(face = "bold",
                                        size = 15),
            axis.title = element_text(face = "bold",
                                      size = 15),
            axis.text = element_text(size = 13),
            plot.caption = element_text(face = "italic",
                                        size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

eff_function_normal("Saquon Barkley", graph = TRUE)
eff_function_normal("Rex Burkhead", graph = TRUE)
eff_function_normal("Jaylen Warren", graph = TRUE)

