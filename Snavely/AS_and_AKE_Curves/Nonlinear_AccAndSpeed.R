library(quantreg)
library(tidyverse)
library(mgcv)

# Player test -------------------------------------------------------------

set.seed(1)
player_runs <- tracking_bc |> 
  filter(displayName == "Latavius Murray")
  
N_FOLDS <- 5
player_runs_modeling <- player_runs |> 
  select(s, a) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
# Function will only perform for Saquon
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold==x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$s)
    test_data <- test_data |>
      filter(s >= max(s_range[1], 1),  #>= max of training lower bound and 1
             s <= min(s_range[2], 9))  #<= min of training upper bound and 9
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rq_fit_95 <- rqss(a~qss(s, lambda=3), tau=.95, data=train_data)
    rq_fit_50 <- rqss(a~qss(s, lambda=3), tau=.5, data=train_data)
    
    out <- tibble(
      rq_pred_95 = predict(rq_fit_95, newdata=test_data),
      rq_pred_50 = predict(rq_fit_50, newdata=test_data),
      test_actual = test_data$a,
      x_values = test_data$s,
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

  

# Function ----------------------------------------------------------------
  eff_function_rqss <- function(name, graph = FALSE, area = FALSE, dis = FALSE){
    player_runs <- tracking_bc |> 
      filter(displayName == name)
    
    N_FOLDS <- 5
    player_runs_modeling <- player_runs |> 
      select(s, a) |> 
      mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
    
    player_runs_cv <- function(x){
      test_data <- player_runs_modeling |> 
        filter(fold == x)
      train_data <- player_runs_modeling |> 
        filter(fold != x)
      
      s_range <- range(train_data$s)
      test_data <- test_data |>
        filter(s >= max(s_range[1], 1),  #>= max of training lower bound and 1
               s <= min(s_range[2], 9))  #<= min of training upper bound and 9
      #nonparametric quantile reg using smooth spline
      #fit a smooth spline of acc as a function of speed
      #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
      rq_fit_95 <- rqss(a~qss(s, lambda=3), tau=.95, data = train_data)
      rq_fit_50 <- rqss(a~qss(s, lambda=3), tau=.5, data = train_data)
      
      # GAM fit
      gam_fit <- gam(a ~ s(s), 
                     data = train_data, 
                     family = gaussian(),
                     method = "REML")
      
      out <- tibble(
        displayName = name,
        rq_pred_95 = predict(rq_fit_95, newdata=test_data),
        rq_pred_50 = predict(rq_fit_50, newdata=test_data),
        gam_pred = predict(gam_fit, newdata=test_data),
        test_actual = test_data$a,
        x_values = test_data$s,
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
        ggplot(aes(x = s, y = a)) +
        geom_point(alpha=.5, color="grey2")+
        stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                    method.args=list(tau=0.95), se=FALSE, color="blue", size=1.2)+
        stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                    method.args=list(tau=0.5), se=FALSE, color="red", size=1.2)+
        stat_smooth(method="gam", formula=y~s(x),
                    se=FALSE, color="gold", size=1.2) + 
        labs(x = "Speed",
             y = "Acceleration",
             title = paste0(name, "'s effort quantile curves (95 and 50)"),
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

  
# Test
eff_function_rqss("Saquon Barkley", graph = TRUE)
eff_function_rqss("Rex Burkhead", graph = TRUE)
eff_function_rqss("Travis Etienne", graph = TRUE)
View(eff_function_rqss("Saquon Barkley"))

# Area calc for all players ----------------------------------------------
  
  rbs <- unique(rb_stats_total_filtered$displayName)
  
  area_scores <- purrr::map(rbs, eff_function_rqss, area = TRUE) |> 
    bind_rows()
  
  View(area_scores)

# Dis calc for all players ----------------------------------------------
  rbs <- unique(rb_stats_total_filtered$displayName)
  
  dis_scores <- purrr::map(rbs, eff_function_rqss, dis = TRUE) |> 
    bind_rows()
  
  View(dis_scores)

  mean_dis_scores <- dis_scores |> 
    group_by(displayName) |> 
    summarize(mean_diffMedian = mean(dis_metric_median),
              mean_diffMean = mean(dis_metric_mean))
  