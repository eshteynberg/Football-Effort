# loading packages

library(tidyverse)
library(ggrepel)
library(GGally)
library(factoextra)
library(ggthemes)


# removing specific dataset from environment
rm(rb_plays_clust)

# clustering
## standardizing variables======================================================
rb_plays_clust <- rb_stats_per_play |>
  filter(displayName == "Josh Jacobs") |>
  select(displayName, mean_ke, dis_gained_x, avg_jerk, expectedPointsAdded) |>
  mutate(across(where(is.numeric),
                ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_sd")) |>
   # slice_sample(n = 100) |> sampling only a subset if desired
  select(ends_with("_sd"))

#========================== elbow plot=========================================

set.seed(231)

elbow_plot <- tibble(k = 1:10) |>
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(rb_plays_clust, .x, nstart = 30)),
    glanced = purrr::map(kmeans_results, glance)) |>
  unnest(cols = c(glanced))


# making the elbow plot
ggplot(elbow_plot, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Num of clusters(k)",
       y = "Total within-cluster sum of squares")


## Clustering standardized variables=============================================

rb_plays_clust_scaled <- rb_plays_clust |>
  kmeans(centers = 8, nstart = 30) # decide number of clusters with elbow plot

# cluster assignments using the standardized data frame and filtered & sampled subset=
# on Josh Jacobs=================================================================== 

rb_plays_clust_assig <- augment(rb_plays_clust_scaled, rb_plays_clust) |>
  rename(cluster_standardized = .cluster)


# scatterplot matrix of clusters based on scaled variables ======================

rb_plays_clust_assig |>
  select(mean_ke_sd, dis_gained_x_sd, avg_jerk_sd, expectedPointsAdded_sd, 
         cluster_standardized) |>
  ggpairs(aes(color = cluster_standardized),
          upper = list(continuous = "blank"),
          progress = FALSE) +
  theme(
    # change all text to black, including strip labels and axis titles
    text = element_text(color = "black", size = 11),
    strip.text = element_text(color = "black"),  # variable names on top of panels
    axis.text = element_text(color = "black")    # axis tick labels
  )


##  =========================== PCA clustering ===========================

rb_plays_clust_scaled |> 
  fviz_cluster(data = rb_plays_clust, 
             geom = "point",           
             ellipse = FALSE, 
             pointsize = 2.3) +         
  ggthemes::scale_color_colorblind() +      
  scale_shape_manual(values = rep(16, 8)) +  # force all points to use the same shape
  theme_light() +
  labs(
    title = "Clustering Effort-related metrics for Josh Jacobs' plays"
  )


#========================= [D'Andre Swift]==============================================


swift_plays_clust <- rb_stats_per_play |>
  filter(displayName == "D'Andre Swift") |>
  select(displayName, mean_ke, dis_gained_x, avg_jerk, expectedPointsAdded) |>
  mutate(across(where(is.numeric),
                ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_sd")) |>
  # slice_sample(n = 100) |> sampling only a subset if desired
  select(ends_with("_sd"))

#========================== elbow plot=========================================

set.seed(200)

elbow_plot <- tibble(k = 1:10) |>
  mutate(
    kmeans_results_2 = purrr::map(k, ~kmeans(swift_plays_clust, .x, nstart = 30)),
    glanced = purrr::map(kmeans_results_2, glance)) |>
  unnest(cols = c(glanced))


# making the elbow plot
ggplot(elbow_plot, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Num of clusters(k)",
       y = "Total within-cluster sum of squares")


## Clustering standardized variables=============================================

swift_plays_clust_scaled <- swift_plays_clust |>
  kmeans(centers = 5, nstart = 30) # decide number of clusters with elbow plot

# cluster assignments using the standardized data frame and filtered & sampled subset=
# on Josh Jacobs=================================================================== 

swift_plays_clust_assig <- augment(swift_plays_clust_scaled, swift_plays_clust) |>
  rename(cluster_standardized = .cluster)


# scatterplot matrix of clusters based on scaled variables ======================

rb_plays_clust_assig |>
  select(mean_ke_sd, dis_gained_x_sd, avg_jerk_sd, expectedPointsAdded_sd, 
         cluster_standardized) |>
  ggpairs(aes(color = cluster_standardized),
          upper = list(continuous = "blank"),
          progress = FALSE) +
  theme(
    # change all text to black, including strip labels and axis titles
    text = element_text(color = "black", size = 11),
    strip.text = element_text(color = "black"),  # variable names on top of panels
    axis.text = element_text(color = "black")    # axis tick labels
  )


##  =========================== PCA clustering ===========================

swift_plays_clust_scaled |> 
  fviz_cluster(data = swift_plays_clust, 
               geom = "point",           
               ellipse = FALSE, 
               pointsize = 2.3) +         
  ggthemes::scale_color_colorblind() +      
  scale_shape_manual(values = rep(16, 8)) +  # force all points to use the same shape
  theme_light() +
  labs(
    title = "Clustering Effort-related metrics for Josh Jacobs' plays"
  )




#========================= [Melvin Gordon]======================================


gordon_plays_clust <- rb_stats_per_play |>
  filter(displayName == "Melvin Gordon") |>
  select(displayName, mean_ke, dis_gained_x, avg_jerk, expectedPointsAdded) |>
  mutate(across(where(is.numeric),
                ~ (.x - mean(.x)) / sd(.x),
                .names = "{.col}_sd")) |>
  # slice_sample(n = 100) |> sampling only a subset if desired
  select(ends_with("_sd"))

#========================== elbow plot=========================================

set.seed(200)

elbow_plot <- tibble(k = 1:10) |>
  mutate(
    kmeans_results_3 = purrr::map(k, ~kmeans(gordon_plays_clust, .x, nstart = 30)),
    glanced = purrr::map(kmeans_results_3, glance)) |>
  unnest(cols = c(glanced))


# making the elbow plot
ggplot(elbow_plot, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Num of clusters(k)",
       y = "Total within-cluster sum of squares")


## Clustering standardized variables=============================================

gordon_plays_clust_scaled <- gordon_plays_clust |>
  kmeans(centers = 4, nstart = 30) # decide number of clusters with elbow plot

# cluster assignments using the standardized data frame and filtered & sampled subset=
# on Melving Gordon=================================================================== 

gordon_plays_clust_assig <- augment(gordon_plays_clust_scaled, gordon_plays_clust) |>
  rename(cluster_standardized = .cluster)


# scatterplot matrix of clusters based on scaled variables ======================

gordon_plays_clust_assig |>
  select(mean_ke_sd, dis_gained_x_sd, avg_jerk_sd, expectedPointsAdded_sd, 
         cluster_standardized) |>
  ggpairs(aes(color = cluster_standardized),
          upper = list(continuous = "blank"),
          progress = FALSE) +
  theme(
    # change all text to black, including strip labels and axis titles
    text = element_text(color = "black", size = 11),
    strip.text = element_text(color = "black"),  # variable names on top of panels
    axis.text = element_text(color = "black")    # axis tick labels
  )


##  =========================== PCA clustering ===========================

gordon_plays_clust_scaled |> 
  fviz_cluster(data = gordon_plays_clust, 
               geom = "point",           
               ellipse = FALSE, 
               pointsize = 2.3) +         
  ggthemes::scale_color_colorblind() +      
  scale_shape_manual(values = rep(16, 8)) +  # force all points to use the same shape
  theme_light() +
  labs(
    title = "Clustering Effort-related metrics for Josh Jacobs' plays"
  )




