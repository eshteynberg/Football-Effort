#FROM Modeling_stuff_new file 

cor(ellipse_stats_play$final_ellipse_score, epa_rushingYards_test_preds$epa_lasso_res)
cor(ellipse_stats_play$final_ellipse_score, epa_rushingYards_test_preds$rushingYards_gam_res)  

cor(dis_scores_plays$dis_score_mix, epa_rushingYards_test_preds$epa_lasso_res)
cor(dis_scores_plays$dis_score_mix, epa_rushingYards_test_preds$rushingYards_gam_res)

cor(ellipse_play$ellipse_score, epa_rushingYards_test_preds$epa_lasso_res)

cor(ellipse_play$ellipse_score, epa_rushingYards_test_preds$rf_res)

cor(tracking_bc_play_stats$num_of_effort_move, epa_rushingYards_test_preds$rf_res)

