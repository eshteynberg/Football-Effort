#FROM Modeling_stuff_new file 

cor(ellipse_stats_play$final_ellipse_score, rushingYards_test_preds$rf_res)  

cor(dis_scores_plays$dis_score_mix, rushingYards_test_preds$rf_res)

cor(dis_scores_plays$dis_score_mix, epa_test_preds$rf_res)

cor(ellipse_play$ellipse_score, rushingYards_test_preds$rf_res)

cor(ellipse_play$ellipse_score, epa_test_preds$rf_res)

cor(tracking_bc_play_stats$num_of_effort_move, rushingYards_test_preds$rf_res)
