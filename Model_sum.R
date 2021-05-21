## 历史数据
# 将结果合并
trans_nrgsum_df <- func_nrgsum_ls_to_df(trans_nrgsum_ls)
ind_nrgsum_df <- func_nrgsum_ls_to_df(ind_nrgsum_ls)
com_nrgsum_df <- func_nrgsum_ls_to_df(com_nrgsum_ls)
other_nrgsum_df <- func_nrgsum_ls_to_df(other_nrgsum_ls)

# 汇总
total_nrgsum_df <- Reduce(rbind, list(trans_nrgsum_df, 
                                      ind_nrgsum_df, 
                                      com_nrgsum_df, 
                                      other_nrgsum_df))
total_nrgsum_df <- aggregate(total_nrgsum_df[, global_nrg_class], 
                             by = list(total_nrgsum_df$year), 
                             function(x) {sum(x, na.rm = TRUE)})
names(total_nrgsum_df)[1] <- "year"
total_nrgsum_df <- 
  func_addnote(total_nrgsum_df, c("year",
                                  rep("吨煤", 2),
                                  rep("吨油品", 5),
                                  "万立方米", "万千瓦时"))
func_show_trend(total_nrgsum_df)
for (i in global_nrg_class) {
  print(func_show_trend(total_nrgsum_df[, c("year", i)]))
}

## 规划情景
proj_trans_nrgsum_df <- func_nrgsum_ls_to_df(proj_trans_nrgsum_ls)
proj_ind_nrgsum_df <- func_nrgsum_ls_to_df(proj_ind_nrgsum_ls)
proj_com_nrgsum_df <- func_nrgsum_ls_to_df(proj_com_nrgsum_ls)
proj_other_nrgsum_df <- func_nrgsum_ls_to_df(proj_other_nrgsum_ls)

# 汇总
proj_total_nrgsum_df <- Reduce(rbind, list(proj_trans_nrgsum_df, 
                                           proj_ind_nrgsum_df, 
                                           proj_com_nrgsum_df, 
                                           proj_other_nrgsum_df))
proj_total_nrgsum_df <- aggregate(proj_total_nrgsum_df[, global_nrg_class], 
                                      by = list(proj_total_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_total_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_total_nrgsum_agg_df)







