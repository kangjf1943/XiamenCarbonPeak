## 将结果合并
names(proj_trans_nrgsum_ls) <- trans_names
names(proj_ind_nrgsum_ls) <- ind_agg_list
names(proj_com_nrgsum_ls) <- c("用电", "用气")
names(proj_other_nrgsum_ls) <- c("household", 
                                 "household_lpg", 
                                 "household_natural_gas", 
                                 "construction_gdp", 
                                 "agriculture_area")

# 给各个数据框添加部门名称和活动水平名称
for (i in c(1:length(proj_trans_nrgsum_ls))) {
  proj_trans_nrgsum_ls[[i]]$act_name <- names(proj_trans_nrgsum_ls)[i]
  proj_trans_nrgsum_ls[[i]]$sector <- func_varname(proj_trans_nrgsum_ls)
  proj_trans_nrgsum_ls[[i]] <- func_supple_colnames(proj_trans_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_ind_nrgsum_ls))) {
  proj_ind_nrgsum_ls[[i]]$act_name <- names(proj_ind_nrgsum_ls)[i]
  proj_ind_nrgsum_ls[[i]]$sector <- func_varname(proj_ind_nrgsum_ls)
  proj_ind_nrgsum_ls[[i]] <- func_supple_colnames(proj_ind_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_com_nrgsum_ls))) {
  proj_com_nrgsum_ls[[i]]$act_name <- names(proj_com_nrgsum_ls)[i]
  proj_com_nrgsum_ls[[i]]$sector <- func_varname(proj_com_nrgsum_ls)
  proj_com_nrgsum_ls[[i]] <- func_supple_colnames(proj_com_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_other_nrgsum_ls))) {
  proj_other_nrgsum_ls[[i]]$act_name <- names(proj_other_nrgsum_ls)[i]
  proj_other_nrgsum_ls[[i]]$sector <- func_varname(proj_other_nrgsum_ls)
  proj_other_nrgsum_ls[[i]] <- func_supple_colnames(proj_other_nrgsum_ls[[i]], nrg_names)
}

# 分部门合并
# 交通
proj_trans_nrgsum_df <- proj_trans_nrgsum_ls[[1]]
for (i in c(2: length(proj_trans_nrgsum_ls))) {
  proj_trans_nrgsum_df <- rbind(proj_trans_nrgsum_df, proj_trans_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_trans_nrgsum_df[, i] <- as.numeric(proj_trans_nrgsum_df[, i])
}
proj_trans_nrgsum_agg_df <- aggregate(proj_trans_nrgsum_df[, nrg_names], 
                                      by = list(proj_trans_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_trans_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_trans_nrgsum_agg_df[, c("year", "diesel")])

# 工业
proj_ind_nrgsum_df <- proj_ind_nrgsum_ls[[1]]
for (i in c(2: length(proj_ind_nrgsum_ls))) {
  proj_ind_nrgsum_df <- rbind(proj_ind_nrgsum_df, proj_ind_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_ind_nrgsum_df[, i] <- as.numeric(proj_ind_nrgsum_df[, i])
}
proj_ind_nrgsum_agg_df <- aggregate(proj_ind_nrgsum_df[, nrg_names], 
                                    by = list(proj_ind_nrgsum_df$year), 
                                    function(x) {sum(x, na.rm = TRUE)})
names(proj_ind_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_ind_nrgsum_agg_df)

# 服务业
proj_com_nrgsum_df <- proj_com_nrgsum_ls[[1]]
for (i in c(2: length(proj_com_nrgsum_ls))) {
  proj_com_nrgsum_df <- rbind(proj_com_nrgsum_df, proj_com_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_com_nrgsum_df[, i] <- as.numeric(proj_com_nrgsum_df[, i])
}
proj_com_nrgsum_agg_df <- aggregate(proj_com_nrgsum_df[, nrg_names], 
                                    by = list(proj_com_nrgsum_df$year), 
                                    function(x) {sum(x, na.rm = TRUE)})
names(proj_com_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_com_nrgsum_agg_df)

# 其他
proj_other_nrgsum_df <- proj_other_nrgsum_ls[[1]]
for (i in c(2: length(proj_other_nrgsum_ls))) {
  proj_other_nrgsum_df <- rbind(proj_other_nrgsum_df, proj_other_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_other_nrgsum_df[, i] <- as.numeric(proj_other_nrgsum_df[, i])
}
proj_other_nrgsum_agg_df <- aggregate(proj_other_nrgsum_df[, nrg_names], 
                                      by = list(proj_other_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_other_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_other_nrgsum_agg_df)

# 汇总
proj_total_nrgsum_df <- Reduce(rbind, list(proj_trans_nrgsum_agg_df, 
                                           proj_ind_nrgsum_agg_df, 
                                           proj_com_nrgsum_agg_df, 
                                           proj_other_nrgsum_agg_df))
proj_total_nrgsum_agg_df <- aggregate(proj_total_nrgsum_df[, nrg_names], 
                                      by = list(proj_total_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_total_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_total_nrgsum_agg_df[names(proj_total_nrgsum_agg_df) %in% c("kerosene") == F])


## 构建排放因子列表
ef_ls <- func_read_trans("8C8EDJVH")
ef_ls_2 <- ef_ls[-1, ]
for (i in c(1:3)) {
  ef_ls_2[i, -1] <- ef_ls_2[i, -1]* ef_ls[1, -1]
}
ef_ls_2
names(ef_ls_2) <- c("ghg", "coal", "coal_product", 
                    "gasoline", "kerosene", "diesel", "residual", "lpg", "natural_gas", 
                    "electricity")

## 计算排放量
# 通过时间序列的各类能耗 + 各类能源的各类排放因子 = 计算时间序列的各类排放的函数
# 测试数据
var_nrgsum <- proj_trans_nrgsum_agg_df
var_emifac <- ef_ls_2
func_emisum <- function(var_nrgsum, var_emifac) {
  # 输入的时间序列各类能耗中能源的名字和排放因子中能源的名字要相同
  # 建立一个列表用于存放二氧化碳、甲烷和氧化亚氮排放量的列表
  emission_ls <- vector("list", 3)
  names(emission_ls) <- var_emifac[, 1]
  # 填充排放列表
  for (j in c(1:3)) {
    for (i in names(var_nrgsum)[names(var_nrgsum) %in% "year" == FALSE]) {
      emission_ls[[j]] <- cbind(emission_ls[[j]], 
                                var_nrgsum[, i] * var_emifac[j, i])
    }
  }
  # 构建输出数据框：观察为时间序列，列名为各类温室气体
  out_df <- data.frame(year = var_nrgsum[, "year"], 
                       co2 = rowSums(emission_ls[[1]]), 
                       ch4 = rowSums(emission_ls[[2]]), 
                       n2o = rowSums(emission_ls[[3]]))
  out_df
}
proj_trans_emission <- func_emisum(proj_trans_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_trans_emission)
proj_com_emission <- func_emisum(proj_com_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_com_emission)
proj_ind_emission <- func_emisum(proj_ind_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_ind_emission)
proj_other_emission <- func_emisum(proj_other_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_other_emission)

proj_total_emission <- rbind(proj_trans_emission, 
                             proj_com_emission, 
                             proj_ind_emission, 
                             proj_other_emission)
proj_total_emission_agg <- aggregate(proj_total_emission[, c("co2", "ch4", "n2o")], 
                                     list(proj_total_emission$year), 
                                     function(x) sum(x, na.rm = TRUE))
names(proj_total_emission_agg)[1] <- "year"
func_show_trend(proj_total_emission_agg)