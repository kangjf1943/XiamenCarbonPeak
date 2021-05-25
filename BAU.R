# DEMAND ----
# Agriculture ----
## Activity level ----
bs_agriculture_act <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 0.6, 0.6), 
                base = func_lastone(by_agriculture_act$agriculture), 
                "area")

## Energy intensity ----
# 问题：由于机械化水品的提高，近期内农业能耗强度是否会增加？
# 区别于规划情景
bs_agriculture_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 1, 1), 
                base = func_lastone(by_agriculture_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
bs_agriculture_nrgsum_df <- 
  func_nrg_sum(bs_agriculture_nrgintst_df, bs_agriculture_act, "area")
bs_agriculture_emissum_df <- func_emissum(bs_agriculture_nrgsum_df, emisfac_df)

## Test ----
plot(bs_agriculture_emissum_df$year, bs_agriculture_emissum_df$co2)

# Industry ----
## Activity level ----
# 先计算未来子部门GDP所占比重
bs_ind_ori_act_prop <- data.frame(year = c(2019:2060))
# 不同于规划情景
# 产业结构保持不变
for (i in ind_subsector) {
  bs_ind_ori_act_prop[, i] <- func_lastone(ind_ori_act_prop[, i])
}
bs_ind_ori_act_prop_long <- melt(bs_ind_ori_act_prop, id = "year")
ggplot(bs_ind_ori_act_prop_long) + geom_bar(aes(year, value, fill = variable), stat = "identity")
# 计算未来各子部门GDP
bs_ind_act <- func_nrg_sum(bs_ind_ori_act_prop, global_indgdp, "GDP")
bs_ind_act[ind_subsector] <- bs_ind_act[ind_subsector]/100

## Energy intensity ----
# 区别：能耗强度略不变
bs_ind_nrgintst_ls <- vector("list", 13)
names(bs_ind_nrgintst_ls) <- ind_subsector
for (i in ind_subsector) {
  bs_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
  for (j in ind_nrgclass) {
    bs_ind_nrgintst_ls[[i]][, j] <- 
      func_interp_3(year = c(2019, 2060), 
                    scale = c(1, 1), 
                    base = func_lastone(ind_nrgintst_ls[[i]][, j]))$value
  }
}

## Energy and emission ----
bs_ind_nrgsum_ls <- func_nrg_sum_ls(bs_ind_nrgintst_ls, bs_ind_act)
bs_ind_nrgsum_df <- func_ls2df(bs_ind_nrgsum_ls)
bs_ind_emissum_df <- func_emissum(bs_ind_nrgsum_df, emisfac_df)

## Test----
# func_history_project_df(ind_act, bs_ind_act)
# func_history_project_ls(ind_nrgintst_ls, bs_ind_nrgintst_ls)
# func_history_project_ls(ind_nrgsum_ls, bs_ind_nrgsum_ls)
plot(bs_ind_emissum_df$year, bs_ind_emissum_df$co2)
func_test(bs_ind_emissum_df$co2, 164)

# Construction ----
## Activity level ----
bs_construct_act <- 
  func_cross(proj_global_gdp, 
             func_interp_2(year = c(2019,2025,2060), 
                           value = c(0.10, 0.08, 0.03)))
## Energy intensity ----
# 区别：能源强度不变
bs_construct_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 1, 1), 
                base = func_lastone(by_construct_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
bs_construct_nrgsum_df <- 
  func_nrg_sum(bs_construct_nrgintst_df, bs_construct_act, "GDP")
bs_construct_emissum_df <- func_emissum(bs_construct_nrgsum_df, emisfac_df)

## Test ----
plot(bs_construct_emissum_df$year, bs_construct_emissum_df$co2)


# Transportation ----
## Activity level ----
# 营运和非营运车辆
# 不同于规划情景：车辆持续增加
bs_trans_act <- data.frame(year = c(2019: 2060))
for (i in trans_subsector[1:12]) {
  bs_trans_act[, i] <- 
    func_interp_3(year = c(2019, 2025, 2060), 
                  scale = c(1, 1.3, 2), 
                  base = func_lastone(trans_act[, i]))$value
}

# 水运
bs_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(10000, 10500, 11000))$value
comment(bs_trans_act$"水路客运") <- "万人公里"
bs_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(bs_trans_act$"水路货运") <- "万吨公里"

## Energy intensity ----
bs_trans_nrgintst_ls <- vector("list", length(trans_subsector))
names(bs_trans_nrgintst_ls) <- trans_subsector
### 营运车辆和非营运车辆
# 均假设和最后一个有数值的年份一致，且假设这个数值是在2019年
for (j in c(1: 12)) {
  bs_trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2060))
  for (i in names(trans_nrgintst_ls[[j]])[
    names(trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
    bs_trans_nrgintst_ls[[j]][, i] <- func_lastone(trans_nrgintst_ls[[j]][, i])
  }
}
# # 但是其中轿车逐渐实现电气化
# bs_trans_carprop <- data.frame(year = bs_trans_nrgintst_ls[["轿车"]]$year)
# bs_trans_carprop$gasoline <-
#   func_interp_2(year = c(2019, 2025, 2030, 2050, 2060),
#                 value = c(1, 0.95, 0.9, 0.2, 0))$value
# bs_trans_carprop$electricity <- 1 - bs_trans_carprop$gasoline
# # 计算相应能耗强度中汽油和电力的变化
# bs_trans_nrgintst_ls[["轿车"]]$electricity <-
#   func_alter(mean(bs_trans_nrgintst_ls[["轿车"]]$gasoline),
#              "gasoline", "electricity")
# bs_trans_nrgintst_ls[["轿车"]] <-
#   func_cross(bs_trans_nrgintst_ls[["轿车"]],
#              bs_trans_carprop)

# 水路客运
# 柴油和燃料油均基于历史数据和比率
bs_trans_nrgintst_ls[["水路客运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路客运"]]$diesel), 
                "diesel")
bs_trans_nrgintst_ls[["水路客运"]]$residual <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual), 
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual)*0.85, 
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual)*0.80),
                "residual")$residual

# 水路货运
# 柴油：基于历史数据和比率
bs_trans_nrgintst_ls[["水路货运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路货运"]]$diesel),
                "diesel")
# 燃料油：基于历史数据和比率
bs_trans_nrgintst_ls[["水路货运"]]$residual <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路货运"]]$residual),
                "residual")$residual

## Energy and emission ----
bs_trans_nrgsum_ls <- func_nrg_sum_ls(bs_trans_nrgintst_ls, bs_trans_act)
bs_trans_nrgsum_df <- func_ls2df(bs_trans_nrgsum_ls)
bs_trans_emissum_df <- func_emissum(bs_trans_nrgsum_df, emisfac_df)

## Test----
# func_history_project_df(trans_act, bs_trans_act)
# func_history_project_ls(trans_nrgintst_ls, bs_trans_nrgintst_ls)
# func_history_project_ls(trans_nrgsum_ls, bs_trans_nrgsum_ls)
plot(bs_trans_emissum_df$year, bs_trans_emissum_df$co2)
func_test(bs_trans_emissum_df$co2, 907)

# Service ----
## Activity level ----
bs_ori_comemployee <- 
  func_cross(proj_global_population[c("year", "population")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.45, 0.48, 0.73)))
bs_ori_comgdp <- 
  func_cross(proj_global_gdp[c("year", "GDP")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.60, 0.65, 0.70)))
bs_com_act <- func_merge_2(list(bs_ori_comemployee, bs_ori_comgdp))
names(bs_com_act) <- c("year", "com_employee", "com_gdp")
bs_com_act$com_gdp[1] <- global_gdp$"#第三产业"[global_gdp$year == 2019]

## Energy intensity ----
# 区别于规划情景：无燃气电气化替代
# 服务业用电强度保持不变
bs_com_nrgintst_ls <- vector("list", 2)
names(bs_com_nrgintst_ls) <- com_subsector
bs_com_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(2958.967, 2958.967*1.2, 2958.967), 
                "electricity")
# 服务业燃气强度略有增加后减少，且逐渐为电气替代
bs_com_nrgintst_ls[[2]] <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(0.002055001,
                          0.002055001*1.2, 
                          0.002055001*0.8), "lpg")
bs_com_nrgintst_ls[[2]]$gas <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(1.353608e-04, 
                          1.353608e-04*1.2, 
                          1.353608e-04*0.8), "gas")$gas

## Energy and emission ----
bs_com_nrgsum_ls <- func_nrg_sum_ls(bs_com_nrgintst_ls, bs_com_act)
bs_com_nrgsum_df <- func_ls2df(bs_com_nrgsum_ls)
bs_com_emissum_df <- func_emissum(bs_com_nrgsum_df, emisfac_df)

## Test ----
# func_history_project_df(com_act, bs_com_act)
# func_history_project_ls(com_nrgintst_ls, bs_com_nrgintst_ls)
# func_history_project_ls(com_nrgsum_ls, bs_com_nrgsum_ls)
plot(bs_com_emissum_df$year, bs_com_emissum_df$co2)
func_test(bs_com_emissum_df$co2, 43)

# Household ----
## Activity level ----
# household_elec
bs_household_ori_household <- proj_global_population[c("year", "household")]
# household_lpg
bs_household_ori_lpguser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 0.65, 0.30), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "lpg")])))
names(bs_household_ori_lpguser)[2] <- "lpg"
# household_gas
bs_household_ori_gasuser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 2, 2.3), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "gas")])))
names(bs_household_ori_gasuser)[2] <- "gas"
# 合并
bs_household_act <- func_merge_2(list(bs_household_ori_household, 
                                      bs_household_ori_lpguser, 
                                      bs_household_ori_gasuser))

## Energy intensity ----
bs_household_nrgintst_ls <- vector("list", length(household_subsector))
names(bs_household_nrgintst_ls) <- household_subsector
# house_elec
bs_household_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(3900, 3900*1.2, 3900*1.4))
names(bs_household_nrgintst_ls[[1]])[2] <- "electricity"

# household_lpg
bs_household_nrgintst_ls[[2]] <- 
  data.frame(year = c(2019: 2060), 
             lpg = 588.52)
names(bs_household_nrgintst_ls[[2]])[2] <- "lpg"

# household_gas
bs_household_nrgintst_ls[[3]] <- 
  data.frame(year = c(2019: 2060), 
             gas = 74.50)
names(bs_household_nrgintst_ls[[3]])[2] <- "gas"

## Consumption and emission ----
bs_household_nrgsum_ls <- 
  func_nrg_sum_ls(bs_household_nrgintst_ls, bs_household_act)
bs_household_nrgsum_df <- func_ls2df(bs_household_nrgsum_ls)
bs_household_emissum_df <- func_emissum(bs_household_nrgsum_df, emisfac_df)

## Test ----
plot(bs_household_emissum_df$year, bs_household_emissum_df$co2)
func_test(bs_household_emissum_df$co2, 26)

# TF & RES ----
# Power generation ----
## Activity level ----
# 取全社会用电量
bs_tfres_act <- func_ls2df(list(bs_agriculture_nrgsum_df, 
                                bs_ind_nrgsum_df, 
                                bs_construct_nrgsum_df, 
                                bs_trans_nrgsum_df, 
                                bs_com_nrgsum_df, 
                                bs_household_nrgsum_df))
bs_tfres_act <- bs_tfres_act[c("year", "electricity")]
names(bs_tfres_act) <- c("year", "elecuse")
# 区别于规划情景：本地发电量保持不变
bs_tfres_act$elecgen <- c(900371)
bs_tfres_act$importelec <- bs_tfres_act$elecuse - bs_tfres_act$elecgen

## Energy intensity ----
# 和基准年持平
bs_tf_nrgintst <- data.frame(year = c(2019: 2060))
for (i in names(tf_nrgintst)[names(tf_nrgintst) %in% "year" == FALSE]) {
  bs_tf_nrgintst[, i] <- func_lastone(tf_nrgintst[, i])
}

## Consumption and emission ----
bs_tf_nrgsum_df <- 
  func_nrg_sum(bs_tf_nrgintst, bs_tfres_act, "elecgen")
bs_tf_emission <- func_emissum(bs_tf_nrgsum_df, emisfac_df)

## Test
plot(bs_tf_emission$year, bs_tf_emission$co2)
func_test(bs_tf_emission$co2, 623)

# Imported elec ----
## Emission ----
bs_res_emisfac_df <- 
  func_interp_2(year = c(2019, 2033, 2050), 
                value = c(4.158130e-04,7.0e-05, 0), "electricity")

bs_res_emissum <- 
  func_cross(bs_res_emisfac_df, bs_tfres_act[c("year", "importelec")])
names(bs_res_emissum)[2] <- "co2"

## Test ---- 
plot(bs_res_emissum$year, bs_res_emissum$co2)
func_test(bs_res_emissum$co2, 894)

# RESULT ----
# 总能耗


# 总排放
bs_total_emissum_df <- 
  func_ls2df(list(bs_agriculture_emissum_df, 
                  bs_ind_emissum_df, 
                  bs_construct_emissum_df, 
                  bs_trans_emissum_df, 
                  bs_com_emissum_df, 
                  bs_household_emissum_df, 
                  bs_tf_emission, bs_res_emissum))

# Test ----
# func_show_trend(bs_trans_emissum_df[c("year", "co2")])
# func_show_trend(bs_ind_emissum_df)
# func_show_trend(bs_com_emissum_df)
# func_show_trend(bs_other_emissum_df)
plot(bs_total_emissum_df$year, bs_total_emissum_df$co2)
bs_total_emissum_df$year[which(bs_total_emissum_df$co2 == 
                                 max(bs_total_emissum_df$co2))]
func_history_project(total_emissum_df, "co2", bs_total_emissum_df, "co2")

par(mfrow = c(2,2))
func_history_project(proj_ind_emissum_df, "co2", bs_ind_emissum_df, "co2")
func_history_project(proj_trans_emissum_df, "co2", bs_trans_emissum_df, "co2")
func_history_project(proj_com_emissum_df, "co2", bs_com_emissum_df, "co2")
func_history_project(proj_household_emissum_df, "co2", bs_household_emissum_df, "co2")
func_history_project(proj_tf_emission, "co2", bs_tf_emission, "co2")

