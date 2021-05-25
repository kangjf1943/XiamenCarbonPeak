# DEMAND ----
# Agriculture ----
## Activity level ----
proj_agriculture_act <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 0.6, 0.6), 
                base = func_lastone(by_agriculture_act$agriculture), 
                "area")

## Energy intensity ----
# 问题：由于机械化水品的提高，近期内农业能耗强度是否会增加？
proj_agriculture_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 0.8, 0.6), 
                base = func_lastone(by_agriculture_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
proj_agriculture_nrgsum_df <- 
  func_nrg_sum(proj_agriculture_nrgintst_df, proj_agriculture_act, "area")
proj_agriculture_emissum_df <- func_emissum(proj_agriculture_nrgsum_df, emisfac_df)

## Test ----
plot(proj_agriculture_emissum_df$year, proj_agriculture_emissum_df$co2)

# Industry ----
## Activity level ----
# 先计算未来子部门GDP所占比重
proj_ind_ori_act_prop <- data.frame(year = c(2019:2060))
# 食品饮料及烟草制造业
proj_ind_ori_act_prop[, ind_subsector[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(5, 3, 2))$value
# 纺织及服装制造业
proj_ind_ori_act_prop[, ind_subsector[2]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(6, 3, 2))$value
# 木材及家具制造业
proj_ind_ori_act_prop[, ind_subsector[3]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.5, 1.5, 2))$value
# 造纸及印刷
proj_ind_ori_act_prop[, ind_subsector[4]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.5, 1.5, 0.5))$value
# 文体工美用品制造业
proj_ind_ori_act_prop[, ind_subsector[5]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(2, 2, 4))$value
# 石油及炼焦
proj_ind_ori_act_prop[, ind_subsector[6]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(0.3, 0, 0))$value
# 化学工业
proj_ind_ori_act_prop[, ind_subsector[7]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(7.4, 0, 0))$value
# 医药制造业
proj_ind_ori_act_prop[, ind_subsector[8]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.6, 5, 10))$value
# 非金属矿物制品业
proj_ind_ori_act_prop[, ind_subsector[9]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.8, 1.3, 0.5))$value
# 金属加工制造业
proj_ind_ori_act_prop[, ind_subsector[10]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(7.6, 4, 2))$value
# 设备制造业
proj_ind_ori_act_prop[, ind_subsector[11]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(12, 8, 5))$value
# 电子电气制造业
proj_ind_ori_act_prop[, ind_subsector[12]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(49, 55, 65))$value
# 其他制造业
proj_ind_ori_act_prop[, ind_subsector[13]] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2019, ]), 
            100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2030, ]), 
            100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2060, ])
  ))$value
proj_ind_ori_act_prop_long <- melt(proj_ind_ori_act_prop, id = "year")
# ggplot(proj_ind_ori_act_prop_long) + geom_bar(aes(year, value, fill = variable), stat = "identity")
# 计算未来各子部门GDP
proj_ind_act <- func_nrg_sum(proj_ind_ori_act_prop, global_indgdp, "GDP")
proj_ind_act[ind_subsector] <- proj_ind_act[ind_subsector]/100

## Energy intensity ----
# 假设能耗强度略有减少
proj_ind_nrgintst_ls <- vector("list", 13)
names(proj_ind_nrgintst_ls) <- ind_subsector
for (i in ind_subsector) {
  proj_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
  for (j in ind_nrgclass) {
    proj_ind_nrgintst_ls[[i]][, j] <- 
      func_interp_3(year = c(2019, 2025, 2060), 
                    scale = c(1, 1, 0.8), 
                    base = func_lastone(ind_nrgintst_ls[[i]][, j]))$value
  }
}

## Energy and emission ----
proj_ind_nrgsum_ls <- func_nrg_sum_ls(proj_ind_nrgintst_ls, proj_ind_act)
proj_ind_nrgsum_df <- func_ls2df(proj_ind_nrgsum_ls)
proj_ind_emissum_df <- func_emissum(proj_ind_nrgsum_df, emisfac_df)

## Test----
# func_history_project_df(ind_act, proj_ind_act)
# func_history_project_ls(ind_nrgintst_ls, proj_ind_nrgintst_ls)
# func_history_project_ls(ind_nrgsum_ls, proj_ind_nrgsum_ls)
plot(proj_ind_emissum_df$year, proj_ind_emissum_df$co2)
func_test(proj_ind_emissum_df$co2, 164)

# Construction ----
## Activity level ----
proj_construct_act <- 
  func_cross(proj_global_gdp, 
             func_interp_2(year = c(2019,2025,2060), 
                           value = c(0.10, 0.08, 0.03)))
## Energy intensity ----
proj_construct_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 0.9, 0.8), 
                base = func_lastone(by_construct_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
proj_construct_nrgsum_df <- 
  func_nrg_sum(proj_construct_nrgintst_df, proj_construct_act, "GDP")
proj_construct_emissum_df <- func_emissum(proj_construct_nrgsum_df, emisfac_df)

## Test ----
plot(proj_construct_emissum_df$year, proj_construct_emissum_df$co2)


# Transportation ----
## Activity level ----
# 营运和非营运车辆
proj_trans_act <- data.frame(year = c(2019: 2060))
for (i in trans_subsector[1:12]) {
  proj_trans_act[, i] <- 
    func_interp_3(year = c(2019, 2025, 2060), 
                  scale = c(1, 1.3, 2), 
                  base = func_lastone(trans_act[, i]))$value
}
# 但是其中摩托车数量减少
proj_trans_act$"摩托车" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(237499.0, 237499.0*0.5, 0))$value

# 水运
proj_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(10000, 10500, 11000))$value
comment(proj_trans_act$"水路客运") <- "万人公里"
proj_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(proj_trans_act$"水路货运") <- "万吨公里"

# func_history_project(trans_act, "轿车", proj_trans_act, "轿车")

## Energy intensity ----
proj_trans_nrgintst_ls <- vector("list", length(trans_subsector))
names(proj_trans_nrgintst_ls) <- trans_subsector
### 营运车辆和非营运车辆
# 均假设和最后一个有数值的年份一致，且假设这个数值是在2019年
for (j in c(1: 12)) {
  proj_trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2060))
  for (i in names(trans_nrgintst_ls[[j]])[
    names(trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
    proj_trans_nrgintst_ls[[j]][, i] <- func_lastone(trans_nrgintst_ls[[j]][, i])
  }
}
# 但是其中轿车逐渐实现电气化
proj_trans_carprop <- data.frame(year = proj_trans_nrgintst_ls[["轿车"]]$year)
proj_trans_carprop$gasoline <-
  func_interp_2(year = c(2019, 2025, 2030, 2050, 2060),
                value = c(1, 0.95, 0.9, 0.2, 0))$value
proj_trans_carprop$electricity <- 1 - proj_trans_carprop$gasoline
# 计算相应能耗强度中汽油和电力的变化
proj_trans_nrgintst_ls[["轿车"]]$electricity <-
  func_alter(mean(proj_trans_nrgintst_ls[["轿车"]]$gasoline),
             "gasoline", "electricity")
proj_trans_nrgintst_ls[["轿车"]] <-
  func_cross(proj_trans_nrgintst_ls[["轿车"]],
             proj_trans_carprop)

# 水路客运
# 柴油和燃料油均基于历史数据和比率
proj_trans_nrgintst_ls[["水路客运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路客运"]]$diesel), 
                "diesel")
proj_trans_nrgintst_ls[["水路客运"]]$residual <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual), 
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual)*0.85, 
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$residual)*0.80),
                "residual")$residual

# 水路货运
# 柴油：基于历史数据和比率
proj_trans_nrgintst_ls[["水路货运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路货运"]]$diesel),
                "diesel")
# 燃料油：基于历史数据和比率
proj_trans_nrgintst_ls[["水路货运"]]$residual <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(trans_nrgintst_ls[["水路货运"]]$residual),
                "residual")$residual

## Energy and emission ----
proj_trans_nrgsum_ls <- func_nrg_sum_ls(proj_trans_nrgintst_ls, proj_trans_act)
proj_trans_nrgsum_df <- func_ls2df(proj_trans_nrgsum_ls)
proj_trans_emissum_df <- func_emissum(proj_trans_nrgsum_df, emisfac_df)

## Test----
# func_history_project_df(trans_act, proj_trans_act)
# func_history_project_ls(trans_nrgintst_ls, proj_trans_nrgintst_ls)
# func_history_project_ls(trans_nrgsum_ls, proj_trans_nrgsum_ls)
plot(proj_trans_emissum_df$year, proj_trans_emissum_df$co2)
func_test(proj_trans_emissum_df$co2, 907)

# Service ----
## Activity level ----
proj_ori_comemployee <- 
  func_cross(proj_global_population[c("year", "population")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.45, 0.5, 0.7)))
proj_ori_comgdp <- 
  func_cross(proj_global_gdp[c("year", "GDP")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.60, 0.65, 0.70)))
proj_com_act <- func_merge_2(list(proj_ori_comemployee, proj_ori_comgdp))
names(proj_com_act) <- c("year", "com_employee", "com_gdp")
proj_com_act$com_gdp[1] <- global_gdp$"#第三产业"[global_gdp$year == 2019]
plot(proj_com_act$com_employee, ylim = c(0, max(proj_com_act$com_employee)))
# func_show_trend(proj_com_act)

## Energy intensity ----
# 服务业用电强度略有增加
proj_com_nrgintst_ls <- vector("list", 2)
names(proj_com_nrgintst_ls) <- com_subsector
proj_com_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(2958.967, 2958.967*1.2, 2958.967), 
                "electricity")
# 服务业燃气强度略有增加后减少，且逐渐为电气替代
proj_com_nrgintst_ls[[2]] <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(0.002055001,
                          0.002055001*1.2, 
                          0.002055001*0.8), "lpg")
proj_com_nrgintst_ls[[2]]$gas <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(1.353608e-04, 
                          1.353608e-04*1.2, 
                          1.353608e-04*0.8), "gas")$gas
# 液化石油气的电气化
proj_com_ori_lpg_elec_prop <- 
  data.frame(year = proj_com_nrgintst_ls[[2]]$year)
proj_com_ori_lpg_elec_prop$lpg <- 
  func_interp_2(year = c(2019, 2050, 2060), 
                value = c(1, 0, 0))$value
proj_com_ori_lpg_elec_prop$electricity <- 
  1 - proj_com_ori_lpg_elec_prop$lpg
proj_com_nrgintst_ls[[2]]$electricity4lpg <- 
  func_alter(proj_com_nrgintst_ls[[2]]$lpg, "lpg", "electricity")
proj_com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")] <- 
  func_cross(proj_com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")], 
             proj_com_ori_lpg_elec_prop)
# 天然气的电气化
proj_com_ori_gas_elec_prop <- 
  data.frame(year = proj_com_nrgintst_ls[[2]]$year)
proj_com_ori_gas_elec_prop$gas <- 
  func_interp_2(year = c(2019, 2050, 2060), 
                value = c(1, 0, 0))$value
proj_com_ori_gas_elec_prop$electricity <- 
  1 - proj_com_ori_gas_elec_prop$gas
proj_com_nrgintst_ls[[2]]$electricity4gas <- 
  func_alter(proj_com_nrgintst_ls[[2]]$gas, "gas", "electricity")
proj_com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")] <- 
  func_cross(proj_com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")], 
             proj_com_ori_gas_elec_prop)
# 整理数据框
proj_com_nrgintst_ls[[2]][, "electricity"] <- 
  proj_com_nrgintst_ls[[2]]$electricity4lpg +
  proj_com_nrgintst_ls[[2]]$electricity4gas
proj_com_nrgintst_ls[[2]] <- 
  proj_com_nrgintst_ls[[2]][c("year", "lpg", "gas", "electricity")]

## Energy and emission ----
proj_com_nrgsum_ls <- func_nrg_sum_ls(proj_com_nrgintst_ls, proj_com_act)
proj_com_nrgsum_df <- func_ls2df(proj_com_nrgsum_ls)
proj_com_emissum_df <- func_emissum(proj_com_nrgsum_df, emisfac_df)

## Test ----
# func_history_project_df(com_act, proj_com_act)
# func_history_project_ls(com_nrgintst_ls, proj_com_nrgintst_ls)
# func_history_project_ls(com_nrgsum_ls, proj_com_nrgsum_ls)
plot(proj_com_emissum_df$year, proj_com_emissum_df$co2)
func_test(proj_com_emissum_df$co2, 43)

# Household ----
## Activity level ----
# household_elec
proj_household_ori_household <- proj_global_population[c("year", "household")]
# household_lpg
proj_household_ori_lpguser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 0.65, 0.30), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "lpg")])))
names(proj_household_ori_lpguser)[2] <- "lpg"
# household_gas
proj_household_ori_gasuser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 2, 2.3), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "gas")])))
names(proj_household_ori_gasuser)[2] <- "gas"
# 合并
proj_household_act <- func_merge_2(list(proj_household_ori_household, 
                                        proj_household_ori_lpguser, 
                                        proj_household_ori_gasuser))

## Energy intensity ----
proj_household_nrgintst_ls <- vector("list", length(household_subsector))
names(proj_household_nrgintst_ls) <- household_subsector
# house_elec
proj_household_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(3900, 3900*1.2, 3900*1.4))
names(proj_household_nrgintst_ls[[1]])[2] <- "electricity"

# household_lpg
proj_household_nrgintst_ls[[2]] <- 
  data.frame(year = c(2019: 2060), 
             lpg = 588.52)
names(proj_household_nrgintst_ls[[2]])[2] <- "lpg"

# household_gas
proj_household_nrgintst_ls[[3]] <- 
  data.frame(year = c(2019: 2060), 
             gas = 74.50)
names(proj_household_nrgintst_ls[[3]])[2] <- "gas"

## Consumption and emission ----
proj_household_nrgsum_ls <- 
  func_nrg_sum_ls(proj_household_nrgintst_ls, proj_household_act)
proj_household_nrgsum_df <- func_ls2df(proj_household_nrgsum_ls)
proj_household_emissum_df <- func_emissum(proj_household_nrgsum_df, emisfac_df)

## Test ----
plot(proj_household_emissum_df$year, proj_household_emissum_df$co2)
func_test(proj_household_emissum_df$co2, 26)

# TF & RES ----
# Power generation ----
## Activity level ----
# 取全社会用电量
proj_tfres_act <- func_ls2df(list(proj_agriculture_nrgsum_df, 
                                  proj_ind_nrgsum_df, 
                                  proj_construct_nrgsum_df, 
                                  proj_trans_nrgsum_df, 
                                  proj_com_nrgsum_df, 
                                  proj_household_nrgsum_df))
proj_tfres_act <- proj_tfres_act[c("year", "electricity")]
names(proj_tfres_act) <- c("year", "elecuse")
# 本地发电量到2025年减少为原来的一半
proj_tfres_act <- 
  func_merge_2(list(proj_tfres_act, 
                    func_stage(year = c(2019, 2025, 2030, 2060),
                               value = c(900371, 900371*0.5, 900371*0.1, 0),
                               "elecgen")))
proj_tfres_act$importelec <- proj_tfres_act$elecuse - proj_tfres_act$elecgen

## Energy intensity ----
# 和基准年持平
proj_tf_nrgintst <- data.frame(year = c(2019: 2060))
for (i in names(tf_nrgintst)[names(tf_nrgintst) %in% "year" == FALSE]) {
  proj_tf_nrgintst[, i] <- func_lastone(tf_nrgintst[, i])
}

## Consumption and emission ----
proj_tf_nrgsum_df <- 
  func_nrg_sum(proj_tf_nrgintst, proj_tfres_act, "elecgen")
proj_tf_emission <- func_emissum(proj_tf_nrgsum_df, emisfac_df)

## Test
plot(proj_tf_emission$year, proj_tf_emission$co2)
func_test(proj_tf_emission$co2, 623)

# Imported elec ----
## Emission ----
proj_res_emisfac_df <- 
  func_interp_2(year = c(2019, 2033, 2050), 
                value = c(4.158130e-04,7.0e-05, 0), "electricity")

proj_res_emissum <- 
  func_cross(proj_res_emisfac_df, proj_tfres_act[c("year", "importelec")])
names(proj_res_emissum)[2] <- "co2"

## Test ---- 
plot(proj_res_emissum$year, proj_res_emissum$co2)
func_test(proj_res_emissum$co2, 894)

# RESULT ----
# 总能耗


# 总排放
proj_total_emissum_df <- 
  func_ls2df(list(proj_agriculture_emissum_df, 
                  proj_ind_emissum_df, 
                  proj_construct_emissum_df, 
                  proj_trans_emissum_df, 
                  proj_com_emissum_df, 
                  proj_household_emissum_df, 
                  proj_tf_emission, proj_res_emissum))

# Test ----
# func_show_trend(proj_trans_emissum_df[c("year", "co2")])
# func_show_trend(proj_ind_emissum_df)
# func_show_trend(proj_com_emissum_df)
# func_show_trend(proj_other_emissum_df)
plot(proj_total_emissum_df$year, proj_total_emissum_df$co2)
proj_total_emissum_df$year[which(proj_total_emissum_df$co2 == 
                                   max(proj_total_emissum_df$co2))]
func_history_project(total_emissum_df, "co2", proj_total_emissum_df, "co2")

