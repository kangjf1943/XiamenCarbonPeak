# DEMAND ----
# Agriculture ----
## Activity level ----
proj_agriculture_act <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(221, 
                          221 * 0.6, 
                          221 * 0.5), "area")

## Energy intensity ----
proj_agriculture_nrgintst_df <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(59, 
                          59 * 0.8,
                          59 * 0.7), "electricity")

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
proj_ind_ori_act_prop[, ind_subsector[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(5, 5, 5))$value
proj_ind_ori_act_prop[, ind_subsector[2]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(6, 6, 6))$value
proj_ind_ori_act_prop[, ind_subsector[3]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.5, 1.5, 2))$value
proj_ind_ori_act_prop[, ind_subsector[4]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.5, 1.5, 0.5))$value
proj_ind_ori_act_prop[, ind_subsector[5]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(2, 2, 4))$value
proj_ind_ori_act_prop[, ind_subsector[6]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(0.3, 0, 0))$value
proj_ind_ori_act_prop[, ind_subsector[7]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(7.4, 0, 0))$value
proj_ind_ori_act_prop[, ind_subsector[8]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.6, 2, 3))$value
proj_ind_ori_act_prop[, ind_subsector[9]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(1.8, 1.3, 0.5))$value
proj_ind_ori_act_prop[, ind_subsector[10]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(7.6, 7.6, 7.6))$value
proj_ind_ori_act_prop[, ind_subsector[11]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(12, 10, 9))$value
proj_ind_ori_act_prop[, ind_subsector[12]] <- 
  func_interp_2(year = c(2019, 2030, 2060), value = c(49, 55, 60))$value
proj_ind_ori_act_prop[, ind_subsector[13]] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2019, ]), 
            100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2030, ]), 
            100 - sum(proj_ind_ori_act_prop[2:13][proj_ind_ori_act_prop$year == 2060, ])
  ))$value
proj_ind_ori_act_prop_long <- melt(proj_ind_ori_act_prop, id = "year")
ggplot(proj_ind_ori_act_prop_long) + geom_bar(aes(year, value, fill = variable), stat = "identity")
# 计算未来各子部门GDP
proj_ind_act <- func_nrg_sum(proj_ind_ori_act_prop, proj_global_indgdp, "GDP")
proj_ind_act[ind_subsector] <- proj_ind_act[ind_subsector]/100

## Energy intensity ----
# 假设能耗强度略有减少
proj_ind_nrgintst_ls <- vector("list", 13)
names(proj_ind_nrgintst_ls) <- ind_subsector
for (i in ind_subsector) {
  proj_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
  for (j in ind_nrgclass) {
    proj_ind_nrgintst_ls[[i]][, j] <- 
      func_interp_3(year = c(2019, 2060), 
                    scale = c(1, 0.9), 
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


# Construction ----
## Activity level ----
proj_construct_act <- 
  func_cross(proj_global_gdp, 
             func_interp_2(year = c(2019,2030,2060), 
                           value = c(0.10, 0.08, 0.03)))
## Energy intensity ----
proj_construct_nrgintst_df <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(0.0081, 
                          0.0081 * 1.2, 
                          0.0081), "electricity")

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
    func_interp_2(year = c(2019, 2030, 2060), 
                  value = c(func_lastone(trans_act[, i]), 
                            func_lastone(trans_act[, i])*1.05, 
                            func_lastone(trans_act[, i])*1.07))$value
}
# 但是其中摩托车数量减少
proj_trans_act$摩托车 <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(237499.0, 237499.0*0.5, 0))$value

# 水运
proj_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(10000, 10500, 11000))$value
comment(proj_trans_act$"水路客运") <- "万人公里"
proj_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(proj_trans_act$"水路货运") <- "万吨公里"

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
# 但是其中：
# 摩托车电气化
proj_trans_motorprop <- data.frame(year = proj_trans_nrgintst_ls[["摩托车"]]$year)
proj_trans_motorprop$gasoline <- 
  func_interp_2(year = c(2019, 2030, 2050, 2060), 
                value = c(1, 0.5, 0, 0))$value
proj_trans_motorprop$electricity <- 1 - proj_trans_motorprop$gasoline
# 计算相应能耗强度中汽油和电力的变化
proj_trans_nrgintst_ls[["轿车"]]$electricity <- 
  func_alter(mean(proj_trans_nrgintst_ls[["轿车"]]$gasoline), 
             "gasoline", "electricity")
proj_trans_nrgintst_ls[["轿车"]] <- 
  func_cross(proj_trans_nrgintst_ls[["轿车"]], 
             proj_trans_motorprop)

# 轿车逐渐实现电气化
proj_trans_carprop <- data.frame(year = proj_trans_nrgintst_ls[["轿车"]]$year)
proj_trans_carprop$gasoline <- 
  func_interp_2(year = c(2019, 2030, 2050, 2060), 
                value = c(1, 0.3, 0, 0))$value
proj_trans_carprop$electricity <- 1 - proj_trans_carprop$gasoline
# 计算相应能耗强度中汽油和电力的变化
proj_trans_nrgintst_ls[["轿车"]]$electricity <- 
  func_alter(mean(proj_trans_nrgintst_ls[["轿车"]]$gasoline), 
             "gasoline", "electricity")
proj_trans_nrgintst_ls[["轿车"]] <- 
  func_cross(proj_trans_nrgintst_ls[["轿车"]], 
             proj_trans_carprop)

# 水路客运
# 柴油：基于历史数据和比率
proj_trans_nrgintst_ls[["水路客运"]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$diesel),
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$diesel)*0.85, 
                  func_lastone(trans_nrgintst_ls[["水路客运"]]$diesel)*0.80), 
                "diesel")
# 燃料油：基于历史数据和比率
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
# plot(proj_trans_emissum_df$year, proj_trans_emissum_df$co2)


# Service ----
## Activity level ----
proj_ori_comemployee <- 
  func_cross(proj_global_population[c("year", "population")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.45, 0.48, 0.73)))
proj_ori_comgdp <- 
  func_cross(proj_global_gdp[c("year", "GDP")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.60, 0.65, 0.70)))

proj_com_act <- func_merge_2(list(proj_ori_comemployee, proj_ori_comgdp))
names(proj_com_act) <- c("year", "com_employee", "com_gdp")

## Energy intensity ----
# 服务业用电强度保持不变
proj_com_nrgintst_ls <- vector("list", 2)
names(proj_com_nrgintst_ls) <- com_subsector
proj_com_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(2958.967, 2958.967, 2958.967), 
                "electricity")
# 服务业燃气强度减少且逐渐为电气替代
proj_com_nrgintst_ls[[2]] <- 
  func_interp_2(year = c(2019, 2030, 2060),
                value = c(0.002055001,
                          0.002055001*0.7, 
                          0.002055001*0.3), "lpg")
proj_com_nrgintst_ls[[2]]$gas <- 
  func_interp_2(year = c(2019, 2030, 2060),
                value = c(1.353608e-04, 
                          1.353608e-04*0.7, 
                          1.353608e-04*0.3), "gas")$gas
# 液化石油气的电气化
proj_com_ori_lpg_elec_prop <- 
  data.frame(year = proj_com_nrgintst_ls[[2]]$year)
proj_com_ori_lpg_elec_prop$lpg <- 
  func_interp_2(year = c(2019, 2040, 2060), 
                value = c(1, 0.1, 0))$value
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
  func_interp_2(year = c(2019, 2040, 2060), 
                value = c(1, 0.1, 0))$value
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


# Household ----
## Activity level ----
# household_elec
proj_household_ori_household <- proj_global_population[c("year", "household")]
# household_lpg
proj_household_ori_lpguser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.30, 0.15, 0.08)))
names(proj_household_ori_lpguser)[2] <- "lpg"
# household_gas
proj_household_ori_gasuser <- 
  func_cross(proj_global_population[c("year", "household")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.35, 0.70, 0.96)))
names(proj_household_ori_gasuser)[2] <- "gas"
# 合并
proj_household_act <- func_merge_2(list(proj_household_ori_household, 
                                        proj_household_ori_lpguser, 
                                        proj_household_ori_gasuser))

## Energy intensity ----
proj_household_nrgintst_ls <- vector("list", length(household_subsector))
names(proj_other_nrgintst_ls) <- household_subsector
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


# TF & RES ----
# Power generation ----
## Activity level ----
# 合并各部门能耗
proj_demand_nrgsum_df <- func_ls2df(list(proj_trans_nrgsum_df, proj_ind_nrgsum_df, 
                                         proj_com_nrgsum_df, proj_other_nrgsum_df))
proj_demand_nrgsum_df <- proj_demand_nrgsum_df[c("year", global_nrg_class)]
# 本地发电量到2025年减少为原来的一半
proj_tfres_act <- 
  func_merge_2(list(proj_demand_nrgsum_df[c("year", "electricity")], 
                    func_interp_2(year = c(2020, 2025, 2030, 2060),
                                  value = c(900371, 900371*0.5, 900371*0.2, 0),
                                                  "elecgen")))
proj_tfres_act$importelec <- proj_tfres_act$electricity - proj_tfres_act$elecgen

## Energy intensity ----
proj_tf_nrgintst <- data.frame(year = c(2019: 2060))
for (i in names(tf_nrgintst)[names(tf_nrgintst) %in% "year" == FALSE]) {
  proj_tf_nrgintst[, i] <- func_lastone(tf_nrgintst[, i])
}

## Energy consumption ----
proj_tf_nrgsum_df <- 
  func_nrg_sum(proj_tf_nrgintst, proj_tfres_act, "elecgen")

## Emission ----
proj_tf_emission <- func_emissum(proj_tf_nrgsum_df, emisfac_df)

# Imported elec ----
## Emission ----
proj_res_emisfac_df <- data.frame(year = c(2019: 2060), 
                                  emisfac = c(0.000415813))
proj_res_emissum <- 
  func_cross(proj_res_emisfac_df, proj_tfres_act[c("year", "importelec" )])
names(proj_res_emissum)[2] <- "co2"


# RESULT ----
# Energy consumption ----
proj_trans_nrgsum_df <- func_ls2df(proj_trans_nrgsum_ls)
proj_ind_nrgsum_df <- func_ls2df(proj_ind_nrgsum_ls)
proj_com_nrgsum_df <- func_ls2df(proj_com_nrgsum_ls)
proj_other_nrgsum_df <- func_ls2df(proj_other_nrgsum_ls)
# 合并各部门能耗
total_nrgsum_ls <- list(trans_nrgsum_df, ind_nrgsum_df, 
                        com_nrgsum_df, other_nrgsum_df)
names(total_nrgsum_ls) <- c("trans", "ind", "com", "other")
total_nrgsum_df <- func_ls2df(total_nrgsum_ls)

# Emission ----
proj_trans_emissum_df <- func_emissum(proj_trans_nrgsum_df, emisfac_df)
proj_ind_emissum_df <- func_emissum(proj_ind_nrgsum_df, emisfac_df)
proj_com_emissum_df <- func_emissum(proj_com_nrgsum_df, emisfac_df)
proj_other_emissum_df <- func_emissum(proj_other_nrgsum_df, emisfac_df)

proj_total_emissum_df <- 
  func_ls2df(list(proj_trans_emissum_df, proj_ind_emissum_df, 
                  proj_com_emissum_df, proj_other_emissum_df, 
                  proj_tf_emission, proj_res_emissum))

func_show_trend(proj_total_emissum_df)

# Test ----
# func_show_trend(proj_trans_emissum_df[c("year", "co2")])
# func_show_trend(proj_ind_emissum_df)
# func_show_trend(proj_com_emissum_df)
# func_show_trend(proj_other_emissum_df)


