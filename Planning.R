# DEMAND ----
# Agriculture ----
## Activity level ----
pln_agriculture_act <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 0.6, 0.6), 
                base = func_lastone(by_agriculture_act$agriculture), 
                "area")

## Energy intensity ----
# 问题：由于机械化水品的提高，近期内农业能耗强度是否会增加？
pln_agriculture_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 0.8, 0.6), 
                base = func_lastone(by_agriculture_nrgintst$diesel), 
                "diesel")
pln_agriculture_nrgintst_df$electricity <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 0.8, 0.6), 
                base = func_lastone(by_agriculture_nrgintst$electricity), 
                "electricity")$electricity



## Consumption and emission ----
pln_agriculture_nrgsum_df <- 
  func_nrg_sum(pln_agriculture_nrgintst_df, pln_agriculture_act, "area")
pln_agriculture_emissum_df <- 
  func_emissum(pln_agriculture_nrgsum_df, global_emisfac_df)

## Test ----
plot(pln_agriculture_emissum_df$year, pln_agriculture_emissum_df$co2)

# Industry ----
## Activity level ----
# 先计算未来子部门GDP所占比重
pln_ind_ori_act_prop <- data.frame(year = c(2019:2060))
pln_ind_ori_act_prop[, "食品饮料及烟草制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$食品饮料及烟草制造业), 3, 2))$value
pln_ind_ori_act_prop[, "纺织及服装制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$纺织及服装制造业), 3, 2))$value
pln_ind_ori_act_prop[, "木材及家具制造业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$木材及家具制造业), 
                          1.5, 2))$value
pln_ind_ori_act_prop[, "造纸及印刷"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$造纸及印刷), 
                          1.5, 0.5))$value
pln_ind_ori_act_prop[, "文体工美用品制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$文体工美用品制造业), 2, 4))$value
pln_ind_ori_act_prop[, "石油及炼焦"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$石油及炼焦), 
                          0, 0))$value
pln_ind_ori_act_prop[, "化学工业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$化学工业), 
                          0, 0))$value
pln_ind_ori_act_prop[, "医药制造业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$医药制造业), 
                          5, 10))$value
pln_ind_ori_act_prop[, "非金属矿物制品业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$非金属矿物制品业), 
                          1.3, 0.5))$value
pln_ind_ori_act_prop[, "金属加工制造业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$金属加工制造业), 
                          4, 2))$value
pln_ind_ori_act_prop[, "设备制造业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$设备制造业), 
                          8, 5))$value
pln_ind_ori_act_prop[, "电子电气制造业"] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(func_lastone(by_ind_ori_act_prop$电子电气制造业), 
                          55, 65))$value
pln_ind_ori_act_prop[, "电力、热力生产和供应业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业"), 
            func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业") * 0.5, 
            func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业") * 0.1))$value
# 其他制造业
pln_ind_ori_act_prop[, "其他制造业"] <- 
  func_saturate(pln_ind_ori_act_prop, "value")$value
# pln_ind_ori_act_prop_long <- melt(pln_ind_ori_act_prop, id = "year")
# ggplot(pln_ind_ori_act_prop_long) + geom_bar(aes(year, value, fill = variable), stat = "identity")
# 计算未来各子部门GDP
pln_ind_act <- 
  func_nrg_sum(pln_ind_ori_act_prop[c("year", global_ind_subsector)], 
               prj_global_gdp[c("year","ind_gdp")], "ind_gdp")
pln_ind_act[global_ind_subsector] <- pln_ind_act[global_ind_subsector]/100

## Energy intensity ----
# 假设能耗强度略有减少
pln_by_ind_nrgintst_ls <- vector("list", 13)
names(pln_by_ind_nrgintst_ls) <- global_ind_subsector
for (i in global_ind_subsector) {
  pln_by_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
  for (j in global_ind_nrgclass) {
    pln_by_ind_nrgintst_ls[[i]][, j] <- 
      func_interp_3(year = c(2019, 2025, 2060), 
                    scale = c(1, 1, 0.8), 
                    base = func_lastone(by_ind_nrgintst_ls[[i]][, j]))$value
  }
}

## Energy and emission ----
pln_ind_nrgsum_ls <- func_nrg_sum_ls(pln_by_ind_nrgintst_ls, pln_ind_act)
pln_ind_nrgsum_df <- func_ls2df(pln_ind_nrgsum_ls)
pln_ind_emissum_df <- func_emissum(pln_ind_nrgsum_df, global_emisfac_df)

## Test----
# func_history_project_df(ind_act, pln_ind_act)
# func_history_project_ls(by_ind_nrgintst_ls, pln_by_ind_nrgintst_ls)
# func_history_project_ls(ind_nrgsum_ls, pln_ind_nrgsum_ls)
plot(pln_ind_emissum_df$year, pln_ind_emissum_df$co2)

# Construction ----
## Activity level ----
pln_construct_act <- 
  func_cross(prj_global_gdp[c("year", "GDP")], 
             func_interp_2(year = c(2019,2025,2060), 
                           value = c(0.10, 0.08, 0.03)))
## Energy intensity ----
pln_construct_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060), 
                scale = c(1, 0.9, 0.8), 
                base = func_lastone(by_construct_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
pln_construct_nrgsum_df <- 
  func_nrg_sum(pln_construct_nrgintst_df, pln_construct_act, "GDP")
pln_construct_emissum_df <- func_emissum(pln_construct_nrgsum_df, global_emisfac_df)

## Test ----
plot(pln_construct_emissum_df$year, pln_construct_emissum_df$co2)


# Transportation ----
## Activity level ----
# 营运和非营运车辆
pln_by_trans_act <- data.frame(year = c(2019: 2060))
for (i in global_trans_subsector[1:12]) {
  pln_by_trans_act[, i] <- 
    func_interp_3(year = c(2019, 2025, 2060), 
                  scale = c(1, 1.3, 2), 
                  base = func_lastone(by_trans_act[, i]))$value
}
# 但是其中摩托车数量减少
pln_by_trans_act$"摩托车" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(237499.0, 237499.0*0.5, 0))$value

# 水运
pln_by_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(10000, 10500, 11000))$value
comment(pln_by_trans_act$"水路客运") <- "万人公里"
pln_by_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(pln_by_trans_act$"水路货运") <- "万吨公里"

# func_history_project(by_trans_act, "轿车", pln_by_trans_act, "轿车")

## Energy intensity ----
pln_trans_nrgintst_ls <- vector("list", length(global_trans_subsector))
names(pln_trans_nrgintst_ls) <- global_trans_subsector
### 营运车辆和非营运车辆
# 均假设和最后一个有数值的年份一致，且假设这个数值是在2019年
for (j in c(1: 12)) {
  pln_trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2060))
  for (i in names(by_trans_nrgintst_ls[[j]])[
    names(by_trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
    pln_trans_nrgintst_ls[[j]][, i] <- func_lastone(by_trans_nrgintst_ls[[j]][, i])
  }
}
# 但是其中轿车逐渐实现电气化
pln_trans_carprop <- data.frame(year = pln_trans_nrgintst_ls[["轿车"]]$year)
pln_trans_carprop$gasoline <-
  func_interp_2(year = c(2019, 2025, 2030, 2050, 2060),
                value = c(1, 0.95, 0.9, 0.2, 0))$value
pln_trans_carprop$electricity <- 1 - pln_trans_carprop$gasoline
# 计算相应能耗强度中汽油和电力的变化
pln_trans_nrgintst_ls[["轿车"]]$electricity <-
  func_alter(mean(pln_trans_nrgintst_ls[["轿车"]]$gasoline),
             "gasoline", "electricity")
pln_trans_nrgintst_ls[["轿车"]] <-
  func_cross(pln_trans_nrgintst_ls[["轿车"]],
             pln_trans_carprop)

# 水路客运
# 柴油和燃料油均基于历史数据和比率
pln_trans_nrgintst_ls[["水路客运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(by_trans_nrgintst_ls[["水路客运"]]$diesel), 
                "diesel")
pln_trans_nrgintst_ls[["水路客运"]]$residual <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(
                  func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual), 
                  func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.85, 
                  func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.80),
                "residual")$residual

# 水路货运
# 柴油：基于历史数据和比率
pln_trans_nrgintst_ls[["水路货运"]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$diesel),
                "diesel")
# 燃料油：基于历史数据和比率
pln_trans_nrgintst_ls[["水路货运"]]$residual <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.85, 0.8), 
                base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$residual),
                "residual")$residual

## Energy and emission ----
pln_trans_nrgsum_ls <- func_nrg_sum_ls(pln_trans_nrgintst_ls, pln_by_trans_act)
pln_trans_nrgsum_df <- func_ls2df(pln_trans_nrgsum_ls)
pln_trans_emissum_df <- func_emissum(pln_trans_nrgsum_df, global_emisfac_df)

## Test----
# func_history_project_df(by_trans_act, pln_by_trans_act)
# func_history_project_ls(trans_nrgintst_ls, pln_trans_nrgintst_ls)
# func_history_project_ls(trans_nrgsum_ls, pln_trans_nrgsum_ls)
plot(pln_trans_emissum_df$year, pln_trans_emissum_df$co2)


# Service ----
## Activity level ----
pln_ori_comemployee <- 
  func_cross(prj_global_population[c("year", "population")], 
             func_interp_2(year = c(2019, 2030, 2060), 
                           value = c(0.45, 0.5, 0.7)))
pln_ori_comgdp <- prj_global_gdp[c("year", "com_gdp")]
pln_com_act <- func_merge_2(list(pln_ori_comemployee, pln_ori_comgdp))
names(pln_com_act) <- c("year", "com_employee", "com_gdp")
pln_com_act$com_gdp[1] <- global_gdp$"#第三产业"[global_gdp$year == 2019]
# func_show_trend(pln_com_act)

## Energy intensity ----
# 服务业用电强度略有增加
pln_com_nrgintst_ls <- vector("list", 2)
names(pln_com_nrgintst_ls) <- global_com_subsector
pln_com_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2025, 2060), 
                value = c(2958.967, 2958.967*1.2, 2958.967), 
                "electricity")
# 服务业燃气强度略有增加后减少，且逐渐为电气替代
pln_com_nrgintst_ls[[2]] <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(0.002055001,
                          0.002055001*1.2, 
                          0.002055001*0.8), "lpg")
pln_com_nrgintst_ls[[2]]$gas <- 
  func_interp_2(year = c(2019, 2025, 2060),
                value = c(1.353608e-04, 
                          1.353608e-04*1.2, 
                          1.353608e-04*0.8), "gas")$gas
# 液化石油气的电气化
pln_com_ori_lpg_elec_prop <- 
  data.frame(year = pln_com_nrgintst_ls[[2]]$year)
pln_com_ori_lpg_elec_prop$lpg <- 
  func_interp_2(year = c(2019, 2050, 2060), 
                value = c(1, 0, 0))$value
pln_com_ori_lpg_elec_prop$electricity <- 
  1 - pln_com_ori_lpg_elec_prop$lpg
pln_com_nrgintst_ls[[2]]$electricity4lpg <- 
  func_alter(pln_com_nrgintst_ls[[2]]$lpg, "lpg", "electricity")
pln_com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")] <- 
  func_cross(pln_com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")], 
             pln_com_ori_lpg_elec_prop)
# 天然气的电气化
pln_com_ori_gas_elec_prop <- 
  data.frame(year = pln_com_nrgintst_ls[[2]]$year)
pln_com_ori_gas_elec_prop$gas <- 
  func_interp_2(year = c(2019, 2050, 2060), 
                value = c(1, 0, 0))$value
pln_com_ori_gas_elec_prop$electricity <- 
  1 - pln_com_ori_gas_elec_prop$gas
pln_com_nrgintst_ls[[2]]$electricity4gas <- 
  func_alter(pln_com_nrgintst_ls[[2]]$gas, "gas", "electricity")
pln_com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")] <- 
  func_cross(pln_com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")], 
             pln_com_ori_gas_elec_prop)
# 整理数据框
pln_com_nrgintst_ls[[2]][, "electricity"] <- 
  pln_com_nrgintst_ls[[2]]$electricity4lpg +
  pln_com_nrgintst_ls[[2]]$electricity4gas
pln_com_nrgintst_ls[[2]] <- 
  pln_com_nrgintst_ls[[2]][c("year", "lpg", "gas", "electricity")]

## Energy and emission ----
pln_com_nrgsum_ls <- func_nrg_sum_ls(pln_com_nrgintst_ls, pln_com_act)
pln_com_nrgsum_df <- func_ls2df(pln_com_nrgsum_ls)
pln_com_emissum_df <- func_emissum(pln_com_nrgsum_df, global_emisfac_df)

## Test ----
# func_history_project_df(com_act, pln_com_act)
# func_history_project_ls(com_nrgintst_ls, pln_com_nrgintst_ls)
# func_history_project_ls(com_nrgsum_ls, pln_com_nrgsum_ls)
plot(pln_com_emissum_df$year, pln_com_emissum_df$co2)

# Household ----
## Activity level ----
# household_elec
pln_household_ori_household <- prj_global_population[c("year", "household")]
# household_lpg
pln_household_ori_lpguser <- 
  func_cross(prj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 0.65, 0.30), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "lpg")])))
names(pln_household_ori_lpguser)[2] <- "lpg"
# household_gas
pln_household_ori_gasuser <- 
  func_cross(prj_global_population[c("year", "household")], 
             func_interp_3(year = c(2019, 2030, 2060), 
                           scale = c(1, 2, 2.3), 
                           base = func_lastone(
                             by_household_ori_users[c("year", "gas")])))
names(pln_household_ori_gasuser)[2] <- "gas"
# 合并
pln_household_act <- func_merge_2(list(pln_household_ori_household, 
                                        pln_household_ori_lpguser, 
                                        pln_household_ori_gasuser))

## Energy intensity ----
pln_household_nrgintst_ls <- vector("list", length(global_household_subsector))
names(pln_household_nrgintst_ls) <- global_household_subsector
# house_elec
pln_household_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(3900, 3900*1.2, 3900*1.4))
names(pln_household_nrgintst_ls[[1]])[2] <- "electricity"

# household_lpg
pln_household_nrgintst_ls[[2]] <- 
  data.frame(year = c(2019: 2060), 
             lpg = 588.52)
names(pln_household_nrgintst_ls[[2]])[2] <- "lpg"

# household_gas
pln_household_nrgintst_ls[[3]] <- 
  data.frame(year = c(2019: 2060), 
             gas = 74.50)
names(pln_household_nrgintst_ls[[3]])[2] <- "gas"

## Consumption and emission ----
pln_household_nrgsum_ls <- 
  func_nrg_sum_ls(pln_household_nrgintst_ls, pln_household_act)
pln_household_nrgsum_df <- func_ls2df(pln_household_nrgsum_ls)
pln_household_emissum_df <- func_emissum(pln_household_nrgsum_df, global_emisfac_df)

## Test ----
plot(pln_household_emissum_df$year, pln_household_emissum_df$co2)

# TF & RES ----
# Power generation ----
## Activity level ----
# 取全社会用电量
pln_tfres_act <- func_ls2df(list(pln_agriculture_nrgsum_df, 
                                  pln_ind_nrgsum_df, 
                                  pln_construct_nrgsum_df, 
                                  pln_trans_nrgsum_df, 
                                  pln_com_nrgsum_df, 
                                  pln_household_nrgsum_df))
pln_tfres_act <- pln_tfres_act[c("year", "electricity")]
names(pln_tfres_act) <- c("year", "elecuse")
# 本地发电量到2025年减少为原来的一半
pln_tfres_act <- 
  func_merge_2(list(pln_tfres_act, 
                    func_stage(year = c(2019, 2025, 2030, 2060),
                               value = c(900371, 900371*0.5, 0, 0),
                               "elecgen")))
pln_tfres_act$importelec <- pln_tfres_act$elecuse - pln_tfres_act$elecgen

## Energy intensity ----
# 和基准年持平
pln_tf_nrgintst <- data.frame(year = c(2019: 2060))
for (i in names(by_tf_nrgintst)[names(by_tf_nrgintst) %in% "year" == FALSE]) {
  pln_tf_nrgintst[, i] <- func_lastone(by_tf_nrgintst[, i])
}

## Consumption and emission ----
pln_tf_nrgsum_df <- 
  func_nrg_sum(pln_tf_nrgintst, pln_tfres_act, "elecgen")
pln_tf_emission <- func_emissum(pln_tf_nrgsum_df, global_emisfac_df)

## Test
plot(pln_tf_emission$year, pln_tf_emission$co2)

# Imported elec ----
## Emission ----
pln_res_global_emisfac_df <- 
  func_interp_2(year = c(2019, 2033, 2050), 
                value = c(4.158130e-04,7.0e-05, 0), "electricity")

pln_res_emissum <- 
  func_cross(pln_res_global_emisfac_df, pln_tfres_act[c("year", "importelec")])
names(pln_res_emissum)[2] <- "co2"

## Test ---- 
plot(pln_res_emissum$year, pln_res_emissum$co2)

# RESULT ----
# 总排放
pln_total_emissum_df <- 
  func_ls2df(list(pln_agriculture_emissum_df, 
                  pln_ind_emissum_df, 
                  pln_construct_emissum_df, 
                  pln_trans_emissum_df, 
                  pln_com_emissum_df, 
                  pln_household_emissum_df,
                  pln_tf_emission, pln_res_emissum))

# Test ----
# func_show_trend(pln_trans_emissum_df[c("year", "co2")])
# func_show_trend(pln_ind_emissum_df)
# func_show_trend(pln_com_emissum_df)
# func_show_trend(pln_other_emissum_df)
plot(pln_total_emissum_df$year, pln_total_emissum_df$co2)
pln_total_emissum_df$year[which(pln_total_emissum_df$co2 == 
                                   max(pln_total_emissum_df$co2))]

