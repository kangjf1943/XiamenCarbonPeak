# DEMAND ----
# Agriculture ----
## Activity level ----
pln_agriculture_act <- 
  func_interp_3(year = c(2019, 2025, 2035, 2050, 2060), 
                scale = c(1, 0.8, 0.7, 0.63, 0.6), 
                base = func_lastone(by_agriculture_act$agriculture), 
                "area")

## Energy intensity ----
# 近期由于机械化水平提高，用电强度增加，但柴油强度则依历史趋势下降
pln_agriculture_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 0.8, 0.6), 
                base = func_lastone(by_agriculture_nrgintst$diesel), 
                "diesel")
pln_agriculture_nrgintst_df$electricity <- 
  func_interp_3(year = c(2019, 2025, 2045, 2060),
                scale = c(1, 1.2, 1.1, 0.9), 
                base = func_lastone(by_agriculture_nrgintst$electricity), 
                "electricity")$electricity

## Consumption and emission ----
pln_agriculture_nrgsum_df <- 
  func_nrg_sum(pln_agriculture_nrgintst_df, pln_agriculture_act, "area")
pln_agriculture_emissum_df <- 
  func_emissum(pln_agriculture_nrgsum_df, global_emisfac_df)

## Test ----
# par(mfrow = c(4, 2))
# func_history_project(by_agriculture_act, "agriculture", 
#                      pln_agriculture_act, "area", 
#                      xlab = "", ylab = "ActLvl")
# plot.new()
# func_history_project(by_agriculture_nrgintst, "diesel", 
#                      pln_agriculture_nrgintst_df, "diesel", 
#                      xlab = "", ylab = "柴油强度")
# func_history_project(by_agriculture_nrgintst, "electricity", 
#                      pln_agriculture_nrgintst_df, "electricity", 
#                      xlab = "", ylab = "用电强度")
# func_history_project(by_agriculture_nrgsum_df, "diesel", 
#                      pln_agriculture_nrgsum_df, "diesel", 
#                      xlab = "", ylab = "柴油总量")
# func_history_project(by_agriculture_nrgsum_df, "electricity", 
#                      pln_agriculture_nrgsum_df, "electricity", 
#                      xlab = "", ylab = "用电总量")
# func_history_project(by_agriculture_emissum_df, "co2", 
#                      pln_agriculture_emissum_df, "co2", 
#                      xlab = "", ylab = "排放总量")

# Industry ----
## Activity level ----
# 先计算未来子部门GDP所占比重
pln_ind_ori_act_prop <- data.frame(year = c(2019:2060))
pln_ind_ori_act_prop[, "食品饮料及烟草制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"食品饮料及烟草制造业"), 3, 2))$value
pln_ind_ori_act_prop[, "纺织及服装制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"纺织及服装制造业"), 3, 2))$value
pln_ind_ori_act_prop[, "木材及家具制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"木材及家具制造业"), 1.5, 2))$value
pln_ind_ori_act_prop[, "造纸及印刷"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"造纸及印刷"), 1.5, 1))$value
pln_ind_ori_act_prop[, "文体工美用品制造业"] <- func_interp_2(
  year = c(2019, 2040, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"文体工美用品制造业"), 2, 4))$value
pln_ind_ori_act_prop[, "石油及炼焦"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"石油及炼焦"), 0.1, 0))$value
pln_ind_ori_act_prop[, "化学工业"] <- func_interp_2(
  year = c(2019, 2030, 2045, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"化学工业"), 5, 0.5, 0))$value
pln_ind_ori_act_prop[, "医药制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"医药制造业"), 2, 4))$value
pln_ind_ori_act_prop[, "非金属矿物制品业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"非金属矿物制品业"), 1.3, 0.5))$value
pln_ind_ori_act_prop[, "金属加工制造业"] <- func_interp_2(
  year = c(2019, 2040, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"金属加工制造业"), 7, 3))$value
pln_ind_ori_act_prop[, "设备制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"设备制造业"), 15, 16))$value
pln_ind_ori_act_prop[, "电子电气制造业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"电子电气制造业"), 57, 64))$value
pln_ind_ori_act_prop[, "电力、热力生产和供应业"] <- func_interp_2(
  year = c(2019, 2030, 2060), 
  value = c(func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业"), 
            0.9, 0.1))$value
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
# 假设大部分能耗强度略有减少
pln_ind_nrgintst_ls <- vector("list", 13)
names(pln_ind_nrgintst_ls) <- global_ind_subsector
for (i in global_ind_subsector) {
  pln_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
  for (j in global_ind_nrgclass[1:6]) {
    pln_ind_nrgintst_ls[[i]][, j] <- 
      func_interp_3(
        year = c(2019, 2023, 2026, 2030, 2035, 2060), 
        scale = c(1.0, 1.0, 1.0, 1.0, 0.8, 0.6), 
        base = func_lastone(by_ind_nrgintst_ls[[i]][, j], zero.rm =  FALSE))$value
  }
}
# 但是天然气在短期内有所上升
for (i in global_ind_subsector) {
    pln_ind_nrgintst_ls[[i]][, "gas"] <- 
      func_interp_3(
        year = c(2019, 2022, 2025, 2030, 2035, 2060), 
        scale = c(1.0, 1.6, 1.8, 1.3, 1.2, 0.9), 
        base = func_lastone(by_ind_nrgintst_ls[[i]][, "gas"], 
                            zero.rm =  FALSE))$value
}
# 电力在短期内有所上升，但比天然气上升幅度小
for (i in global_ind_subsector) {
    pln_ind_nrgintst_ls[[i]][, "electricity"] <- 
      func_interp_3(
        year = c(2019, 2023, 2026, 2030, 2035, 2060), 
        scale = c(1.0, 1.3, 1.3, 1.2, 1.1, 1.0), 
        base = func_lastone(by_ind_nrgintst_ls[[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
}

## Energy and emission ----
pln_ind_nrgsum_ls <- func_nrg_sum_ls(pln_ind_nrgintst_ls, pln_ind_act)
pln_ind_nrgsum_df <- func_ls2df(pln_ind_nrgsum_ls)
pln_ind_emissum_df <- func_emissum(pln_ind_nrgsum_df, global_emisfac_df)

## Test----
# 问题：工业很有可能已经达峰了，不管是韩晖表格的历史数据还是模型的多种测算都是如此
# func_history_project_df(by_ind_ori_act_prop, pln_ind_ori_act_prop)
# func_history_project_df(by_ind_act, pln_ind_act)
# func_history_project_ls(by_ind_nrgintst_ls, pln_ind_nrgintst_ls)
# func_history_project_ls(by_ind_nrgsum_ls, pln_ind_nrgsum_ls)
par(mfrow = c(1, 1))
func_history_project(by_ind_emissum_df, "co2", pln_ind_emissum_df, "co2")
func_peakyear(pln_ind_emissum_df, "co2")


# Construction ----
## Activity level ----
pln_construct_act <- prj_global_gdp[c("year", "const_gdp")]
names(pln_construct_act)[2] <- "construct_gdp"

## Energy intensity ----
pln_construct_nrgintst_df <- 
  func_interp_3(year = c(2019, 2025, 2045, 2060), 
                scale = c(1, 0.8, 0.3, 0.1), 
                base = func_lastone(by_construct_nrgintst$electricity), 
                "electricity")

## Consumption and emission ----
pln_construct_nrgsum_df <- 
  func_nrg_sum(pln_construct_nrgintst_df, pln_construct_act, "construct_gdp")
pln_construct_emissum_df <- 
  func_emissum(pln_construct_nrgsum_df, global_emisfac_df)

## Test ----
par(mfrow = c(2, 2))
func_history_project(by_construct_act, "construct_gdp", 
                     pln_construct_act, "construct_gdp", 
                     xlab = "", ylab = "建筑业GDP")
func_history_project(by_construct_nrgintst, "electricity", 
                     pln_construct_nrgintst_df, "electricity", 
                     xlab = "", ylab = "用电强度")
func_history_project(by_construct_nrgsum_df, "electricity", 
                     pln_construct_nrgsum_df, "electricity", 
                     xlab = "", ylab = "用电总量")
func_history_project(by_construct_emissum_df, "co2", 
                     pln_construct_emissum_df, "co2", 
                     xlab = "", ylab = "排放量")


# Transportation ----
## Activity level ----
# 营运和非营运车辆
pln_by_trans_act <- data.frame(year = c(2019: 2060))
for (i in global_trans_subsector[1:12]) {
  pln_by_trans_act[, i] <- 
    func_interp_3(year = c(2019, 2025, 2035, 2060), 
                  scale = c(1, 1.5, 1.7, 1.9), 
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
    pln_trans_nrgintst_ls[[j]][, i] <- 
      func_interp_3(year = c(2019, 2025, 2060), 
                    scale = c(1, 1.2, 0.8), 
                    base = func_lastone(by_trans_nrgintst_ls[[j]][, i]))$value
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
pln_ori_comemployee <- func_cross(
  prj_global_population[c("year", "population")], 
  func_interp_2(
    year = c(2019, 2030, 2060),
    value = c(by_com_act$com_employee[by_com_act$year == 2019]/
                global_population$常住人口[global_population$year == 2019], 
              0.6, 0.75)))
pln_ori_comgdp <- prj_global_gdp[c("year", "com_gdp")]
pln_com_act <- func_merge_2(list(pln_ori_comemployee, pln_ori_comgdp))
names(pln_com_act) <- c("year", "com_employee", "com_gdp")

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
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 1.2, 0.8), 
                base = func_lastone(by_com_nrgintst_ls[[2]]$lpg), 
                "lpg")
pln_com_nrgintst_ls[[2]]$gas <- 
  func_interp_3(year = c(2019, 2025, 2060),
                scale = c(1, 1.2, 0.8), 
                base = func_lastone(by_com_nrgintst_ls[[2]]$gas), 
                "gas")$gas

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
# func_history_project_df(by_com_act, pln_com_act)
# func_history_project_ls(by_com_nrgintst_ls, pln_com_nrgintst_ls)
# func_history_project_ls(by_com_nrgsum_ls, pln_com_nrgsum_ls)
# par(mfrow = c(1, 1))
# func_history_project(by_com_emissum_df, "co2", pln_com_emissum_df, "co2")
# func_peakyear(pln_com_emissum_df, "co2")


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
                           scale = c(1, 2, 2), 
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
# house_coal_elec
# 生活用电强度
pln_household_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2060), 
                value = c(3900, 3900*1.2, 3900*1.4))
names(pln_household_nrgintst_ls[[1]])[2] <- "electricity"
# 生活用煤强度
pln_household_nrgintst_ls[[1]]$coal <- 0

# household_lpg
pln_household_nrgintst_ls[[2]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.8, 0.5), 
                base = func_lastone(by_household_nrgintst_ls[["lpg"]]$lpg), 
                "lpg")
# household_gas
pln_household_nrgintst_ls[[3]] <- 
  func_interp_3(year = c(2019, 2030, 2060), 
                scale = c(1, 0.8, 0.5), 
                base = func_lastone(by_household_nrgintst_ls[["gas"]]$gas), 
                "gas")

## Consumption and emission ----
pln_household_nrgsum_ls <- 
  func_nrg_sum_ls(pln_household_nrgintst_ls, pln_household_act)
pln_household_nrgsum_df <- func_ls2df(pln_household_nrgsum_ls)
pln_household_emissum_df <- 
  func_emissum(pln_household_nrgsum_df, global_emisfac_df)

## Test ----
# func_history_project_df(by_household_act, pln_household_act)
# func_history_project_ls(by_household_nrgintst_ls, pln_household_nrgintst_ls)
# func_history_project_ls(by_household_nrgsum_ls, pln_household_nrgsum_ls)
# par(mfrow = c(1, 1))
# func_history_project(by_com_emissum_df, "co2", pln_com_emissum_df, "co2")
# func_peakyear(pln_com_emissum_df, "co2")


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
                    func_interp_2(year = c(2019, 2026, 2030, 2040, 2060),
                                  value = c(900371, 900371*0.5, 900371*0.3, 0, 0),
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
pln_res_emisfac_df <- 
  func_interp_2(year = c(2019, 2025, 2030, 2035, 2050, 2060), 
                value = c(0.3910e-3,0.35e-3, 0.32e-3, 0.25e-3, 0, 0), "electricity")

pln_res_emissum <- 
  func_cross(pln_res_emisfac_df, pln_tfres_act[c("year", "importelec")])
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
plot(pln_total_emissum_df$year, pln_total_emissum_df$co2)
func_peakyear(pln_total_emissum_df, "co2")
func_history_project(by_total_emissum_df, "co2", pln_total_emissum_df, "co2")

