# Init ----
# 主要措施包括工业结构优化、电动车普及、服务业节能、生活节能
init_measures <- c("INDSTR", "ELECCAR", "COMCONS", "HHCONS", "OTHER")
init_scenarios <- 
  func_sgen("BAU", init_measures)
# 构建输出结果变量
# 需要输出的主要数据性质
init_outputs <- c("_nrgsum_ls", "_emissum_dir_ls", "_emissum_ls")
# 输出结果的形式模板
init_output_templatels <- vector("list", length(init_scenarios))
names(init_output_templatels) <- init_scenarios
# 生成输出结果包装盒
for (i in c(global_sectors, "tot")) {
  for (j in init_outputs) {
    assign(paste0(i, j), init_output_templatels)
  }
}
# 增加其他所需输出变量
for (i in c("tot_emisbysec_ls", "trans_carprop_ls")) {
  assign(i, init_output_templatels)
}
# 删除不必要的包装盒
# 电力等不区分直接排放和间接排放故删除
rm(init_output_templatels, 
   tf_emissum_dir_ls, res_emissum_dir_ls, tot_emissum_dir_ls)


# Settings ----
set_scalcs <- init_scenarios
set_plotstyle <- "base"
set_calc_cache <- FALSE
set_parmsim <- FALSE

# Analysis ----
for (set_scalc in set_scalcs) {
  # Agriculture ----
  ## Activity level ----
  agri_act <- 
    func_interp_3(year = c(2019, 2025, 2035, 2050, 2060), 
                  scale = c(1, 0.68, 0.64, 0.63, 0.62), 
                  base = func_lastone(by_agri_act$agri), 
                  "area")
  
  ## Energy intensity ----
  if (grepl("OTHER", set_scalc)) {
    ### OTHER ----
    # 近期由于机械化水平提高，用电强度增加，但柴油强度则依历史趋势下降
    agri_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2050, 2060),
                    scale = c(1, 0.8, 0, 0), 
                    base = func_lastone(by_agri_nrgintst$diesel), 
                    "diesel")
    agri_nrgintst_df$electricity <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060),
                    scale = c(1, 1.2, 1.1, 0.9), 
                    base = func_lastone(by_agri_nrgintst$electricity), 
                    "electricity")$electricity
  } else { ### BAU ----
    # 效率较低
    agri_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.4, 1.3), 
                    base = func_lastone(by_agri_nrgintst$diesel), 
                    "diesel")
    agri_nrgintst_df$electricity <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060),
                    scale = c(1, 1.6, 1.4, 1.3), 
                    base = func_lastone(by_agri_nrgintst$electricity), 
                    "electricity")$electricity
  }
  
  ## Consumption and emission ----
  agri_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum(agri_nrgintst_df, agri_act, "area")
  agri_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(agri_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Industry ----
  ## Activity level ----
  # 先计算未来子部门GDP所占比重
  ind_ori_act_prop <- data.frame(year = c(2019:2060))
  ind_ori_act_prop[, "食品饮料及烟草制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"食品饮料及烟草制造业"), 3, 2))$value
  ind_ori_act_prop[, "纺织及服装制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"纺织及服装制造业"), 3, 2))$value
  ind_ori_act_prop[, "木材及家具制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"木材及家具制造业"), 1.5, 2))$value
  ind_ori_act_prop[, "造纸及印刷"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"造纸及印刷"), 1.5, 1))$value
  ind_ori_act_prop[, "文体工美用品制造业"] <- func_interp_2(
    year = c(2019, 2040, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"文体工美用品制造业"), 2, 4))$value
  ind_ori_act_prop[, "石油及炼焦"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"石油及炼焦"), 0.1, 0))$value
  ind_ori_act_prop[, "化学工业"] <- func_interp_2(
    year = c(2019, 2030, 2045, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"化学工业"), 5, 0.5, 0))$value
  ind_ori_act_prop[, "医药制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"医药制造业"), 2, 4))$value
  ind_ori_act_prop[, "非金属矿物制品业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"非金属矿物制品业"), 1.3, 0.5))$value
  ind_ori_act_prop[, "金属加工制造业"] <- func_interp_2(
    year = c(2019, 2040, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"金属加工制造业"), 7, 3))$value
  ind_ori_act_prop[, "设备制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"设备制造业"), 15, 16))$value
  ind_ori_act_prop[, "电子电气制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"电子电气制造业"), 57, 64))$value
  ind_ori_act_prop[, "电力、热力生产和供应业"] <- func_interp_2(
    year = c(2019, 2030, 2060), 
    value = c(func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业"), 
              0.9, 0.1))$value
  ind_ori_act_prop[, "其他制造业"] <- 
    func_saturate(ind_ori_act_prop, "value")$value
  # 计算未来各子部门GDP
  ind_act <- 
    func_nrg_sum(ind_ori_act_prop[c("year", global_ind_subsector)], 
                 prj_global_gdp[c("year","indgdp")], "indgdp")
  ind_act[global_ind_subsector] <- ind_act[global_ind_subsector]/100
  
  ## Energy intensity ----
  ### All scenarios ----
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    # 假设大部分能耗强度略有减少
    ind_nrgintst_ls <- vector("list", 13)
    names(ind_nrgintst_ls) <- global_ind_subsector
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
      for (j in global_ind_nrgclass[1:6]) {
        ind_nrgintst_ls[[i]][, j] <- 
          func_interp_3(
            year = c(2019, 2023, 2025, 2030, 2035, 2060), 
            scale = c(1.0, 1.1, 1.1, 1.0, 0.8, 0.6), 
            base = func_lastone(by_ind_nrgintst_ls[[i]][, j], zero.rm =  FALSE))$value
      }
    }
  } else { ### BAU ----
    # 效率较低
    ind_nrgintst_ls <- vector("list", 13)
    names(ind_nrgintst_ls) <- global_ind_subsector
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
      for (j in global_ind_nrgclass[1:6]) {
        ind_nrgintst_ls[[i]][, j] <- 
          func_interp_3(
            year = c(2019, 2023, 2025, 2030, 2035, 2060), 
            scale = c(1.0, 1.3, 1.3, 1.3, 1, 0.8), 
            base = func_lastone(by_ind_nrgintst_ls[[i]][, j], zero.rm =  FALSE))$value
      }
    }
  }
  
  # 但是天然气在短期内有所上升
  for (i in global_ind_subsector) {
    ind_nrgintst_ls[[i]][, "gas"] <- 
      func_interp_3(
        year = c(2019, 2022, 2025, 2030, 2035, 2060), 
        scale = c(1.0, 1.45, 1.4, 1.2, 1.2, 1), 
        base = func_lastone(by_ind_nrgintst_ls[[i]][, "gas"], 
                            zero.rm =  FALSE))$value
  }
  # 电力在短期内有所上升，但比天然气上升幅度小
  for (i in global_ind_subsector) {
    ind_nrgintst_ls[[i]][, "electricity"] <- 
      func_interp_3(
        year = c(2019, 2023, 2026, 2030, 2035, 2060), 
        scale = c(1.0, 1.4, 1.4, 1.2, 1.1, 1.0), 
        base = func_lastone(by_ind_nrgintst_ls[[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
  }
  
  ## Energy and emission ----
  ind_nrgsum_ls[[set_scalc]] <- func_nrg_sum_ls(ind_nrgintst_ls, ind_act)
  ind_nrgsum_ls[[set_scalc]] <- 
    func_ls2df(ind_nrgsum_ls[[set_scalc]])
  ind_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(ind_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Construction ----
  ## Activity level ----
  const_act <- prj_global_gdp[c("year", "constgdp")]
  names(const_act)[2] <- "const_gdp"
  
  ## Energy intensity ----
  if (grepl("OTHER", set_scalc)) {### OTHER ----
    const_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060), 
                    scale = c(1, 0.8, 0.3, 0.1), 
                    base = func_lastone(by_const_nrgintst$electricity), 
                    "electricity")
  } else { ### BAU ----
    # 效率较低
    const_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060), 
                    scale = c(1, 1, 1, 0.5), 
                    base = func_lastone(by_const_nrgintst$electricity), 
                    "electricity")
  }
  
  
  ## Consumption and emission ----
  const_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum(const_nrgintst_df, const_act, "const_gdp")
  const_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(const_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Transportation ----
  ## Activity level ----
  # 公路
  trans_act <- data.frame(year = c(2019: 2060))
  # 常规公交和快速公交以初始3%的增长率增长至2030年饱和
  for (i in c("常规公交", "快速公交")) {
    trans_act[, i] <- 
      func_curve_1(
        baseyear = 2019, basevalue = func_lastone(by_trans_act[, i]), 
        maxyear = 2030, endyear = 2060, init_rate = 0.03)$value
  }
  
  # 出租车保持不变或波动
  trans_act[, "出租车"] <- 
    func_curve_1(
      baseyear = 2019, basevalue = func_lastone(by_trans_act[, "出租车"]), 
      maxyear = 2035, endyear = 2060, init_rate = -0.01)$value
  
  # 农村客运因城乡交流频繁，2025年前以初始7%的速率增长
  trans_act[, "农村客车"] <- 
    func_curve_1(
      baseyear = 2019, basevalue = func_lastone(by_trans_act[, "农村客车"]), 
      maxyear = 2025, endyear = 2060, init_rate = 0.06)$value
  
  # 水运
  trans_act$"水路客运" <- 
    func_interp_2(year = c(2019, 2025, 2060), 
                  value = c(10000, 10500, 11000))$value
  comment(trans_act$"水路客运") <- "万人公里"
  trans_act$"水路货运" <- 
    func_interp_2(year = c(2019, 2025, 2060), 
                  value = c(21538635, 21538635*1.5, 21538635*2.0))$value
  comment(trans_act$"水路货运") <- "万吨公里"
  
  ## Energy intensity ----
  trans_nrgintst_ls <- vector("list", length(global_trans_subsector))
  names(trans_nrgintst_ls) <- global_trans_subsector
  ### 营运车辆和非营运车辆
  # 均假设和最后一个有数值的年份一致，且假设这个数值是在2019年
  for (j in c(1: 6)) {
    trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2060))
    for (i in names(by_trans_nrgintst_ls[[j]])[
      names(by_trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
      trans_nrgintst_ls[[j]][, i] <- 
        func_interp_3(year = c(2019, 2030, 2035, 2060), 
                      scale = c(1, 1.2, 1.2, 0.8), 
                      base = func_lastone(by_trans_nrgintst_ls[[j]][, i]))$value
    }
  }
  if (grepl("ELECCAR", set_scalc)) { ### ELECCAR ----
    # 轿车逐渐实现电气化
    # 替代比例
    trans_carprop_ls[[set_scalc]] <- 
      data.frame(year = trans_nrgintst_ls[["公路其他汽油"]]$year)
    trans_carprop_ls[[set_scalc]]$gasoline <-
      func_interp_2(year = c(2019, 2025, 2030, 2050, 2060),
                    value = c(1, 0.95, 0.9, 0.2, 0))$value
    trans_carprop_ls[[set_scalc]]$electricity <- 1 - trans_carprop_ls[[set_scalc]]$gasoline
    # 计算相应能耗强度中汽油和电力的变化
    trans_nrgintst_ls[["公路其他汽油"]]$electricity <-
      func_alter(mean(trans_nrgintst_ls[["公路其他汽油"]]$gasoline),
                 "gasoline", "electricity")
    trans_nrgintst_ls[["公路其他汽油"]] <-
      func_cross(trans_nrgintst_ls[["公路其他汽油"]],
                 trans_carprop_ls[[set_scalc]])
    
    # 水路客运
    # 柴油和燃料油均基于历史数据和比率
    trans_nrgintst_ls[["水路客运"]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 0.85, 0.8), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路客运"]]$diesel), 
                    "diesel")
    trans_nrgintst_ls[["水路客运"]]$residual <- 
      func_interp_2(year = c(2019, 2030, 2060), 
                    value = c(
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual), 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.85, 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.80),
                    "residual")$residual
    
    # 水路货运
    # 柴油：基于历史数据和比率
    trans_nrgintst_ls[["水路货运"]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 0.85, 0.8), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$diesel),
                    "diesel")
    # 燃料油：基于历史数据和比率
    trans_nrgintst_ls[["水路货运"]]$residual <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 0.85, 0.8), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$residual),
                    "residual")$residual
  } else { ### BAU ----
    # 其中轿车逐渐实现电气化
    # 替代比例
    # 推迟年份
    trans_carprop_ls[[set_scalc]] <- 
      data.frame(year = trans_nrgintst_ls[["公路其他汽油"]]$year)
    trans_carprop_ls[[set_scalc]]$gasoline <-
      func_interp_2(year = c(2019, 2030, 2035, 2055, 2060),
                    value = c(1, 0.95, 0.9, 0.2, 0))$value
    trans_carprop_ls[[set_scalc]]$electricity <- 1 - trans_carprop_ls[[set_scalc]]$gasoline
    # 计算相应能耗强度中汽油和电力的变化
    trans_nrgintst_ls[["公路其他汽油"]]$electricity <-
      func_alter(mean(trans_nrgintst_ls[["公路其他汽油"]]$gasoline),
                 "gasoline", "electricity")
    trans_nrgintst_ls[["公路其他汽油"]] <-
      func_cross(trans_nrgintst_ls[["公路其他汽油"]],
                 trans_carprop_ls[[set_scalc]])
    # 水路客运
    # 柴油和燃料油均基于历史数据和比率
    # 推迟年份
    trans_nrgintst_ls[["水路客运"]] <- 
      func_interp_3(year = c(2019, 2040, 2060), 
                    scale = c(1, 1, 1), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路客运"]]$diesel), 
                    "diesel")
    trans_nrgintst_ls[["水路客运"]]$residual <- 
      func_interp_2(year = c(2019, 2040, 2060), 
                    value = c(
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual), 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.85, 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.80),
                    "residual")$residual
    
    # 水路货运
    # 柴油：基于历史数据和比率
    # 推迟年份
    trans_nrgintst_ls[["水路货运"]] <- 
      func_interp_3(year = c(2019, 2040, 2060), 
                    scale = c(1, 1, 1), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$diesel),
                    "diesel")
    # 燃料油：基于历史数据和比率
    trans_nrgintst_ls[["水路货运"]]$residual <- 
      func_interp_3(year = c(2019, 2040, 2060), 
                    scale = c(1, 1, 1), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路货运"]]$residual),
                    "residual")$residual
  }
  
  ## Energy and emission ----
  trans_nrgsum_ls[[set_scalc]] <- func_nrg_sum_ls(trans_nrgintst_ls, trans_act)
  trans_nrgsum_ls[[set_scalc]] <- func_ls2df(trans_nrgsum_ls[[set_scalc]])
  trans_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(trans_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Service ----
  ## Activity level ----
  ori_comemployee <- func_cross(
    prj_global_population[c("year", "population")], 
    func_interp_2(
      year = c(2019, 2030, 2060),
      value = c(by_com_act$com_employee[by_com_act$year == 2019]/
                  global_population$"常住人口"[global_population$year == 2019], 
                0.6, 0.75)))
  ori_comgdp <- prj_global_gdp[c("year", "comgdp")]
  com_act <- func_merge_2(list(ori_comemployee, ori_comgdp))
  names(com_act) <- c("year", "com_employee", "com_gdp")
  
  ## Energy intensity ----
  if (grepl("COMCONS", set_scalc)) { ### COMCONS ----
    # 服务业用电强度略有增加
    com_nrgintst_ls <- vector("list", 2)
    names(com_nrgintst_ls) <- global_com_subsector
    com_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2025, 2060), 
                    value = c(2958.967, 2958.967*1.2, 2958.967), 
                    "electricity")
    # 服务业燃气强度略有增加后减少，且逐渐为电气替代
    com_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.2, 0.8), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$lpg), 
                    "lpg")
    com_nrgintst_ls[[2]]$gas <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.2, 0.8), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$gas), 
                    "gas")$gas
    
    # 液化石油气的电气化
    com_ori_lpg_elec_prop <- 
      data.frame(year = com_nrgintst_ls[[2]]$year)
    com_ori_lpg_elec_prop$lpg <- 
      func_interp_2(year = c(2019, 2030, 2050, 2060), 
                    value = c(1, 0.4, 0, 0))$value
    com_ori_lpg_elec_prop$electricity <- 
      1 - com_ori_lpg_elec_prop$lpg
    com_nrgintst_ls[[2]]$electricity4lpg <- 
      func_alter(com_nrgintst_ls[[2]]$lpg, "lpg", "electricity")*0.9
    com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")] <- 
      func_cross(com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")], 
                 com_ori_lpg_elec_prop)
    # 天然气的电气化
    com_ori_gas_elec_prop <- 
      data.frame(year = com_nrgintst_ls[[2]]$year)
    com_ori_gas_elec_prop$gas <- 
      func_interp_2(year = c(2019, 2030, 2050, 2060), 
                    value = c(1, 0.4, 0, 0))$value
    com_ori_gas_elec_prop$electricity <- 
      1 - com_ori_gas_elec_prop$gas
    com_nrgintst_ls[[2]]$electricity4gas <- 
      func_alter(com_nrgintst_ls[[2]]$gas, "gas", "electricity")*0.9
    com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")] <- 
      func_cross(com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")], 
                 com_ori_gas_elec_prop)
    # 整理数据框
    com_nrgintst_ls[[2]][, "electricity"] <- 
      com_nrgintst_ls[[2]]$electricity4lpg +
      com_nrgintst_ls[[2]]$electricity4gas
    com_nrgintst_ls[[2]] <- 
      com_nrgintst_ls[[2]][c("year", "lpg", "gas", "electricity")]
  } else { ### BAU ----
    # 服务业用电强度略有增加
    # 效率较低，推迟年份
    com_nrgintst_ls <- vector("list", 2)
    names(com_nrgintst_ls) <- global_com_subsector
    com_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2030, 2060), 
                    value = c(2958.967, 2958.967*1.4, 2958.967), 
                    "electricity")
    # 服务业燃气强度略有增加后减少，且逐渐为电气替代
    com_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.5, 1.1), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$lpg), 
                    "lpg")
    com_nrgintst_ls[[2]]$gas <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.5, 1.1), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$gas), 
                    "gas")$gas
    
    # 液化石油气的电气化
    # 推迟年份
    com_ori_lpg_elec_prop <- 
      data.frame(year = com_nrgintst_ls[[2]]$year)
    com_ori_lpg_elec_prop$lpg <- 
      func_interp_2(year = c(2019, 2040, 2050, 2060), 
                    value = c(1, 0.4, 0, 0))$value
    com_ori_lpg_elec_prop$electricity <- 
      1 - com_ori_lpg_elec_prop$lpg
    com_nrgintst_ls[[2]]$electricity4lpg <- 
      func_alter(com_nrgintst_ls[[2]]$lpg, "lpg", "electricity")*0.9
    com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")] <- 
      func_cross(com_nrgintst_ls[[2]][c("year", "lpg", "electricity4lpg")], 
                 com_ori_lpg_elec_prop)
    # 天然气的电气化
    com_ori_gas_elec_prop <- 
      data.frame(year = com_nrgintst_ls[[2]]$year)
    com_ori_gas_elec_prop$gas <- 
      func_interp_2(year = c(2019, 2040, 2050, 2060), 
                    value = c(1, 0.4, 0, 0))$value
    com_ori_gas_elec_prop$electricity <- 
      1 - com_ori_gas_elec_prop$gas
    com_nrgintst_ls[[2]]$electricity4gas <- 
      func_alter(com_nrgintst_ls[[2]]$gas, "gas", "electricity")*0.9
    com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")] <- 
      func_cross(com_nrgintst_ls[[2]][c("year", "gas", "electricity4gas")], 
                 com_ori_gas_elec_prop)
    # 整理数据框
    com_nrgintst_ls[[2]][, "electricity"] <- 
      com_nrgintst_ls[[2]]$electricity4lpg +
      com_nrgintst_ls[[2]]$electricity4gas
    com_nrgintst_ls[[2]] <- 
      com_nrgintst_ls[[2]][c("year", "lpg", "gas", "electricity")]
  }
  
  
  ## Energy and emission ----
  com_nrgsum_ls[[set_scalc]] <- func_nrg_sum_ls(com_nrgintst_ls, com_act)
  com_nrgsum_ls[[set_scalc]] <- func_ls2df(com_nrgsum_ls[[set_scalc]])
  com_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(com_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Household ----
  ## Parameter ----
  hh_parm_ls[[set_scalc]] <- c(
    # LPG用户数下降到2019年的65%的年份
    sample(2025: 2035, 1)
  )
  
  ## Activity level ----
  # hh_elec
  hh_ori_household <- prj_global_population[c("year", "household")]
  # hh_lpg
  hh_ori_lpguser <- 
    func_cross(prj_global_population[c("year", "household")], 
               func_interp_3(year = c(2019, 2033, 2060), 
                             scale = c(1, 0.65, 0.30), 
                             base = func_lastone(
                               by_hh_ori_users_prop[c("year", "lpg")])))
  names(hh_ori_lpguser)[2] <- "lpg"
  # hh_gas
  hh_ori_gasuser <- 
    func_cross(prj_global_population[c("year", "household")], 
               func_interp_3(year = c(2019, 2030, 2060), 
                             scale = c(1, 2, 2), 
                             base = func_lastone(
                               by_hh_ori_users_prop[c("year", "gas")])))
  names(hh_ori_gasuser)[2] <- "gas"
  # 合并
  hh_act <- func_merge_2(list(hh_ori_household, 
                                     hh_ori_lpguser, 
                                     hh_ori_gasuser))
  
  ## Energy intensity ----
  if (grepl("HHCONS", set_scalc)) { ### HHCONS ----
    hh_nrgintst_ls <- vector("list", length(global_hh_subsector))
    names(hh_nrgintst_ls) <- global_hh_subsector
    # house_coal_elec
    # 生活用电强度
    hh_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2030, 2060), 
                    value = c(3900, 3900*1.2, 3900*1.4))
    names(hh_nrgintst_ls[[1]])[2] <- "electricity"
    # 生活用煤强度
    hh_nrgintst_ls[[1]]$rawcoal <- 0
    
    # hh_lpg
    hh_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 0.8, 0.5), 
                    base = func_lastone(by_hh_nrgintst_ls[["lpg"]]$lpg), 
                    "lpg")
    # hh_gas
    hh_nrgintst_ls[[3]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 0.8, 0.5), 
                    base = func_lastone(by_hh_nrgintst_ls[["gas"]]$gas), 
                    "gas")
  } else { ### BAU ----
    hh_nrgintst_ls <- vector("list", length(global_hh_subsector))
    names(hh_nrgintst_ls) <- global_hh_subsector
    # house_coal_elec
    # 生活用电强度
    hh_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2030, 2060), 
                    value = c(3900, 3900*1.3, 3900*1.4))
    names(hh_nrgintst_ls[[1]])[2] <- "electricity"
    # 生活用煤强度
    hh_nrgintst_ls[[1]]$rawcoal <- 0
    
    # hh_lpg
    hh_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 1.3, 0.7), 
                    base = func_lastone(by_hh_nrgintst_ls[["lpg"]]$lpg), 
                    "lpg")
    # hh_gas
    hh_nrgintst_ls[[3]] <- 
      func_interp_3(year = c(2019, 2030, 2060), 
                    scale = c(1, 1.3, 0.7), 
                    base = func_lastone(by_hh_nrgintst_ls[["gas"]]$gas), 
                    "gas")
  }
  
  ## Consumption and emission ----
  hh_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum_ls(hh_nrgintst_ls, hh_act)
  hh_nrgsum_ls[[set_scalc]] <- func_ls2df(hh_nrgsum_ls[[set_scalc]])
  hh_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(hh_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Power generation ----
  ## Parameter ----
  tf_parm_ls[[set_scalc]] <- c(
    # 本地发电两到什么时候减少为原来的一半
    sample(2025: 2045, 1)
  )
  
  ## Energy intensity ----
  # 逐渐下降
  tf_nrgintst <- data.frame(year = c(2019: 2060))
  for (i in names(by_tf_nrgintst)[names(by_tf_nrgintst) %in% "year" == FALSE]) {
    tf_nrgintst[, i] <- 
      func_interp_3(year = c(2019, 2025, 2060), 
                    scale = c(1, 0.95, 0.5), 
                    base = func_lastone(by_tf_nrgintst[, i]))$value
  }
  
  ## Activity level ----
  # 全社会用电量
  tfres_act <- func_ls2df(list(agri_nrgsum_ls[[set_scalc]], 
                               ind_nrgsum_ls[[set_scalc]], 
                               const_nrgsum_ls[[set_scalc]], 
                               trans_nrgsum_ls[[set_scalc]], 
                               com_nrgsum_ls[[set_scalc]], 
                               hh_nrgsum_ls[[set_scalc]]))
  tfres_act <- tfres_act[c("year", "electricity")]
  names(tfres_act) <- c("year", "elecuse")
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    # 本地发电量减少为原来的一半
    tfres_act <- 
      func_merge_2(list(
        tfres_act, 
        func_interp_2(
          year = c(2019, 2030, 2050, 2060),
          value = c(900371, 900371*0.5, 0, 0), "elecgen", showplot = FALSE)))
  } else { ### BAU ----
    # 本地发电量到2025年减少为原来的一半
    tfres_act <- 
      func_merge_2(list(
        tfres_act, 
        func_interp_2(year = c(2019, 2030, 2040, 2060),
                      value = c(900371, 900371, 900371*0.7, 900371*0.3), "elecgen")))
  }
  # 本地发电所用电量
  tfres_act$tfelecuse <- 
    func_nrg_sum(tf_nrgintst[c("year", "electricity")], 
                 tfres_act, "elecgen")$electricity
  # 外调电力
  tfres_act$importelec <- 
    tfres_act$elecuse - tfres_act$elecgen - tfres_act$tfelecuse
  
  ## Consumption and emission ----
  tf_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum(tf_nrgintst, tfres_act, "elecgen")
  tf_emissum_ls[[set_scalc]] <- func_emissum(tf_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  # Imported elec ----
  ## Emission ----
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    res_emisfac_df <- 
      func_interp_2(year = c(2019, 2035, 2040, 2050, 2060), 
                    value = c(0.39e-3, 0.218e-3, 0.186e-3, 0, 0), "electricity")
    
    res_emissum_ls[[set_scalc]] <- 
      func_cross(res_emisfac_df, tfres_act[c("year", "importelec")])
    names(res_emissum_ls[[set_scalc]])[2] <- "co2"
  } else if (grepl("BAU", set_scalc)) { ### BAU ----
    res_emisfac_df <- 
      func_interp_2(year = c(2019, 2025, 2030, 2035, 2050, 2060), 
                    value = c(0.39e-3, 0.35e-3, 0.30e-3, 0.25e-3, 0, 0), "electricity")
    
    res_emissum_ls[[set_scalc]] <- 
      func_cross(res_emisfac_df, tfres_act[c("year", "importelec")])
    names(res_emissum_ls[[set_scalc]])[2] <- "co2"
  }
  
  
  # RESULT ----
  # 各部门用电量
  tot_elecbysec <- 
    func_mrgcol(list(agri_nrgsum_ls[[set_scalc]], ind_nrgsum_ls[[set_scalc]], 
                     const_nrgsum_ls[[set_scalc]], trans_nrgsum_ls[[set_scalc]], 
                     com_nrgsum_ls[[set_scalc]], hh_nrgsum_ls[[set_scalc]]), 
                "electricity", namesnew = global_sectors[1:6])
  tot_elecsharebysec <- 
    func_nrg_intst(tot_elecbysec, tfres_act, "elecuse")
  # 电力总排放
  tot_elecemis <- 
    func_cross(tf_emissum_ls[[set_scalc]], res_emissum_ls[[set_scalc]], "sum")
  # 分配电力排放到各个终端部门
  tot_elecemisbysec <- 
    func_nrg_sum(tot_elecsharebysec, tot_elecemis, "co2")
  # 汇总各部门直接排放
  tot_diremisbysec <- 
    func_mrgcol(list(agri_emissum_dir_ls[[set_scalc]], ind_emissum_dir_ls[[set_scalc]], 
                     const_emissum_dir_ls[[set_scalc]], trans_emissum_dir_ls[[set_scalc]], 
                     com_emissum_dir_ls[[set_scalc]], hh_emissum_dir_ls[[set_scalc]]), 
                "co2", namesnew = global_sectors[1:6])
  # 终端部门总排放=电力排放+直接排放
  tot_emisbysec_ls[[set_scalc]] <- func_cross(tot_elecemisbysec, tot_diremisbysec, "sum")
  # 总排放
  tot_emissum_ls[[set_scalc]] <- data.frame(year = tot_emisbysec_ls[[set_scalc]]$year)
  tot_emissum_ls[[set_scalc]]$co2 <- 
    rowSums(tot_emisbysec_ls[[set_scalc]][names(tot_emisbysec_ls[[set_scalc]]) != "year"])
  
  
  # Output ----
  # 各部门达峰时间
  cat("\n", scalc, "\n")
  for (i in global_sectors[1:6]) {
    cat(i, "peak in", 
         func_peakyear(tot_emisbysec_ls[[set_scalc]], i), "\n")
  }
}

# 比较不同情景总排放和达峰时间差异
plot(tot_emissum_ls[["BAU"]]$year, tot_emissum_ls[["BAU"]]$co2, 
     type = "l", ylim = c(0, 3500), col = 1)
par(mfrow = c(2, 2))
for (i in c(1:32)) {
  
  plot(tot_emissum_ls[[i]]$year, tot_emissum_ls[[i]]$co2, 
       type = "l", ylim = c(0, 3500), color = i)
  #print(max(tot_emissum_ls[[i]]$co2))
  print(func_peakyear(tot_emissum_ls[[i]], "co2"))
}



plot(tot_emissum_ls[["OTHER_KEEPCOAL"]]$year, tot_emissum_ls[["OTHER_KEEPCOAL"]]$co2, 
     type = "l", ylim = c(0, 3500), col = "orange")
par(new = TRUE)
plot(tot_emissum_ls[["OTHER"]]$year, tot_emissum_ls[["OTHER"]]$co2, 
     type = "l", ylim = c(0, 3500), col = "blue")
for (i in scenarios) {
  cat(i, "peak in", func_peakyear(tot_emissum_ls[[i]], "co2"), "\n")
}

# Data export ----
# 导出各情景下总排放
tot_emis_export <- createWorkbook()
for (i in names(tot_emissum_ls)) {
  addWorksheet(tot_emis_export, i)
  writeData(tot_emis_export, i, tot_emissum_ls[[i]])
}
if (file.exists("各情景总排放量.xlsx")) {
  file.remove("各情景总排放量.xlsx")
}
saveWorkbook(tot_emis_export, "各情景总排放量.xlsx")

# 导出规划情景下各部门排放量
tot_emisbysec_export <- createWorkbook()
addWorksheet(tot_emisbysec_export, "sectoremis")
writeData(tot_emisbysec_export, "sectoremis", tot_emisbysec_ls[["OTHER_KEEPCOAL"]])
if (file.exists("规划情景各部门排放量.xlsx")) {
  file.remove("规划情景各部门排放量.xlsx")
}
saveWorkbook(tot_emisbysec_export, "规划情景各部门排放量.xlsx")

# 导出强化情景下的总排放
tot_emisbysec_export <- createWorkbook()
addWorksheet(tot_emisbysec_export, "sectoremis")
writeData(tot_emisbysec_export, "sectoremis", tot_emisbysec_ls[["OTHER"]])
if (file.exists("强化情景各部门排放量.xlsx")) {
  file.remove("强化情景各部门排放量.xlsx")
}
saveWorkbook(tot_emisbysec_export, "强化情景各部门排放量.xlsx")


  # ## Agri
  # # 活动水平，能耗强度，能耗总量
  # par(mfrow = c(3, 2))
  # func_history_project(by_agri_act, "agri",
  #                      agri_act, "area",
  #                      xlab = "", ylab = "播种面积", style = set_plotstyle)
  # func_history_project(by_agri_nrgintst, "diesel",
  #                      agri_nrgintst_df, "diesel",
  #                      xlab = "", ylab = "柴油强度", style = set_plotstyle)
  # func_history_project(by_agri_nrgintst, "electricity",
  #                      agri_nrgintst_df, "electricity",
  #                      xlab = "", ylab = "用电强度", style = set_plotstyle)
  # func_history_project(by_agri_nrgsum_df, "diesel",
  #                      agri_nrgsum_ls[[set_scalc]], "diesel",
  #                      xlab = "", ylab = "柴油总量", style = set_plotstyle)
  # func_history_project(by_agri_nrgsum_df, "electricity",
  #                      agri_nrgsum_ls[[set_scalc]], "electricity",
  #                      xlab = "", ylab = "用电总量", style = set_plotstyle)
  # 
  # ## Ind
  # # 各行业GDP变化
  # par(mfrow = c(4, 4))
  # for (i in global_ind_subsector) {
  #   func_history_project(by_ind_act, i, ind_act, i, 
  #                        xlab = "", ylab = i, style = set_plotstyle)
  # }
  # 
  # ## Const
  # # 活动水平，能耗强度，能耗总量
  # par(mfrow = c(2, 2))
  # func_history_project(by_const_act, "const_gdp", 
  #                      const_act, "const_gdp", 
  #                      xlab = "", ylab = "建筑业GDP", style = set_plotstyle)
  # func_history_project(by_const_nrgintst, "electricity", 
  #                      const_nrgintst_df, "electricity", 
  #                      xlab = "", ylab = "用电强度", style = set_plotstyle)
  # func_history_project(by_const_nrgsum_df, "electricity", 
  #                      const_nrgsum_ls[[set_scalc]], "electricity", 
  #                      xlab = "", ylab = "用电总量", style = set_plotstyle)
  # 
  # ### Trans
  # par(mfrow = c(3, 3))
  # for (i in global_trans_subsector) {
  #   func_history_project(by_trans_act, i, trans_act, i, 
  #                        xlab = "", ylab = i, style = set_plotstyle)
  # }
  # 
  # ## Service
  # par(mfrow = c(3, 3))
  # func_history_project(by_com_act, "com_employee", com_act, "com_employee", 
  #                      xlab = "", ylab = "服务业从业人数", style = set_plotstyle)
  # func_history_project(by_com_act, "com_gdp", com_act, "com_gdp", 
  #                      xlab = "", ylab = "服务业GDP", style = set_plotstyle)
  # func_history_project(by_com_nrgintst_ls[["electricity"]], "electricity", 
  #                      com_nrgintst_ls[["electricity"]], "electricity", 
  #                      xlab = "", ylab = "从业人均用电强度", style = set_plotstyle)
  # for (i in c("lpg", "gas", "electricity")) {
  #   func_history_project(by_com_nrgintst_ls[["lpg_and_gas"]], i,  
  #                        com_nrgintst_ls[["lpg_and_gas"]], i, 
  #                        xlab = "", ylab = paste0("单位GDP", i, "强度"), style = set_plotstyle)
  # }
  # for (i in c("lpg", "gas", "electricity")) {
  #   func_history_project(by_com_nrgsum_df, i,  
  #                        com_nrgsum_ls[[set_scalc]], i, 
  #                        xlab = "", ylab = paste0(i, "总量"), style = set_plotstyle)
  # }
  # 
  # ## Household
  # par(mfrow = c(3, 4))
  # # 活动水平
  # for (i in c("household", "lpg", "gas")) {
  #   func_history_project(by_hh_act, i, hh_act, i, 
  #                        xlab = "", ylab = paste0(i, "用户"), style = set_plotstyle)
  # }
  # # 能耗强度
  # for (i in c("electricity", "rawcoal")) {
  #   func_history_project(by_hh_nrgintst_ls[["household"]], i, 
  #                        hh_nrgintst_ls[["hh_coal_elec"]], i, 
  #                        xlab = "", ylab = paste0(i, "强度"), style = set_plotstyle)
  # }
  # func_history_project(by_hh_nrgintst_ls[["lpg"]], "lpg", 
  #                      hh_nrgintst_ls[["hh_lpg"]], "lpg", 
  #                      xlab = "", ylab = paste0("lpg", "强度"), style = set_plotstyle)
  # func_history_project(by_hh_nrgintst_ls[["gas"]], "gas", 
  #                      hh_nrgintst_ls[["hh_gas"]], "gas", 
  #                      xlab = "", ylab = paste0("gas", "强度"), style = set_plotstyle)
  # # 能耗总量
  # for (i in c("electricity", "rawcoal", "lpg", "gas")) {
  #   func_history_project(by_hh_nrgsum_df, i, 
  #                        hh_nrgsum_ls[[set_scalc]], i, 
  #                        xlab = "", ylab = paste0(i, "总量"), style = set_plotstyle)
  # }
  # 
  # # 各部门排放量和总排放量
  # par(mfrow = c(3, 3))
  # for (i in global_sectors[1:6]) {
  #   func_history_project(by_tot_emisbysec, i, tot_emisbysec_ls[[set_scalc]], i,
  #                        xlab = "", ylab = i, style = set_plotstyle)
  # }
  # func_history_project(by_tf_emissum_df, "co2", tf_emissum_ls[[set_scalc]], "co2", 
  #                      xlab = "", ylab = "本地发电排放", style = set_plotstyle)
  # func_history_project(by_res_emissum_df, "co2", res_emissum_ls[[set_scalc]], "co2", 
  #                      xlab = "", ylab = "外调电力排放", style = set_plotstyle)
  # func_history_project(by_tot_emis, "co2", tot_emissum_ls[[set_scalc]], "co2",
  #                      xlab = "", ylab = "全市总排放", style = set_plotstyle)
func_history_project(by_trans_nrgintst_ls)



