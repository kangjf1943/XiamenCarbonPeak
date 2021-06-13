# Init ----
# 主要措施包括工业结构优化、电动车普及、服务业节能、生活节能
init_measures <- 
  c("INDSTR", "ELECCAR", "COMCONS", "HHCONS", "24COAL", "26COAL", "OTHER")
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
set_scalcs <- c("BAU", 
                "BAU_INDSTR", "BAU_ELECCAR", "BAU_COMCONS", "BAU_HHCONS",
                "BAU_INDSTR_ELECCAR_COMCONS_HHCONS_OTHER", 
                "BAU_26COAL", 
                "BAU_24COAL")
set_plotstyle <- "base"
set_calc_cache <- TRUE
set_elecfac_meth <- TRUE
set_resultout <- TRUE
set_dataexport <- FALSE
set_figureexport <- TRUE
set_parmexport <- FALSE

# Analysis ----
global_starttime <- Sys.time()
for (set_scalc in set_scalcs) {
  # Agriculture ----
  ## Activity level ----
  agri_act <- 
    func_interp_3(year = c(2019, 2025, 2035, 2050, 2060), 
                  scale = c(1, 0.68, 0.64, 0.63, 0.62), 
                  base = func_lastone(by_agri_act$agri), 
                  "area")
  
  ## Energy intensity ----
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    # 近期由于机械化水平提高，用电强度增加，但柴油强度则依历史趋势下降
    agri_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2050, 2060),
                    scale = c(1, 0.8, 0, 0), 
                    base = func_lastone(by_agri_nrgintst$diesel), 
                    "diesel")
    agri_nrgintst_df$electricity <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060),
                    scale = c(1, 1.1, 1.1, 0.9), 
                    base = func_lastone(by_agri_nrgintst$electricity), 
                    "electricity")$electricity
  } else { ### BAU ----
    # 效率较低
    agri_nrgintst_df <- 
      func_interp_3(year = c(2019, 2030, 2060),
                    scale = c(1, 0.9, 0.3), 
                    base = func_lastone(by_agri_nrgintst$diesel), 
                    "diesel")
    agri_nrgintst_df$electricity <- 
      func_interp_3(year = c(2019, 2025, 2045, 2060),
                    scale = c(1, 1.2, 1.2, 1), 
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
  if (set_calc_cache == FALSE) { ### Cache ----
    ind_ori_act_prop <- data.frame(year = c(2019:2060))
    ind_ori_act_prop[, "食品饮料及烟草制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"食品饮料及烟草制造业"), 3, 2))$value
    ind_ori_act_prop[, "纺织及服装制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"纺织及服装制造业"), 3, 2))$value
    ind_ori_act_prop[, "木材及家具制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"木材及家具制造业"), 1, 0.5))$value
    ind_ori_act_prop[, "造纸及印刷"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"造纸及印刷"), 1.5, 1))$value
    ind_ori_act_prop[, "文体工美用品制造业"] <- func_interp_2(
      year = c(2019, 2040, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"文体工美用品制造业"), 2, 4))$value
    ind_ori_act_prop[, "石油及炼焦"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"石油及炼焦"), 0.1, 0))$value
    ind_ori_act_prop[, "医药制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"医药制造业"), 2, 4))$value
    ind_ori_act_prop[, "非金属矿物制品业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"非金属矿物制品业"), 1.3, 0.5))$value
    ind_ori_act_prop[, "金属加工制造业"] <- func_interp_2(
      year = c(2019, 2040, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"金属加工制造业"), 7, 3))$value
    ind_ori_act_prop[, "电力、热力生产和供应业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"电力、热力生产和供应业"), 
                0.9, 0.1))$value
  }
  if (grepl("INDSTR", set_scalc)) { ### INDSTR ----
    ind_ori_act_prop[, "化学工业"] <- func_interp_2(
      year = c(2019, 2030, 2045, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"化学工业"), 5, 0.5, 0))$value
    ind_ori_act_prop[, "设备制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"设备制造业"), 15, 16))$value
    ind_ori_act_prop[, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"电子电气制造业"), 57, 64))$value
  } else { ### BAU ----
    # 时间推迟，比例不同
    ind_ori_act_prop[, "化学工业"] <- func_interp_2(
      year = c(2019, 2035, 2050, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"化学工业"), 7, 3, 0))$value
    ind_ori_act_prop[, "设备制造业"] <- func_interp_2(
      year = c(2019, 2040, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"设备制造业"), 13, 15))$value
    ind_ori_act_prop[, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2040, 2060), 
      value = c(func_lastone(by_ind_ori_act_prop$"电子电气制造业"), 50, 52))$value
  }
  ind_ori_act_prop[, "其他制造业"] <- func_saturate(
    ind_ori_act_prop[c("year", global_ind_subsector[global_ind_subsector != "其他制造业"])], "value")$value
  # 计算未来各子部门GDP
  ind_act <- 
    func_nrg_sum(ind_ori_act_prop[c("year", global_ind_subsector)], 
                 prj_global_gdp[c("year","indgdp")], "indgdp")
  ind_act[global_ind_subsector] <- ind_act[global_ind_subsector]/100
  
  ## Energy intensity ----
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    # 假设大部分能耗强度略有减少
    ind_nrgintst_ls <- vector("list", 13)
    names(ind_nrgintst_ls) <- global_ind_subsector
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2060))
      for (j in global_ind_nrgclass[1:6]) {
        ind_nrgintst_ls[[i]][, j] <- 
          func_interp_3(
            year = c(2019, 2025, 2030, 2035, 2060), 
            scale = c(1, 1, 1.0, 0.8, 0.6), 
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
            year = c(2019, 2025, 2030, 2040, 2060), 
            scale = c(1.0, 1.1, 1.2, 1, 1), 
            base = func_lastone(by_ind_nrgintst_ls[[i]][, j], zero.rm =  FALSE))$value
      }
    }
  }
  # 但是天然气在短期内有所上升
  if (grepl("OTHER", set_scalc)) {
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]][, "gas"] <- 
        func_interp_3(
          year = c(2019, 2025, 2030, 2060), 
          scale = c(1.0, 1.1, 1.2, 1), 
          base = func_lastone(by_ind_nrgintst_ls[[i]][, "gas"], 
                              zero.rm =  FALSE))$value
    }
  } else {
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]][, "gas"] <- 
        func_interp_3(
          year = c(2019, 2025, 2030, 2060), 
          scale = c(1.0, 1.3, 1.5, 1), 
          base = func_lastone(by_ind_nrgintst_ls[[i]][, "gas"], 
                              zero.rm =  FALSE))$value
    }
  }
  
  # 电力在短期内有所上升，但比天然气上升幅度小
  if (grepl("OTHER", set_scalc)) {
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]][, "electricity"] <- 
        func_interp_3(
          year = c(2019, 2025, 2030, 2060), 
          scale = c(1.0, 1.2, 1.2, 1.0), 
          base = func_lastone(by_ind_nrgintst_ls[[i]][, "electricity"], 
                              zero.rm =  FALSE))$value
    }
  } else {
    for (i in global_ind_subsector) {
      ind_nrgintst_ls[[i]][, "electricity"] <- 
        func_interp_3(
          year = c(2019, 2025, 2030, 2060), 
          scale = c(1.0, 1.2, 1.3, 1.0), 
          base = func_lastone(by_ind_nrgintst_ls[[i]][, "electricity"], 
                              zero.rm =  FALSE))$value
    }
  }
  
  ## Energy and emission ----
  ind_nrgsum_ls[[set_scalc]] <- func_nrg_sum_ls(ind_nrgintst_ls, ind_act)
  ind_nrgsum_ls[[set_scalc]] <- 
    func_ls2df(ind_nrgsum_ls[[set_scalc]])
  ind_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(ind_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Construction ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
    const_act <- prj_global_gdp[c("year", "constgdp")]
    names(const_act)[2] <- "const_gdp"
  }
  
  ## Energy intensity ----
  if (grepl("OTHER", set_scalc)) {### OTHER ----
    const_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2040, 2060), 
                    scale = c(1, 0.8, 0.3, 0.1), 
                    base = func_lastone(by_const_nrgintst$electricity), 
                    "electricity")
  } else { ### BAU ----
    # 效率较低
    const_nrgintst_df <- 
      func_interp_3(year = c(2019, 2025, 2050, 2060), 
                    scale = c(1, 1.1, 1, 0.8), 
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
  if (set_calc_cache == FALSE) { ### Cache ----
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
    
    # 公路其他汽油
    # 私家车按照初始增长率5%增长，至2035年饱和
    trans_act[, "公路其他汽油"] <- 
      func_curve_1(
        baseyear = 2019, basevalue = func_lastone(by_trans_act[, "公路其他汽油"]), 
        maxyear = 2035, endyear = 2060, init_rate = 0.05)$value
    
    # 公路其他柴油
    # 按照初始增长率5%增长，至2030年饱和
    trans_act[, "公路其他柴油"] <- 
      func_curve_1(
        baseyear = 2019, basevalue = func_lastone(by_trans_act[, "公路其他柴油"]), 
        maxyear = 2030, endyear = 2060, init_rate = 0.05)$value
    
    # 水运
    trans_act$"水路客运" <- 
      func_interp_2(year = c(2019, 2025, 2060), 
                    value = c(10000, 10500, 11000))$value
    comment(trans_act$"水路客运") <- "万人公里"
    trans_act$"水路货运" <- 
      func_interp_2(year = c(2019, 2025, 2060), 
                    value = c(21538635, 21538635*1.5, 21538635*2.0))$value
    comment(trans_act$"水路货运") <- "万吨公里"
  }
  
  ## Energy intensity ----
  trans_nrgintst_ls <- vector("list", length(global_trans_subsector))
  names(trans_nrgintst_ls) <- global_trans_subsector
  # 公路交通
  # 常规公交、快速公交、出租车、农村客车、公路其他柴油的能耗强度逐渐下降
  for (j in c(1: 4, 6)) {
    trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2060))
    for (i in names(by_trans_nrgintst_ls[[j]])[
      names(by_trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
      trans_nrgintst_ls[[j]][, i] <- 
        func_interp_3(year = c(2019, 2060), 
                      scale = c(1, 0.8), 
                      base = func_lastone(by_trans_nrgintst_ls[[j]][, i]))$value
    }
  }
  # 公路汽油：私家车的电气化
  trans_nrgintst_ls[["公路其他汽油"]] <- data.frame(year = c(2019: 2060))
  trans_nrgintst_ls[["公路其他汽油"]][, "gasoline"] <- func_interp_3(
    year = c(2019, 2030, 2040, 2060), 
    scale = c(1, 1, 0.8, 0.7), 
    base = func_lastone(by_trans_nrgintst_ls[["公路其他汽油"]]$gasoline))$value
  trans_nrgintst_ls[["公路其他汽油"]][, "electricity"] <- 0
  if (grepl("ELECCAR", set_scalc)) { ### ELECCAR ----
    # 轿车逐渐实现电气化
    trans_nrgintst_ls[["公路其他汽油"]] <- func_nrgsub(
      nrgori = trans_nrgintst_ls[["公路其他汽油"]], 
      namenrgoris = list("gasoline"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2025, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.05, 0.10, 0.80, 1)), 
      alterscales = list(0.8))
  } else { ### BAU ----
    # 其中轿车逐渐实现电气化较晚
    trans_nrgintst_ls[["公路其他汽油"]] <- func_nrgsub(
      nrgori = trans_nrgintst_ls[["公路其他汽油"]], 
      namenrgoris = list("gasoline"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2035, 2040, 2055, 2060)), 
      propsubs = list(c(0, 0.05, 0.10, 0.80, 0.80)), 
      alterscales = list(1))
  }
  # 水路客货运
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
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
    # 水路客运
    # 柴油和燃料油均基于历史数据和比率
    # 推迟年份
    trans_nrgintst_ls[["水路客运"]] <- 
      func_interp_3(year = c(2019, 2045, 2060), 
                    scale = c(1, 1, 1), 
                    base = func_lastone(by_trans_nrgintst_ls[["水路客运"]]$diesel), 
                    "diesel")
    trans_nrgintst_ls[["水路客运"]]$residual <- 
      func_interp_2(year = c(2019, 2045, 2060), 
                    value = c(
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual), 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual), 
                      func_lastone(by_trans_nrgintst_ls[["水路客运"]]$residual)*0.9),
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
  
  
  # Commerce ----
  ## Activity level ----
  if (set_calc_cache == FALSE) {
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
  }
  
  ## Energy intensity ----
  com_nrgintst_ls <- vector("list", 2)
  names(com_nrgintst_ls) <- global_com_subsector
  if (grepl("OTHER", set_scalc)) { ### OTHER ----
    # 服务业用电强度略有增加
    com_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2025, 2060), 
                    value = c(2958.967, 2958.967, 2958.967*0.7), 
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
  } else { ### BAU ----
    # 效率较低，推迟年份
    com_nrgintst_ls[[1]] <- 
      func_interp_2(year = c(2019, 2035, 2060), 
                    value = c(2958.967, 2958.967*1.2, 2958.967), 
                    "electricity")
    # 服务业燃气强度略有增加后减少，且逐渐为电气替代
    com_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2030, 2060),
                    scale = c(1, 1.5, 1.1), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$lpg), 
                    "lpg")
    com_nrgintst_ls[[2]]$gas <- 
      func_interp_3(year = c(2019, 2025, 2060),
                    scale = c(1, 1.5, 1.1), 
                    base = func_lastone(by_com_nrgintst_ls[[2]]$gas), 
                    "gas")$gas
  }
  if (grepl("COMCONS", set_scalc)) { ### COMCONS ----
    # 燃气的电气化较早
    com_nrgintst_ls[[2]] <- func_nrgsub(
      nrgori = com_nrgintst_ls[[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2030, 2050, 2060), 
                      c(2019, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.6, 1, 1), 
                      c(0, 0.6, 1, 1)), 
      alterscales = list(0.8, 0.8))
  } else { ### BAU ----
    # 燃气的电气化推迟
    com_nrgintst_ls[[2]] <- func_nrgsub(
      nrgori = com_nrgintst_ls[[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2040, 2055, 2060), 
                      c(2019, 2040, 2055, 2060)), 
      propsubs = list(c(0, 0.6, 1, 1), 
                      c(0, 0.6, 1, 1)), 
      alterscales = list(1, 1))
  }

  
  ## Energy and emission ----
  com_nrgsum_ls[[set_scalc]] <- func_nrg_sum_ls(com_nrgintst_ls, com_act)
  com_nrgsum_ls[[set_scalc]] <- func_ls2df(com_nrgsum_ls[[set_scalc]])
  com_emissum_dir_ls[[set_scalc]] <- 
    func_emissum(com_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # Household ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
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
  }
  
  ## Energy intensity ----
  hh_nrgintst_ls <- vector("list", length(global_hh_subsector))
  names(hh_nrgintst_ls) <- global_hh_subsector
  # 生活用电强度
  if (grepl("HHCONS", set_scalc)) { ### HHCONS ----
    hh_nrgintst_ls[[1]] <- func_interp_3(
      year = c(2019, 2035, 2060), 
      scale = c(1, 1.2, 1.4), 
      base = func_lastone(by_hh_nrgintst_ls[["household"]][, "electricity"]))
  } else { ### BAU ----
    hh_nrgintst_ls[[1]] <- func_interp_3(
      year = c(2019, 2035, 2060), 
      scale = c(1, 1.4, 1.6), 
      base = func_lastone(by_hh_nrgintst_ls[["household"]][, "electricity"]))
  }
  names(hh_nrgintst_ls[[1]])[2] <- "electricity"
  
  # 生活用煤强度
  hh_nrgintst_ls[[1]]$rawcoal <- 0
  
  # 生活液化石油气
  if (grepl("HHCONS", set_scalc)) { ### HHCONS ----
    hh_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2035, 2060), 
                    scale = c(1, 0.8, 0.5), 
                    base = func_lastone(by_hh_nrgintst_ls[["lpg"]]$lpg), 
                    "lpg")
  } else { ### BAU ----
    hh_nrgintst_ls[[2]] <- 
      func_interp_3(year = c(2019, 2040, 2060), 
                    scale = c(1, 1.2, 0.7), 
                    base = func_lastone(by_hh_nrgintst_ls[["lpg"]]$lpg), 
                    "lpg")
  }
  
  # 生活天然气
  if (grepl("HHCONS", set_scalc)) { ### HHCONS ----
    hh_nrgintst_ls[[3]] <- 
      func_interp_3(year = c(2019, 2035, 2060), 
                    scale = c(1, 0.8, 0.5), 
                    base = func_lastone(by_hh_nrgintst_ls[["gas"]]$gas), 
                    "gas")
  } else { ### BAU ----
    hh_nrgintst_ls[[3]] <- 
      func_interp_3(year = c(2019, 2040, 2060), 
                    scale = c(1, 1.2, 0.7), 
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
  if (grepl("24COAL", set_scalc)) { ### 24COAL ----
    # 2024年开始减煤，两年内减为原来的一半
    tfres_act <- 
      func_merge_2(list(
        tfres_act, 
        func_interp_2(
          year = c(2019, 2023, 2025, 2050, 2060),
          value = c(900371, 900371, 900371*0.5, 0, 0), "elecgen", 
          showplot = FALSE)))
  } else if (grepl("26COAL", set_scalc)) { ### 26COAL ----
    # 2026年开始减煤，五年内减为原来的一半
    tfres_act <- 
      func_merge_2(list(
        tfres_act, 
        func_interp_2(
          year = c(2019, 2025, 2030, 2050, 2060),
          value = c(900371, 900371, 900371*0.5, 0, 0), "elecgen", 
          showplot = FALSE)))
  } else { ### BAU ----
    # 2030年开始减煤，十年内减为原来的一半，之后保持
    tfres_act <- 
      func_merge_2(list(
        tfres_act, 
        func_interp_2(year = c(2019, 2030, 2040, 2060),
                      value = c(900371, 900371, 900371*0.5, 900371*0.5), "elecgen")))
  }
  # 本地发电所用电量
  tfres_act$tfelecuse <- 
    func_nrg_sum(tf_nrgintst[c("year", "electricity")], 
                 tfres_act, "elecgen")$electricity
  
  # 生成省电网发电结构
  tfres_provelecgenstr <- data.frame(
    year = c(2019: 2060), 
    thrm_prop = func_interp_2(
      year = c(2019, 2035, 2040, 2050, 2060),
      value = c(0.546504985, 0.291973506, 0.249449437, 0.161468824, 0))$value)
  tfres_provelecgenstr$clean_prop <- 1 - tfres_provelecgenstr$thrm_prop
  
  # 计算外调电力需求并将其分成煤电和清洁能源发电
  tfres_importelec <- data.frame(
    year = c(2019: 2060), 
    importelec = tfres_act$elecuse - tfres_act$elecgen - tfres_act$tfelecuse)
  tfres_importelec <- 
    func_nrg_sum(tfres_provelecgenstr, tfres_importelec, "importelec")
  names(tfres_importelec) <- c("year", "importthrm", "importclean")
  # 合并活动水平
  tfres_act <- func_merge_2(list(tfres_act, tfres_importelec))
  
  ## Consumption and emission ----
  tf_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum(tf_nrgintst, tfres_act, "elecgen")
  tf_emissum_ls[[set_scalc]] <- 
    func_emissum(tf_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  # Imported elec ----
  ## Energy intensity ----
  # 省电网煤电发电效率保持不变
  res_nrgintst <- data.frame(year = c(2019: 2060))
  for (i in names(by_res_nrgintst)[names(by_res_nrgintst) != "year"]) {
    res_nrgintst[, i] <- func_lastone(by_res_nrgintst[, i])
  }
  
  ## Consumption and emission ----
  res_nrgsum_ls[[set_scalc]] <- 
    func_nrg_sum(res_nrgintst, tfres_act, "importthrm")
  res_emissum_ls[[set_scalc]] <- 
    func_emissum(res_nrgsum_ls[[set_scalc]], prj_emisfac_df)
  
  
  # RESULT ----
  ## Total energy ----
  if (set_elecfac_meth == FALSE) {
    # 一次能源能耗之和
    tot_nrgsum_byfuel <- func_ls2df(list(
      agri_nrgsum_ls[[set_scalc]], ind_nrgsum_ls[[set_scalc]],
      const_nrgsum_ls[[set_scalc]], trans_nrgsum_ls[[set_scalc]],
      com_nrgsum_ls[[set_scalc]], hh_nrgsum_ls[[set_scalc]],
      tf_nrgsum_ls[[set_scalc]], res_nrgsum_ls[[set_scalc]]))
    # 换算成标准煤
    tot_nrgsum_byfuel_ce <- func_toce(tot_nrgsum_byfuel)
    # 换算成各年份总和
    tot_nrgsum_ls[[set_scalc]] <- data.frame(
      year = tot_nrgsum_byfuel_ce$year, 
      energyconsump = rowSums(tot_nrgsum_byfuel[names(tot_nrgsum_byfuel) != "year"]))
  } else {
    # 除了外调电力外其他部门一次能源之和
    tot_nrgsum_byfuel <- func_ls2df(list(
      agri_nrgsum_ls[[set_scalc]], ind_nrgsum_ls[[set_scalc]],
      const_nrgsum_ls[[set_scalc]], trans_nrgsum_ls[[set_scalc]],
      com_nrgsum_ls[[set_scalc]], hh_nrgsum_ls[[set_scalc]],
      tf_nrgsum_ls[[set_scalc]]))
    # 换算成标准煤
    tot_nrgsum_byfuel_ce <- func_toce(tot_nrgsum_byfuel)
    # 换算成各年份总和
    tot_nrgsum_ce <- data.frame(
      year = tot_nrgsum_byfuel_ce$year, 
      energyconsump = 
        rowSums(tot_nrgsum_byfuel_ce[names(tot_nrgsum_byfuel_ce) != "year"]))
    # 计算外调电力火电折标煤系数
    tot_ori_elecequalfac <- 
      func_elecequalfac(res_nrgsum_ls[[set_scalc]], tfres_act[c("year", "importthrm")])
    # 最后一年省电网火电发电为0，故电力折标煤系数也为0
    tot_ori_elecequalfac[which(
      tot_ori_elecequalfac$year == 2060), "nrg_input"] <- 0
    # 计算外调电力折标煤量
    tot_ori_elecequal <- func_cross(
      tot_ori_elecequalfac, 
      func_cross(tfres_act[c("year", "importthrm")], 
                 tfres_act[c("year", "importclean")], method = "sum"), 
      method = "product")
    # 本地一次能源和外调电力一次能源之和
    tot_nrgsum_ls[[set_scalc]] <- 
      func_cross(tot_nrgsum_ce, tot_ori_elecequal, method = "sum")
  }
  
  
  ## Total emission ----
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
    func_mrgcol(
      list(agri_emissum_dir_ls[[set_scalc]], ind_emissum_dir_ls[[set_scalc]], 
           const_emissum_dir_ls[[set_scalc]], trans_emissum_dir_ls[[set_scalc]], 
           com_emissum_dir_ls[[set_scalc]], hh_emissum_dir_ls[[set_scalc]]), 
      "co2", namesnew = global_sectors[1:6])
  # 终端部门总排放=电力排放+直接排放
  tot_emisbysec_ls[[set_scalc]] <- 
    func_cross(tot_elecemisbysec, tot_diremisbysec, "sum")
  # 总排放
  tot_emissum_ls[[set_scalc]] <- 
    data.frame(year = tot_emisbysec_ls[[set_scalc]]$year)
  tot_emissum_ls[[set_scalc]]$co2 <- 
    rowSums(
      tot_emisbysec_ls[[set_scalc]][names(tot_emisbysec_ls[[set_scalc]]) != "year"])
  
  # 输出各部门达峰时间
  cat("\n", set_scalc, "\n")
  for (i in global_sectors[1:6]) {
    cat(i, "peak in", 
         func_peakyear(tot_emisbysec_ls[[set_scalc]], i), "\n")
  }
  # 比较不同部门排放
  print(ggplot(melt(tot_emisbysec_ls[[set_scalc]], id = "year")) + 
          geom_line(aes(year, value, color = variable), size = 1.5))
}
# 查看运行时间
Sys.time() - global_starttime


# Result output ----
if (set_resultout == TRUE) {
  # 比较不同情景总排放和达峰时间差异
  print(func_scompplot(tot_emissum_ls, "co2"))
  for (i in set_scalcs) {
    cat(i, "\n", 
        "energy peak in", func_peakyear(tot_nrgsum_ls[[i]], "energyconsump"), "\n", 
        "emission peak in", func_peakyear(tot_emissum_ls[[i]], "co2"), "\n")
  }
  
  # 查看特定年份服务业能耗中电力占比
  com_nrgsum_ce_ls <- com_nrgsum_ls
  for (i in set_scalcs) {
    # 将LPG和天然气换算成标准量
    com_nrgsum_ce_ls[[i]][c("year", "lpg", "gas")] <- 
      func_toce(com_nrgsum_ce_ls[[i]][c("year", "lpg", "gas")])
    # 将电力换算成标准量
    # 问题：直接用最后一个情景的电力换算系数
    com_nrgsum_ce_ls[[i]][c("year", "electricity")] <- 
      func_cross(com_nrgsum_ce_ls[[i]][c("year", "electricity")], 
                 tot_ori_elecequalfac, method = "product")
    # 计算电力占比
    com_nrgsum_ce_ls[[i]][, "elecprop"] <- 
      com_nrgsum_ce_ls[[i]][, "electricity"]/
      (rowSums(com_nrgsum_ce_ls[[i]][names(com_nrgsum_ce_ls[[i]]) != "year"]))
  }
  result_var <- func_mrgcol(com_nrgsum_ce_ls[set_scalcs], "elecprop", set_scalcs)
  result_var[which(result_var$year %in% c(2019, 2025, 2030, 2035)), ]
  
  # 查看特定年份生活能耗中电力占比
  hh_nrgsum_ce_ls <- hh_nrgsum_ls
  for (i in set_scalcs) {
    # 将LPG和天然气换算成标准量
    hh_nrgsum_ce_ls[[i]][c("year", "lpg", "gas")] <- 
      func_toce(hh_nrgsum_ce_ls[[i]][c("year", "lpg", "gas")])
    # 将电力换算成标准量
    # 问题：直接用最后一个情景的电力换算系数
    hh_nrgsum_ce_ls[[i]][c("year", "electricity")] <- 
      func_cross(hh_nrgsum_ce_ls[[i]][c("year", "electricity")], 
                 tot_ori_elecequalfac, method = "product")
    # 计算电力占比
    hh_nrgsum_ce_ls[[i]][, "elecprop"] <- 
      hh_nrgsum_ce_ls[[i]][, "electricity"]/
      (rowSums(hh_nrgsum_ce_ls[[i]][names(hh_nrgsum_ce_ls[[i]]) != "year"]))
  }
  result_var <- func_mrgcol(hh_nrgsum_ce_ls[set_scalcs], "elecprop", set_scalcs)
  result_var[which(result_var$year %in% c(2019, 2025, 2030, 2035)), ]
}


# Data export ----
if (set_dataexport == TRUE) {
  # 导出各情景下能耗和排放达峰时间
  exportname <- paste0("各情景下能耗和排放达峰时间", Sys.Date(), ".xlsx")
  exportwb <- createWorkbook()
  addWorksheet(exportwb, "peaktime")
  exportvar <- data.frame(scenario = set_scalcs, nrg_peak = NA, emis_peak = NA)
  for (i in set_scalcs) {
    exportvar[which(exportvar$scenario == i), "nrg_peak"] <- 
              func_peakyear(tot_nrgsum_ls[[i]], "energyconsump")
    exportvar[which(exportvar$scenario == i), "emis_peak"] <- 
              func_peakyear(tot_emissum_ls[[i]], "co2")
  }
  writeData(exportwb, "peaktime", exportvar)
  if (file.exists(exportname)) {
    file.remove(exportname)
  }
  saveWorkbook(exportwb, exportname)
  
  # 导出各情景下总排放
  exportname <- paste0("各情景总排放量", Sys.Date(), ".xlsx")
  export <- createWorkbook()
  func_mrgcol(tot_emissum_ls[set_scalcs], "co2", set_scalcs)
  addWorksheet(export, "总排放")
  writeData(export, "总排放", 
            func_mrgcol(tot_emissum_ls[set_scalcs], "co2", set_scalcs))
  if (file.exists(exportname)) {
    file.remove(exportname)
  }
  saveWorkbook(exportwb, exportname)
  
  # 导出特定情景下各部门排放量
  exportname <- paste0("提前退煤情景各部门排放量", Sys.Date(), ".xlsx")
  export <- createWorkbook()
  addWorksheet(export, "sectoremis")
  writeData(export, "sectoremis", 
            tot_emisbysec_ls[["BAU_26COAL"]])
  if (file.exists(exportname)) {
    file.remove(exportname)
  }
  saveWorkbook(export, exportname)
  
  # 导出各情景下每隔五年GDP和能耗总量
  exportname <- paste0("各情景下每隔五年GDP和能耗总量", Sys.Date(), ".xlsx")
  exportwb <- createWorkbook()
  addWorksheet(exportwb, "nrg_per_gdp")
  # 选取可被5整除的年份
  exportvar <- 
    prj_global_gdp[which(prj_global_gdp$year %% 5 == 0), c("year", "GDP")]
  for (i in set_scalcs) {
    exportvar[, paste0(i, "nrg (万吨标煤)")] <- 
      tot_nrgsum_ls[[i]][which(tot_nrgsum_ls[[i]]$year %% 5 == 0), ]$energyconsump/10000
    exportvar[, paste0(i, " (吨标煤/万元GDP)")] <- 
      exportvar[, paste0(i, "nrg (万吨标煤)")]*10000 / exportvar[, "GDP"]
    exportvar[, paste0(i, " 变化率")] <- 
      func_ratecalc(exportvar[, c("year", paste0(i, " (吨标煤/万元GDP)"))], 
                    paste0(i, " (吨标煤/万元GDP)"))$rate
  }
  writeData(exportwb, "nrg_per_gdp", exportvar)
  if (file.exists(exportname)) {
    file.remove(exportname)
  }
  saveWorkbook(exportwb, exportname)
}


# Figure export ----
if (set_figureexport == TRUE) {
  # 导出历史产业结构图
  png(
    filename = paste0("历史产业结构图", Sys.Date(), ".png"), 
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1000,
    bg = "transparent" # 透明背景
  )
  export_plot <- ggplot(melt(
    global_gdp[c("year", "agrigdp_prop", "secgdp_prop", "comgdp_prop")], 
    id = "year")) + 
    geom_area(aes(year, value, fill = variable), stat = "identity") + 
    labs(x = "", y = "GDP占比") +
    scale_fill_manual(name = "", 
                      breaks = c("agrigdp_prop", "secgdp_prop", "comgdp_prop"), 
                      labels = c("第一产业", "第二产业", "第三产业"), 
                      values = c("#34bf49", "#ff4c4c", "#0099e5")) +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) + 
    scale_x_continuous(breaks = seq(1950, 2020, by = 10))
  func_excelplot(export_plot)
  dev.off()
  
  # 导出历史工业产业结构图
  png(
    filename = paste0("历史工业产业结构图", Sys.Date(), ".png"),
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1000,
    bg = "transparent" # 透明背景
  )
  export_plot <- ggplot(melt(by_ind_ori_act_prop, id = "year")) + 
    geom_area(aes(year, value, fill = variable), stat = "identity") + 
    labs(x = "", y = "GDP占比") +
    scale_fill_manual(
      name = "", 
      values = c(brewer.pal(name="Spectral", n = 11), 
                 brewer.pal(name="Set3", n = 3))) +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) + 
    scale_x_continuous(breaks = seq(2000, 2020, by = 5))
  func_excelplot(export_plot)
  dev.off()
  
  # 输出各情景能耗总量变化图
  png(
    filename = paste0("各情景能耗总量变化图", Sys.Date(), ".png"),
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1000,
    bg = "transparent" # 透明背景
  )
  export_var <- tot_nrgsum_ls
  for (i in set_scalcs) {
    export_var[[i]][, "energyconsump"] <- 
      export_var[[i]][, "energyconsump"]/10000
  }
  export_plot <- func_scompplot(export_var, "energyconsump") +  
    labs(x = "", y = "能耗总量（万吨标准煤）") + 
    scale_color_manual(
      name = "", 
      breaks = set_scalcs, 
      labels = c("惯性情景", paste0("单部门减排-", c(1:4)), 
                 "多部门减排", "退煤情景", "提前退煤情景"), 
      values = c("#800000", 
                 "#FF0000", "#FFA500", "#FFD700", "#FFFF00", 
                 "#50C878", 
                 "#007FFF", "#003399"))
  func_excelplot(export_plot)
  dev.off()
  
  # 输各情景排放总量图
  png(
    filename = paste0("各情景总排放总", Sys.Date(), ".png"),
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 900,
    bg = "transparent" # 透明背景
  )
  
  func_excelplot(func_scompplot(tot_emissum_ls, "co2"), "top") + 
    labs(x = "", y = "二氧化碳排放（万吨）") +
    scale_y_continuous(breaks = seq(0, 3500, by = 500)) + 
    scale_x_continuous(breaks = seq(2015, 2060, by = 5)) +
    scale_color_manual(
      name = "", 
      breaks = set_scalcs, 
      labels = c("惯性情景", paste0("单部门减排-", c(1:4)), 
                 "多部门减排", "退煤情景", "提前退煤情景"), 
      values = c("#800000", 
                 "#FF0000", "#FFA500", "#FFD700", "#FFFF00", 
                 "#50C878", 
                 "#007FFF", "#003399"))
  dev.off()
}

# Parameters export ----
if (set_parmexport == TRUE) {
  ## Agri
  # 活动水平，能耗强度，能耗总量
  par(mfrow = c(3, 2))
  func_history_project(by_agri_act, "agri",
                       agri_act, "area",
                       xlab = "", ylab = "播种面积", style = set_plotstyle)
  func_history_project(by_agri_nrgintst, "diesel",
                       agri_nrgintst_df, "diesel",
                       xlab = "", ylab = "柴油强度", style = set_plotstyle)
  func_history_project(by_agri_nrgintst, "electricity",
                       agri_nrgintst_df, "electricity",
                       xlab = "", ylab = "用电强度", style = set_plotstyle)
  func_history_project(by_agri_nrgsum_df, "diesel",
                       agri_nrgsum_ls[[set_scalc]], "diesel",
                       xlab = "", ylab = "柴油总量", style = set_plotstyle)
  func_history_project(by_agri_nrgsum_df, "electricity",
                       agri_nrgsum_ls[[set_scalc]], "electricity",
                       xlab = "", ylab = "用电总量", style = set_plotstyle)
  
  ## Ind
  # 各行业GDP变化
  par(mfrow = c(4, 4))
  for (i in global_ind_subsector) {
    func_history_project(by_ind_act, i, ind_act, i,
                         xlab = "", ylab = i, style = set_plotstyle)
  }
  
  ## Const
  # 活动水平，能耗强度，能耗总量
  par(mfrow = c(2, 2))
  func_history_project(by_const_act, "const_gdp",
                       const_act, "const_gdp",
                       xlab = "", ylab = "建筑业GDP", style = set_plotstyle)
  func_history_project(by_const_nrgintst, "electricity",
                       const_nrgintst_df, "electricity",
                       xlab = "", ylab = "用电强度", style = set_plotstyle)
  func_history_project(by_const_nrgsum_df, "electricity",
                       const_nrgsum_ls[[set_scalc]], "electricity",
                       xlab = "", ylab = "用电总量", style = set_plotstyle)
  
  ### Trans
  par(mfrow = c(3, 3))
  for (i in global_trans_subsector) {
    func_history_project(by_trans_act, i, trans_act, i,
                         xlab = "", ylab = i, style = set_plotstyle)
  }
  
  ## Service
  par(mfrow = c(3, 3))
  func_history_project(by_com_act, "com_employee", com_act, "com_employee",
                       xlab = "", ylab = "服务业从业人数", style = set_plotstyle)
  func_history_project(by_com_act, "com_gdp", com_act, "com_gdp",
                       xlab = "", ylab = "服务业GDP", style = set_plotstyle)
  func_history_project(by_com_nrgintst_ls[["electricity"]], "electricity",
                       com_nrgintst_ls[["electricity"]], "electricity",
                       xlab = "", ylab = "从业人均用电强度", style = set_plotstyle)
  for (i in c("lpg", "gas", "electricity")) {
    func_history_project(by_com_nrgintst_ls[["lpg_and_gas"]], i,
                         com_nrgintst_ls[["lpg_and_gas"]], i,
                         xlab = "", ylab = paste0("单位GDP", i, "强度"), style = set_plotstyle)
  }
  for (i in c("lpg", "gas", "electricity")) {
    func_history_project(by_com_nrgsum_df, i,
                         com_nrgsum_ls[[set_scalc]], i,
                         xlab = "", ylab = paste0(i, "总量"), style = set_plotstyle)
  }
  
  ## Household
  par(mfrow = c(3, 4))
  # 活动水平
  for (i in c("household", "lpg", "gas")) {
    func_history_project(by_hh_act, i, hh_act, i,
                         xlab = "", ylab = paste0(i, "用户"), style = set_plotstyle)
  }
  # 能耗强度
  for (i in c("electricity", "rawcoal")) {
    func_history_project(by_hh_nrgintst_ls[["household"]], i,
                         hh_nrgintst_ls[["hh_coal_elec"]], i,
                         xlab = "", ylab = paste0(i, "强度"), style = set_plotstyle)
  }
  func_history_project(by_hh_nrgintst_ls[["lpg"]], "lpg",
                       hh_nrgintst_ls[["hh_lpg"]], "lpg",
                       xlab = "", ylab = paste0("lpg", "强度"), style = set_plotstyle)
  func_history_project(by_hh_nrgintst_ls[["gas"]], "gas",
                       hh_nrgintst_ls[["hh_gas"]], "gas",
                       xlab = "", ylab = paste0("gas", "强度"), style = set_plotstyle)
  # 能耗总量
  for (i in c("electricity", "rawcoal", "lpg", "gas")) {
    func_history_project(by_hh_nrgsum_df, i,
                         hh_nrgsum_ls[[set_scalc]], i,
                         xlab = "", ylab = paste0(i, "总量"), style = set_plotstyle)
  }
  
  # 各部门排放量和总排放量
  par(mfrow = c(3, 3))
  for (i in global_sectors[1:6]) {
    func_history_project(by_tot_emisbysec, i, tot_emisbysec_ls[[set_scalc]], i,
                         xlab = "", ylab = i, style = set_plotstyle)
  }
  func_history_project(by_tf_emissum_df, "co2", tf_emissum_ls[[set_scalc]], "co2",
                       xlab = "", ylab = "本地发电排放", style = set_plotstyle)
  func_history_project(by_res_emissum_df, "co2", res_emissum_ls[[set_scalc]], "co2",
                       xlab = "", ylab = "外调电力排放", style = set_plotstyle)
  func_history_project(by_tot_emis, "co2", tot_emissum_ls[[set_scalc]], "co2",
                       xlab = "", ylab = "全市总排放", style = set_plotstyle)
}


