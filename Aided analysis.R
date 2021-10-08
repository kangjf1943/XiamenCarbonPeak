library(nsga2R)

global_com_lookup <- 
  Reduce(rbind, list(
    data.frame(ind_agg = c("公共、商业和交通仓储"), 
               ind_ori = c("交通运输、仓储和邮政业", 
                           "商业、住宿和餐饮业", 
                           "公共服务及管理组织")), 
    data.frame(ind_agg = c("信息和金融"), 
               ind_ori = c("信息传输、软件和信息技术服务业", 
                           "金融、房地产、商务及居民服务业"))))

# 建立列表存储各情景结果
com_index <- vector("list", length(init_scenarios))
names(com_index) <- init_scenarios
pararateopt <- vector("list", length(init_scenarios))
names(pararateopt) <- init_scenarios

# 服务业各行业用电强度分析 ----
# 读取服务业各行业用电量
com_elecsubsec <- func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")
com_elecsubsec <- com_elecsubsec[!grepl("#", names(com_elecsubsec))]
com_elecsubsec <- com_elecsubsec[
  names(com_elecsubsec) %in% c("农、林、牧、渔业", "工业", "建筑业") == FALSE]

# 计算服务业各行业用电量比例
com_elecpropsubsec <- com_elecsubsec
com_elecpropsubsec$totalelec <- rowSums(
  com_elecsubsec[names(com_elecsubsec) != "year"])
com_elecpropsubsec <- 
  func_nrg_intst(com_elecsubsec, 
                 com_elecpropsubsec, "totalelec")

# 将排放分配到服务业各行业
com_emissubsec <- 
  func_nrg_sum(com_elecpropsubsec, by_tot_emissec, "com")

# 读取服务业各行业GDP
com_gdpsubsec <- func_read_trans("NUYB4HPZ", "XCP服务业各行业GDP")
com_gdpsubsec <- com_gdpsubsec[!grepl("#", names(com_gdpsubsec))]
# 补全服务业GDP的缺失值
com_gdpsubsec$`信息传输、软件和信息技术服务业`[which(
  com_gdpsubsec$year == 2019)] <- 
  com_gdpsubsec$`信息传输、软件和信息技术服务业`[which(
    com_gdpsubsec$year == 2018)]
com_gdpsubsec$`公共服务及管理组织`[which(com_gdpsubsec$year == 2019)] <- 
  com_gdpsubsec$公共服务及管理组织[which(com_gdpsubsec$year == 2018)]

# 计算服务业各行业GDP所占比重
com_gdppropsubsec <- com_gdpsubsec
com_gdppropsubsec$totalgdp <- rowSums(
  com_gdpsubsec[names(com_gdpsubsec) != "year"]
)
com_gdppropsubsec <- 
  func_nrg_intst(com_gdpsubsec, com_gdppropsubsec, "totalgdp")
  
# 计算服务业各行业增长速度
com_gdpratesubsec <- com_gdpsubsec
com_gdpratesubsec[names(com_gdpratesubsec) != "year"] <- 
  sapply(com_gdpratesubsec[names(com_gdpratesubsec) != "year"], 
         function(x){
           func_ratecalc(data.frame(year = (1978:2019), value = x), "value")$rate
         })

# 计算服务业各行业用电强度
com_elecintstsubsec <- func_cross(
  com_elecsubsec, com_gdpsubsec, method = "rate"
)

# 计算各行业单位GDP排放强度
com_emisintstsubsec <- 
  func_cross(com_emissubsec, com_gdpsubsec, method = "rate")

# 查看GDP变化趋势
ggplot(melt(com_gdpsubsec, id = "year"), aes(year, value)) + 
  geom_line(aes(color = variable))
# 查看GDP变化率的变化趋势
ggplot(melt(com_gdpratesubsec, id = "year"), aes(year, value)) + 
  geom_line(aes(color = variable))
# 查看电耗强度差异
ggplot(melt(com_elecintstsubsec, id = "year"), aes(variable, value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))
# 查看总用电量差异
ggplot(melt(com_elecsubsec, id = "year"), aes(variable, value)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))
# 查看排放强度变化及差异
ggplot(melt(com_emisintstsubsec, id = "year"), aes(year, value)) + 
  geom_line(aes(color = variable))

# 计算未来服务业用电强度
com_elecintstsubsec_future <- data.frame(year = 2019: 2060)
for (i in names(com_elecintstsubsec)[names(com_elecintstsubsec) != "year"]) {
  com_elecintstsubsec_future[, i] <- func_interp_3(
    year = c(2019, 2060), scale = c(1, 0.5), 
    base = com_elecintstsubsec[which(com_elecintstsubsec$year == 2018),i]
  )$value
}

# 调参数
# 封装服务业用电量计算函数
func_comelec <- function(pararate, showplot = FALSE, returngdp = FALSE) {
  # 可供调整的参数：各行业2060年和2019年GDP比值
  # 未来服务业各行业GDP呈曲线变化
  j <- 1
  com_gdpsubsec_future <- data.frame(year = 2019: 2060)
  for (i in names(com_gdpsubsec)[
    names(com_gdpsubsec) != "year"]) {
    com_gdpsubsec_future[, i] <- func_interp_3(
      year = c(2019, 2060), scale = c(1, pararate[j]), 
      base = com_gdpsubsec[which(com_gdpsubsec$year == 2019), i]
    )$value
    j <- j + 1
  }
  
  # 目标函数
  # 计算未来服务业各行业用电量
  com_elecsubsec_future <- 
    func_cross(com_elecintstsubsec_future, com_gdpsubsec_future, method = "product")
  
  # 计算未来服务业总用电量
  com_elecsum_future <- data.frame(
    year = 2019: 2060, 
    elec = rowSums(com_elecsubsec_future[, -1])
  )
  
  # 计算未来服务业总GDP
  com_gdpsum_future <- data.frame(
    year = 2019: 2060, 
    gdp = rowSums(com_gdpsubsec_future[, -1])
  )
  
  # 优化条件
  diff_comgdp <- sum(abs(com_gdpsum_future$gdp - prj_global_gdp$comgdp))
  diff_elec <- sum(abs(com_elecsum_future$elec - com_nrgsum_ls[[scenario]]$electricity))
  
  # 作图
  if (showplot == TRUE) {
    par(mfrow = c(1, 2))
    plot(com_gdpsum_future$gdp,
         ylim = c(0, max(c(com_gdpsum_future$gdp, prj_global_gdp$comgdp))))
    lines(prj_global_gdp$comgdp)
    
    plot(com_elecsum_future$elec,
         ylim = c(0, max(c(com_elecsum_future$elec, com_nrgsum_ls[[scenario]]$electricity))))
    lines(com_nrgsum_ls[[scenario]]$electricity)
    par(mfrow = c(1, 1))
  }
  
  if (returngdp == TRUE) {
    return(com_gdpsubsec_future)
  } else {
    return(c(diff_elec, 1))
  }
}

scenario <- "BAU_SLC_OTHER"
garesult <- 
  nsga2R(fn = func_comelec, 
         varNo = 5, objDim = 2, generations = 5, popSize = 8, 
         lowerBounds = c(5, 5, 5, 7, 7), 
         upperBounds = c(8, 8, 8, 11, 11))
# 查看优化结果
garesult$objectives
plot(garesult$objectives[, 1], garesult$objectives[, 2])
# 代入优化结果
pararateopt[[scenario]] <- as.numeric(garesult$parameters[1, ])
# 输出gdp
com_gdpsubsec_future <- func_comelec(pararateopt[[scenario]], showplot = TRUE, returngdp = TRUE)
# 计算各行业GDP占比
com_gdppropsubsec_future <- com_gdpsubsec_future
com_gdppropsubsec_future$totalgdp <- rowSums(
  com_gdpsubsec_future[names(com_gdpsubsec_future) != "year"]
)
com_gdppropsubsec_future <- 
  func_nrg_intst(com_gdpsubsec_future, com_gdppropsubsec_future, "totalgdp")
# 输出所需数据并存储
com_modelout <- 
  com_gdppropsubsec_future[which(
    com_gdppropsubsec_future$year %in% c(2019, 2025, 2030, 2035)), ]
com_index[[scenario]] <- func_secagg(com_modelout, global_com_lookup)

# 查看历史比例数据
com_gdppropsubsec_output <- func_secagg(com_gdppropsubsec, global_com_lookup)
com_gdppropsubsec_output <- com_gdppropsubsec_output[which(com_gdppropsubsec_output$year %in% c(2005, 2010, 2015)), ]


# 查看工业历史结构 ----
ind_gdppropsubsec_out <- func_secagg(ind_ori_act_prop[["BY"]], global_indsecclass_lookup)
ind_gdppropsubsec_out <- ind_gdppropsubsec_out[which(ind_gdppropsubsec_out$year %in% c(2005, 2010, 2015)), ]
write.xlsx(round(ind_gdppropsubsec_out, 2), "工业结构.xlsx")


# 工业各大类行业排放量 ----
# 减煤情景工业各行业直接排放量
ind_diremissec <- 
  lapply(ind_nrgsecfuel$BAU_SLC_DECOAL_OTHER, func_emissum, emisfac_df = prj_emisfac_df)
ind_diremissec <- func_mrgcol(ind_diremissec, "co2", global_ind_subsector)

# 减煤情景工业各行业电力排放量简单计算
# 各行业用电量及其占总用电量比例
ind_elecsec <- 
  func_mrgcol(ind_nrgsecfuel$BAU_SLC_DECOAL_OTHER, "electricity", global_ind_subsector)
ind_elecpropsec <- 
  func_nrg_intst(ind_elecsec, tfres_act$BAU_SLC_DECOAL_OTHER, "elecuse")
# 各行业用电排放量
ind_elecemissec <- func_nrg_sum(
  ind_elecpropsec, 
  func_cross(tf_diremissum$BAU_SLC_DECOAL_OTHER, 
             res_diremissum$BAU_SLC_DECOAL_OTHER, "sum"), "co2")

ind_elecemissec[-1] <- ind_elecemissec[-1]*0.0004969992

# 加总上述两部分排放
ind_emissec <- func_cross(ind_diremissec, ind_elecemissec, "sum")
ind_emisaggsec <- func_secagg(ind_emissec, global_indsecclass_lookup)
ggplot(melt(ind_emisaggsec, id = "year")) + geom_point(aes(year, value, color = variable))
for (i in names(ind_emisaggsec)[2:4]) {
  cat(i, "达峰于", func_peakyear(ind_emisaggsec, i), "\n")
}
write.xlsx(ind_emisaggsec, "工业各大类行业未来排放量.xlsx")
