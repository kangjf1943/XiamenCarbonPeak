## 服务业
## 历史数据
# 活动水平：服务业从业人口和GDP
com_act <- func_read_trans("2VHEE264", "从业人口")
com_act <- com_act[, c("year", "第三产业")]
names(com_act)[2] <- "com_employee"
com_act$com_employee <- com_act$com_employee/10000
comment(com_act$com_employee) <- "万人"
com_act <- merge(com_act, gdp[c("year", "#第三产业")], by = "year")
names(com_act)[3] <- "com_gdp"
com_act
# 活动总量：电力，液化石油气，天然气
com_nrgsum_ls <- vector("list")
# 读取厦门市用电数据
com_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")[, c("year", "##第三产业")]
names(com_nrgsum_ls[[1]])[2] <- "electricity"
# 读取厦门市服务业用能
com_nrgsum_ls[[2]] <- func_read_trans("HV4JBQTQ")
names(com_nrgsum_ls) <- c("electricity", "lpg_and_gas")
# 活动强度
com_nrgintst_ls <- func_nrg_intst_ls(com_nrgsum_ls, com_act)
func_show_trend(com_act)

## 预测分析
# 活动水平
proj_com_act <- data.frame(year = c(2019: 2050))
proj_com_act$com_employee <- proj_population$value * 
  func_interp_2(year = c(2019, 2030, 2050), 
                         value = c(0.45, 0.48, 0.73))$value
proj_com_act$com_gdp <- proj_gdp$GDP * 
  func_interp_2(year = c(2019, 2030, 2050), 
                         value = c(0.60, 0.65, 0.70))$value
func_history_project_df(com_act, proj_com_act)

# 能耗强度
proj_com_nrgintst_ls <- vector("list")
proj_com_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(2958.967, 2958.967, 2958.967), 
                "electricity")
proj_com_nrgintst_ls[[2]] <- 
  func_interp_2(year = c(2019, 2030, 2050),
             value = c(0.002055001,
                       0.002055001*0.8, 
                       0.002055001*0.7), "lpg")
proj_com_nrgintst_ls[[2]]$gas <- 
  func_interp_2(year = c(2019, 2030, 2050),
             value = c(1.353608e-08, 
                       1.353608e-08*0.8, 
                       1.353608e-08*0.6), "gas")$gas
proj_com_nrgintst_ls
func_history_project_ls(com_nrgintst_ls, proj_com_nrgintst_ls)

# 能耗总量
proj_com_nrgsum_ls <- func_nrg_sum_ls(proj_com_nrgintst_ls, proj_com_act)
func_history_project_ls(com_nrgsum_ls, proj_com_nrgsum_ls)

