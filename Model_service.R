## 服务业
## 现状分析
working_population <- func_read_trans("2VHEE264", "从业人口")
com_act <- data.frame(year = c(2005:2019))
com_act <- merge(com_act, working_population[c("year", "第三产业")], by = "year")
names(com_act)[2] <- "work_pop"
com_act
com_act <- merge(com_act, gdp[c("year", "#第三产业")], by = "year")
names(com_act)[3] <- "com_gdp"
com_act

com_nrgsum_ls <- vector("list")
com_nrgsum_ls[[1]] <- data.frame(year = c(2010: 2015), 
                                 electricity = func_read_trans("2I4DKY2A")$"##第三产业用电")
com_nrgsum_ls[[2]] <- func_read_trans("HV4JBQTQ")

com_nrgintst_ls <- vector("list")
for (i in c(1:2)) {
  com_nrgintst_ls[[i]] <- func_nrg_intst(com_nrgsum_ls[[i]], 
                                         com_act, 
                                         names(com_act)[[i]])
}
func_show_trend(com_nrgintst_ls[[1]])
func_show_trend(com_nrgintst_ls[[2]])

## 预测分析
# 未来活动水平
# 补全一下总人口预测的数据
proj_com_act <- data.frame(year = c(2005: 2050))
proj_com_act$work_pop <- proj_household$population * 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(45, 45, 48, 73)))$value / 100
proj_com_act$com_gdp <- proj_gdp$value * 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(60, 60, 65, 70)))$value / 100
proj_com_act

## 未来活动强度
proj_com_nrgintst_ls <- vector("list")
proj_com_nrgintst_ls[[1]] <- data.frame(year = c(2005: 2050))
proj_com_nrgintst_ls[[1]]$electricity <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0.2757e4, 0.2757e4, 0.2757*0.97e4, 0.2757*0.90e4)))$value
proj_com_nrgintst_ls[[2]] <- data.frame(year = c(2005: 2050))
proj_com_nrgintst_ls[[2]]$lpg <- func_interp(
  data.frame(year = c(2005, 2019, 2030, 2050),
             value = c(22.3*0.4/10000*0.45, 
                       22.3*0.4/10000*0.45, 
                       22.3*0.4/10000*0.4, 
                       22.3*0.4/10000*0.25)))$value
proj_com_nrgintst_ls[[2]]$natural_gas <- func_interp(
  data.frame(year = c(2005, 2019, 2030, 2050),
             value = c(1.4678/10000+2.23*0.35*1.1/10000, 
                       1.4678/10000+2.23*0.35*1.1/10000, 
                       1.4678/10000+2.23*0.35*1.1/10000*0.8, 
                       1.4678/10000+2.23*0.35*1.1/10000*0.6)))$value
proj_com_nrgintst_ls

# 则未来能耗总量
proj_com_nrgsum_ls <- vector("list")
for (i in c(1:2)) {
  proj_com_nrgsum_ls[[i]] <- func_nrg_sum(proj_com_nrgintst_ls[[i]], 
                                          proj_com_act, names(proj_com_act)[i])
}
func_show_trend(proj_com_nrgsum_ls[[1]])
func_show_trend(proj_com_nrgsum_ls[[2]])