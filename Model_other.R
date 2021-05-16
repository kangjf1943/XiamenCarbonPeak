## 其他部门：家庭，建筑业和农业
names_other_act <- c("household", "lpg_user", "gas_user", 
                     "construct_gdp", "agriculture_area")
names_other_ls <- c("household_electricity", "household_lpg", "household_gas", 
                    "construct_electricity", "agriculture_electricity")

# 历史数据
# 构建其他部门活动水平数据框
# 家庭户数
ori_other_act_house <- data.frame(year = population$year, 
                                  household = population$常住人口 / 
                                    population$调查城镇家庭规模)
plot(ori_other_act_house$year, ori_other_act_house$household)
comment(ori_other_act_house$household) <- "万户"

# 用液化石油气的户数
ori_other_act_house_lpg <- func_read_trans("S32RZEF7", "瓶装液化气总用户数")
ori_other_act_house_lpg <- ori_other_act_house_lpg[, c("year", "民用")]
ori_other_act_house_lpg$民用 <- ori_other_act_house_lpg$民用 / 10000
ori_other_act_house_lpg <- merge(population[, c("year", "household")],
                                 ori_other_act_house_lpg, by = "year")
names(ori_other_act_house_lpg)[2] <- "total_household"
names(ori_other_act_house_lpg)[3] <- "lpg_user"
plot(ori_other_act_house_lpg$lpg_user / ori_other_act_house_lpg$total_household)

# 用管道天然气的用户数
ori_other_act_house_gas <- func_read_trans("S32RZEF7", "管道天然气总用户数")
ori_other_act_house_gas <- ori_other_act_house_gas[, c("year", "民用")]
ori_other_act_house_gas$民用 <- ori_other_act_house_gas$民用 / 10000
ori_other_act_house_gas <- merge(population[, c("year", "household")],
                                 ori_other_act_house_gas, by = "year")
names(ori_other_act_house_gas)[2] <- "total_household"
names(ori_other_act_house_gas)[3] <- "gas_user"
plot(ori_other_act_house_gas$gas_user / ori_other_act_house_gas$total_household)
# 比例上：液化石油气下降，管道天然气可能是上升

# 建筑业的GDP
ori_other_act_construct_gdp <- gdp[, c("year", "##建筑业")]

# 农业的播种面积
ori_other_agriculture_area <- func_read_trans("4NJ97NS9")
ori_other_agriculture_area <- 
  ori_other_agriculture_area[, c("year", "全年农作物总播种面积")]
ori_other_agriculture_area[, "全年农作物总播种面积"] <- 
  ori_other_agriculture_area[, "全年农作物总播种面积"]/1500
ori_other_agriculture_area
comment(ori_other_agriculture_area$全年农作物总播种面积) <- "平方公里"

# 合并成一个活动水平数据框
other_act <- Reduce(func_merge, list(ori_other_act_house, 
                                     ori_other_act_house_lpg[, c("year", "lpg_user")], 
                                     ori_other_act_house_gas[, c("year", "gas_user")],
                                     ori_other_act_construct_gdp, 
                                     ori_other_agriculture_area))
comment(other_act$household) <- attributes(ori_other_act_house$household)[[1]]
comment(other_act$lpg_user) <- attributes(ori_other_act_house_lpg$lpg_user)[[1]]
comment(other_act$gas_user) <- attributes(ori_other_act_house_gas$gas_user)[[1]]
comment(other_act$"##建筑业") <- attributes(ori_other_act_construct_gdp$"##建筑业")[[1]]
comment(other_act$全年农作物总播种面积) <- attributes(ori_other_agriculture_area$全年农作物总播种面积)[[1]]
func_looknote(other_act)
func_show_trend(other_act)

# 构建其他部门的能耗总量列表
other_nrgsum_ls <- vector("list", 5)
# 家庭用电部分
names(other_nrgsum_ls)[1] <- "家庭用电"
other_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")
other_nrgsum_ls[[1]] <- other_nrgsum_ls[[1]][, c("year", "#城乡居民生活用电")]
names(other_nrgsum_ls[[1]]) <- c("year", "household_electricity")
# 家庭液化石油气部分
names(other_nrgsum_ls)[2] <- "家庭液化石油气"
other_nrgsum_ls[[2]] <- func_read_trans("HHKVE85Q", "瓶装液化气")
other_nrgsum_ls[[2]] <- other_nrgsum_ls[[2]][, c("year", "家庭")]
names(other_nrgsum_ls[[2]]) <- c("year", "household_lpg")
# 家庭天然气部分
names(other_nrgsum_ls)[3] <- "家庭天然气"
other_nrgsum_ls[[3]] <- func_read_trans("HHKVE85Q", "管道天然气")
other_nrgsum_ls[[3]] <- other_nrgsum_ls[[3]][, c("year", "家庭")]
names(other_nrgsum_ls[[3]]) <- c("year", "household_gas")
# 建筑用电部分
names(other_nrgsum_ls)[4] <- "建筑用电"
other_nrgsum_ls[[4]] <- func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")
other_nrgsum_ls[[4]] <- other_nrgsum_ls[[4]][, c("year", "建筑业")]
names(other_nrgsum_ls[[4]]) <- c("year", "construct_electricity")
# 农业用电部分
names(other_nrgsum_ls)[5] <- "农业用电"
other_nrgsum_ls[[5]] <- ori_global_electricity[, c("year", "##第一产业")]
names(other_nrgsum_ls[[5]]) <- c("year", "agriculture_electricity")

# 检查各部分的单位
func_looknote_ls(other_nrgsum_ls)
func_show_trend_ls(other_nrgsum_ls)

# 计算各行业用能强度
other_nrgintst_ls <- func_nrg_intst_ls(other_nrgsum_ls, other_act)
func_show_trend_ls(other_nrgintst_ls)

# 和广州家庭用电数据比较
electricity_living_guangzhou <- func_read_trans("QSPLFZXP", 3)
population_guangzhou <- func_read_trans("QSPLFZXP")
func_merge_rate(electricity_living_guangzhou, "#生活用电", 
                population_guangzhou, "总户数")
# 单从数据而言，厦门市还是有很大的增长空间的

# 未来活动水平
# 家庭用电
proj_other_act <- proj_household
func_history_project(population, "household", proj_other_act, "household")
comment(proj_other_act$household) <- "万户"
# 家庭液化石油气部分
proj_other_act$lpg_user <- 
  func_nrg_sum(proj_other_act[, c("year", "household")], 
               func_interp_2(year = c(2019, 2030, 2050), value = c(0.30, 0.15, 0.08)), 
               "value")$household
func_history_project(other_act, "lpg_user", proj_other_act, "lpg_user")
comment(proj_other_act$lpg_user) <- "万户"
# 家庭天然气部分
proj_other_act$gas_user <- 
  func_nrg_sum(proj_other_act[, c("year", "household")], 
               func_interp_2(year = c(2019, 2030, 2050), value = c(0.35, 0.70, 0.96)), 
               "value")$household
func_history_project(other_act, "gas_user", proj_other_act, "gas_user")
comment(proj_other_act$gas_user) <- "万户"
# 建筑物用电部分
proj_other_act$construction_gdp <- 
  func_nrg_sum(proj_gdp, 
               func_interp_2(year = c(2019,2030,2050), 
                             value = c(0.10, 
                                       0.08,
                                       0.03)), 
               "value")$GDP
func_history_project(gdp[gdp$year > 2000, ], "##建筑业", proj_other_act, "construction_gdp")
comment(proj_other_act$construction_gdp) <- "万元"
# 农业用电部分
proj_other_act$agriculture_area <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(221, 
                          221 * 0.6, 
                          221 * 0.5))$value
comment(proj_other_act$agriculture_area) <- "平方千米"

func_show_trend(proj_other_act)

# 活动强度
proj_other_nrgintst_ls <- vector("list")
# 家庭用电部分
proj_other_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(3900, 3900*1.2, 3900*1.4))
names(proj_other_nrgintst_ls[[1]])[2] <- "家庭用电强度"
func_history_project(other_nrgintst_ls[[1]], "household_electricity", 
                     proj_other_nrgintst_ls[[1]], "家庭用电强度")

# 家庭液化石油气部分
proj_other_nrgintst_ls[[2]] <- 
  data.frame(year = c(2019: 2050), 
             lpg = 588.52)
names(proj_other_nrgintst_ls[[2]])[2] <- "每户液化石油气"
func_history_project(other_nrgintst_ls[[2]], "household_lpg", 
                     proj_other_nrgintst_ls[[2]], "每户液化石油气")

# 家庭天然气部分
proj_other_nrgintst_ls[[3]] <- 
  data.frame(year = c(2019: 2050), 
             natural_gas = 74.50)
names(proj_other_nrgintst_ls[[3]])[2] <- "每户天然气"
func_history_project(other_nrgintst_ls[[3]], "household_gas", 
                     proj_other_nrgintst_ls[[3]], "每户天然气")

# 建筑业用电部分
proj_other_nrgintst_ls[[4]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(0.0081, 
                          0.0081 * 1.2, 
                          0.0081))
names(proj_other_nrgintst_ls[[4]])[2] <- "单位建筑GDP用电"
func_history_project(other_nrgintst_ls[[4]], "construct_electricity", 
                     proj_other_nrgintst_ls[[4]], "单位建筑GDP用电")

# 农业用电
proj_other_nrgintst_ls[[5]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(59, 
                          59 * 0.8,
                          59 * 0.7))
names(proj_other_nrgintst_ls[[5]])[2] <- "农业单位面积用电量"
func_history_project(other_nrgintst_ls[[5]], "agriculture_electricity", 
                     proj_other_nrgintst_ls[[5]], "农业单位面积用电量")

# 则能耗总量为
proj_other_nrgsum_ls <- func_nrg_sum_ls(proj_other_nrgintst_ls, proj_other_act)

func_history_project_ls(other_nrgintst_ls, proj_other_nrgintst_ls)
func_history_project_df(other_act, proj_other_act)
func_history_project_ls(other_nrgsum_ls, proj_other_nrgsum_ls)