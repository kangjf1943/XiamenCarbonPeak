## 全局变量
## GDP
gdp <- func_read_trans("2VHEE264", "GDP")

# GDP预测
proj_gdp_rate <- func_interp_2(
  year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050),
  value = c(8, 7.90, 6.00, 6.00, 5.00, 4.00, 3.00))
proj_gdp <- func_rate(baseyear = 2019, basevalue = 59950422, 
                      rate_df = proj_gdp_rate)
names(proj_gdp)[2] <- "GDP"
func_history_project(gdp, "GDP", proj_gdp, "GDP")
comment(proj_gdp$value) <- "万元当年价"

# 工业GDP（需求：暂时算的是第二产业）
proj_gdp_ind_prop <- func_interp_2(
  year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050), 
  value = c(41, 41.60, 41, 30, 26, 23, 19))
comment(proj_gdp_ind_prop$value) <- "%"
proj_gdp_ind <- data.frame(year = c(2005:2050), 
                           value = proj_gdp$value * 
                             proj_gdp_ind_prop$value / 100)
comment(proj_gdp_ind$value) <- "万元当年价"

# 人口和户数
population <- func_read_trans("2VHEE264")
population$year <- as.numeric(population$year)
# 假设2016-2017的城镇家庭户数和2015年一致
population$调查城镇家庭规模[population$year > 2015] <- 
  population$调查城镇家庭规模[population$year == 2015]
population$household <- population$"常住人口" / population$"调查城镇家庭规模"
# 计算户数
proj_population <- func_interp_2(
  year = c(2019, 2025, 2030, 2035, 2050), 
  value = c(population$"常住人口"[population$year == 2019], 
            550, 737, 730, 800))
proj_household <- proj_population
names(proj_household)[2] <- "household"
proj_household$household <- proj_household$household/2.5
comment(proj_household$household) <- "万户"