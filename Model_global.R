## 全局变量
## 常用名称变量
# 能源类别
global_nrg_class <- c("coal", "coalproduct", 
                      "gasoline", "diesel", "kerosene", "residual", "lpg", 
                      "gas", 
                      "electricity")
global_emis_class <- c("co2", "ch4", "n2o")


## GDP及其预测值
global_gdp <- func_read_trans("2VHEE264", "GDP")
proj_global_gdprate <- func_interp_2(
  year = c(2020, 2025, 2030, 2035, 2060),
  value = c(6.00, 6.00, 5.00, 4.00, 3.00))
proj_global_gdp <- func_rate(baseyear = 2019, basevalue = 59950422, 
                      rate_df = proj_global_gdprate)
names(proj_global_gdp)[2] <- "GDP"
comment(proj_global_gdp$GDP) <- "万元当年价"
# 测试
# func_history_project(global_gdp, "GDP", proj_global_gdp, "GDP")
# 工业GDP
# 工业GDP比重历史数据
global_indgdp_prop <- data.frame(year = global_gdp$year)
global_indgdp_prop$proportion <- global_gdp$"##工业" / global_gdp$GDP * 100
# 工业GDP比重预测数据
proj_global_indgdp_prop <- func_interp_2(year = c(2020, 2030, 2060), 
                                         value = c(30, 18, 14), "proportion")
comment(proj_global_indgdp_prop$proportion) <- "%"
# 测试：问题：工业GDP比重变化有点生硬
# func_history_project(global_indgdp_prop, "proportion", proj_global_indgdp_prop, "proportion")
# 未来工业GDP
proj_global_indgdp <- 
  data.frame(year = c(2020:2060), 
             value = proj_global_gdp$GDP * proj_global_indgdp_prop$proportion / 100)
names(proj_global_indgdp)[2] <- "GDP"
comment(proj_global_indgdp$GDP) <- "万元当年价"
# 测试：问题：工业GDP变化趋势不太对
# func_history_project(global_gdp, "##工业", proj_global_indgdp, "GDP")


## 人口和户数
global_population <- func_read_trans("2VHEE264")
# 假设全市综合家庭规模为城镇家庭规模和农村家庭规模的加权平均值
global_population$household_size <- 
  global_population$调查城镇家庭规模 * global_population$城镇化率/100 + 
  global_population$调查农村家庭规模 * (1 - global_population$城镇化率/100)
# 假设2016-2019的综合家庭户数和2015年相同
global_population$household_size[global_population$year > 2015] <- 
  global_population$household_size[global_population$year == 2015]
# 计算户数
global_population$household <- 
  global_population$"常住人口" / global_population$household_size
# 未来户数
proj_global_population <- func_interp_2(
  year = c(2019, 2025, 2030, 2035, 2060), 
  value = c(429, 550, 737, 730, 730), "population")
proj_global_population$household <- 
  proj_global_population$population / func_lastone(global_population$household_size)
comment(proj_global_population$household) <- "万户"

