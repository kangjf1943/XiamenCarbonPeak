# GLOBAL VAR ----
## Vector names ----
# 能源类别
global_nrg_class <- c("coal", "coalproduct", 
                      "gasoline", "diesel", "kerosene", "residual", "lpg", 
                      "gas", 
                      "electricity")
global_emis_class <- c("co2", "ch4", "n2o")


## GDP ----
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

## 人口和户数 ----
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

## Factors ----
## 各部门排放和总二氧化碳排放
# 构建排放因子列表
emisfac_df <- func_read_data("8C8EDJVH")
emisfac_df$year <- c("co2", "ch4", "n2o")

# DEMAND ----
# Transportation ----
trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                     "摩托车", "轿车", 
                     "轻型客车", "大型客车", 
                     "轻型货车", "中型货车", "重型货车", 
                     "农用运输车", 
                     "航空", "水路客运", "水路货运")

## Activity level ----
# 营运车辆里程数：需求：没有2018-2019年的营运车辆里程数数据
trans_act_operation <- func_read_trans("IZM9FWIY", "里程数")
trans_act_operation <- 
  trans_act_operation[, c("year", "常规公交", "BRT", "出租车", "农村客车")]
names(trans_act_operation) <- c("year", trans_subsector[1: 4])
# 非营运车辆数量
trans_act_nonoperation <- func_read_trans("Y3PGVSR7")
trans_act_nonoperation <- 
  trans_act_nonoperation[, c("year", trans_subsector[5:12])]
# 航空活动水平
trans_act_aviation <- data.frame(year = c(2005: 2019), 航空 = c(1))
comment(trans_act_aviation$航空) <- "nounit"
# 水路客运周转量和水路货运周转量
trans_act_water <- func_read_trans("P6KQQFUP")
trans_act_water <- trans_act_water[, c("year", "客运周转量", "货运周转量")]
names(trans_act_water) <- c("year", trans_subsector[14:15])
# 合并为活动水平数据框
trans_act <- 
  func_merge_2(list(trans_act_operation, trans_act_nonoperation, 
                    trans_act_aviation, trans_act_water))
# 测试：
# 问题：常规公交里程数在2015年之后下降
# plot(trans_act$year, trans_act$常规公交)
# 问题：摩托车在2014年之后断崖式下降，之后又缓慢上升
# plot(trans_act$year, trans_act$摩托车)
# 待办：查看基于统计局数据的“厦门市各区民用车辆拥有量”
# 问题：水路客运在2012年后陡降，在2016年之后又逐渐上升
# plot(trans_act$year, trans_act$水路客运)
# func_show_trend(trans_act)

## Consumption & intensity ---- 
# 营运车辆部分先输入总量后算强度，非营运车辆则相反
# 营运车辆部分能耗总量和能耗强度
# 营运车辆能耗总量
trans_nrgsum_ls_ori <- vector("list")
# 各类车总能耗：按能源类型分
for (i in c(1: 3)) {
  trans_nrgsum_ls_ori[[i]] <- 
    func_read_trans("IZM9FWIY", c("汽油消费量", "柴油消费量", "天然气消费量")[i])
}
names(trans_nrgsum_ls_ori) <- c("gasoline", "diesel", "gas")
# 筛选和计算数据
# 汽油消费量
trans_nrgsum_ls_ori[[1]] <- 
  trans_nrgsum_ls_ori[[1]][, c("year", "常规公交", "出租车合计")]
names(trans_nrgsum_ls_ori[[1]])<- c("year", "常规公交", "出租车")
# 汽油密度0.72千克/升 = 7.2吨/万升
trans_nrgsum_ls_ori[[1]]$"常规公交" <- trans_nrgsum_ls_ori[[1]]$"常规公交" * 7.2
comment(trans_nrgsum_ls_ori[[1]]$常规公交) <- "吨"
# 柴油消费量
func_looknote(trans_nrgsum_ls_ori[[2]])
trans_nrgsum_ls_ori[[2]] <- 
  trans_nrgsum_ls_ori[[2]][c("year", "常规公交新数据", "BRT新数据", "农村客车")]
names(trans_nrgsum_ls_ori[[2]]) <- c("year", "常规公交", "快速公交", "农村客车")
# 柴油密度0.85千克/升 = 8.5吨/万升
trans_nrgsum_ls_ori[[2]]$常规公交 <- trans_nrgsum_ls_ori[[2]]$常规公交 * 8.5
comment(trans_nrgsum_ls_ori[[2]]$常规公交) <- "吨"
trans_nrgsum_ls_ori[[2]]$快速公交 <- trans_nrgsum_ls_ori[[2]]$快速公交 * 8.5
comment(trans_nrgsum_ls_ori[[2]]$快速公交) <- "吨"
# 天然气消费量
func_looknote(trans_nrgsum_ls_ori[[3]])
trans_nrgsum_ls_ori[[3]] <- 
  trans_nrgsum_ls_ori[[3]][c("year", "公交合计", "出租车合计")]
names(trans_nrgsum_ls_ori[[3]]) <- c("year", "常规公交", "出租车")
# 转化为按车辆类型分的能耗量列表
trans_nrgsum_ls <- func_ls_transition(trans_nrgsum_ls_ori)
trans_nrgsum_ls <- trans_nrgsum_ls[trans_subsector[1: 4]]
rm(trans_nrgsum_ls_ori)
# 营运性车辆的能耗强度
trans_nrgintst_ls <- 
  func_nrg_intst_ls(trans_nrgsum_ls, 
                    trans_act[c("year", trans_subsector[1: 4])])

# 非营运车辆部分
# 问题：能耗强度是按照全国各类非营运车辆平均数据计算，各年份相同
trans_ori_nonoperation_nrgintst <- 
  func_read_data("Y3PGVSR7", "非营运性车辆年均里程数和百公里能耗")
trans_ori_nonoperation_nrgintst <- 
  trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")]
# 去掉数据中的空格
trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")] <- 
  lapply(trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")], 
         function(x) {gsub(" ", "", x)})
# 将年均能耗转换成数字类型
trans_ori_nonoperation_nrgintst[c("年均能耗")] <- 
  lapply(trans_ori_nonoperation_nrgintst[c("年均能耗")], as.numeric)
# 将能耗类别转换成英语
trans_ori_nonoperation_nrgintst[c("能耗类别")] <- 
  lapply(trans_ori_nonoperation_nrgintst[c("能耗类别")], 
         function(x) {gsub("汽油", "gasoline", x)})
trans_ori_nonoperation_nrgintst[c("能耗类别")] <- 
  lapply(trans_ori_nonoperation_nrgintst[c("能耗类别")], 
         function(x) {gsub("柴油", "diesel", x)})
# 将该数据框转化为列表
trans_ori_nrgintst_ls <- vector("list", nrow(trans_ori_nonoperation_nrgintst))
if (identical(trans_ori_nonoperation_nrgintst$项目, trans_subsector[5: 12])) {
  for (i in c(1: 8)) {
    trans_ori_nrgintst_ls[[i]] <- 
      data.frame(year = c(2005: 2019),
                 value = c(trans_ori_nonoperation_nrgintst$年均能耗[i]))
    names(trans_ori_nrgintst_ls[[i]])[2] <- 
      trans_ori_nonoperation_nrgintst$能耗类别[i]
  }
} else {
  print("warning: the order is incorrect.")
}
trans_nrgintst_ls[5: 12] <- trans_ori_nrgintst_ls
names(trans_nrgintst_ls)[5: 12] <- trans_subsector[5: 12]
# 计算非营运车辆能耗总量
trans_nrgsum_ls[c(5:12)] <- 
  func_nrg_sum_ls(trans_nrgintst_ls[c(5:12)], 
                  trans_act[c("year", trans_subsector[c(5:12)])])
names(trans_nrgsum_ls)[5: 12] <- trans_subsector[5: 12]

# 航空和水运部分能耗总量和能耗强度
trans_nrgsum_ls$"航空" <- func_read_trans("JXG6KGSA")[, c("year", "国内")]
names(trans_nrgsum_ls$航空)[2] <- "kerosene"
trans_nrgsum_ls$"水路客运" <- func_read_trans("NTNZD6VV", "国内客运")
names(trans_nrgsum_ls$水路客运)[2:3] <- c("diesel", "residual")
trans_nrgsum_ls$"水路货运" <- func_read_trans("NTNZD6VV", "国内货运")
names(trans_nrgsum_ls$水路货运)[2:3] <- c("diesel", "residual")
names(trans_nrgsum_ls)[13:15] <- trans_subsector[13:15]
trans_nrgintst_ls[trans_subsector[13:15]] <- 
  func_nrg_intst_ls(trans_nrgsum_ls[trans_subsector[13:15]], 
                    trans_act[, c(1, 14:16)])
# 测试：
# 问题：常规公交2014年后略有下降
# 问题：快速公交2014年后陡然升高
# 问题：出租车汽油2012年后陡降
# 问题：出租车汽油2013年后陡升
# 问题：轿车2014年后陡降
# plot(trans_act$year, trans_act$轿车)
# plot(trans_nrgsum_ls$轿车[, "year"], trans_nrgsum_ls$轿车[, "gasoline"])
# 问题：农用运输车2013年后陡降
# 待办：逐项检查它们的活动水平和活动强度变化
# func_show_trend_ls(trans_nrgsum_ls)
# 测试：
# 问题：出租车2013年陡然升高
# func_show_trend_ls(trans_nrgintst_ls)



# Industry ----
# 用于聚合工业各行业的函数
# 问题：如何强制转化为数字以避免计算错误
ind_lookup <- 
  Reduce(rbind, 
         list(data.frame(ind_agg = c("食品饮料及烟草制造业"), 
                         ind_ori = c("非金属矿采选业", 
                                     "农副食品加工业", 
                                     "食品制造业", 
                                     "酒、饮料和精制茶制造业", 
                                     "烟草制品业")),
              data.frame(ind_agg = c("纺织及服装制造业"), 
                         ind_ori = c("纺织业", 
                                     "纺织服装、服饰业", 
                                     "皮革、毛皮、羽毛及其制品和制鞋业")),
              data.frame(ind_agg = c("木材及家具制造业"), 
                         ind_ori = c("木材加工和木、竹、藤、棕、草制品业", 
                                     "家具制造业")), 
              data.frame(ind_agg = c("造纸及印刷"), 
                         ind_ori = c("造纸和纸制品业", 
                                     "印刷和记录媒介复制业")), 
              data.frame(ind_agg = c("文体工美用品制造业"), 
                         ind_ori = c("文教、工美、体育和娱乐用品制造业")), 
              data.frame(ind_agg = c("石油及炼焦"), 
                         ind_ori = c("石油加工、炼焦和核燃料加工业")), 
              data.frame(ind_agg = c("化学工业"), 
                         ind_ori = c("化学原料和化学制品制造业", 
                                     "化学纤维制造业", 
                                     "橡胶和塑料制品业")), 
              data.frame(ind_agg = c("医药制造业"), 
                         ind_ori = c("医药制造业")), 
              data.frame(ind_agg = c("非金属矿物制品业"), 
                         ind_ori = c("非金属矿物制品业")), 
              data.frame(ind_agg = c("金属加工制造业"), 
                         ind_ori = c("黑色金属冶炼和压延加工业", 
                                     "有色金属冶炼和压延加工业", 
                                     "金属制品业")), 
              data.frame(ind_agg = c("设备制造业"), 
                         ind_ori = c("通用设备制造业", 
                                     "专用设备制造业", 
                                     "交通运输设备制造业")), 
              data.frame(ind_agg = c("电子电气制造业"), 
                         ind_ori = c("电气机械和器材制造业", 
                                     "计算机、通信和其他电子设备制造业", 
                                     "仪器仪表制造业")), 
              data.frame(ind_agg = c("其他制造业"), 
                         ind_ori = c("其他制造业", 
                                     "废弃资源综合利用业", 
                                     "金属制品、机械和设备修理业", 
                                     "燃气生产和供应业", 
                                     "水的生产和供应业")), 
              data.frame(ind_agg = c("电力、热力生产和供应业"), 
                         ind_ori = c("电力、热力生产和供应业"))))

# 聚合成的能源大类和行业大类
# 剔除电力、热力生产和供应业
ind_ori_subsector <- c("食品饮料及烟草制造业", 
                       "纺织及服装制造业", "木材及家具制造业", 
                       "造纸及印刷", "文体工美用品制造业",    
                       "石油及炼焦", "化学工业", "医药制造业",
                       "非金属矿物制品业", "金属加工制造业",   
                       "设备制造业", 
                       "电子电气制造业", "其他制造业", 
                       "电力、热力生产和供应业")
ind_subsector <- c("食品饮料及烟草制造业", 
                   "纺织及服装制造业", "木材及家具制造业", 
                   "造纸及印刷", "文体工美用品制造业",    
                   "石油及炼焦", "化学工业", "医药制造业",
                   "非金属矿物制品业", "金属加工制造业",   
                   "设备制造业", 
                   "电子电气制造业", "其他制造业")
ind_nrgclass <- c("coal", "coalproduct", 
                  "gasoline", "diesel", "residual", "lpg", 
                  "gas", "electricity")

## Activity level ----
# 读取规上工业各行业GDP
ind_ori_act_scale <- func_read_trans("7TP7UDE6", "工业GDP")
ind_ori_act_scale <- func_secagg(ind_ori_act_scale, ind_lookup)

# 计算规上工业各行业所占比例
ind_ori_act_prop <- ind_ori_act_scale[, -1]/rowSums(ind_ori_act_scale[, -1])*100
ind_ori_act_prop$year <- ind_ori_act_scale$year
# 假设2018-2019年规上工业各行业比例同2017年
ind_ori_act_prop[ind_ori_act_prop$year %in% c(2018, 2019),][ind_ori_subsector] <- 
  ind_ori_act_prop[ind_ori_act_prop$year == 2017,][ind_ori_subsector]

# 补全2018-2019年规上工业各行业GDP
# 根据网上资料，厦门市2018年规上工业增加值1611.35亿元，2019年1749.93亿元
ind_ori_act_scale[ind_ori_act_scale$year == 2018,][ind_ori_subsector] <- 
  1611.35 * 10000 * ind_ori_act_prop[ind_ori_act_prop$year == 2018,][ind_ori_subsector]/100
ind_ori_act_scale[ind_ori_act_scale$year == 2019,][ind_ori_subsector] <- 
  1749.93 * 10000 * ind_ori_act_prop[ind_ori_act_prop$year == 2019,][ind_ori_subsector]/100

# 计算规上工业的总GDP
ind_ori_indgdp_scale <- data.frame(year = ind_ori_act_scale$year)
ind_ori_indgdp_scale$GDP <- 
  rowSums(ind_ori_act_scale[names(ind_ori_act_scale) %in% "year" == FALSE])

# 活动强度为全市工业各行业GDP：剔除电力、热力生产和供应业
ind_act <- func_nrg_sum(ind_ori_act_prop, global_gdp, "##工业")
ind_act <- ind_act[c("year", ind_subsector)]
ind_act[ind_subsector] <- ind_act[ind_subsector]/100

## Energy intensity ---- 
# 读取规上工业各行业各类能耗总量
# 原本的数据是按能源分类的
ind_ori_nrgsum_ls <- 
  func_read_multitable("7TP7UDE6", 
    names_tbl = c("煤", "煤制品", 
                  "汽油", "柴油", "燃料油", "液化石油气", "天然气", "电力"), 
    names_ls = ind_nrgclass)
ind_ori_nrgsum_ls <- func_secagg_ls(ind_ori_nrgsum_ls, ind_lookup)

# 转化为按行业分的能耗总量
ind_ori_nrgsum_scale_ls <- func_ls_transition(ind_ori_nrgsum_ls)
# 基于规上工业能耗总量和规上工业GDP算出单位GDP能耗
ind_ori_nrgintst_ls <- func_nrg_intst_ls(ind_ori_nrgsum_scale_ls, ind_ori_act_scale)
# 剔除电力热力供应业
ind_nrgintst_ls <- ind_ori_nrgintst_ls[ind_subsector]

## Energy consumption ----
ind_nrgsum_ls <- func_nrg_sum_ls(ind_nrgintst_ls, ind_act)


# Service -----
com_subsector <- c("electricity", "lpg_and_gas")

## Activity level ----
# 服务业从业人口和GDP
# 服务业从业人口
com_act <- func_read_trans("2VHEE264", "从业人口")
com_act <- com_act[, c("year", "第三产业")]
names(com_act)[2] <- "com_employee"
# 补全2015-2019年数据：假设线性外推
com_act[which(com_act$year %in% c(2015:2019)), "com_employee"] <- 
  func_linear(com_act, "com_employee", startyear = 2015, endyear = 2019)$com_employee[16:20]
# 服务业GDP
com_act$com_employee <- com_act$com_employee/10000
comment(com_act$com_employee) <- "万人"
com_act <- merge(com_act, global_gdp[c("year", "#第三产业")], by = "year")
names(com_act)[3] <- "com_gdp"
# 测试
# func_show_trend(com_act)

## Energy consumption ----
com_nrgsum_ls <- vector("list", 2)
names(com_nrgsum_ls) <- com_subsector
# 读取厦门市用电数据
com_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")[, c("year", "##第三产业")]
names(com_nrgsum_ls[[1]])[2] <- "electricity"
# 读取厦门市服务业用能
com_nrgsum_ls[[2]] <- func_read_trans("HV4JBQTQ")
names(com_nrgsum_ls[[2]])[2:3] <- c("lpg", "gas")
com_nrgsum_ls[[2]]$gas <- com_nrgsum_ls[[2]]$gas*10000
comment(com_nrgsum_ls[[2]]$gas) <- "万立方米"
## Energy intensity ---- 
com_nrgintst_ls <- func_nrg_intst_ls(com_nrgsum_ls, com_act)
# 测试
# 问题：2015年用气强度比2014年少
# func_show_trend_ls(com_nrgsum_ls)
# func_show_trend_ls(com_nrgintst_ls)


# Other sectors ----
other_subsector <- c("household_electricity", "household_lpg", "household_gas", 
                     "construct_electricity", "agriculture_electricity")

## Activity level ----
# 家庭户数
ori_other_act_house <- global_population[c("year", "household")]
# 用液化石油气的户数
ori_other_act_house_lpg <- func_read_trans("S32RZEF7", "瓶装液化气总用户数")
ori_other_act_house_lpg <- ori_other_act_house_lpg[, c("year", "民用")]
names(ori_other_act_house_lpg)[2] <- "lpg"
ori_other_act_house_lpg$lpg <- ori_other_act_house_lpg$lpg/10000
# 用管道天然气的用户数
ori_other_act_house_gas <- func_read_trans("S32RZEF7", "管道天然气总用户数")
ori_other_act_house_gas <- ori_other_act_house_gas[, c("year", "民用")]
names(ori_other_act_house_gas)[2] <- "gas"
ori_other_act_house_gas$gas <- ori_other_act_house_gas$gas/10000
# 建筑业的GDP
ori_other_act_construct_gdp <- global_gdp[, c("year", "##建筑业")]
names(ori_other_act_construct_gdp)[2] <- "construct"
# 农业的播种面积
ori_other_agriculture_area <- func_read_trans("4NJ97NS9")
ori_other_agriculture_area <- 
  ori_other_agriculture_area[, c("year", "全年农作物总播种面积")]
names(ori_other_agriculture_area)[2] <- "agriculture"
ori_other_agriculture_area$agriculture <- 
  ori_other_agriculture_area$agriculture/1500
comment(ori_other_agriculture_area$agriculture) <- "平方公里"
# 合并成一个活动水平数据框
other_act <- func_merge_2(list(ori_other_act_house, 
                               ori_other_act_house_lpg, 
                               ori_other_act_house_gas,
                               ori_other_act_construct_gdp, 
                               ori_other_agriculture_area))
other_act <- func_addnote(other_act, 
                          c("year", "万户", "万户", "万户", "万元当年价", "平方千米"))
func_looknote(other_act)
# 测试：
# 问题：农业播种面积在2016年后陡降
# func_show_trend(other_act)

## Energy consumption ----
other_nrgsum_ls <- vector("list", 5)
names(other_nrgsum_ls) <- other_subsector
# 家庭用电部分
names(other_nrgsum_ls)[1] <- "家庭用电"
other_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")
other_nrgsum_ls[[1]] <- other_nrgsum_ls[[1]][, c("year", "#城乡居民生活用电")]
names(other_nrgsum_ls[[1]]) <- c("year", "electricity")
# 家庭液化石油气部分
names(other_nrgsum_ls)[2] <- "家庭液化石油气"
other_nrgsum_ls[[2]] <- func_read_trans("HHKVE85Q", "瓶装液化气")
other_nrgsum_ls[[2]] <- other_nrgsum_ls[[2]][, c("year", "家庭")]
names(other_nrgsum_ls[[2]]) <- c("year", "lpg")
# 家庭天然气部分
names(other_nrgsum_ls)[3] <- "家庭天然气"
other_nrgsum_ls[[3]] <- func_read_trans("HHKVE85Q", "管道天然气")
other_nrgsum_ls[[3]] <- other_nrgsum_ls[[3]][, c("year", "家庭")]
names(other_nrgsum_ls[[3]]) <- c("year", "gas")
# 建筑用电部分
names(other_nrgsum_ls)[4] <- "建筑用电"
other_nrgsum_ls[[4]] <- func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")
other_nrgsum_ls[[4]] <- other_nrgsum_ls[[4]][, c("year", "建筑业")]
names(other_nrgsum_ls[[4]]) <- c("year", "electricity")
# 农业用电部分
names(other_nrgsum_ls)[5] <- "农业用电"
other_nrgsum_ls[[5]] <- func_read_trans("2I4DKY2A")
other_nrgsum_ls[[5]] <- other_nrgsum_ls[[5]][, c("year", "##第一产业")]
names(other_nrgsum_ls[[5]]) <- c("year", "electricity")
# 测试
# func_looknote_ls(other_nrgsum_ls)
# func_show_trend_ls(other_nrgsum_ls)
## Energy intensity ---- 
other_nrgintst_ls <- func_nrg_intst_ls(other_nrgsum_ls, other_act)
# 测试
# func_show_trend_ls(other_nrgintst_ls)

# All sectors ----
## Energy consumption ----
trans_nrgsum_df <- func_ls2df(trans_nrgsum_ls)
ind_nrgsum_df <- func_ls2df(ind_nrgsum_ls)
com_nrgsum_df <- func_ls2df(com_nrgsum_ls)
other_nrgsum_df <- func_ls2df(other_nrgsum_ls)
# 合并各部门能耗
demand_nrgsum_ls <- list(trans_nrgsum_df, ind_nrgsum_df, 
                        com_nrgsum_df, other_nrgsum_df)
demand_nrgsum_df <- func_ls2df(demand_nrgsum_ls)
demand_nrgsum_df <- demand_nrgsum_df[c("year", global_nrg_class)]

## Emission ----
trans_emissum_df <- func_emissum(trans_nrgsum_df, emisfac_df)
ind_emissum_df <- func_emissum(ind_nrgsum_df, emisfac_df)
com_emissum_df <- func_emissum(com_nrgsum_df, emisfac_df)
other_emissum_df <- func_emissum(other_nrgsum_df, emisfac_df)

demand_emissum_df <- func_emissum(demand_nrgsum_df, emisfac_df)

# TF & RES ----
# Power generation ----
## Activity level ----
# 读取本地发电量数据
tfres_act <- func_read_trans("2I4DKY2A", "全市发电量")
# 构建数据框：用电量，能源行业用电量，本地发电量，外调电量
tfres_act <- 
  func_merge_2(list(demand_nrgsum_df[c("year", "electricity")], 
    ind_ori_nrgsum_scale_ls[["电力、热力生产和供应业"]][c("year", "electricity")],
                               tfres_act[c("year", "合计")]))
names(tfres_act) <- c("year", "elecuse", "tf_elecuse", "local_elec")
tfres_act$importelec <- 
  tfres_act$elecuse + tfres_act$tf_elecuse - tfres_act$local_elec

## Energy consumption ----
tf_nrgsum_df <- ind_ori_nrgsum_scale_ls[["电力、热力生产和供应业"]]

## Energy intensity ----
tf_nrgintst <- func_nrg_intst(tf_nrgsum_df, tfres_act, "local_elec")
func_show_trend(tf_nrgintst)

## Emission ---- 
tf_emissum <- func_emissum(tf_nrgsum_df, emisfac_df)

# Imported elec ----
## Emission ----
# 读取福建省电力生产排放因子
res_emifac_df <- func_read_trans("M7IB8W4U")
res_emifac_df[2] <- res_emifac_df[2]/10000
comment(res_emifac_df$福建省电网平均电力碳排放因子) <- 
  "万吨二氧化碳/万千瓦时"
res_emissum <- func_cross(tfres_act[c("year", "importelec")], 
                          res_emifac_df)
names(res_emissum)[2] <- "co2"

# RESULT ----
# Emission ----
total_emissum_df <- 
  func_ls2df(list(demand_emissum_df, tf_emissum, 
                  res_emissum))


