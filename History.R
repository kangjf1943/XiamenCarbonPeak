# GLOBAL VAR ----
## Names ----
# 能源类别
global_nrg_class <- c("coal", "coalproduct", 
                      "gasoline", "diesel", "kerosene", "residual", "lpg", 
                      "gas", "electricity")
# 排放类别
global_emis_class <- c("co2", "ch4", "n2o")

## Subsectors ----
# 工业行业聚合方式
global_ind_lookup <- 
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

# 工业子部门
global_ind_ori_subsector <- c("食品饮料及烟草制造业", 
                              "纺织及服装制造业", "木材及家具制造业", 
                              "造纸及印刷", "文体工美用品制造业",    
                              "石油及炼焦", "化学工业", "医药制造业",
                              "非金属矿物制品业", "金属加工制造业",   
                              "设备制造业", 
                              "电子电气制造业", "其他制造业", 
                              "电力、热力生产和供应业")
global_ind_subsector <- c("食品饮料及烟草制造业", 
                          "纺织及服装制造业", "木材及家具制造业", 
                          "造纸及印刷", "文体工美用品制造业",    
                          "石油及炼焦", "化学工业", "医药制造业",
                          "非金属矿物制品业", "金属加工制造业",   
                          "设备制造业", 
                          "电子电气制造业", "其他制造业")
global_ind_nrgclass <- c("coal", "coalproduct", 
                         "gasoline", "diesel", "residual", "lpg", 
                         "gas", "electricity")
global_trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                            "摩托车", "轿车", 
                            "轻型客车", "大型客车", 
                            "轻型货车", "中型货车", "重型货车", 
                            "农用运输车", 
                            "水路客运", "水路货运")

# 服务业子部门
global_com_subsector <- c("electricity", "lpg_and_gas")
# 生活子部门
global_household_subsector <- 
  c("household_coal_elec", "household_lpg", "household_gas")


## Factors ----
# 构建排放因子列表
global_emisfac_df <- func_read_data("8C8EDJVH")
global_emisfac_df$year <- c("co2", "ch4", "n2o")


## GDP ----
global_gdp <- func_read_trans("2VHEE264", "GDP")
# 各产业比重
global_gdp$agri_prop <- global_gdp$"#第一产业"/global_gdp$GDP*100
global_gdp$second_prop <- global_gdp$"#第二产业"/global_gdp$GDP*100
global_gdp$ind_prop <- global_gdp$"##工业"/global_gdp$GDP*100
global_gdp$const_prop <- global_gdp$"##建筑业"/global_gdp$GDP*100
global_gdp$com_prop <- global_gdp$"#第三产业"/global_gdp$GDP*100

# 预测GDP相关项目变化
# 预测GDP
prj_global_gdp <- 
  func_rate(baseyear = 2019, 
            basevalue = global_gdp$GDP[global_gdp$year == 2019], 
            rate_df = # 未来GDP增长率减缓
              func_interp_2(year = c(2020, 2025, 2030, 2035, 2040, 2060),
                            value = c(6.00, 6.00, 5.00, 4.00, 3.00, 2.00)))
names(prj_global_gdp)[2] <- "GDP"
comment(prj_global_gdp$GDP) <- "万元当年价"
# 预测各产业所占比重
prj_global_gdp$second_prop <- 
  func_interp_2(year = c(2019, 2025, 2030, 2035, 2060), 
                value = c(global_gdp$second_prop[global_gdp$year == 2019], 
                          32, 26, 23, 15))$value
prj_global_gdp$com_prop <- 
  func_interp_2(year = c(2019, 2025, 2030, 2035, 2060), 
                value = c(global_gdp$com_prop[global_gdp$year == 2019], 
                          67.7, 73.7, 76.8, 84.8))$value
prj_global_gdp$agri_prop <- 
  func_saturate(prj_global_gdp[c("year", "second_prop", "com_prop")], 
                "agri_prop")$agri_prop
prj_global_gdp$const_prop <- 
  func_interp_2(
    year = c(2019, 2060), 
    value = c(global_gdp$const_prop[global_gdp$year == 2019], 15))$value
prj_global_gdp$ind_prop <- 
  func_saturate(prj_global_gdp[c("agri_prop", "const_prop", "com_prop")], 
                "ind_prop")$ind_prop

# 预测各行业GDP
prj_global_gdp$agri_gdp <- prj_global_gdp$GDP * prj_global_gdp$agri_prop/100
prj_global_gdp$second_gdp <- prj_global_gdp$GDP * prj_global_gdp$second_prop/100
prj_global_gdp$ind_gdp <- prj_global_gdp$GDP * prj_global_gdp$ind_prop/100
prj_global_gdp$const_gdp <- prj_global_gdp$GDP * prj_global_gdp$const_prop/100
prj_global_gdp$com_gdp <- prj_global_gdp$GDP * prj_global_gdp$com_prop/100

# Test
# func_history_project_df(global_gdp, prj_global_gdp)

## Population ----
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
# 预测人口相关项目
prj_global_population <- func_interp_2(
  year = c(2019, 2025, 2035, 2045, 2060), 
  value = c(global_population$"常住人口"[global_population$year == 2019], 
            580, 730, 780, 800), "population")
# 问题：如果用第七次人口普查数据，则2019年到2020年的人口跳跃式增长，且韩晖的预测数据将偏小。
prj_global_population$household <- 
  prj_global_population$population / func_lastone(global_population$household_size)
comment(prj_global_population$household) <- "万户"


# DEMAND ----
# Agriculture ----
## Activity level ----
# 农业的播种面积
# 读取《碳排放峰值模型参数选择检验》
by_agriculture_act <- func_read_trans("4NJ97NS9")
by_agriculture_act <- 
  by_agriculture_act[, c("year", "全年农作物总播种面积")]
names(by_agriculture_act)[2] <- "agriculture"
by_agriculture_act$agriculture <- 
  by_agriculture_act$agriculture/1500
comment(by_agriculture_act$agriculture) <- "平方公里"

## Consumption and emission ----
# 读取《碳排放峰值模型参数选择检验》
by_agriculture_ori_diesel <- 
  func_read_trans("4NJ97NS9")[, c("year", "农用柴油使用量")]
names(by_agriculture_ori_diesel)[2] <- "diesel"
# 读取《厦门市电力数据》
by_agriculture_ori_electricity <- 
  func_read_trans("2I4DKY2A")[, c("year", "##第一产业")]
names(by_agriculture_ori_electricity)[2] <- "electricity"
# 合并活动水平
by_agriculture_nrgsum_df <- 
  func_merge_2(list(by_agriculture_ori_diesel, by_agriculture_ori_electricity))
# 计算排放量
by_agriculture_emissum_df <- 
  func_emissum(by_agriculture_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_agriculture_nrgintst <- 
  func_nrg_intst(by_agriculture_nrgsum_df, by_agriculture_act, "agriculture")


# Industry ----
## Activity level ----
# 读取规上工业各行业GDP
by_ind_ori_act_scale <- func_read_trans("7TP7UDE6", "工业GDP")
by_ind_ori_act_scale <- func_secagg(by_ind_ori_act_scale, global_ind_lookup)
# 计算规上工业各行业所占比例
by_ind_ori_act_prop <- 
  by_ind_ori_act_scale[, -1]/rowSums(by_ind_ori_act_scale[, -1])*100
by_ind_ori_act_prop$year <- by_ind_ori_act_scale$year
by_ind_ori_act_prop <- by_ind_ori_act_prop[c("year", global_ind_ori_subsector)]
# 假设2018-2019年规上工业各行业比例同2017年
by_ind_ori_act_prop[by_ind_ori_act_prop$year %in% c(2018, 2019),][global_ind_ori_subsector] <- 
  by_ind_ori_act_prop[by_ind_ori_act_prop$year == 2017,][global_ind_ori_subsector]
# func_propplot(by_ind_ori_act_prop)
# 补全2018-2019年规上工业各行业GDP
# 根据网上资料，厦门市2018年规上工业增加值1611.35亿元，2019年1749.93亿元
by_ind_ori_act_scale[by_ind_ori_act_scale$year == 2018,][global_ind_ori_subsector] <- 
  1611.35 * 10000 * 
  by_ind_ori_act_prop[by_ind_ori_act_prop$year == 2018,][global_ind_ori_subsector]/100
by_ind_ori_act_scale[by_ind_ori_act_scale$year == 2019,][global_ind_ori_subsector] <- 
  1749.93 * 10000 * 
  by_ind_ori_act_prop[by_ind_ori_act_prop$year == 2019,][global_ind_ori_subsector]/100

# 计算规上工业的总GDP
by_ind_ori_indgdp_scale <- data.frame(year = by_ind_ori_act_scale$year)
by_ind_ori_indgdp_scale$GDP <- 
  rowSums(by_ind_ori_act_scale[names(by_ind_ori_act_scale) %in% "year" == FALSE])

# 活动强度为全市工业各行业GDP：剔除电力、热力生产和供应业
by_ind_act <- func_nrg_sum(by_ind_ori_act_prop, global_gdp, "##工业")
by_ind_act <- by_ind_act[c("year", global_ind_subsector)]
by_ind_act[global_ind_subsector] <- by_ind_act[global_ind_subsector]/100

## Energy intensity ---- 
# 读取规上工业各行业各类能耗总量
# 原本的数据是按能源分类的
by_ind_ori_nrgsum_ls <- 
  func_read_multitable("7TP7UDE6", 
                       names_tbl = c("煤", "煤制品", 
                                     "汽油", "柴油", "燃料油", "液化石油气", 
                                     "天然气", "电力"), 
                       names_ls = global_ind_nrgclass)
by_ind_ori_nrgsum_ls <- func_secagg_ls(by_ind_ori_nrgsum_ls, global_ind_lookup)

# 转化为按行业分的能耗总量
by_ind_ori_nrgsum_scale_ls <- func_ls_transition(by_ind_ori_nrgsum_ls)
# 基于规上工业能耗总量和规上工业GDP算出单位GDP能耗
by_ind_ori_nrgintst_ls <- 
  func_nrg_intst_ls(by_ind_ori_nrgsum_scale_ls, by_ind_ori_act_scale)
# 剔除电力热力供应业
by_ind_nrgintst_ls <- by_ind_ori_nrgintst_ls[global_ind_subsector]

## Consumption and emission----
by_ind_nrgsum_ls <- func_nrg_sum_ls(by_ind_nrgintst_ls, by_ind_act)
by_ind_nrgsum_df <- func_ls2df(by_ind_nrgsum_ls)
by_ind_emissum_df <- func_emissum(by_ind_nrgsum_df, global_emisfac_df)


# Construction ----
## Activity level ----
by_construct_act <- global_gdp[, c("year", "##建筑业")]
names(by_construct_act)[2] <- "construct_gdp"

## Consumption and emission ----
# 读取《厦门市电力数据》
by_construct_nrgsum_df <- 
  func_read_trans(
    "2I4DKY2A", "全市电力消费情况表分具体行业")[, c("year", "建筑业")]
names(by_construct_nrgsum_df) <- c("year", "electricity")
by_construct_emissum_df <- 
  func_emissum(by_construct_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_construct_nrgintst <- 
  func_nrg_intst(by_construct_nrgsum_df, by_construct_act, "construct_gdp")


# Transportation ----
# 问题：轨道交通能耗呢？
## Activity level ----
# 营运车辆里程数
# 需求：没有2018-2019年的营运车辆里程数数据
by_trans_act_operation <- func_read_trans("IZM9FWIY", "里程数")
by_trans_act_operation <- 
  by_trans_act_operation[, c("year", "常规公交", "BRT", "出租车", "农村客车")]
names(by_trans_act_operation) <- c("year", global_trans_subsector[1: 4])
# 非营运车辆数量
by_trans_act_nonoperation <- func_read_trans("Y3PGVSR7")
by_trans_act_nonoperation <- 
  by_trans_act_nonoperation[, c("year", global_trans_subsector[5:12])]
# 水路客运周转量和水路货运周转量
by_trans_act_water <- func_read_trans("P6KQQFUP")
by_trans_act_water <- by_trans_act_water[, c("year", "客运周转量", "货运周转量")]
names(by_trans_act_water) <- c("year", global_trans_subsector[13:14])
# 合并为活动水平数据框
by_trans_act <- 
  func_merge_2(list(by_trans_act_operation, 
                    by_trans_act_nonoperation, 
                    by_trans_act_water))

## Consumption & intensity ---- 
# 营运车辆部分先输入总量后算强度，非营运车辆则相反
# 营运车辆部分能耗总量和能耗强度
# 营运车辆能耗总量
# 读取《营运性车辆活动水平和能耗.xlsx》
by_trans_nrgsum_ls_ori <- 
  func_read_multitable("IZM9FWIY", 
                       c("汽油消费量", "柴油消费量", "天然气消费量"), 
                       c("gasoline", "diesel", "gas"))
# 筛选和计算数据
# 汽油消费量
by_trans_nrgsum_ls_ori[[1]] <- 
  by_trans_nrgsum_ls_ori[[1]][, c("year", "出租车合计")]
names(by_trans_nrgsum_ls_ori[[1]])<- c("year", "出租车")
# 柴油消费量
by_trans_nrgsum_ls_ori[[2]] <- 
  by_trans_nrgsum_ls_ori[[2]][c("year", "常规公交", "BRT", "农村客车")]
names(by_trans_nrgsum_ls_ori[[2]]) <- c("year", "常规公交", "快速公交", "农村客车")
# 天然气消费量
func_looknote(by_trans_nrgsum_ls_ori[[3]])
by_trans_nrgsum_ls_ori[[3]] <- 
  by_trans_nrgsum_ls_ori[[3]][c("year", "公交合计", "出租车合计")]
names(by_trans_nrgsum_ls_ori[[3]]) <- c("year", "常规公交", "出租车")
# 转化为按车辆类型分的能耗量列表并整理各元素顺序
by_trans_nrgsum_ls <- func_ls_transition(by_trans_nrgsum_ls_ori)
by_trans_nrgsum_ls <- by_trans_nrgsum_ls[global_trans_subsector[1: 4]]
# 营运性车辆的能耗强度
by_trans_nrgintst_ls <- 
  func_nrg_intst_ls(by_trans_nrgsum_ls, 
                    by_trans_act[c("year", global_trans_subsector[1: 4])])

# 非营运车辆部分
# 读取《非营运性车辆保有量和里程数.xlsx》
# 问题：能耗强度是按照全国各类非营运车辆平均数据计算，各年份相同
# 问题：原清单报告中，P41表3.2.3有错误
by_trans_ori_nonoperation_nrgintst <- 
  func_read_data("Y3PGVSR7", "非营运性车辆年均里程数和百公里能耗")
by_trans_ori_nonoperation_nrgintst <- 
  by_trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")]
# 去掉数据中的空格
by_trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")] <- 
  lapply(by_trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")], 
         function(x) {gsub(" ", "", x)})
# 将年均能耗转换成数字类型
by_trans_ori_nonoperation_nrgintst[c("年均能耗")] <- 
  lapply(by_trans_ori_nonoperation_nrgintst[c("年均能耗")], as.numeric)
# 将能耗类别转换成英语
by_trans_ori_nonoperation_nrgintst[c("能耗类别")] <- 
  lapply(by_trans_ori_nonoperation_nrgintst[c("能耗类别")], 
         function(x) {gsub("汽油", "gasoline", x)})
by_trans_ori_nonoperation_nrgintst[c("能耗类别")] <- 
  lapply(by_trans_ori_nonoperation_nrgintst[c("能耗类别")], 
         function(x) {gsub("柴油", "diesel", x)})
# 将该数据框转化为列表
by_trans_ori_nrgintst_ls <- vector("list", nrow(by_trans_ori_nonoperation_nrgintst))
if (identical(by_trans_ori_nonoperation_nrgintst$项目, global_trans_subsector[5: 12])) {
  for (i in c(1: 8)) {
    by_trans_ori_nrgintst_ls[[i]] <- 
      data.frame(year = c(2005: 2019),
                 value = c(by_trans_ori_nonoperation_nrgintst$年均能耗[i]))
    names(by_trans_ori_nrgintst_ls[[i]])[2] <- 
      by_trans_ori_nonoperation_nrgintst$能耗类别[i]
  }
} else {
  print("warning: the order is incorrect.")
}
by_trans_nrgintst_ls[5: 12] <- by_trans_ori_nrgintst_ls
names(by_trans_nrgintst_ls)[5: 12] <- global_trans_subsector[5: 12]
# 计算非营运车辆能耗总量
by_trans_nrgsum_ls[c(5:12)] <- 
  func_nrg_sum_ls(by_trans_nrgintst_ls[c(5:12)], 
                  by_trans_act[c("year", global_trans_subsector[c(5:12)])])
names(by_trans_nrgsum_ls)[5: 12] <- global_trans_subsector[5: 12]

# 水运部分能耗总量和能耗强度
by_trans_nrgsum_ls$"水路客运" <- func_read_trans("NTNZD6VV", "国内客运")
names(by_trans_nrgsum_ls$水路客运)[2:3] <- c("diesel", "residual")
by_trans_nrgsum_ls$"水路货运" <- func_read_trans("NTNZD6VV", "国内货运")
names(by_trans_nrgsum_ls$水路货运)[2:3] <- c("diesel", "residual")
names(by_trans_nrgsum_ls)[13:14] <- global_trans_subsector[13:14]
by_trans_nrgintst_ls[global_trans_subsector[13:14]] <- 
  func_nrg_intst_ls(by_trans_nrgsum_ls[global_trans_subsector[13:14]], 
                    by_trans_act[, c(1, 14:15)])

## Consumption and emission ----
by_trans_nrgsum_df <- func_ls2df(by_trans_nrgsum_ls)
by_trans_emissum_df <- func_emissum(by_trans_nrgsum_df, global_emisfac_df)


# Service -----
## Activity level ----
# 服务业从业人口
by_com_act <- func_read_trans("2VHEE264", "从业人口")
by_com_act <- by_com_act[, c("year", "第三产业")]
names(by_com_act)[2] <- "com_employee"
# 补全2015-2019年数据：假设线性外推
by_com_act[which(by_com_act$year %in% c(2015:2019)), "com_employee"] <- 
  func_linear(by_com_act, "com_employee", startyear = 2015, endyear = 2019)$com_employee[16:20]
# 服务业GDP
by_com_act$com_employee <- by_com_act$com_employee/10000
comment(by_com_act$com_employee) <- "万人"
by_com_act <- merge(by_com_act, global_gdp[c("year", "#第三产业")], by = "year")
names(by_com_act)[3] <- "com_gdp"

## Consumption and emission ----
by_com_nrgsum_ls <- vector("list", 2)
names(by_com_nrgsum_ls) <- global_com_subsector
# 读取厦门市用电数据
by_com_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")[, c("year", "##第三产业")]
names(by_com_nrgsum_ls[[1]])[2] <- "electricity"
# 读取厦门市服务业用能
by_com_nrgsum_ls[[2]] <- func_read_trans("HV4JBQTQ")
names(by_com_nrgsum_ls[[2]])[2:3] <- c("lpg", "gas")
by_com_nrgsum_ls[[2]]$gas <- by_com_nrgsum_ls[[2]]$gas*10000
comment(by_com_nrgsum_ls[[2]]$gas) <- "万立方米"
# 设置由服务业GDP驱动的用电量
by_com_nrgsum_ls[[2]]$electricity <- 0
# 生成能耗总量数据框
by_com_nrgsum_df <- func_ls2df(by_com_nrgsum_ls)
# 计算排放量
by_com_emissum_df <- func_emissum(by_com_nrgsum_df, global_emisfac_df)

## Energy intensity ---- 
by_com_nrgintst_ls <- func_nrg_intst_ls(by_com_nrgsum_ls, by_com_act)
names(by_com_nrgintst_ls) <- global_com_subsector


# Household ----
## Activity level ----
# 家庭户数
by_ori_household<- global_population[c("year", "household")]
# 用液化石油气的户数
by_ori_lpguser <- func_read_trans("S32RZEF7", "瓶装液化气总用户数")
by_ori_lpguser <- by_ori_lpguser[, c("year", "民用")]
names(by_ori_lpguser)[2] <- "lpg"
by_ori_lpguser$lpg <- by_ori_lpguser$lpg/10000
# 用管道天然气的用户数
by_ori_gasuser <- func_read_trans("S32RZEF7", "管道天然气总用户数")
by_ori_gasuser <- by_ori_gasuser[, c("year", "民用")]
names(by_ori_gasuser)[2] <- "gas"
by_ori_gasuser$gas <- by_ori_gasuser$gas/10000
# 合并成活动水平数据框
by_household_act <- 
  func_merge_2(list(by_ori_household, by_ori_lpguser, by_ori_gasuser))
# 通过线性拟合补全2016-2019年燃气用户数据
by_household_act$lpg[by_household_act$year > 2015] <- 
  func_linear(by_household_act, "lpg", startyear = 2016, endyear = 2019)$lpg[
    func_linear(by_household_act, "lpg", startyear = 2016, endyear = 2019)$color == 
      "predicted"]
by_household_act$gas[by_household_act$year > 2015] <- 
  func_linear(by_household_act, "gas", startyear = 2016, endyear = 2019)$gas[
    func_linear(by_household_act, "gas", startyear = 2016, endyear = 2019)$color == 
      "predicted"]
# 查看燃气用户比例变化
by_household_ori_users_prop <- 
  func_nrg_intst(by_household_act[c("year", "lpg", "gas")], 
                 by_household_act, "household")

## Consumption and emission ----
by_household_nrgsum_ls <- vector("list", length(global_household_subsector))
names(by_household_nrgsum_ls) <- global_household_subsector
# household_coal_elec
# 电力部分
by_household_nrgsum_ls[[1]] <- 
  func_read_trans("2I4DKY2A")[, c("year", "#城乡居民生活用电")]
names(by_household_nrgsum_ls[[1]]) <- c("year", "electricity")
# 煤炭部分
by_household_nrgsum_ls[[1]]$coal <- func_read_trans("H4REI5RK")$"居民生活用煤"
names(by_household_nrgsum_ls[[1]])[3] <- "coal"
# household_lpg
by_household_nrgsum_ls[[2]] <- 
  func_read_trans("HHKVE85Q", "瓶装液化气")[, c("year", "家庭")]
names(by_household_nrgsum_ls[[2]]) <- c("year", "lpg")
# household_gas
by_household_nrgsum_ls[[3]] <- 
  func_read_trans("HHKVE85Q", "管道天然气")[, c("year", "家庭")]
names(by_household_nrgsum_ls[[3]]) <- c("year", "gas")
# emission 
by_household_nrgsum_df <- func_ls2df(by_household_nrgsum_ls)
by_household_emissum_df <- 
  func_emissum(by_household_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_household_nrgintst_ls <- 
  func_nrg_intst_ls(by_household_nrgsum_ls, by_household_act)


# TF & RES ----
# Power generation ----
## Activity level ----
# 合并需求端能耗
by_tfres_ori_demandnrg <- func_ls2df(list(by_agriculture_nrgsum_df,
                                          by_ind_nrgsum_df, 
                                          by_construct_nrgsum_df, 
                                          by_trans_nrgsum_df, 
                                          by_com_nrgsum_df, 
                                          by_household_nrgsum_df))
# 读取本地发电量数据
by_tfres_act <- func_read_trans("2I4DKY2A", "全市发电量")
# 构建数据框：用电量，能源行业用电量，本地发电量，外调电量
by_tfres_act <- 
  func_merge_2(list(by_tfres_ori_demandnrg[c("year", "electricity")], 
                    by_ind_ori_nrgsum_scale_ls[["电力、热力生产和供应业"]][c("year", "electricity")],
                    by_tfres_act[c("year", "合计")]))
names(by_tfres_act) <- c("year", "elecuse", "by_tf_elecuse", "local_elec")
by_tfres_act$importelec <- 
  by_tfres_act$elecuse + by_tfres_act$by_tf_elecuse - by_tfres_act$local_elec

## Energy consumption ----
by_tf_nrgsum_df <- by_ind_ori_nrgsum_scale_ls[["电力、热力生产和供应业"]]

## Energy intensity ----
by_tf_nrgintst <- func_nrg_intst(by_tf_nrgsum_df, by_tfres_act, "local_elec")

## Emission ---- 
by_tf_emissum <- func_emissum(by_tf_nrgsum_df, global_emisfac_df)

# Imported elec ----
## Emission ----
# 读取福建省电力生产排放因子
by_res_emifac_df <- func_read_trans("M7IB8W4U")
by_res_emifac_df[2] <- by_res_emifac_df[2]/10000
comment(by_res_emifac_df$福建省电网平均电力碳排放因子) <- 
  "万吨二氧化碳/万千瓦时"
by_res_emissum <- func_cross(by_tfres_act[c("year", "importelec")], 
                          by_res_emifac_df)
names(by_res_emissum)[2] <- "co2"

# RESULT ----
# 总能耗
by_total_nrgsum_df <- func_ls2df(list(by_agriculture_nrgsum_df, 
                                      by_ind_nrgsum_df, 
                                      by_construct_nrgsum_df, 
                                      by_trans_nrgsum_df, 
                                      by_com_nrgsum_df, 
                                      by_household_nrgsum_df, 
                                      by_tf_nrgsum_df))
by_total_nrgsum_ce_df <- func_toce(by_total_nrgsum_df)
rowSums(by_total_nrgsum_ce_df[names(by_total_nrgsum_ce_df) %in% "year" == FALSE])

# 总排放
by_demand_emissum_df <- 
  func_ls2df(list(by_agriculture_emissum_df, by_ind_emissum_df, 
                  by_construct_emissum_df, by_trans_emissum_df, 
                  by_com_emissum_df, by_household_emissum_df))
by_total_emissum_df <- 
  func_ls2df(list(by_demand_emissum_df, by_tf_emissum, 
                  by_res_emissum))
plot(by_total_emissum_df$year, by_total_emissum_df$co2)

