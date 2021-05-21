## 交通部门
# 子部门
trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                     "摩托车", "轿车", 
                     "轻型客车", "大型客车", 
                     "轻型货车", "中型货车", "重型货车", 
                     "农用运输车", 
                     "航空", "水路客运", "水路货运")

## 历史数据
## 活动水平
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

# 能耗总量和能耗强度
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



## 工业部门
# 用于聚合工业各行业的函数
# 问题：如何强制转化为数字以避免计算错误
func_ind_agg <- function(input_df) {
  new_df <- data.frame(year = input_df[, "year"])
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
  ind_subsector <- unique(ind_lookup$ind_agg)
  for (i in c(1:length(ind_subsector))) {
    name_ind_agg <- ind_subsector[i]
    name_ind_ori <- ind_lookup$ind_ori[ind_lookup$ind_agg == name_ind_agg]
    new_df[, name_ind_agg] <- 
      rowSums(input_df[names(input_df) %in% name_ind_ori], na.rm = TRUE)
  }
  new_df
}

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

# 历史数据
## 活动水平
# 读取规上工业各行业GDP
ind_ori_act_scale <- func_read_trans("7TP7UDE6", "工业GDP")
ind_ori_act_scale <- func_ind_agg(ind_ori_act_scale)

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

## 能耗强度
# 读取规上工业各行业各类能耗总量
# 原本的数据是按能源分类的
ind_ori_nrgsum_ls <- vector("list", 8)
for (i in c(1: 8)) {
  ind_ori_nrgsum_ls[[i]] <- 
    func_read_trans("7TP7UDE6", c("煤", "煤制品", "汽油", "柴油", "燃料油", "液化石油气", 
                                  "天然气", "电力")[i])                                           
  ind_ori_nrgsum_ls[[i]] <- func_ind_agg(ind_ori_nrgsum_ls[[i]])
  # 并且删除电力、热力生产和供应业
  ind_ori_nrgsum_ls[[i]] <- ind_ori_nrgsum_ls[[i]][c("year", ind_subsector)]
}
names(ind_ori_nrgsum_ls) <- ind_nrgclass
# 转化为按行业分的能耗总量
ind_ori_nrgsum_scale_ls <- func_ls_transition(ind_ori_nrgsum_ls)
# 基于规上工业能耗总量和规上工业GDP算出单位GDP能耗
ind_nrgintst_ls <- func_nrg_intst_ls(ind_ori_nrgsum_scale_ls, ind_ori_act_scale)

## 能耗总量
ind_nrgsum_ls <- func_nrg_sum_ls(ind_nrgintst_ls, ind_act)


## 服务业
com_subsector <- c("electricity", "lpg_and_gas")

## 历史数据
## 活动水平：服务业从业人口和GDP
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

## 能耗总量
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
# 活动强度
com_nrgintst_ls <- func_nrg_intst_ls(com_nrgsum_ls, com_act)
# 测试
# 问题：2015年用气强度比2014年少
# func_show_trend_ls(com_nrgsum_ls)
# func_show_trend_ls(com_nrgintst_ls)


