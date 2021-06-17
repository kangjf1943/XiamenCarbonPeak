# GLOBAL VAR ----
## Names ----
# 能源类别
global_nrg_class <- c("rawcoal", "coalproduct", 
                      "gasoline", "diesel", "residual", "kerosene", "lpg", 
                      "gas", "electricity")
global_sectors <- c("agri", "ind", "const", "trans", "com", "hh", "tf", "res")

## Subsector ----
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
global_ind_nrgclass <- c("rawcoal", "coalproduct", 
                         "gasoline", "diesel", "residual", "lpg", 
                         "gas", "electricity")
if (set_nrgplng_scope == TRUE) {
  global_trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                              "公路其他汽油", "公路其他柴油", 
                              "水路客运", "水路货运", "航空")
} else {
  global_trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                              "公路其他汽油", "公路其他柴油", 
                              "水路客运", "水路货运")
}

# 服务业子部门
global_com_subsector <- c("electricity", "lpg_and_gas")
# 生活子部门
global_hh_subsector <- 
  c("hh_coal_elec", "hh_lpg", "hh_gas")

## Factors ----
# 构建排放因子列表
global_ori_emisfac_df <- func_read_data("8C8EDJVH")[global_nrg_class]
global_emisfac_df <- data.frame(year = c(2000: 2019))
for (i in global_nrg_class) {
  global_emisfac_df[, i] <- global_ori_emisfac_df[, i]
}
rm(global_ori_emisfac_df)
# 预测排放因子变化：从2040年开始各类能耗开始脱碳，至2055年为0
prj_emisfac_df <- data.frame(year = c(2019: 2060))
for (i in global_nrg_class) {
  prj_emisfac_df[, i] <- 
    func_interp_2(
      year = c(2019, 2040, 2050, 2060), 
      value = c(global_emisfac_df[which(global_emisfac_df$year == 2019), i], 
                global_emisfac_df[which(global_emisfac_df$year == 2019), i],
                global_emisfac_df[which(global_emisfac_df$year == 2019), i]*0.7,
                0))$value
}


## GDP ----
# 规上工业各行业GDP
global_indscale_gdp4sctr <- func_read_trans("7TP7UDE6", "工业GDP")
global_indscale_gdp <- data.frame(year = global_indscale_gdp4sctr$year)
global_indscale_gdp$GDP <- 
  rowSums(global_indscale_gdp4sctr[names(global_indscale_gdp4sctr) 
                                   %in% "year" == FALSE], na.rm = TRUE)
# 补全2018年和2019年的数据
global_indscale_gdp[which(
  global_indscale_gdp$year %in% c(2018:2019)), "GDP"] <- 
  c(16113500, 17960000)
comment(global_indscale_gdp) <- "规上工业总GDP"

# 全市各产业GDP
global_gdp <- func_read_trans("2VHEE264", "GDP")[1:7]
names(global_gdp)[names(global_gdp) == "#第一产业"] <- "agrigdp"
names(global_gdp)[names(global_gdp) == "#第二产业"] <- "secgdp"
names(global_gdp)[names(global_gdp) == "##工业"] <- "indgdp"
names(global_gdp)[names(global_gdp) == "##建筑业"] <- "constgdp"
names(global_gdp)[names(global_gdp) == "#第三产业"] <- "comgdp"
# 各产业比重
global_gdp$agrigdp_prop <- global_gdp$agrigdp/global_gdp$GDP*100
global_gdp$secgdp_prop <- global_gdp$secgdp/global_gdp$GDP*100
global_gdp$indgdp_prop <- global_gdp$indgdp/global_gdp$GDP*100
global_gdp$constgdp_prop <- global_gdp$constgdp/global_gdp$GDP*100
global_gdp$comgdp_prop <- global_gdp$comgdp/global_gdp$GDP*100

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
prj_global_gdp$secgdp_prop <- 
  func_interp_2(year = c(2019, 2025, 2030, 2040, 2060), 
                value = c(global_gdp$secgdp_prop[global_gdp$year == 2019], 
                          32, 26, 23, 15, 12))$value
prj_global_gdp$comgdp_prop <- 
  func_interp_2(year = c(2019, 2025, 2030, 2040, 2060), 
                value = c(global_gdp$comgdp_prop[global_gdp$year == 2019], 
                          67.7, 73.7, 76.8, 84.8, 87.8))$value
prj_global_gdp$agrigdp_prop <- 
  func_saturate(prj_global_gdp[c("year", "secgdp_prop", "comgdp_prop")])$value
prj_global_gdp$constgdp_prop <- 
  func_interp_2(
    year = c(2019, 2060), 
    value = c(global_gdp$constgdp_prop[global_gdp$year == 2019], 10))$value
prj_global_gdp$indgdp_prop <- 
  func_saturate(
    prj_global_gdp[c("year", 
                     "agrigdp_prop", "constgdp_prop", "comgdp_prop")])$value

# 预测各行业GDP
prj_global_gdp$agrigdp <- prj_global_gdp$GDP * prj_global_gdp$agrigdp_prop/100
prj_global_gdp$secgdp <- prj_global_gdp$GDP * prj_global_gdp$secgdp_prop/100
prj_global_gdp$indgdp <- prj_global_gdp$GDP * prj_global_gdp$indgdp_prop/100
prj_global_gdp$constgdp <- prj_global_gdp$GDP * prj_global_gdp$constgdp_prop/100
prj_global_gdp$comgdp <- prj_global_gdp$GDP * prj_global_gdp$comgdp_prop/100

# Test
# func_history_project_df(global_gdp, prj_global_gdp, "ggpoint")

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
  prj_global_population$population / 
  func_lastone(global_population$household_size)
comment(prj_global_population$household) <- "万户"

## Read data ----
# 读取规上工业各行业能耗分能耗类型-行业数据
global_indscale_nrgls_bynrg <- 
  func_read_multitable(
    "7TP7UDE6", 
    names_tbl = c("煤", "煤制品", 
                  "汽油", "柴油", "燃料油", "液化石油气", 
                  "天然气", "电力"), 
    names_ls = c("rawcoal", "coalproduct", 
                 "gasoline", "diesel", "residual", "lpg", 
                 "gas", "electricity"))
comment(global_indscale_nrgls_bynrg) <- 
  "规上工业能耗：8能源-35行业"

# 聚合各行业：工业用能分能耗类型-聚合行业数据
global_indscale_nrgls_bynrg_secagg <- 
  func_secagg_ls(global_indscale_nrgls_bynrg, global_ind_lookup)
comment(global_indscale_nrgls_bynrg_secagg) <- 
  "规上工业能耗：8能源-14行业"

# 转变成工业用能分聚合行业-能耗类型数据：包含电力热力供应业
global_indscale_nrg_bysecagg <- 
  func_ls_transition(global_indscale_nrgls_bynrg_secagg)
comment(global_indscale_nrg_bysecagg) <- 
  "规上工业能耗：14行业-8能源"

# 转变成工业用能分聚合行业数据框：不包含电力热力供应业
global_indscale_nrg_bysecagg_noelec <- 
  global_indscale_nrg_bysecagg[global_ind_subsector]
comment(global_indscale_nrg_bysecagg_noelec) <- "规上工业能耗列表：13行业-8能源"

# 压缩成数据框
global_indscale_nrg_bysecagg_noelec_df <- 
  func_ls2df(global_indscale_nrg_bysecagg_noelec)
comment(global_indscale_nrg_bysecagg_noelec_df) <- "规上工业能耗数据框：8能源"

# 农业柴油
global_agri_diesel <- func_read_trans("4NJ97NS9")[, c("year", "农用柴油使用量")]

# 生活原煤
global_hh_coal <- func_read_trans("WJU7N3EL")
global_hh_coal$"生活用煤" <- global_hh_coal$"生活用煤"*10000
comment(global_hh_coal$"生活用煤") <- "吨"

# 读取统计年鉴的农业柴油
global_agri_diesel <- func_read_trans("4NJ97NS9")[, c("year", "农用柴油使用量")]
comment(global_agri_diesel$"农用柴油使用量")

# 读取厦航煤油
# 问题：能源平衡表煤油到底包含不包含福州机场煤油消费？
global_trans_kerosene <- func_read_trans("M8UPDTJN")

# 读取航空煤油数据并整理出不含福州机场部分的煤油消费量之和
global_avnnrg <- func_read_trans("JXG6KGSA")
# 其中“厦航煤油”项可能是包含福州机场的煤油消费量之和
# 补全国际航班合计之和
global_avnnrg[which(global_avnnrg$year %in% c(2011: 2017)), "国际航班合计"] <- 
  global_avnnrg[which(global_avnnrg$year %in% c(2011: 2017)), "#国内国际段航班"] +
  global_avnnrg[which(global_avnnrg$year %in% c(2011: 2017)), "#外航航班"]
# 生成国内国际航班之和
global_avnnrg$kerosene <- 
  global_avnnrg$"国内航班" + global_avnnrg$"国际航班合计"
# 补全2018-2019年国内航班之和
# 假设国内国际航空煤油消费量和包含福州机场在内的消费量比例同2017年
global_avnnrg[which(global_avnnrg$year %in% c(2018: 2019)), "kerosene"] <- 
  func_lastone(global_avnnrg[which(global_avnnrg$year == 2017), "kerosene"] / 
                 global_avnnrg[which(global_avnnrg$year == 2017), "厦航煤油"]) * 
  global_avnnrg[which(global_avnnrg$year %in% c(2018: 2019)), "厦航煤油"]
global_avnnrg <- global_avnnrg[c("year", "kerosene")]

# 读取航空客货运周转量
global_avn_act <- 
  func_read_trans("U737THYU")[c("year", "客运周转量", "货运周转量")]
names(global_avn_act) <- c("year", "avn_rpk", "avn_rftk")

# 读取水运客货运周转量
global_water_act <- 
  func_read_trans("P6KQQFUP")[c("year", "客运周转量", "货运周转量")]
names(global_water_act) <- c("year", "water_rpk", "water_rftk")

# 读取园林局LPG数据
global_ind_com_hh_lpg <- func_read_trans("SRYBIXUY")
global_ind_com_hh_lpg[c("工业", "服务业", "生活消费")] <- 
  global_ind_com_hh_lpg[c("工业", "服务业", "生活消费")]*10000
global_ind_com_hh_lpg[c("工业", "服务业", "生活消费")] <- func_addnote(
  global_ind_com_hh_lpg[c("工业", "服务业", "生活消费")], c("吨", "吨", "吨"))

# 读取园林局天然气数据
global_com_hh_gas <- func_read_trans("4ALKEGTV")
global_com_hh_gas[c("服务业", "生活消费")] <- 
  global_com_hh_gas[c("服务业", "生活消费")]*10000
global_com_hh_gas[c("服务业", "生活消费")] <- func_addnote(
  global_com_hh_gas[c("服务业", "生活消费")], c("万立方米", "万立方米"))

# 读取交通天然气
global_trans_gas <- func_read_trans("QH2KBX3X")

# 读取用电数据
global_electricity_sec <- func_read_trans("2I4DKY2A", "全市电力消费情况表")
global_electricity_finesec <- 
  func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")

# 读取本地发电数据并计算清洁和非清洁发电比例
global_elecgen <- func_read_trans("2I4DKY2A", "全市发电量")
global_elecgen$clean <- 
  global_elecgen$"#水电" + global_elecgen$"#垃圾发电" + global_elecgen$"#太阳能"
global_elecgen$clean_prop <- global_elecgen$clean / global_elecgen$"合计"
global_elecgen$thrm_prop <- 1 - global_elecgen$clean_prop

# 读取水运燃料油
global_trans_residual <- func_read_trans("68Z975NU")
global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")] <- 
  global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")]*10000
global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")] <- 
  func_addnote(
    global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")], 
    c("吨", "吨", "吨", "吨"))

# 读取慧梅推算：交通水运部分（基于港口局数据）
global_water_railway_diesel <- func_read_trans("VZS39ZM8")
global_water_railway_diesel[c("水运国内客运", "水运国内货运", 
                              "水运国际客运", "水运国际货运", "铁路")] <- 
  global_water_railway_diesel[c("水运国内客运", "水运国内货运", 
                                "水运国际客运", "水运国际货运", "铁路")]*10000
global_water_railway_diesel[c("水运国内客运", "水运国内货运", 
                              "水运国际客运", "水运国际货运", "铁路")] <- 
  func_addnote(
    global_water_railway_diesel[c("水运国内客运", "水运国内货运", 
                                  "水运国际客运", "水运国际货运", "铁路")], 
    rep("吨", 4))

# 读取慧梅推算：铁路（客运周转量*能耗强度）
global_roadoper_diesel <- 
  func_read_trans("IZM9FWIY", "柴油消费量")[c("year","常规公交","BRT","农村客车")]

# 读取慧梅推算数据：营运非营运车辆柴油
global_roadnonoper_diesel <- 
  func_read_trans("Y3PGVSR7", "非营运性车辆柴油消费推算")
global_roadnonoper_diesel[c("非营运客车", "货车")] <- 
  global_roadnonoper_diesel[c("非营运客车", "货车")]*10000
global_roadnonoper_diesel[c("非营运客车", "货车")] <- 
  func_addnote(global_roadnonoper_diesel[c("非营运客车", "货车")], 
               rep("吨", 2))

# 读取刘洋太阳能发电预测
global_solarelecgen_fut <- func_read_trans("4R9XNW4Z")

# 读取全省发电量
global_provelecgen <- func_read_trans("S3CNPRZE", "发电量")


# SETTING ----
set_by_elecequalfac_meth <- TRUE
set_nrgplng_scope <- TRUE # 是否计算能源规划口径能耗

# NRG BALANCE ----
# 构建空能源平衡表
by_nrgbal_years <- as.character(c(2015: 2019))
by_nrgbal_ls <- vector("list", 5)
names(by_nrgbal_ls) <- by_nrgbal_years
for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]] <- 
    data.frame(
      iterm = c("tf", "agri","ind","const", "trans","com", "hh"), 
      rawcoal = 0, coalproduct = 0, 
      gasoline = 0, diesel = 0, 
      kerosene = 0, residual = 0, lpg = 0, 
      gas = 0, electricity = 0)
}

# 1.1 Transformation input ----
# 输入发电那一行
for (j in by_nrgbal_years) {
  for (i in global_nrg_class[c(1:5, 7)]) {
    by_nrgbal_ls[[j]][which(by_nrgbal_ls[[j]]$iterm == "tf"),i] <- 
      global_indscale_nrg_bysecagg$"电力、热力生产和供应业"[which(
        global_indscale_nrg_bysecagg$"电力、热力生产和供应业"$year == j), i]
  }
}

for (i in by_nrgbal_years) {
  # 1.2 Household rawcoal ----
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "rawcoal"] <- 
    global_hh_coal[which(global_hh_coal$year == i), "生活用煤"]
  # 1.3 Agri diesel ----
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "agri"), "diesel"] <- 
    global_agri_diesel[which(global_agri_diesel$year == i), "农用柴油使用量"]
  # 1.4 Trans kerosene ----
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "kerosene"] <- 
    global_trans_kerosene[which(global_trans_kerosene$year == i), "厦航煤油"]
  # 1.5 Ind & Com & Household LPG ----
  by_nrgbal_ls[[i]][which(
    by_nrgbal_ls[[i]]$iterm %in% c("ind", "com", "hh")), "lpg"] <- 
    as.numeric(global_ind_com_hh_lpg[which(global_ind_com_hh_lpg$year == i), 
                              c("工业", "服务业", "生活消费")])
  # 1.6 Com & Household gas ----
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm %in% c("com", "hh")), "gas"] <- 
    as.numeric(
      global_com_hh_gas[which(global_com_hh_gas$year == i), c("服务业", "生活消费")])
  # 1.7 Trans gas ----
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gas"] <- 
    sum(global_trans_gas[which(
      global_trans_gas$year == i), c("公共汽车", "出租车")])
}

# 1.8 Electricity ----
for (i in by_nrgbal_years) {
  # 发电量
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "tf"), "electricity"] <- 
    global_elecgen[which(global_elecgen$year == i), "合计"]
  # 农业用电
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "agri"), "electricity"] <- 
    global_electricity_sec[which(global_electricity_sec$year == i), "##第一产业"]
  # 建筑业用电
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "const"), "electricity"] <- 
    global_electricity_finesec[which(global_electricity_finesec$year == i), "建筑业"]
  # 工业用电
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "electricity"] <- 
    global_electricity_sec[which(global_electricity_sec$year == i), "##第二产业"] - 
    global_electricity_finesec[which(global_electricity_finesec$year == i), "建筑业"]
  # 服务业用电
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "com"), "electricity"] <- 
    global_electricity_sec[which(global_electricity_sec$year == i), "##第三产业"]
  # 生活用电
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "electricity"] <- 
    global_electricity_sec[which(global_electricity_sec$year == i), "##第三产业"]
}

# 2.1 Ind oil ----
# 构造每年全市工业GDP/规上工业GDP缩放因子
by_nrgbal_ind_scalefac <- func_cross(
  global_gdp[c("year", "indgdp")], global_indscale_gdp, method = "rate")
names(by_nrgbal_ind_scalefac)[2] <- "scalefac"
# 不包含电力热力行业的工业能耗数据
by_nrgbal_indscale_oil <- 
  func_ls2df(global_indscale_nrg_bysecagg[global_ind_subsector])
by_nrgbal_indscale_oil <- 
  by_nrgbal_indscale_oil[c("year", "gasoline", "diesel", "residual")]
# 进行缩放
by_nrgbal_ind_oil <- 
  func_nrg_sum(by_nrgbal_indscale_oil, by_nrgbal_ind_scalefac, "scalefac")

for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), 
                    c("gasoline", "diesel", "residual")] <- 
    by_nrgbal_ind_oil[which(by_nrgbal_ind_oil$year == i), 
                c("gasoline", "diesel", "residual")]
}

# 3.1 Ind coalproduct ----
# 填写工业煤制品
# 读取黄若谷统计局核对数据
by_nrgbal_check <- 
  func_read_multitable(
    "2IRV6STN", names_tbl = c("煤合计", "原煤", "油品", "天然气"), 
    names_ls = c("coaltotal", "coal", "oil", "gas"))
# 更改单位
for (i in c("coaltotal", "coal", "oil", "gas")) {
  by_nrgbal_check[[i]][, -1] <- by_nrgbal_check[[i]][, -1]*10000
}

for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "coalproduct"] <- 
    by_nrgbal_check[["coaltotal"]][which(
      by_nrgbal_check[["coaltotal"]]$year == i), "工业"] - 
    by_nrgbal_check[["coal"]][which(
      by_nrgbal_check[["coal"]]$year == i), "工业"] 
}

# 3.2 Ind coal ----
# 4 = 若谷煤合计-刚算的煤制品-生活用煤-发电用煤
# 读取若谷总量数据
nrgcheck_total <- func_read_trans("LPLPNXCQ")
nrgcheck_total[c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")] <- 
  nrgcheck_total[c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")]*10000
nrgcheck_total[c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")] <- 
  func_addnote(
    nrgcheck_total[c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")], 
    c("吨", "吨", "万立方米", "万千瓦时"))

for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "rawcoal"] <- 
    nrgcheck_total[which(nrgcheck_total$year == i), "煤炭消费量"] - 
    # 刚算的煤制品
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "coalproduct"] -
    # 生活用煤
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "rawcoal"] -
    # 发电用煤
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "tf"), "rawcoal"]
}

# 3.3 Trans residual ----
# 9 = 水运燃料油消耗量（港口局数据推算，慧梅）
for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "residual"] <- 
    sum(global_trans_residual[which(global_trans_residual$year == i), 
                       c("国内客运", "国内货运", "国际客运", "国际货运")])
}

# 3.4 Trans diesel ----
for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "diesel"] <- 
    sum(global_water_railway_diesel[which(
      global_water_railway_diesel$year == i), 
      c("水运国内客运", "水运国内货运","水运国际客运","水运国际货运","铁路")], 
      na.rm = TRUE) +
    # 公路营运
    sum(global_roadoper_diesel[which(
      global_roadoper_diesel$year == i), c("常规公交","BRT","农村客车")], na.rm = TRUE) +
    # 公路非营运
    sum(global_roadnonoper_diesel[which(
      global_roadnonoper_diesel$year == i), c("非营运客车","货车")], na.rm = TRUE)
}

# 3.5 Trans gasoline ----
# 18 = 交通汽油柴油合计量 = 全部油品总量-目前已有的数据
trans_gasolin_diesel_ls <- 
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gasoline"] <- 
      # 各年份油品总量
      nrgcheck_total[which(nrgcheck_total$year == i), "油品消费量"] - 
      # 目前已有的各类油耗数据
      sum(by_nrgbal_ls[[i]][, c("gasoline", "diesel", "kerosene","residual","lpg")])
  }

# 3.6  Ind gas----
# 7 = 若谷天然气总量-生活消费-服务业-交通-发电
for (i in by_nrgbal_years) {
  by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "gas"] <- 
    # 若谷核对统计局总量
    nrgcheck_total[which(nrgcheck_total$year == i), "天然气消费量"] - 
    # 生活消费
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "gas"] -
    # 服务业
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "com"), "gas"] -
    # 交通
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gas"] -
    # 发电
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "tf"), "gas"]
}

# Data export ----
nrgbal_out <- createWorkbook()
for (i in by_nrgbal_years) {
  addWorksheet(nrgbal_out, i)
  writeData(nrgbal_out, i, by_nrgbal_ls[[i]])
}
if (file.exists("生成能源平衡表.xlsx")) {
  file.remove("生成能源平衡表.xlsx")
}
saveWorkbook(nrgbal_out, "生成能源平衡表.xlsx")


# Agri ----
## Activity level ----
# 农业的播种面积
# 读取《碳排放峰值模型参数选择检验》
by_agri_act <- func_read_trans("4NJ97NS9")
by_agri_act <- 
  by_agri_act[, c("year", "全年农作物总播种面积")]
names(by_agri_act)[2] <- "agri"
by_agri_act$agri <- 
  by_agri_act$agri/1500
comment(by_agri_act$agri) <- "平方公里"

## Consumption and emission ----
# 读取《碳排放峰值模型参数选择检验》
by_agri_ori_diesel <- global_agri_diesel
names(by_agri_ori_diesel)[2] <- "diesel"
# 读取《厦门市电力数据》
by_agri_ori_electricity <- global_electricity_sec[, c("year", "##第一产业")]
names(by_agri_ori_electricity)[2] <- "electricity"
# 合并活动水平
by_agri_nrgsum_df <- 
  func_merge_2(list(by_agri_ori_diesel, by_agri_ori_electricity))
# 计算排放量
by_agri_emissum_df <- 
  func_emissum(by_agri_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_agri_nrgintst <- 
  func_nrg_intst(by_agri_nrgsum_df, by_agri_act, "agri")


# Industry ----
## Activity level ----
# 问题：需要通过调研补全规上各行业GDP数据
# 读取规上工业各行业GDP
by_ind_ori_act_scale <- func_secagg(global_indscale_gdp4sctr, global_ind_lookup)
# 计算规上工业各行业所占比例
by_ind_ori_act_prop <- 
  by_ind_ori_act_scale[, -1]/rowSums(by_ind_ori_act_scale[, -1])*100
by_ind_ori_act_prop$year <- by_ind_ori_act_scale$year
by_ind_ori_act_prop <- by_ind_ori_act_prop[c("year", global_ind_ori_subsector)]
# 假设2018-2019年规上工业各行业比例同2017年
by_ind_ori_act_prop[by_ind_ori_act_prop$year %in% c(2018, 2019),
                    global_ind_ori_subsector] <- 
  by_ind_ori_act_prop[by_ind_ori_act_prop$year == 2017,][global_ind_ori_subsector]
# 作图：func_propplot(by_ind_ori_act_prop)
# 活动强度为全市工业各行业GDP：剔除电力、热力生产和供应业
by_ind_act <- func_nrg_sum(by_ind_ori_act_prop, global_gdp, "indgdp")
by_ind_act <- by_ind_act[c("year", global_ind_subsector)]
by_ind_act[global_ind_subsector] <- by_ind_act[global_ind_subsector]/100


## Consumption and emission----
# 读取规上工业各行业各类能耗总量
# 原本的数据是按能源分类的
by_ind_ori_nrgsum_bynrg_ls <- global_indscale_nrgls_bynrg_secagg

# 构造缩放因子
by_ind_ori_scalefac <- data.frame(year = by_nrgbal_years)
for (j in c(global_nrg_class[global_nrg_class %in% "kerosene" == FALSE])) {
  by_ind_ori_scalefac[, j] <- NA
  for (i in by_nrgbal_years) {
    by_ind_ori_scalefac[by_ind_ori_scalefac$year == i, j] <- 
      by_nrgbal_ls[[i]][which(
        by_nrgbal_ls[[i]]$iterm == "ind"), j]/
      global_indscale_nrg_bysecagg_noelec_df[which(
        global_indscale_nrg_bysecagg_noelec_df$year == i), j]
  }
}

# 缩放数据
for (i in global_nrg_class[global_nrg_class %in% "kerosene" == FALSE]) {
  by_ind_ori_nrgsum_bynrg_ls[[i]] <- 
    func_nrg_sum(by_ind_ori_nrgsum_bynrg_ls[[i]], 
                 by_ind_ori_scalefac, i)
}

# 转化为按行业分的能耗总量并剔除电力热力行业
by_ind_nrgsum_ls <- func_ls_transition(by_ind_ori_nrgsum_bynrg_ls)
by_ind_nrgsum_ls <- by_ind_nrgsum_ls[global_ind_subsector]
# 计算工业的各类能耗总量
by_ind_nrgsum_df <- func_ls2df(by_ind_nrgsum_ls)
# 计算排放量
by_ind_emissum_df <- func_emissum(by_ind_nrgsum_df, global_emisfac_df)


## Energy intensity ---- 
by_ind_nrgintst_ls <- 
  func_nrg_intst_ls(by_ind_nrgsum_ls, by_ind_act)


# Construction ----
## Activity level ----
by_const_act <- global_gdp[, c("year", "constgdp")]
names(by_const_act)[2] <- "const_gdp"

## Consumption and emission ----
# 读取《厦门市电力数据》
by_const_nrgsum_df <- global_electricity_finesec[, c("year", "建筑业")]
names(by_const_nrgsum_df) <- c("year", "electricity")
by_const_emissum_df <- 
  func_emissum(by_const_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_const_nrgintst <- 
  func_nrg_intst(by_const_nrgsum_df, by_const_act, "const_gdp")


# Transportation ----
# 问题：轨道交通能耗呢？
## Activity level ----
# 营运车辆里程数
# 需求：没有2018-2019年的营运车辆里程数数据
by_trans_act_operation <- func_read_trans("IZM9FWIY", "里程数")
by_trans_act_operation <- 
  by_trans_act_operation[, c("year", "常规公交", "BRT", "出租车", "农村客车")]
names(by_trans_act_operation) <- c("year", global_trans_subsector[1: 4])
# 公路其他汽油：私家车
by_trans_act_nonoperation <- func_read_trans("Y3PGVSR7")[c("year", "轿车")]
names(by_trans_act_nonoperation)[2] <- global_trans_subsector[5]
# 公路其他柴油：货运周转量
by_trans_ori_turnover <- data.frame("year" = c(2017:2019), 
                                    "公路其他柴油" = c(1919251, 2037836, 2216748))
# 水路客运周转量和水路货运周转量
by_trans_act_water <- global_water_act
names(by_trans_act_water) <- c("year", global_trans_subsector[7:8])
# 合并为活动水平数据框
by_trans_act <- 
  func_merge_2(list(by_trans_act_operation, 
                    by_trans_act_nonoperation,
                    by_trans_ori_turnover, 
                    by_trans_act_water))
# 能源规划口径下增加航空客运周转量一列
if (set_nrgplng_scope == TRUE) { ## Nrgplng scope ----
  by_trans_ori_avn <- global_avn_act[c("year", "avn_rpk")]
  names(by_trans_ori_avn) <- c("year", global_trans_subsector[9])
  by_trans_act <- func_merge_2(list(by_trans_act, by_trans_ori_avn))
}

# 假设：营运车辆2018-2019年数据为历史数据线性外推
for (i in global_trans_subsector[1:3]) {
  by_trans_act[which(by_trans_act$year > 2017), i] <- 
    tail(func_linear(by_trans_act, i, startyear = 2018, endyear = 2019)[, i], 2)
}
by_trans_act[which(by_trans_act$year > 2014), "农村客车"] <- 
  tail(func_linear(by_trans_act, "农村客车", 
                   startyear = 2015, endyear = 2019)[, "农村客车"], 5)


## Consumption and emission ---- 
# 定义存储数据框
by_trans_nrgsum_ls <- vector("list", length(global_trans_subsector))
names(by_trans_nrgsum_ls) <- global_trans_subsector

# 营运车辆能耗总量
by_trans_nrgsum_ls_ori <- vector("list", 3)
names(by_trans_nrgsum_ls_ori) <- c("gasoline", "diesel", "gas")
# 汽油消费量
by_trans_nrgsum_ls_ori[[1]] <- 
  func_read_trans("IZM9FWIY", "汽油消费量")[, c("year", "出租车合计")]
names(by_trans_nrgsum_ls_ori[[1]])<- c("year", "出租车")
# 柴油消费量
by_trans_nrgsum_ls_ori[[2]] <- global_roadoper_diesel
names(by_trans_nrgsum_ls_ori[[2]]) <- c("year", "常规公交", "快速公交", "农村客车")
# 天然气消费量
by_trans_nrgsum_ls_ori[[3]] <- global_trans_gas
names(by_trans_nrgsum_ls_ori[[3]]) <- c("year", "常规公交", "出租车")
# 转化为按车辆类型分的能耗量列表并整理各元素顺序
by_trans_nrgsum_ls[global_trans_subsector[1: 4]] <- 
  func_ls_transition(by_trans_nrgsum_ls_ori)

# 其他汽油 = 能源平衡表汽油总量扣除当前汽油之和
by_trans_nrgsum_ls[["公路其他汽油"]] <- data.frame(year = by_nrgbal_years)
by_trans_nrgsum_ls[["公路其他汽油"]]$gasoline <- NA
for (i in by_nrgbal_years) {
  by_trans_nrgsum_ls[["公路其他汽油"]][which(
    by_trans_nrgsum_ls[["公路其他汽油"]]$year == i), "gasoline"] <- 
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gasoline"] -
    by_trans_nrgsum_ls_ori[["gasoline"]][which(
      by_trans_nrgsum_ls_ori[["gasoline"]]$year == i), "出租车"]
}
# 另增其他电力，作为未来的分支
by_trans_nrgsum_ls[["公路其他汽油"]]$electricity <- 0

# 水路客运能耗
if (set_nrgplng_scope == TRUE) {
  by_trans_nrgsum_ls[["水路客运"]] <- 
    func_merge_2(list(
      func_cross(global_water_railway_diesel[c("year", "水运国内客运")], 
                 global_water_railway_diesel[c("year", "水运国际客运")], "sum"), 
      func_cross(global_trans_residual[c("year", "国内客运")], 
                 global_trans_residual[c("year", "国际客运")], "sum")))
} else {
  by_trans_nrgsum_ls[["水路客运"]] <- 
    func_merge_2(list(
      global_water_railway_diesel[c("year", "水运国内客运")], 
      global_trans_residual[c("year", "国内客运")]))
}
names(by_trans_nrgsum_ls$水路客运)[2:3] <- c("diesel", "residual")

# 水路货运能耗
if (set_nrgplng_scope == TRUE) {
  by_trans_nrgsum_ls[["水路货运"]] <- 
    func_merge_2(list(
      func_cross(global_water_railway_diesel[c("year", "水运国内货运")], 
                 global_water_railway_diesel[c("year", "水运国际货运")], "sum"), 
      func_cross(global_trans_residual[c("year", "国内货运")], 
                 global_trans_residual[c("year", "国际货运")], "sum")))
} else {
  by_trans_nrgsum_ls[["水路货运"]] <- 
    func_merge_2(list(
      global_water_railway_diesel[c("year", "水运国内货运")], 
      global_trans_residual[c("year", "国内货运")]))
}
names(by_trans_nrgsum_ls$水路货运)[2:3] <- c("diesel", "residual")

# 其他柴油 = 能源平衡表柴油总量扣除当前柴油之和
by_trans_nrgsum_ls[["公路其他柴油"]] <- data.frame(year = by_nrgbal_years)
by_trans_nrgsum_ls[["公路其他柴油"]]$diesel <- NA
for (i in by_nrgbal_years) {
  by_trans_nrgsum_ls[["公路其他柴油"]][which(
    by_trans_nrgsum_ls[[5]]$year == i), "diesel"] <- 
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "diesel"] -
    # 公路运输的柴油消费
    sum(by_trans_nrgsum_ls_ori[["diesel"]][which(
      by_trans_nrgsum_ls_ori[["gasoline"]]$year == i), 
      c("常规公交", "快速公交", "农村客车")]) -
    # 水路客运柴油消费
    by_trans_nrgsum_ls[["水路客运"]][which(
      by_trans_nrgsum_ls[["水路客运"]]$year == i), "diesel"] -
    # 水路货运柴油消费
    by_trans_nrgsum_ls[["水路货运"]][which(
      by_trans_nrgsum_ls[["水路货运"]]$year == i), "diesel"]
}

# 能源规划口径下：计算航空煤油
if (set_nrgplng_scope == TRUE) {
  by_trans_nrgsum_ls[["航空"]] <- 
    global_avnnrg
}

# 能耗总量和排放
by_trans_nrgsum_df <- func_ls2df(by_trans_nrgsum_ls)
by_trans_emissum_df <- func_emissum(by_trans_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_trans_nrgintst_ls <- 
  func_nrg_intst_ls(by_trans_nrgsum_ls, by_trans_act)


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
by_com_act <- merge(by_com_act, global_gdp[c("year", "comgdp")], by = "year")
names(by_com_act)[3] <- "com_gdp"

## Consumption and emission ----
by_com_nrgsum_ls <- vector("list", 2)
names(by_com_nrgsum_ls) <- global_com_subsector
# 读取厦门市用电数据
by_com_nrgsum_ls[[1]] <- global_electricity_sec[c("year", "##第三产业")]
names(by_com_nrgsum_ls[[1]])[2] <- "electricity"
# 读取厦门市服务业LPG消费
by_com_nrgsum_ls[[2]] <- 
  func_merge_2(list(global_ind_com_hh_lpg[c("year", "服务业")], 
                    global_com_hh_gas[c("year", "服务业")]))
names(by_com_nrgsum_ls[[2]])[2:3] <- c("lpg", "gas")
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
by_hh_act <- 
  func_merge_2(list(by_ori_household, by_ori_lpguser, by_ori_gasuser))
# 通过线性拟合补全2016-2019年燃气用户数据
by_hh_act$lpg[by_hh_act$year > 2015] <- 
  func_linear(by_hh_act, "lpg", startyear = 2016, endyear = 2019)$lpg[
    func_linear(by_hh_act, "lpg", startyear = 2016, endyear = 2019)$color == 
      "predicted"]
by_hh_act$gas[by_hh_act$year > 2015] <- 
  func_linear(by_hh_act, "gas", startyear = 2016, endyear = 2019)$gas[
    func_linear(by_hh_act, "gas", startyear = 2016, endyear = 2019)$color == 
      "predicted"]
# 查看燃气用户比例变化
by_hh_ori_users_prop <- 
  func_nrg_intst(by_hh_act[c("year", "lpg", "gas")], 
                 by_hh_act, "household")

## Consumption and emission ----
by_hh_nrgsum_ls <- vector("list", length(global_hh_subsector))
names(by_hh_nrgsum_ls) <- global_hh_subsector
# hh_coal_elec
# 电力部分
by_hh_nrgsum_ls[[1]] <- 
  global_electricity_sec[, c("year", "#城乡居民生活用电")]
names(by_hh_nrgsum_ls[[1]]) <- c("year", "electricity")
# 煤炭部分
by_hh_nrgsum_ls[[1]] <- 
  func_merge_2(list(by_hh_nrgsum_ls[[1]], global_hh_coal))
names(by_hh_nrgsum_ls[[1]])[3] <- "rawcoal"
# hh_lpg
by_hh_nrgsum_ls[[2]] <- 
  global_ind_com_hh_lpg[, c("year", "生活消费")]
names(by_hh_nrgsum_ls[[2]]) <- c("year", "lpg")
# hh_gas
by_hh_nrgsum_ls[[3]] <- 
  global_com_hh_gas[c("year", "生活消费")]
names(by_hh_nrgsum_ls[[3]]) <- c("year", "gas")
# emission 
by_hh_nrgsum_df <- func_ls2df(by_hh_nrgsum_ls)
by_hh_emissum_df <- 
  func_emissum(by_hh_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_hh_nrgintst_ls <- 
  func_nrg_intst_ls(by_hh_nrgsum_ls, by_hh_act)


# Power generation ----
## Activity level ----
# 分成4部分计算：发电外用电，本地发电用电，本地发电，外调电量
# 发电外用电量
by_tfres_ori_elecuse <- 
  func_ls2df(list(by_agri_nrgsum_df, by_ind_nrgsum_df, by_const_nrgsum_df, 
                  by_trans_nrgsum_df, by_com_nrgsum_df, by_hh_nrgsum_df))
by_tfres_ori_elecuse <- by_tfres_ori_elecuse[c("year", "electricity")]
names(by_tfres_ori_elecuse)[2] <- "elecuse"

# 本地发电用电量
by_tfres_ori_tfelecuse <- 
  global_indscale_nrg_bysecagg$"电力、热力生产和供应业"[c("year", "electricity")]
names(by_tfres_ori_tfelecuse)[2] <- "tfelecuse"

# 本地发电量：继续拆分成清洁发电和火电
by_tfres_ori_elecgen <- 
  global_elecgen[c("year", "合计")]
by_tfres_ori_elecgen <- 
  func_nrg_sum(global_elecgen[c("year", "thrm_prop", "clean_prop")], 
               by_tfres_ori_elecgen, "合计")
names(by_tfres_ori_elecgen) <- c("year", "elecgen_thrm", "elecgen_clean")

# 合并以上几项
by_tfres_act <- 
  func_merge_2(
    list(by_tfres_ori_elecuse, by_tfres_ori_tfelecuse, by_tfres_ori_elecgen))

# 计算所需外调电力并将其分成火力发电和清洁发电
# 生成省电网发电结构
by_tfres_ori_provelecgenstr <- global_provelecgen
by_tfres_ori_provelecgenstr$sum <- 
  by_tfres_ori_provelecgenstr$"火电" + by_tfres_ori_provelecgenstr$"其他"
by_tfres_ori_provelecgenstr$thrm_prop <- 
  by_tfres_ori_provelecgenstr$"火电"/by_tfres_ori_provelecgenstr$sum
by_tfres_ori_provelecgenstr$clean_prop <- 
  by_tfres_ori_provelecgenstr$"其他"/by_tfres_ori_provelecgenstr$sum
# 计算所需外调电力
by_tfres_ori_importelec <- 
  data.frame(
    year = by_tfres_act$year, 
    importelec = by_tfres_act$elecuse + by_tfres_act$tfelecuse - 
      by_tfres_act$elecgen_thrm - by_tfres_act$elecgen_clean)
by_tfres_ori_importelec <- 
  func_nrg_sum(by_tfres_ori_provelecgenstr[c("year", "thrm_prop","clean_prop")],
               by_tfres_ori_importelec, "importelec")
names(by_tfres_ori_importelec) <- c("year", "importthrm", "importclean")

# 合并活动水平
by_tfres_act <- func_merge_2(list(by_tfres_act, by_tfres_ori_importelec))


## Consumption and emission ----
by_tf_nrgsum_df <- global_indscale_nrg_bysecagg$"电力、热力生产和供应业"
by_tf_emissum_df <- func_emissum(by_tf_nrgsum_df, global_emisfac_df)

## Energy intensity ----
by_tf_nrgintst <- func_nrg_intst(by_tf_nrgsum_df, by_tfres_act, "elecgen_thrm")

# Imported elec ----
## Energy intensity ----
# 读取省电网发电能耗量
global_provelecgen_nrgsum <- 
  func_read_trans("S3CNPRZE")[c("year", "原煤", "柴油", "燃料油", "天然气")]
names(global_provelecgen_nrgsum) <- c("year", "rawcoal", "diesel", "residual", "gas")

# 火电能耗强度
by_res_nrgintst <- 
  func_nrg_intst(global_provelecgen_nrgsum, global_provelecgen, "火电")

## Consumption and emission ----
by_res_nrgsum_df <- func_nrg_sum(by_res_nrgintst, by_tfres_act, "importthrm")
by_res_emissum_df <- func_emissum(by_res_nrgsum_df, global_emisfac_df)


# RESULT ----
## Total energy ----
if (set_by_elecequalfac_meth == TRUE) { ### Elecequalfac meth ----
  ### Energy by secs ----
  # 计算外调电力火电折标煤系数
  by_tot_ori_elecequalfac <- 
    func_elecequalfac(by_res_nrgsum_df, by_tfres_act[c("year", "importthrm")])
  # 计算外调电力折标煤量
  by_tot_ori_elecequal <- func_cross(
    by_tot_ori_elecequalfac, 
    func_cross(by_tfres_act[c("year", "importthrm")], 
               by_tfres_act[c("year", "importclean")], method = "sum"), 
    method = "product")
  
  # 计算本地发电标准煤量
  by_tot_ori_elecgenequal <- 
    func_toce(by_tf_nrgsum_df, agg = TRUE)
  # 计算本地发电和外调电力标准煤量之和
  by_tot_ori_elecstdcoal <- 
    func_cross(by_tot_ori_elecequal, by_tot_ori_elecgenequal, method = "sum")
  
  # 各部门电力消费占比
  by_tot_elecbysec <- 
    func_mrgcol(list(by_agri_nrgsum_df, by_ind_nrgsum_df, 
                     by_const_nrgsum_df, by_trans_nrgsum_df, 
                     by_com_nrgsum_df, by_hh_nrgsum_df), 
                "electricity", namesnew = global_sectors[1:6])
  by_tot_elecsharebysec <- 
    func_nrg_intst(by_tot_elecbysec, by_tfres_act, "elecuse")
  
  # 分配电力标准煤量到各部门
  by_tot_elecstdcoalbysec <- 
    func_nrg_sum(by_tot_elecsharebysec, by_tot_ori_elecstdcoal, "nrg_input")
  
  # 计算各部门能耗标准量
  by_agri_nrgsumce <- func_cross(
    func_toce(by_agri_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "agri")], method = "sum")
  by_ind_nrgsumce <- func_cross(
    func_toce(by_ind_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "ind")], method = "sum")
  by_const_nrgsumce <- func_cross(
    func_toce(by_const_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "const")], method = "sum")
  by_trans_nrgsumce <- func_cross(
    func_toce(by_trans_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "trans")], method = "sum")
  by_com_nrgsumce <- func_cross(
    func_toce(by_com_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "com")], method = "sum")
  by_hh_nrgsumce <- func_cross(
    func_toce(by_hh_nrgsum_df, agg = TRUE), 
    by_tot_elecstdcoalbysec[c("year", "hh")], method = "sum")
  # 合并各部门
  by_tot_nrgsecce <- func_mrgcol(list(
    by_agri_nrgsumce, by_ind_nrgsumce, by_const_nrgsumce, 
    by_trans_nrgsumce, by_com_nrgsumce, by_hh_nrgsumce), 
    "stdcoal", global_sectors[1: 6])
  
  ### Total energy ----
  # 计算能耗标准量之和
  by_tot_nrgsumce <- data.frame(
    year = by_tot_nrgsecce$year, 
    energyconsump = rowSums(by_tot_nrgsecce[, -1]))
  
  ### Energy by fuels ----
  # 除了电力外其他能耗物理量及标准煤
  by_tot_nrgfuel <- 
    func_ls2df(list(by_agri_nrgsum_df, by_ind_nrgsum_df, by_const_nrgsum_df, 
                    by_trans_nrgsum_df, by_com_nrgsum_df, by_hh_nrgsum_df, 
                    by_tf_nrgsum_df))
  by_tot_nrgfuelce <- func_toce(by_tot_nrgsec)
  # 加上电力标准量
  by_tot_nrgfuelce <- 
    func_merge_2(list(by_tot_nrgfuelce, by_tot_ori_elecequal))
  names(by_tot_nrgfuelce)[names(by_tot_nrgfuelce) == "nrg_input"] <- "electricity"
    
  # 聚合成煤油气电
  by_tot_nrgaggfuel <- func_secagg(by_tot_nrgfuel, global_nrg_lookup)
  by_tot_nrgaggfuelce <- func_secagg(by_tot_nrgfuelce, global_nrg_lookup)
} else {
  # 除电力外的其他能耗之和
  by_tot_nrgsum_byfuel <- 
    func_ls2df(list(by_agri_nrgsum_df, by_ind_nrgsum_df, by_const_nrgsum_df, 
                    by_trans_nrgsum_df, by_com_nrgsum_df, by_hh_nrgsum_df, 
                    by_tf_nrgsum_df, by_res_nrgsum_df))
  by_tot_nrgsum_byfuel <- by_tot_nrgsum_byfuel[names(by_tot_nrgsum_byfuel) != "electricity"]
  # 换算成标准煤
  by_tot_nrgsum_byfuel_ce <- func_toce(by_tot_nrgsum_byfuel)
  # 换算成各年份总和
  by_tot_nrgsum_ce <- data.frame(
    year = by_tot_nrgsum_byfuel_ce$year, 
    energyconsump = rowSums(by_tot_nrgsum_byfuel[names(by_tot_nrgsum_byfuel) != "year"]))
}


## Total emission ----
# 各部门用电量
by_tot_elecbysec <- 
  func_mrgcol(list(by_agri_nrgsum_df, by_ind_nrgsum_df, 
                   by_const_nrgsum_df, by_trans_nrgsum_df, 
                   by_com_nrgsum_df, by_hh_nrgsum_df), 
              "electricity", namesnew = global_sectors[1:6])
by_tot_elecsharebysec <- 
  func_nrg_intst(by_tot_elecbysec, by_tfres_act, "elecuse")

# 电力总排放
by_tot_elecemis <- 
  func_cross(by_tf_emissum_df, by_res_emissum_df, "sum")

# 分配电力排放到各个终端部门
by_tot_elecemisbysec <- 
  func_nrg_sum(by_tot_elecsharebysec, by_tot_elecemis, "co2")

# 汇总各部门直接排放
by_tot_diremisbysec <- 
  func_mrgcol(list(by_agri_emissum_df, by_ind_emissum_df, 
                   by_const_emissum_df, by_trans_emissum_df, 
                   by_com_emissum_df, by_hh_emissum_df), 
              "co2", namesnew = global_sectors[1:6])
# 终端部门总排放=电力排放+直接排放
by_tot_emisbysec <- func_cross(by_tot_elecemisbysec, by_tot_diremisbysec, "sum")
# 总排放
by_tot_emis <- data.frame(year = by_tot_emisbysec$year)
by_tot_emis$co2 <- rowSums(by_tot_emisbysec[names(by_tot_emisbysec) != "year"])

