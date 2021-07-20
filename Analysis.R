# SETTING ----
# 计算内容或口径相关设置
# 设置要计算的情景
set_scalcs <- "BAU_SLC_DECOAL_OTHER"
  # c("BAU", "BAU_WLC_OTHER", "BAU_SLCPLUS_OTHER", "BAU_SLC_DECOAL_OTHER")
set_nrgplng_scope <- FALSE # 是否采用能源规划口径
set_lowdev <- FALSE #是否采用经济低发展情景

# 缓存相关设置
set_cache_globalvar <- FALSE # 是否已有全局变量缓存
set_cache_nrgbal <- FALSE # 是否已有能源平衡表缓存
set_cache_hiscalc <- FALSE # 是否已有历史数据计算缓存
set_cache_init <- FALSE # 是否已有初始化缓存
set_cache_readdata <- TRUE # 是否已有数据读取缓存

# 结果相关设置
set_plotstyle <- "base" # 设置作图风格
set_figureexport <- FALSE # 是否输出图片

# 设置全局变量
if (set_cache_globalvar == FALSE) {
  # GLOBAL VAR ----
  ## Classification ----
  # 能源类别
  global_nrg_class <- 
    c("rawcoal", "coalproduct", 
      "gasoline", "diesel", "residual", "kerosene", "lpg", 
      "gas", "electricity")
  global_sectors <- c("agri", "ind", "const", "trans", "com", "hh","tf", "res")
  
  # 能源合并方式
  global_nrg_lookup <- 
    Reduce(rbind, list(
      data.frame(ind_agg = c("煤炭"), 
                           ind_ori = c("rawcoal", "coalproduct")),
      data.frame(ind_agg = c("油品"), 
                 ind_ori = c("gasoline", "diesel", "residual", "kerosene", 
                             "lpg")), 
      data.frame(ind_agg = c("天然气"), 
                 ind_ori = c("gas")), 
      data.frame(ind_agg = c("电力"), 
                 ind_ori = c("electricity")), 
      data.frame(ind_agg = c("非化石"), 
                 ind_ori = c("clean"))))
  
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
  
  # 工业行业根据能源消耗强度分类
  global_indsecclass_lookup <- 
    Reduce(rbind, 
      list(
        data.frame(ind_agg = c("高能耗传统行业"), 
                   ind_ori = c("化学工业", "食品饮料及烟草制造业", 
                               "非金属矿物制品业", "金属加工制造业", 
                               "石油及炼焦")), 
        data.frame(ind_agg = c("低能耗传统行业"), 
                   ind_ori = c("纺织及服装制造业", "木材及家具制造业", 
                               "造纸及印刷", "文体工美用品制造业", 
                               "其他制造业")), 
        data.frame(ind_agg = c("新兴行业"), 
                   ind_ori = c("医药制造业", "设备制造业", "电子电气制造业"))
      )
    )
  
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
  global_ind_finesec <- global_ind_lookup$ind_ori
  global_ind_nrgclass <- c("rawcoal", "coalproduct", 
                           "gasoline", "diesel", "residual", "lpg", 
                           "gas", "electricity")
  global_trans_subsector <- 
    c("常规公交", "快速公交", "出租车", "农村客车", "地铁", 
      "公路其他汽油", "纯电动私家车", "公路其他柴油", 
      "水路客运", "水路货运", "航空")
  
  # 服务业子部门
  global_com_subsector <- c("electricity", "lpg_and_gas")
  # 生活子部门
  global_hh_subsector <- 
    c("hh_coal_elec", "hh_lpg", "hh_gas")
  
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
  global_gdp <- func_read_trans("2VHEE264", "GDP")
  global_gdp <- global_gdp[names(global_gdp) != "人均GDP"]
  names(global_gdp)[names(global_gdp) == "#第一产业"] <- "agrigdp"
  names(global_gdp)[names(global_gdp) == "#第二产业"] <- "secgdp"
  names(global_gdp)[names(global_gdp) == "##工业"] <- "indgdp"
  names(global_gdp)[names(global_gdp) == "##建筑业"] <- "constgdp"
  names(global_gdp)[names(global_gdp) == "#第三产业"] <- "comgdp"
  # 基于当年价计算各产业比重
  global_gdp$agrigdp_prop <- global_gdp$agrigdp/global_gdp$GDP*100
  global_gdp$secgdp_prop <- global_gdp$secgdp/global_gdp$GDP*100
  global_gdp$indgdp_prop <- global_gdp$indgdp/global_gdp$GDP*100
  global_gdp$constgdp_prop <- global_gdp$constgdp/global_gdp$GDP*100
  global_gdp$comgdp_prop <- global_gdp$comgdp/global_gdp$GDP*100
  # 转换为2015年可比价
  global_gdp$GDP <- func_compprice(global_gdp, "地区生产总值指数", 2015)$GDP
  global_gdp$agrigdp <- global_gdp$GDP * global_gdp$agrigdp_prop/100
  global_gdp$secgdp <- global_gdp$GDP * global_gdp$secgdp_prop/100
  global_gdp$indgdp <- global_gdp$GDP * global_gdp$indgdp_prop/100
  global_gdp$constgdp <- global_gdp$GDP * global_gdp$constgdp_prop/100
  global_gdp$comgdp <- global_gdp$GDP * global_gdp$comgdp_prop/100
  
  # 预测GDP相关项目变化
  # 预测GDP：分正常发展情景和低发展情景
  if (set_lowdev == TRUE) {
    prj_global_gdp <- func_rate(
      baseyear = 2019, basevalue = global_gdp$GDP[global_gdp$year == 2019], 
      rate_df = # 未来GDP增长率减缓
        func_interp_2(year = c(2020, 2021, 2026, 2031, 2036, 2041, 2060),
                      value = c(5.8, 6.00, 5.00, 4.00, 3.00, 2.00, 1.00)))
  } else {
    prj_global_gdp <- func_rate(
      baseyear = 2019, basevalue = global_gdp$GDP[global_gdp$year == 2019], 
      rate_df = # 未来GDP增长率减缓
        func_interp_2(year = c(2020, 2021, 2025, 2030, 2035, 2040, 2060),
                      value = c(5.8, 7.50, 7.00, 6.00, 5.00, 3.00, 2.00)))
  }
  names(prj_global_gdp)[2] <- "GDP"
  comment(prj_global_gdp$GDP) <- "2015可比价万元"
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
  prj_global_gdp$constgdp <- prj_global_gdp$GDP * 
    prj_global_gdp$constgdp_prop/100
  prj_global_gdp$comgdp <- prj_global_gdp$GDP * prj_global_gdp$comgdp_prop/100
  
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
  
  prj_global_population$household <- 
    prj_global_population$population / 
    func_lastone(global_population$household_size)
  comment(prj_global_population$household) <- "万户"
}

# 读取数据
# 在该模块集中读取原始数据
if (set_cache_readdata == FALSE) {
  # Read data ----
  # 读取并构建排放因子数据
  global_ori_emisfac_df <- func_read_data("8C8EDJVH")[global_nrg_class]
  global_emisfac_df <- rep(
    as.numeric(global_ori_emisfac_df), each = length(c(2000: 2019)))
  global_emisfac_df <- matrix(
    global_emisfac_df, ncol = ncol(global_ori_emisfac_df))
  global_emisfac_df <- as.data.frame(global_emisfac_df)
  names(global_emisfac_df) <- names(global_ori_emisfac_df)
  global_emisfac_df <- cbind(year = c(2000: 2019), global_emisfac_df)
  # 预测排放因子变化：从2040年开始各类能耗开始脱碳，至2055年为0
  prj_emisfac_df <- cbind(
    data.frame(year = c(2019: 2060)), 
    sapply(global_nrg_class, function(i) {func_interp_3(
      year = c(2019, 2040, 2050, 2060), scale = c(1.0, 1.0, 0.7, 0.0), 
      base = global_emisfac_df[which(global_emisfac_df$year == 2019), i])$value}))
  
  # 读取规上工业各行业能耗分能耗类型-行业数据
  global_indscale_nrgls_bynrg <- func_read_multitable(
    "7TP7UDE6", 
    names_tbl = c(
      "煤", "煤制品", "汽油", "柴油", "燃料油", "液化石油气", "天然气", "电力"), 
    names_ls = c("rawcoal", "coalproduct", 
                 "gasoline", "diesel", "residual", "lpg","gas", "electricity"))
  for (i in names(global_indscale_nrgls_bynrg)) {
    global_indscale_nrgls_bynrg[[i]] <- 
      global_indscale_nrgls_bynrg[[i]][c("year", global_ind_finesec)]
    global_indscale_nrgls_bynrg[[i]][is.na(global_indscale_nrgls_bynrg[[i]])] <-
      0
  }
  comment(global_indscale_nrgls_bynrg) <- "规上工业能耗：8能源-35行业"
  
  # 聚合各行业：工业用能分能耗类型-聚合行业数据
  global_indscale_nrgls_bynrg_secagg <- 
    func_secagg_ls(global_indscale_nrgls_bynrg, global_ind_lookup)
  comment(global_indscale_nrgls_bynrg_secagg) <- 
    "规上工业能耗：8能源-14行业"
  
  # 转变成工业用能分聚合行业-能耗类型数据：包含电力热力供应业
  global_indscale_nrgaggsec <- 
    func_ls_transition(global_indscale_nrgls_bynrg_secagg)
  comment(global_indscale_nrgaggsec) <- 
    "规上工业能耗：14行业-8能源"
  
  # 转变成工业用能分聚合行业数据框：不包含电力热力供应业
  global_indscale_nrgaggsec_noelec <- 
    global_indscale_nrgaggsec[global_ind_subsector]
  comment(global_indscale_nrgaggsec_noelec) <- "规上工业能耗列表：13行业-8能源"
  
  # 压缩成数据框
  global_indscale_nrgaggsec_noelec_df <- 
    func_ls2df(global_indscale_nrgaggsec_noelec)
  comment(global_indscale_nrgaggsec_noelec_df) <- "规上工业能耗数据框：8能源"
  
  # 农业柴油
  global_agri_diesel <- 
    func_read_trans("4NJ97NS9")[, c("year", "农用柴油使用量")]
  
  # 生活原煤
  global_hh_coal <- func_read_trans("WJU7N3EL")
  global_hh_coal$"生活用煤" <- global_hh_coal$"生活用煤"*10000
  comment(global_hh_coal$"生活用煤") <- "吨"
  
  # 读取统计年鉴的农业柴油
  global_agri_diesel <- 
    func_read_trans("4NJ97NS9")[, c("year", "农用柴油使用量")]
  comment(global_agri_diesel$"农用柴油使用量")
  
  # 读取航空煤油数据：含福州机场部分的煤油消费量之和
  global_avnnrg <- func_read_trans("JXG6KGSA")
  # 补全包含福州机场消费量在内的总消费量
  global_avnnrg[which(global_avnnrg$year %in% c(2005: 2009)), "厦航煤油"] <- 
    global_avnnrg[which(global_avnnrg$year %in% c(2005: 2009)), "国内航班"] +
    global_avnnrg[which(global_avnnrg$year %in% c(2005: 2009)), "国际航班合计"]
  global_avnnrg <- global_avnnrg[c("year", "厦航煤油")]
  names(global_avnnrg) <- c("year", "kerosene")
  
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
  global_trans_gas <- func_read_trans("IZM9FWIY", "天然气消费量")
  
  # 读取交通电力消费并合并地铁用电
  # 此处仅进入部分车型用电量
  # 问题：网约车用电量不低，但因无保有量数据，难以从私家车区分出来，暂不处理
  global_trans_elecsec <- 
    func_read_trans("IZM9FWIY", "电力消费")
  global_trans_elecsec$"地铁" <- 
    global_trans_elecsec$"地铁牵引用电" + global_trans_elecsec$"地铁其他用电"
  global_trans_elecsec <- global_trans_elecsec[c(
    "year", "常规公交", "BRT", "纯电动出租车","地铁")]
  
  # 读取用电数据
  global_elecaggsec <- func_read_trans("2I4DKY2A", "全市电力消费情况表")
  global_elecfinesec <- 
    func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")
  
  # 读取本地发电数据并计算清洁和非清洁发电比例
  global_elecgen <- func_read_trans("2I4DKY2A", "全市发电量")
  global_elecgen$clean <- 
    global_elecgen$"#水电" + global_elecgen$"#垃圾发电" +
    global_elecgen$"#太阳能"
  global_elecgen$clean_prop <- global_elecgen$clean / global_elecgen$"合计"
  global_elecgen$thrm_prop <- 1 - global_elecgen$clean_prop
  
  # 读取水运燃料油
  global_trans_residual <- func_read_trans("68Z975NU")
  global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")] <- 
    global_trans_residual[c("国内客运", "国内货运", "国际客运", "国际货运")]*
    10000
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
    func_read_trans("IZM9FWIY", "柴油消费量")[
      c("year","常规公交","BRT","农村客车")]
  
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
  
  # 读取外调电力折标煤系数
  global_importthrm_cefac <- func_read_trans("2I4DKY2A", "XCP外调电力")
  global_importthrm_cefac <- 
    global_importthrm_cefac[c("year", "外调电力折标煤系数")]
  names(global_importthrm_cefac) <- c("year", "nrg_input")
}

# 初始化
# 在此模块构建情景和空的对象，以存放结果
if (set_cache_init == FALSE) {
  # INIT ----
  # 情景包括：惯性，弱低碳，强低碳，提前退煤，退煤5个情景
  init_scenarios <- set_scalcs
  
  # 输出结果的形式模板
  init_output_templatels <- vector("list", length(init_scenarios) + 1)
  names(init_output_templatels) <- c("BY", init_scenarios)
  # 构建所需输出变量
  for (i in c(
    # 各终端部门的活动水平、能耗强度和能耗总量
    paste(rep(global_sectors[1: 6], each = 5), 
          c("act", "nrgintst", "nrgfuel", "nrgsumce", 
            "diremissum"), sep = "_"), 
    # 能源部门的活动水平
    "tfres_act", 
    # 终端部门能耗强度和能耗总量
    paste(rep(global_sectors[7: 8], each = 4), 
          c("nrgintst", "nrgfuel", "nrgsumce", "diremissum"), 
          sep = "_"), 
    # 汇总部分能耗总量
    paste("tot", 
          c("nrgaggfuel", "nrgpropaggfuel", "nrgsecce", "nrgsumce", 
            "emissum", "emispergdp"), sep = "_"), 
    # 其他变量
    "tot_emisbysec_ls", "trans_carprop_ls", "ind_ori_act_prop", 
    "trans_carprop_ls")) {
    assign(i, init_output_templatels)
  }
  ind_nrgsecfuel <- init_output_templatels
  trans_nrgsecfuel <- init_output_templatels
  com_nrgsecfuel <- init_output_templatels
  hh_nrgsecfuel <- init_output_templatels
  tot_nrgaggfuelce <- init_output_templatels
  tot_emissec <- init_output_templatels
  tot_emisaggfuel <- init_output_templatels
}

# 构建能源平衡表
if (set_cache_nrgbal == FALSE) {
  # NRG BALANCE ----
  # 构建空能源平衡表
  by_nrgbal_years <- as.character(c(2015: 2019))
  by_nrgbal_ls <- lapply(
    # 每个年份都写入一个空的
    by_nrgbal_years, function(i) {data.frame(
      iterm = c("tf", "agri","ind","const", "trans","com", "hh"), 
      rawcoal = 0, coalproduct = 0, 
      gasoline = 0, diesel = 0, kerosene = 0, residual = 0, lpg = 0, 
      gas = 0, electricity = 0)})
  names(by_nrgbal_ls) <- by_nrgbal_years
  
  ## 1.1 Transformation input ----
  # 输入发电那一行
  for (j in by_nrgbal_years) {
    for (i in c("rawcoal", "coalproduct", 
                "gasoline", "diesel", "residual", "lpg", 
                "gas", "electricity")) {
      by_nrgbal_ls[[j]][which(by_nrgbal_ls[[j]]$iterm == "tf"),i] <- 
        global_indscale_nrgaggsec$"电力、热力生产和供应业"[which(
          global_indscale_nrgaggsec$"电力、热力生产和供应业"$year == j), i]
    }
  }
  
  for (i in by_nrgbal_years) {
    ## 1.2 Household rawcoal ----
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "rawcoal"] <- 
      global_hh_coal[which(global_hh_coal$year == i), "生活用煤"]
    ## 1.3 Agri diesel ----
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "agri"), "diesel"] <- 
      global_agri_diesel[which(global_agri_diesel$year == i), "农用柴油使用量"]
    ## 1.4 Trans kerosene ----
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "kerosene"] <- 
      global_avnnrg[which(global_avnnrg$year == i), "kerosene"]
    ## 1.5 Ind & Com & Household LPG ----
    by_nrgbal_ls[[i]][which(
      by_nrgbal_ls[[i]]$iterm %in% c("ind", "com", "hh")), "lpg"] <- 
      as.numeric(global_ind_com_hh_lpg[which(global_ind_com_hh_lpg$year == i), 
                                       c("工业", "服务业", "生活消费")])
    ## 1.6 Com & Household gas ----
    by_nrgbal_ls[[i]][which(
      by_nrgbal_ls[[i]]$iterm %in% c("com", "hh")), "gas"] <- 
      as.numeric(global_com_hh_gas[which(
        global_com_hh_gas$year == i), c("服务业", "生活消费")])
    ## 1.7 Trans gas ----
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gas"] <- 
      sum(global_trans_gas[which(
        global_trans_gas$year == i), c("公交合计", "出租车合计")])
  }
  
  ## 1.8 Electricity ----
  for (i in by_nrgbal_years) {
    # 发电量
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "tf"), "electricity"] <- 
      global_elecgen[which(global_elecgen$year == i), "合计"]
    # 农业用电
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "agri"), "electricity"] <- 
      global_elecaggsec[which(global_elecaggsec$year == i), "##第一产业"]
    # 建筑业用电
    by_nrgbal_ls[[i]][which(
      by_nrgbal_ls[[i]]$iterm == "const"), "electricity"] <- 
      global_elecfinesec[which(global_elecfinesec$year == i), "建筑业"]
    # 工业用电
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "electricity"] <- 
      global_elecaggsec[which(global_elecaggsec$year == i), "##第二产业"] - 
      global_elecfinesec[which(global_elecfinesec$year == i), "建筑业"]
    # 交通用电量
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "electricity"] <- 
      global_trans_elecsec[which(global_trans_elecsec$year == i), "常规公交"] + 
      global_trans_elecsec[which(global_trans_elecsec$year == i), "纯电动出租车"] + 
      global_trans_elecsec[which(global_trans_elecsec$year == i), "地铁"]
    # 服务业用电
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "com"), "electricity"] <- 
      global_elecaggsec[which(global_elecaggsec$year == i), "##第三产业"] - 
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "electricity"]
    # 生活用电
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "electricity"] <- 
      global_elecaggsec[which(global_elecaggsec$year == i), "##第三产业"]
  }
  
  ## 2.1 Ind oil and coalproduct ----
  # 构造每年全市工业GDP/规上工业GDP缩放因子
  by_nrgbal_ind_scalefac <- func_cross(
    global_gdp[c("year", "indgdp")], global_indscale_gdp, method = "rate")
  names(by_nrgbal_ind_scalefac)[2] <- "scalefac"
  # 不包含电力热力行业的工业煤制品和油品能耗数据
  by_nrgbal_indscale_coalproductoil <- 
    func_ls2df(global_indscale_nrgaggsec[global_ind_subsector])
  by_nrgbal_indscale_coalproductoil <- by_nrgbal_indscale_coalproductoil[
    c("year", "coalproduct", "gasoline", "diesel", "residual")]
  # 进行缩放
  by_nrgbal_ind_oil <- func_nrg_sum(
    by_nrgbal_indscale_coalproductoil, by_nrgbal_ind_scalefac, "scalefac")
  
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), 
                      c("coalproduct", "gasoline", "diesel", "residual")] <- 
      by_nrgbal_ind_oil[which(by_nrgbal_ind_oil$year == i), 
                        c("coalproduct", "gasoline", "diesel", "residual")]
  }
  
  ## 3.2 Ind coal ----
  # 4 = 若谷煤合计-刚算的煤制品-生活用煤-发电用煤
  # 读取统计局物理量总量数据
  by_checknrgaggfuel <- func_read_trans("LPLPNXCQ")
  by_checknrgaggfuel[
    c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")] <- 
    by_checknrgaggfuel[
      c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")]*10000
  by_checknrgaggfuel[
    c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")] <- 
    func_addnote(
      by_checknrgaggfuel[
        c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")], 
      c("吨", "吨", "万立方米", "万千瓦时"))
  
  # 读取统计局标准量总量数据
  by_checknrgaggfuelce <- func_read_trans("LPLPNXCQ", "XCP_煤油气电标准量")
  by_checknrgaggfuelce[
    c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")] <- 
    by_checknrgaggfuelce[
      c("煤炭消费量", "油品消费量", "天然气消费量", "调入电力")]*10000
  
  # 计算各年份工业原煤消费量
  # 将值赋给能源平衡表
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "rawcoal"] <- 
      func_alter(
        by_checknrgaggfuelce[which(
          by_checknrgaggfuelce$year == i), "煤炭消费量"] - 
          # 刚算的煤制品
          func_alter(by_nrgbal_ls[[i]][which(
            by_nrgbal_ls[[i]]$iterm == "ind"), "coalproduct"], 
            "coalproduct", "ce") - 
          # 生活用煤
          func_alter(by_nrgbal_ls[[i]][which(
            by_nrgbal_ls[[i]]$iterm == "hh"), "rawcoal"], "rawcoal", "ce") -
          # 发电用煤
          func_alter(by_nrgbal_ls[[i]][which(
            by_nrgbal_ls[[i]]$iterm == "tf"), "rawcoal"], "rawcoal", "ce"), 
        "ce", "rawcoal"
      )
  }
  
  ## 3.3 Trans residual ----
  # 9 = 水运燃料油消耗量（港口局数据推算，慧梅）
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "residual"] <- 
      sum(global_trans_residual[which(
        global_trans_residual$year == i), 
        c("国内客运", "国内货运", "国际客运", "国际货运")])
  }
  
  ## 3.4 Trans diesel ----
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "diesel"] <- 
      sum(global_water_railway_diesel[which(
        global_water_railway_diesel$year == i), 
        c("水运国内客运", "水运国内货运","水运国际客运","水运国际货运","铁路")], 
        na.rm = TRUE) +
      # 公路营运
      sum(global_roadoper_diesel[which(
        global_roadoper_diesel$year == i), c("常规公交","BRT","农村客车")], 
        na.rm = TRUE) +
      # 公路非营运
      sum(global_roadnonoper_diesel[which(
        global_roadnonoper_diesel$year == i), c("非营运客车","货车")], 
        na.rm = TRUE)
  }
  
  ## 3.5 Trans gasoline ----
  # 18 = 交通汽油柴油合计量 = 全部油品总量-目前已有的数据
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gasoline"] <- 
      # 各年份油品总量
      by_checknrgaggfuel[which(by_checknrgaggfuel$year == i), "油品消费量"] - 
      # 目前已有的各类油耗数据
      sum(
        by_nrgbal_ls[[i]][,c("gasoline","diesel","kerosene","residual","lpg")])
  }
  
  ## 3.6  Ind gas----
  # 7 = 若谷天然气总量-生活消费-服务业-交通-发电
  for (i in by_nrgbal_years) {
    by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "ind"), "gas"] <- 
      # 若谷核对统计局总量
      by_checknrgaggfuel[which(by_checknrgaggfuel$year == i), "天然气消费量"] - 
      # 生活消费
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "hh"), "gas"] -
      # 服务业
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "com"), "gas"] -
      # 交通
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gas"] -
      # 发电
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "tf"), "gas"]
  }
}

# 计算历史能耗和排放量
if (set_cache_hiscalc == FALSE) {
  # HISTORY ----
  ## Agri ----
  ### Activity level ----
  # 农业的播种面积
  # 读取《碳排放峰值模型参数选择检验》
  agri_act[["BY"]] <- func_read_trans("4NJ97NS9")
  agri_act[["BY"]] <- 
    agri_act[["BY"]][, c("year", "全年农作物总播种面积")]
  names(agri_act[["BY"]])[2] <- "agri"
  agri_act[["BY"]]$agri <- 
    agri_act[["BY"]]$agri/1500
  comment(agri_act[["BY"]]$agri) <- "平方公里"
  
  ### Consumption and emission ----
  # 读取《碳排放峰值模型参数选择检验》
  by_agri_ori_diesel <- global_agri_diesel
  names(by_agri_ori_diesel)[2] <- "diesel"
  # 读取《厦门市电力数据》
  by_agri_ori_electricity <- global_elecaggsec[, c("year", "##第一产业")]
  names(by_agri_ori_electricity)[2] <- "electricity"
  # 合并活动水平
  agri_nrgfuel[["BY"]] <- 
    func_merge_2(list(by_agri_ori_diesel, by_agri_ori_electricity))
  # 计算排放量
  agri_diremissum[["BY"]] <- 
    func_emissum(agri_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ----
  agri_nrgintst[["BY"]] <- 
    func_nrg_intst(agri_nrgfuel[["BY"]], agri_act[["BY"]], "agri")
  
  
  ## Industry ----
  ### Activity level ----
  # 问题：需要通过调研补全规上各行业GDP数据
  # 读取规上工业各行业GDP
  by_ind_ori_act_scale <- func_secagg(
    global_indscale_gdp4sctr, global_ind_lookup)
  # 计算规上工业各行业所占比例
  ind_ori_act_prop[["BY"]] <- 
    by_ind_ori_act_scale[, -1]/rowSums(by_ind_ori_act_scale[, -1])*100
  ind_ori_act_prop[["BY"]]$year <- by_ind_ori_act_scale$year
  ind_ori_act_prop[["BY"]] <- ind_ori_act_prop[["BY"]][c("year", global_ind_ori_subsector)]
  # 假设2018-2019年规上工业各行业比例同2017年
  ind_ori_act_prop[["BY"]][ind_ori_act_prop[["BY"]]$year %in% c(2018, 2019),
                      global_ind_ori_subsector] <- 
    ind_ori_act_prop[["BY"]][ind_ori_act_prop[["BY"]]$year == 2017,][global_ind_ori_subsector]
  # 作图：func_propplot(ind_ori_act_prop[["BY"]])
  # 活动强度为全市工业各行业GDP：剔除电力、热力生产和供应业
  ind_act[["BY"]] <- func_nrg_sum(ind_ori_act_prop[["BY"]],global_gdp,"indgdp")
  ind_act[["BY"]] <- ind_act[["BY"]][c("year", global_ind_subsector)]
  ind_act[["BY"]][global_ind_subsector] <- 
    ind_act[["BY"]][global_ind_subsector]/100
  
  
  ### Consumption and emission----
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
        global_indscale_nrgaggsec_noelec_df[which(
          global_indscale_nrgaggsec_noelec_df$year == i), j]
    }
  }
  
  # 缩放数据
  for (i in global_nrg_class[global_nrg_class %in% "kerosene" == FALSE]) {
    by_ind_ori_nrgsum_bynrg_ls[[i]] <- 
      func_nrg_sum(by_ind_ori_nrgsum_bynrg_ls[[i]], 
                   by_ind_ori_scalefac, i)
  }
  
  # 转化为按行业分的能耗总量并剔除电力热力行业
  ind_nrgsecfuel[["BY"]] <- func_ls_transition(by_ind_ori_nrgsum_bynrg_ls)
  ind_nrgsecfuel[["BY"]] <- ind_nrgsecfuel[["BY"]][global_ind_subsector]
  # 计算工业的各类能耗总量
  ind_nrgfuel[["BY"]] <- func_ls2df(ind_nrgsecfuel[["BY"]])
  # 计算排放量
  ind_diremissum[["BY"]] <- func_emissum(ind_nrgfuel[["BY"]], global_emisfac_df)
  
  
  ### Energy intensity ---- 
  ind_nrgintst[["BY"]] <- 
    func_nrg_intst_ls(ind_nrgsecfuel[["BY"]], ind_act[["BY"]])
  
  
  ## Construction ----
  ### Activity level ----
  const_act[["BY"]] <- global_gdp[, c("year", "constgdp")]
  names(const_act[["BY"]])[2] <- "const_gdp"
  
  ### Consumption and emission ----
  # 读取《厦门市电力数据》
  const_nrgfuel[["BY"]] <- global_elecfinesec[, c("year", "建筑业")]
  names(const_nrgfuel[["BY"]]) <- c("year", "electricity")
  const_diremissum[["BY"]] <- 
    func_emissum(const_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ----
  const_nrgintst[["BY"]] <- 
    func_nrg_intst(const_nrgfuel[["BY"]], const_act[["BY"]], "const_gdp")
  
  
  ## Transportation ----
  # 问题：轨道交通能耗呢？
  ### Activity level ----
  trans_act$BY <- func_branch(global_trans_subsector, c(2005: 2019))
  
  # 营运车辆里程数
  by_trans_operation <- func_read_trans("IZM9FWIY", "里程数")
  by_trans_operation <- 
    by_trans_operation[, c(
      "year", "常规公交", "BRT", "出租车", "农村客车", "地铁")]
  names(by_trans_operation) <- 
    c("year", "常规公交", "快速公交", "出租车", "农村客车", "地铁")
  trans_act$BY <- func_jigsaw(by_trans_operation, trans_act$BY)
  
  # 公路其他汽油：私家车保有量；纯电动私家车：保有量
  by_trans_nonoperation <- 
    func_read_trans("Y3PGVSR7")[c("year", "#常规私家车", "#纯电动私家车")]
  names(by_trans_nonoperation) <- 
    c("year", "公路其他汽油", "纯电动私家车")
  # 且假设纯电动私家车在2019年之前均为0
  by_trans_nonoperation[which(
    by_trans_nonoperation$year %in% c(2010: 2018)), "纯电动私家车"] <- 0
  # 嵌入活动水平数据框中
  trans_act$BY <- func_jigsaw(by_trans_nonoperation, trans_act$BY)
  
  # 公路其他柴油：货运周转量
  trans_act$BY$公路其他柴油[trans_act$BY$year %in% c(2017:2019)] <- 
    c(1919251, 2037836, 2216748)
  
  # 水路客运周转量和水路货运周转量
  by_trans_water <- global_water_act
  names(by_trans_water) <- c("year", "水路客运", "水路货运")
  trans_act$BY <- func_jigsaw(by_trans_water, trans_act$BY)
  
  # 航空客运周转量：非能源规划口径下设置为0
  if (set_nrgplng_scope == TRUE) { 
    #### Nrgplng scope ----
    by_trans_ori_avn <- global_avn_act[c("year", "avn_rpk")]
    names(by_trans_ori_avn) <- c("year", global_trans_subsector[10])
  } else {
    by_trans_ori_avn <- data.frame(year = c(2005: 2019), 航空 = c(0))
  }
  trans_act$BY <- func_jigsaw(by_trans_ori_avn, trans_act$BY)
  
  # 假设：出租车2018-2019年数据和农村客车2015-2019年数据为历史数据线性外推
  trans_act[["BY"]][which(
    trans_act[["BY"]]$year %in% c(2018:2019)), "出租车"] <- 
    tail(func_linear(trans_act[["BY"]], "出租车", 
                     startyear = 2018, endyear = 2019)[, "出租车"], 2)
  trans_act[["BY"]][which(
    trans_act[["BY"]]$year %in% c(2015:2019)), "农村客车"] <- 
    tail(func_linear(trans_act[["BY"]], "农村客车", 
                     startyear = 2015, endyear = 2019)[, "农村客车"], 5)
  
  ### Consumption and emission ---- 
  # 定义存储数据框
  trans_nrgsecfuel[["BY"]] <- vector("list", length(global_trans_subsector))
  names(trans_nrgsecfuel[["BY"]]) <- global_trans_subsector
  
  # 营运车辆能耗总量
  by_trans_nrgfuel_ori <- vector("list", 4)
  names(by_trans_nrgfuel_ori) <- c("gasoline", "diesel", "gas", "electricity")
  # 汽油消费量
  by_trans_nrgfuel_ori[[1]] <- 
    func_read_trans("IZM9FWIY", "汽油消费量")[, c("year", "出租车合计")]
  names(by_trans_nrgfuel_ori[[1]])<- c("year", "出租车")
  # 柴油消费量
  by_trans_nrgfuel_ori[[2]] <- global_roadoper_diesel
  names(by_trans_nrgfuel_ori[[2]]) <- c("year", "常规公交", "快速公交", "农村客车")
  # 天然气消费量
  by_trans_nrgfuel_ori[[3]] <- global_trans_gas[c("year", "公交合计", "出租车合计")]
  names(by_trans_nrgfuel_ori[[3]]) <- c("year", "常规公交", "出租车")
  # 电力消费量
  by_trans_nrgfuel_ori[[4]] <- 
    global_trans_elecsec[c("year", "常规公交", "纯电动出租车", "地铁")]
  names(by_trans_nrgfuel_ori[[4]]) <- c("year", "常规公交", "出租车", "地铁")
  # 转化为按车辆类型分的能耗量列表并整理各元素顺序
  trans_nrgsecfuel[["BY"]][global_trans_subsector[1: 5]] <- 
    func_ls_transition(by_trans_nrgfuel_ori)
  
  # 其他汽油 = 能源平衡表汽油总量扣除当前汽油之和
  trans_nrgsecfuel[["BY"]][["公路其他汽油"]] <- 
    data.frame(year = by_nrgbal_years)
  trans_nrgsecfuel[["BY"]][["公路其他汽油"]]$gasoline <- sapply(
    by_nrgbal_years, function(i) {
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "gasoline"] -
        by_trans_nrgfuel_ori[["gasoline"]][which(
          by_trans_nrgfuel_ori[["gasoline"]]$year == i), "出租车"]})
  
  # 纯电动私家车：能耗总量为此前估算过的私家车每车能耗和上面活动水平部分计算的纯
  # 电动私家车数量之乘积
  trans_nrgsecfuel[["BY"]][["纯电动私家车"]] <- data.frame(
    year = trans_act[["BY"]]$year, 
    electricity = trans_act[["BY"]]$"纯电动私家车" * 1.249542)
  
  # 水路客运能耗
  if (set_nrgplng_scope == TRUE) {
    trans_nrgsecfuel[["BY"]][["水路客运"]] <- 
      func_merge_2(list(
        func_cross(global_water_railway_diesel[c("year", "水运国内客运")], 
                   global_water_railway_diesel[c("year", "水运国际客运")],
                   "sum"), 
        func_cross(global_trans_residual[c("year", "国内客运")], 
                   global_trans_residual[c("year", "国际客运")], "sum")))
  } else {
    trans_nrgsecfuel[["BY"]][["水路客运"]] <- 
      func_merge_2(list(
        global_water_railway_diesel[c("year", "水运国内客运")], 
        global_trans_residual[c("year", "国内客运")]))
  }
  names(trans_nrgsecfuel[["BY"]]$水路客运)[2:3] <- c("diesel", "residual")
  
  # 水路货运能耗
  if (set_nrgplng_scope == TRUE) {
    trans_nrgsecfuel[["BY"]][["水路货运"]] <- 
      func_merge_2(list(
        func_cross(global_water_railway_diesel[c("year", "水运国内货运")], 
                   global_water_railway_diesel[c("year", "水运国际货运")], 
                   "sum"), 
        func_cross(global_trans_residual[c("year", "国内货运")], 
                   global_trans_residual[c("year", "国际货运")], "sum")))
  } else {
    trans_nrgsecfuel[["BY"]][["水路货运"]] <- 
      func_merge_2(list(
        global_water_railway_diesel[c("year", "水运国内货运")], 
        global_trans_residual[c("year", "国内货运")]))
  }
  names(trans_nrgsecfuel[["BY"]]$水路货运)[2:3] <- c("diesel", "residual")
  
  # 其他柴油 = 能源平衡表柴油总量扣除当前柴油之和
  trans_nrgsecfuel[["BY"]][["公路其他柴油"]] <- 
    data.frame(year = by_nrgbal_years)
  trans_nrgsecfuel[["BY"]][["公路其他柴油"]]$diesel <- sapply(
    by_nrgbal_years, 
    function(i) {
      by_nrgbal_ls[[i]][which(by_nrgbal_ls[[i]]$iterm == "trans"), "diesel"] -
        # 公路运输的柴油消费
        sum(by_trans_nrgfuel_ori[["diesel"]][which(
          by_trans_nrgfuel_ori[["gasoline"]]$year == i), 
          c("常规公交", "快速公交", "农村客车")]) -
        # 水路客运柴油消费
        trans_nrgsecfuel[["BY"]][["水路客运"]][which(
          trans_nrgsecfuel[["BY"]][["水路客运"]]$year == i), "diesel"] -
        # 水路货运柴油消费
        trans_nrgsecfuel[["BY"]][["水路货运"]][which(
          trans_nrgsecfuel[["BY"]][["水路货运"]]$year == i), "diesel"]})
  
  # 航空煤油：非能源规划口径下设置为0
  if (set_nrgplng_scope == TRUE) {
    trans_nrgsecfuel[["BY"]][["航空"]] <- 
      global_avnnrg[c("year", "kerosene")]
  } else {
    trans_nrgsecfuel[["BY"]][["航空"]] <- 
      data.frame(year = c(2005: 2019), kerosene = c(0))
  }
  
  # 能耗总量和排放
  trans_nrgfuel[["BY"]] <- func_ls2df(trans_nrgsecfuel[["BY"]])
  trans_diremissum[["BY"]] <- func_emissum(trans_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ----
  trans_nrgintst[["BY"]] <- 
    func_nrg_intst_ls(trans_nrgsecfuel[["BY"]], trans_act[["BY"]])
  # 非能源规划口径下设置航空煤油强度为0
  if (set_nrgplng_scope == FALSE) {
    trans_nrgintst[["BY"]][["航空"]] <- 
      data.frame(year = c(2005: 2019), kerosene = c(0))
  }
  
  
  ## Service -----
  ### Activity level ----
  # 服务业从业人口
  com_act[["BY"]] <- func_read_trans("2VHEE264", "从业人口")
  com_act[["BY"]] <- com_act[["BY"]][, c("year", "第三产业")]
  names(com_act[["BY"]])[2] <- "com_employee"
  # 补全2015-2019年数据：假设线性外推
  com_act[["BY"]][which(
    com_act[["BY"]]$year %in% c(2015:2019)), "com_employee"] <- 
    func_linear(com_act[["BY"]], "com_employee", 
                startyear = 2015, endyear = 2019)$com_employee[16:20]
  # 服务业GDP
  com_act[["BY"]]$com_employee <- com_act[["BY"]]$com_employee/10000
  comment(com_act[["BY"]]$com_employee) <- "万人"
  com_act[["BY"]] <- merge(
    com_act[["BY"]], global_gdp[c("year", "comgdp")], by = "year")
  names(com_act[["BY"]])[3] <- "com_gdp"
  
  ### Consumption and emission ----
  com_nrgsecfuel[["BY"]] <- vector("list", 2)
  names(com_nrgsecfuel[["BY"]]) <- global_com_subsector
  # 读取厦门市用电数据
  # 从服务业中扣除交通电力消费量
  com_nrgsecfuel[["BY"]][[1]] <- 
    func_cross(global_elecaggsec[c("year", "##第三产业")], 
               trans_nrgfuel$BY[c("year", "electricity")], 
               "difference")
  names(com_nrgsecfuel[["BY"]][[1]])[2] <- "electricity"
  # 读取厦门市服务业LPG消费
  com_nrgsecfuel[["BY"]][[2]] <- 
    func_merge_2(list(global_ind_com_hh_lpg[c("year", "服务业")], 
                      global_com_hh_gas[c("year", "服务业")]))
  names(com_nrgsecfuel[["BY"]][[2]])[2:3] <- c("lpg", "gas")
  # 设置由服务业GDP驱动的用电量
  com_nrgsecfuel[["BY"]][[2]]$electricity <- 0
  # 生成能耗总量数据框
  com_nrgfuel[["BY"]] <- func_ls2df(com_nrgsecfuel[["BY"]])
  # 计算排放量
  com_diremissum[["BY"]] <- func_emissum(com_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ---- 
  com_nrgintst[["BY"]] <- func_nrg_intst_ls(com_nrgsecfuel[["BY"]], com_act[["BY"]])
  names(com_nrgintst[["BY"]]) <- global_com_subsector
  
  
  ## Household ----
  ### Activity level ----
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
  hh_act[["BY"]] <- 
    func_merge_2(list(by_ori_household, by_ori_lpguser, by_ori_gasuser))
  # 通过线性拟合补全2016-2019年燃气用户数据
  hh_act[["BY"]]$lpg[hh_act[["BY"]]$year > 2015] <- 
    func_linear(hh_act[["BY"]], "lpg", startyear = 2016, endyear = 2019)$lpg[
      func_linear(
        hh_act[["BY"]], "lpg", startyear = 2016, endyear = 2019)$color == 
        "predicted"]
  hh_act[["BY"]]$gas[hh_act[["BY"]]$year > 2015] <- 
    func_linear(hh_act[["BY"]], "gas", startyear = 2016, endyear = 2019)$gas[
      func_linear(
        hh_act[["BY"]], "gas", startyear = 2016, endyear = 2019)$color == 
        "predicted"]
  # 查看燃气用户比例变化
  by_hh_ori_users_prop <- 
    func_nrg_intst(hh_act[["BY"]][c("year", "lpg", "gas")], 
                   hh_act[["BY"]], "household")
  
  ### Consumption and emission ----
  hh_nrgsecfuel[["BY"]] <- vector("list", length(global_hh_subsector))
  names(hh_nrgsecfuel[["BY"]]) <- global_hh_subsector
  # hh_coal_elec
  # 电力部分
  hh_nrgsecfuel[["BY"]][[1]] <- 
    global_elecaggsec[, c("year", "#城乡居民生活用电")]
  names(hh_nrgsecfuel[["BY"]][[1]]) <- c("year", "electricity")
  # 煤炭部分
  hh_nrgsecfuel[["BY"]][[1]] <- 
    func_merge_2(list(hh_nrgsecfuel[["BY"]][[1]], global_hh_coal))
  names(hh_nrgsecfuel[["BY"]][[1]])[3] <- "rawcoal"
  # hh_lpg
  hh_nrgsecfuel[["BY"]][[2]] <- 
    global_ind_com_hh_lpg[, c("year", "生活消费")]
  names(hh_nrgsecfuel[["BY"]][[2]]) <- c("year", "lpg")
  # hh_gas
  hh_nrgsecfuel[["BY"]][[3]] <- 
    global_com_hh_gas[c("year", "生活消费")]
  names(hh_nrgsecfuel[["BY"]][[3]]) <- c("year", "gas")
  # emission 
  hh_nrgfuel[["BY"]] <- func_ls2df(hh_nrgsecfuel[["BY"]])
  hh_diremissum[["BY"]] <- 
    func_emissum(hh_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ----
  hh_nrgintst[["BY"]] <- 
    func_nrg_intst_ls(hh_nrgsecfuel[["BY"]], hh_act[["BY"]])
  
  
  ## Power generation ----
  ### Activity level ----
  # 分成4部分计算：发电外用电，本地发电用电，本地发电，外调电量
  # 发电外用电量
  by_tfres_ori_elecuse <- 
    func_ls2df(list(agri_nrgfuel[["BY"]], ind_nrgfuel[["BY"]], const_nrgfuel[["BY"]], 
                    trans_nrgfuel[["BY"]], com_nrgfuel[["BY"]], hh_nrgfuel[["BY"]]))
  by_tfres_ori_elecuse <- by_tfres_ori_elecuse[c("year", "electricity")]
  names(by_tfres_ori_elecuse)[2] <- "elecuse"
  
  # 本地发电用电量
  by_tfres_ori_tfelecuse <- 
    global_indscale_nrgaggsec$"电力、热力生产和供应业"[c("year", "electricity")]
  names(by_tfres_ori_tfelecuse)[2] <- "tfelecuse"
  
  # 本地发电量：继续拆分成清洁发电和火电
  by_tfres_ori_elecgen <- 
    global_elecgen[c("year", "合计")]
  by_tfres_ori_elecgen <- 
    func_nrg_sum(global_elecgen[c("year", "thrm_prop", "clean_prop")], 
                 by_tfres_ori_elecgen, "合计")
  names(by_tfres_ori_elecgen) <- c("year", "elecgen_thrm", "elecgen_clean")
  
  # 合并以上几项
  tfres_act[["BY"]] <- 
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
      year = tfres_act[["BY"]]$year, 
      importelec = tfres_act[["BY"]]$elecuse + tfres_act[["BY"]]$tfelecuse - 
        tfres_act[["BY"]]$elecgen_thrm - tfres_act[["BY"]]$elecgen_clean)
  by_tfres_ori_importelec <- func_nrg_sum(
    by_tfres_ori_provelecgenstr[c("year", "thrm_prop","clean_prop")],
    by_tfres_ori_importelec, "importelec")
  names(by_tfres_ori_importelec) <- c("year", "importthrm", "importclean")
  
  # 合并活动水平
  tfres_act[["BY"]] <- func_merge_2(list(tfres_act[["BY"]], by_tfres_ori_importelec))
  
  
  ### Consumption and emission ----
  tf_nrgfuel[["BY"]] <- global_indscale_nrgaggsec$"电力、热力生产和供应业"
  tf_diremissum[["BY"]] <- func_emissum(tf_nrgfuel[["BY"]], global_emisfac_df)
  
  ### Energy intensity ----
  tf_nrgintst[["BY"]] <- func_nrg_intst(tf_nrgfuel[["BY"]], tfres_act[["BY"]], "elecgen_thrm")
  
  ## Imported elec ----
  ### Energy intensity ----
  # 读取省电网发电能耗量
  global_provelecgen_nrgsum <- 
    func_read_trans("S3CNPRZE")[c("year", "原煤", "柴油", "燃料油", "天然气")]
  names(global_provelecgen_nrgsum) <- 
    c("year", "rawcoal", "diesel", "residual", "gas")
  
  # 火电能耗强度
  res_nrgintst[["BY"]] <- 
    func_nrg_intst(global_provelecgen_nrgsum, global_provelecgen, "火电")
  
  ### Consumption and emission ----
  res_nrgfuel[["BY"]] <- 
    func_nrg_sum(res_nrgintst[["BY"]], tfres_act[["BY"]], "importthrm")
  res_diremissum[["BY"]] <- func_emissum(res_nrgfuel[["BY"]], global_emisfac_df)
  
  
  ## Results ----
  tot_results <- func_resultcalc("BY")
  # 将结果赋值到各个变量
  tot_nrgsumce[["BY"]] <- tot_results[[1]]
  tot_nrgaggfuel[["BY"]] <- tot_results[[2]]
  tot_nrgaggfuelce[["BY"]] <- tot_results[[3]]
  tot_nrgpropaggfuel[["BY"]] <- tot_results[[4]]
  tot_nrgsecce[["BY"]] <- tot_results[[5]]
  tot_emissum[["BY"]] <- tot_results[[6]]
  tot_emispergdp[["BY"]] <- tot_results[[7]]
  tot_emissec[["BY"]] <- tot_results[[8]]
  tot_emisaggfuel[["BY"]] <- tot_results[[9]]
}

# SCENARIO ANLYS ----
# 进行情景分析
global_starttime <- Sys.time()
for (set_scalc in set_scalcs) {
  # 在多于一个情景的计算中，从第二次开始将计算缓存设为真
  if (match(set_scalc, set_scalcs) > 1) {
    set_calc_cache <- TRUE
  } else {
    set_calc_cache <- FALSE
  }
  
  # Agri ----
  ## Activity level ----
  agri_act[[set_scalc]] <- func_interp_3(
    year = c(2019, 2030, 2050, 2060), scale = c(1, 0.80, 0.63, 0.62), 
    base = func_lastone(agri_act[["BY"]]$agri), "area")
  
  ## Energy intensity ----
  ### BR.Diesel ----
  # 柴油强度则依历史趋势下降
  if (grepl("SLC", set_scalc)) {
    #### SLC ----
    agri_nrgintst[[set_scalc]] <- func_interp_3( 
      year = c(2019, 2030, 2060), scale = c(1, 1, 0.3), 
      base = func_lastone(agri_nrgintst[["BY"]]$diesel), "diesel")
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    agri_nrgintst[[set_scalc]] <- func_interp_3(
      year = c(2019, 2030, 2060), scale = c(1, 1, 0.4), 
      base = func_lastone(agri_nrgintst[["BY"]]$diesel), "diesel")
  } else { 
    #### BAU ----
    agri_nrgintst[[set_scalc]] <- func_interp_3(
      year = c(2019, 2030, 2060), scale = c(1, 1, 0.5), 
      base = func_lastone(agri_nrgintst[["BY"]]$diesel), "diesel")
  }
  
  ### BR.Elec ----
  # 近期由于机械化水平提高，用电强度增加
  if (grepl("SLC", set_scalc)) {
    #### SLC ----
    agri_nrgintst[[set_scalc]]$electricity <- func_interp_3(
      year = c(2019, 2030, 2045, 2060),scale = c(1, 1.1, 1.15, 0.9), 
      base = func_lastone(agri_nrgintst[["BY"]]$electricity), 
      "electricity")$electricity
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    agri_nrgintst[[set_scalc]]$electricity <- func_interp_3(
      year = c(2019, 2030, 2045, 2060),scale = c(1, 1.13, 1.13, 0.95), 
      base = func_lastone(agri_nrgintst[["BY"]]$electricity), 
      "electricity")$electricity
  } else { 
    #### BAU ----
    agri_nrgintst[[set_scalc]]$electricity <- func_interp_3(
      year = c(2019, 2025, 2045, 2060),scale = c(1, 1.2, 1.2, 1), 
      base = func_lastone(agri_nrgintst[["BY"]]$electricity), 
      "electricity")$electricity
  }
  
  ## Consumption and emission ----
  agri_nrgfuel[[set_scalc]] <- 
    func_nrg_sum(agri_nrgintst[[set_scalc]], agri_act[[set_scalc]], "area")
  agri_diremissum[[set_scalc]] <- 
    func_emissum(agri_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Industry ----
  ## Activity level ----
  # 未来子部门GDP所占比重
  ind_ori_act_prop[[set_scalc]] <- data.frame(year = c(2019:2060))
  ind_ori_act_prop[[set_scalc]][, "食品饮料及烟草制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"食品饮料及烟草制造业"[
        ind_ori_act_prop$BY$year == 2019], 
      3, 2))$value
  ind_ori_act_prop[[set_scalc]][, "木材及家具制造业"] <- func_interp_2(
    year = c(2019, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"木材及家具制造业"[
        ind_ori_act_prop$BY$year == 2019],
      1.12, 0.5))$value
  ind_ori_act_prop[[set_scalc]][, "造纸及印刷"] <- func_interp_2(
    year = c(2019, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"造纸及印刷"[ind_ori_act_prop$BY$year == 2019], 
      1.5, 1))$value
  ind_ori_act_prop[[set_scalc]][, "文体工美用品制造业"] <- func_interp_2(
    year = c(2019, 2040, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"文体工美用品制造业"[
        ind_ori_act_prop$BY$year == 2019], 
      2, 2.3))$value
  ind_ori_act_prop[[set_scalc]][, "石油及炼焦"] <- func_interp_2(
    year = c(2019, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"石油及炼焦"[ind_ori_act_prop$BY$year == 2019], 
      ind_ori_act_prop[["BY"]]$"石油及炼焦"[ind_ori_act_prop$BY$year == 2020], 
      ind_ori_act_prop[["BY"]]$"石油及炼焦"[ind_ori_act_prop$BY$year == 2018], 
      0.1, 0))$value
  ind_ori_act_prop[[set_scalc]][, "医药制造业"] <- func_interp_2(
    year = c(2019, 2020, 2021, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"医药制造业"[ind_ori_act_prop$BY$year == 2019], 
      ind_ori_act_prop[["BY"]]$"医药制造业"[ind_ori_act_prop$BY$year == 2020], 
      ind_ori_act_prop[["BY"]]$"医药制造业"[ind_ori_act_prop$BY$year == 2018], 
      2, 4))$value
  ind_ori_act_prop[[set_scalc]][, "非金属矿物制品业"] <- func_interp_2(
    year = c(2019, 2020, 2021, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"非金属矿物制品业"[
        ind_ori_act_prop$BY$year == 2019], 
      ind_ori_act_prop[["BY"]]$"非金属矿物制品业"[
        ind_ori_act_prop$BY$year == 2020], 
      ind_ori_act_prop[["BY"]]$"非金属矿物制品业"[
        ind_ori_act_prop$BY$year == 2018], 
      1.3, 0.5))$value
  ind_ori_act_prop[[set_scalc]][, "金属加工制造业"] <- func_interp_2(
    year = c(2019, 2040, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"金属加工制造业"[
        ind_ori_act_prop$BY$year == 2019], 
      7, 3))$value
  ind_ori_act_prop[[set_scalc]][, "电力、热力生产和供应业"] <- func_interp_2(
    year = c(2019, 2030, 2060), value = c(
      ind_ori_act_prop[["BY"]]$"电力、热力生产和供应业"[
        ind_ori_act_prop$BY$year == 2019],
      0.9, 0.1))$value
  
  ### BR.Ind str ----
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    ind_ori_act_prop[[set_scalc]][, "化学工业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2045, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"化学工业"[ind_ori_act_prop$BY$year == 2019], 
        6.3, 5, 0, 0))$value
    ind_ori_act_prop[[set_scalc]][, "设备制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"设备制造业"[ind_ori_act_prop$BY$year == 2019], 
        13.8, 16, 20))$value
    ind_ori_act_prop[[set_scalc]][, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"电子电气制造业"[
          ind_ori_act_prop$BY$year == 2019], 
        52, 57, 66))$value
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    ind_ori_act_prop[[set_scalc]][, "化学工业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2045, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"化学工业"[ind_ori_act_prop$BY$year == 2019], 
        6.3, 5.5, 1, 0))$value
    ind_ori_act_prop[[set_scalc]][, "设备制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"设备制造业"[ind_ori_act_prop$BY$year == 2019], 
        13.8, 15, 19))$value
    ind_ori_act_prop[[set_scalc]][, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"电子电气制造业"[
          ind_ori_act_prop$BY$year == 2019], 
        52, 54.5, 65))$value
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    ind_ori_act_prop[[set_scalc]][, "化学工业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2045, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"化学工业"[ind_ori_act_prop$BY$year == 2019], 
        6.6, 6, 2, 0))$value
    ind_ori_act_prop[[set_scalc]][, "设备制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"设备制造业"[ind_ori_act_prop$BY$year == 2019], 
        13.4, 14.5, 17.5))$value
    ind_ori_act_prop[[set_scalc]][, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2025, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"电子电气制造业"[
          ind_ori_act_prop$BY$year == 2019], 
        51.7, 53, 62))$value
  } else { 
    #### BAU ----
    ind_ori_act_prop[[set_scalc]][, "化学工业"] <- func_interp_2(
      year = c(2019, 2030, 2045, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"化学工业"[ind_ori_act_prop$BY$year == 2019], 
        7, 3, 0))$value
    ind_ori_act_prop[[set_scalc]][, "设备制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"设备制造业"[ind_ori_act_prop$BY$year == 2019], 
        13, 15))$value
    ind_ori_act_prop[[set_scalc]][, "电子电气制造业"] <- func_interp_2(
      year = c(2019, 2030, 2060), value = c(
        ind_ori_act_prop[["BY"]]$"电子电气制造业"[
          ind_ori_act_prop$BY$year == 2019], 
        50, 55))$value
  }
  ind_ori_act_prop[[set_scalc]][, "纺织及服装制造业"] <- 
    (100 - rowSums(ind_ori_act_prop[[set_scalc]][-1]))/2
  ind_ori_act_prop[[set_scalc]][, "其他制造业"] <- 
    ind_ori_act_prop[[set_scalc]][, "纺织及服装制造业"]
  # 检测各行业比例加和是否为100%
  if (sum(rowSums(ind_ori_act_prop[[set_scalc]][-1]) != 100) > 0) {
    print("warning: sum larger than 100%.")
    break
  }
  # 计算未来各子部门GDP
  ind_act[[set_scalc]] <- func_nrg_sum(
    ind_ori_act_prop[[set_scalc]][c("year", global_ind_subsector)], 
    prj_global_gdp[c("year","indgdp")], "indgdp")
  ind_act[[set_scalc]][global_ind_subsector] <-
    ind_act[[set_scalc]][global_ind_subsector]/100
  
  ## Energy intensity ----
  ind_nrgintst[[set_scalc]] <- vector("list", 13)
  names(ind_nrgintst[[set_scalc]]) <- global_ind_subsector
  
  ### BR.MostNrgIntst ----
  # 假设大部分能耗强度略有减少
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]] <- cbind(
        data.frame(year = c(2019: 2060)),
        sapply(global_ind_nrgclass[1:6], function(j) {
          func_interp_3(
            year = c(2019, 2025, 2026, 2030, 2035, 2060), 
            scale = c(1.0, 1.00, 1.00, 1.03, 0.95, 0.6), 
            base = func_lastone(ind_nrgintst[["BY"]][[i]][, j], 
                                zero.rm =  FALSE))$value}))
    }
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]] <- cbind(
        data.frame(year = c(2019: 2060)),
        sapply(global_ind_nrgclass[1:6], function(j) {
          func_interp_3(
            year = c(2019, 2025, 2030, 2035, 2060), 
            scale = c(1.0, 1.0, 1.00, 1.00, 0.7), 
            base = func_lastone(ind_nrgintst[["BY"]][[i]][, j], 
                                zero.rm =  FALSE))$value}))
    }
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]] <- cbind(
        data.frame(year = c(2019: 2060)),
        sapply(global_ind_nrgclass[1:6], function(j) {
          func_interp_3(
            year = c(2019, 2025, 2028, 2030, 2035, 2060), 
            scale = c(1.0, 0.99, 1.05, 1.01, 1.00,  0.8), 
            base = func_lastone(ind_nrgintst[["BY"]][[i]][, j], 
                                zero.rm =  FALSE))$value}))
    }
  } else { 
    #### BAU ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]] <- cbind(
        data.frame(year = c(2019: 2060)),
        sapply(global_ind_nrgclass[1:6], function(j) {
          func_interp_3(
            year = c(2019, 2025, 2030, 2040, 2060), 
            scale = c(1.0, 1.10, 1.23,  1.0,  1.0), 
            base = func_lastone(ind_nrgintst[["BY"]][[i]][, j], 
                                zero.rm =  FALSE))$value}))
    }
  }
  
  ### BR.GasIntst ----
  # 但是天然气在短期内有所上升
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "gas"] <- func_interp_3(
        year = c(2019, 2025, 2026, 2030, 2060), 
        scale = c(1.0, 0.99, 0.99, 1.05, 1.00), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "gas"], 
                            zero.rm =  FALSE))$value
          
    }
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "gas"] <- func_interp_3(
        year = c(2019, 2025, 2028, 2030, 2035, 2060), 
        scale = c(1.0, 0.99, 1.12, 1.25, 1.30, 1.10), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "gas"], 
                            zero.rm =  FALSE))$value
    }
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "gas"] <- func_interp_3(
        year = c(2019, 2025, 2028, 2030, 2035, 2060), 
        scale = c(1.0, 0.99, 1.16, 1.22, 1.35, 1.15), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "gas"], 
                            zero.rm =  FALSE))$value
    }
  } else { 
    #### BAU ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "gas"] <- func_interp_3(
        year = c(2019, 2025, 2030, 2035, 2060), 
        scale = c(1.0, 1.05, 1.45, 1.6, 1.2), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "gas"], 
                            zero.rm =  FALSE))$value
    }
  }
  ### BR.ElecIntst ----
  # 电力在短期内有所上升，但比天然气上升幅度小
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "electricity"] <- func_interp_3(
        year = c(2019, 2025, 2030, 2060), 
        scale = c(1.0, 1.08, 1.1, 1.2), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
    }
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "electricity"] <- func_interp_3(
        year = c(2019, 2025, 2028, 2030, 2035, 2060), 
        scale = c(1.0, 1.08, 1.16, 1.25, 1.27, 1.2), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
    }
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "electricity"] <- func_interp_3(
        year = c(2019, 2025, 2028, 2030, 2060), 
        scale = c(1.0, 1.05, 1.17, 1.25, 1.25), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
    }
  } else { 
    #### BAU ----
    for (i in global_ind_subsector) {
      ind_nrgintst[[set_scalc]][[i]][, "electricity"] <- func_interp_3(
        year = c(2019, 2025, 2030, 2035, 2060), 
        scale = c(1.0, 1.10, 1.33, 1.4, 1.3), 
        base = func_lastone(ind_nrgintst[["BY"]][[i]][, "electricity"], 
                            zero.rm =  FALSE))$value
    }
  }
  
  ## Energy and emission ----
  ind_nrgsecfuel[[set_scalc]] <- 
    func_nrg_sum_ls(ind_nrgintst[[set_scalc]], ind_act[[set_scalc]])
  ind_nrgfuel[[set_scalc]] <- 
    func_ls2df(ind_nrgsecfuel[[set_scalc]])
  ind_diremissum[[set_scalc]] <- 
    func_emissum(ind_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Construction ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
    const_act[[set_scalc]] <- prj_global_gdp[c("year", "constgdp")]
    names(const_act[[set_scalc]])[2] <- "const_gdp"
  } else {
    # 如果已有缓存，就继承缓存内容
    const_act[[set_scalc]] <- const_act[[set_scalcs[1]]]
  }
  
  ## Energy intensity ----
  ### BR.NrgIntst ----
  if (grepl("SLC", set_scalc)) {
    #### SLC ----
    const_nrgintst[[set_scalc]] <- func_interp_3(
      year = c(2019, 2023, 2040, 2060), scale = c(1, 0.8, 0.4, 0.25), 
      base = func_lastone(const_nrgintst[["BY"]]$electricity), "electricity")
  } else if (grepl("WLC", set_scalc)) {
    #### WLC ----
    const_nrgintst[[set_scalc]] <- func_interp_3(
      year = c(2019, 2023, 2040, 2060), scale = c(1, 0.8, 0.55, 0.4), 
      base = func_lastone(const_nrgintst[["BY"]]$electricity), "electricity")
  } else { 
    #### BAU ----
    const_nrgintst[[set_scalc]] <- func_interp_3(
      year = c(2019, 2023, 2040, 2060), scale = c(1, 0.82, 0.56, 0.5), 
      base = func_lastone(const_nrgintst[["BY"]]$electricity), "electricity")
  }
  
  
  ## Consumption and emission ----
  const_nrgfuel[[set_scalc]] <- 
    func_nrg_sum(const_nrgintst[[set_scalc]],const_act[[set_scalc]],"const_gdp")
  const_diremissum[[set_scalc]] <- 
    func_emissum(const_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Transportation ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
    trans_act[[set_scalc]] <- 
      cbind(data.frame(year = c(2019: 2060)), 
            sapply(global_trans_subsector, function(i) rep(NA, 42)))
    # 公路
    # 常规公交和快速公交以初始3%的增长率增长至2030年饱和
    trans_act[[set_scalc]][, c("常规公交", "快速公交")] <- 
      sapply(c("常规公交", "快速公交"), function(i) {func_curve_1(
        baseyear = 2019, basevalue = func_lastone(trans_act[["BY"]][, i]), 
        maxyear = 2030, endyear = 2060, init_rate = 0.03)$value})
    
    # 出租车保持不变或波动
    trans_act[[set_scalc]][, "出租车"] <- func_curve_1(
      baseyear = 2019, basevalue = func_lastone(trans_act[["BY"]][, "出租车"]), 
      maxyear = 2035, endyear = 2060, init_rate = -0.01)$value
    
    # 农村客运因城乡交流频繁，2025年前以初始7%的速率增长
    trans_act[[set_scalc]][, "农村客车"] <- func_curve_1(
      baseyear = 2019, basevalue = func_lastone(trans_act[["BY"]][,"农村客车"]), 
      maxyear = 2030, endyear = 2060, init_rate = 0.06)$value
    
    # 地铁
    trans_act[[set_scalc]][, "地铁"] <- func_curve_1(
      baseyear = 2019, basevalue = func_lastone(trans_act[["BY"]][,"地铁"]), 
      maxyear = 2030, endyear = 2060, init_rate = 0.04)$value
    
    # 公路其他柴油
    # 按照初始增长率5%增长，至2030年饱和
    trans_act[[set_scalc]][, "公路其他柴油"] <- func_curve_1(
      baseyear = 2019, 
      basevalue = func_lastone(trans_act[["BY"]][, "公路其他柴油"]), 
      maxyear = 2030, endyear = 2060, init_rate = 0.05)$value
    
    # 水运
    trans_act[[set_scalc]]$"水路客运" <- func_interp_3(
      year = c(2019, 2025, 2060), scale = c(1, 1.05, 1.15), 
      base = func_lastone(trans_act[["BY"]]$"水路客运"))$value
    trans_act[[set_scalc]]$"水路货运" <- func_interp_3(
      year = c(2019, 2025, 2060), scale = c(1, 1.5, 2.0), 
      base = func_lastone(trans_act[["BY"]]$水路货运))$value
  } else {
    # 如果已有缓存，就继承缓存内容
    trans_act[[set_scalc]] <- trans_act[[set_scalcs[1]]]
  }
  
  ### BR.CarNum ----
  # 私家车：先预测全部私家车变化趋势再分成常规和纯电动私家车
  # 全部私家车按照初始增长率增长，至2033-2035年饱和
  if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    trans_act[[set_scalc]][, "私家车"] <- func_curve_1(
      baseyear = 2019, 
      basevalue = func_lastone(trans_act[["BY"]][, "公路其他汽油"]), 
      maxyear = 2035, endyear = 2060, init_rate = 0.03)$value
  } else { 
    #### NO SLC ----
    trans_act[[set_scalc]][, "私家车"] <- func_curve_1(
      baseyear = 2019, 
      basevalue = func_lastone(trans_act[["BY"]][, "公路其他汽油"]), 
      maxyear = 2035, endyear = 2060, init_rate = 0.05)$value
  }
  # 开始拆分：常规私家车和纯电动私家车比例
  ### BR.Elec4Gasoline ----
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    trans_carprop_ls[[set_scalc]] <- func_interp_2(
      year = c(2019, 2025, 2030, 2050, 2060), 
      value = c(0.022, 0.05, 0.15, 0.65, 0.75), "elec")
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    trans_carprop_ls[[set_scalc]] <- func_interp_2(
      year = c(2019, 2025, 2030, 2050, 2060), 
      value = c(0.022, 0.05, 0.10, 0.60, 0.70), "elec")
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    trans_carprop_ls[[set_scalc]] <- func_interp_2(
      year = c(2019, 2025, 2028, 2030, 2050, 2060), 
      value = c(0.022, 0.04, 0.07, 0.08, 0.50, 0.60), "elec")
  } else { 
    #### BAU ----
    trans_carprop_ls[[set_scalc]] <- func_interp_2(
      year = c(2019, 2025, 2030, 2050, 2060), 
      value = c(0.022, 0.03, 0.04, 0.40, 0.50), "elec")
  }
  trans_carprop_ls[[set_scalc]]$nonelec <- 
    1 - trans_carprop_ls[[set_scalc]]$elec
  
  # 公路其他汽油：常规私家车保有量
  trans_act[[set_scalc]][, "公路其他汽油"] <- 
    trans_act[[set_scalc]][, "私家车"]*trans_carprop_ls[[set_scalc]]$nonelec
  # 纯电动私家车
  trans_act[[set_scalc]]$"纯电动私家车" <- 
    trans_act[[set_scalc]][, "私家车"]*trans_carprop_ls[[set_scalc]]$elec
  # 删除私家车总量一列
  trans_act[[set_scalc]]$"私家车" <- NULL
  
  # 航空
  trans_act[[set_scalc]]$"航空" <- func_rate(
    baseyear = 2019, basevalue = func_lastone(trans_act[["BY"]]$航空), 
    rate_df = func_stage(
      year = c(2019, 2024, 2029, 2034, 2040, 2050, 2060), 
      value = c(9.02, 8.33, 7.68, 7.30, 4.00, 2.0, 2.0)))$value
  
  ## Energy intensity ----
  trans_nrgintst[[set_scalc]] <- vector("list", length(global_trans_subsector))
  names(trans_nrgintst[[set_scalc]]) <- global_trans_subsector
  # 公路交通
  # 常规公交、快速公交、出租车、农村客车、公路其他柴油的能耗强度逐渐下降
  for (j in c(1: 4, 8)) {
    trans_nrgintst[[set_scalc]][[j]] <- cbind(
      data.frame(year = c(2019: 2060)), 
      sapply(names(trans_nrgintst[["BY"]][[j]])[
        names(trans_nrgintst[["BY"]][[j]]) %in% "year" == FALSE], 
        function(i) {
          func_interp_3(
            year = c(2019, 2060), scale = c(1, 0.8), 
            base = func_lastone(trans_nrgintst[["BY"]][[j]][, i]))$value}))
  }
  
  # 地铁
  trans_nrgintst[[set_scalc]][["地铁"]] <- 
    func_interp_3(year = c(2019, 2035, 2060), scale = c(1, 0.95, 0.9), 
                  base = trans_nrgintst$BY$"地铁"$electricity[
                    trans_nrgintst$BY$"地铁"$year == 2019], "electricity")
  
  # 公路汽油
  trans_nrgintst[[set_scalc]][["公路其他汽油"]] <- 
    data.frame(year = c(2019: 2060))
  trans_nrgintst[[set_scalc]][["公路其他汽油"]][, "gasoline"] <- func_interp_3(
    year = c(2019, 2030, 2040, 2060), 
    scale = c(1, 1.05, 0.8, 0.7), 
    base = func_lastone(trans_nrgintst[["BY"]][["公路其他汽油"]]$gasoline))$value
  
  # 纯电动私家车
  trans_nrgintst[[set_scalc]][["纯电动私家车"]] <- 
    data.frame(year = c(2019: 2060))
  trans_nrgintst[[set_scalc]][["纯电动私家车"]][, "electricity"] <- 
    func_interp_3(
      year = c(2019, 2030, 2040, 2060), scale = c(1, 1.05, 0.8, 0.7), 
      base = func_lastone(
        trans_nrgintst[["BY"]][["纯电动私家车"]]$electricity))$value
  
  # 水路客运
  ### RB.WaterPax ----
  # 柴油和燃料油均基于历史数据和比率
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    trans_nrgintst[[set_scalc]][["水路客运"]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.95, 0.76, 0.7), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$diesel), 
      "diesel")
    trans_nrgintst[[set_scalc]][["水路客运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1.0, 0.92, 0.75, 0.7), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$residual), 
      "residual")$residual
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    trans_nrgintst[[set_scalc]][["水路客运"]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.95, 1.00, 0.8), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$diesel), 
      "diesel")              
    trans_nrgintst[[set_scalc]][["水路客运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1.0, 0.92, 0.85, 0.80), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$residual), "residual"
    )$residual
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    trans_nrgintst[[set_scalc]][["水路客运"]] <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2060), scale = c(1, 0.96, 0.96, 0.9, 0.9), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$diesel), 
      "diesel")
    trans_nrgintst[[set_scalc]][["水路客运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2028,2030, 2060), scale = c(1.0, 0.93,0.94,0.9, 0.9), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$residual),
      "residual")$residual
  } else { 
    #### BAU ----
    trans_nrgintst[[set_scalc]][["水路客运"]] <- 
      func_interp_3(year = c(2019, 2045, 2060), 
                    scale = c(1, 1, 1), 
                    base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$diesel), 
                    "diesel")
    trans_nrgintst[[set_scalc]][["水路客运"]]$residual <- func_interp_3(
      year = c(2019, 2060), scale = c(1.0, 1.0), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路客运"]]$residual),
      "residual")$residual
  }
  
  ### BR.WaterFrt ----
  # 水路货运
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    trans_nrgintst[[set_scalc]][["水路货运"]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.95, 0.85, 0.75), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$diesel),"diesel")
    trans_nrgintst[[set_scalc]][["水路货运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.97, 0.85, 0.75), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$residual),
      "residual")$residual
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    trans_nrgintst[[set_scalc]][["水路货运"]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.95, 0.90, 0.80), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$diesel),"diesel")
    # 燃料油：基于历史数据和比率
    trans_nrgintst[[set_scalc]][["水路货运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 0.97, 0.92, 0.80), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$residual),
      "residual")$residual
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    trans_nrgintst[[set_scalc]][["水路货运"]] <- func_interp_3(
      year = c(2019, 2025, 2028,2030, 2060), scale = c(1, 0.96, 1.00,1.00, 0.9), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$diesel),"diesel")
    trans_nrgintst[[set_scalc]][["水路货运"]]$residual <- func_interp_3(
      year = c(2019, 2025, 2028,2030, 2060), scale = c(1, 0.98,1.00,0.98, 0.9), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$residual),
      "residual")$residual
  } else { 
    #### BAU ----
    trans_nrgintst[[set_scalc]][["水路货运"]] <- func_interp_3(
      year = c(2019, 2040, 2060), scale = c(1, 1, 1), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$diesel),"diesel")
    trans_nrgintst[[set_scalc]][["水路货运"]]$residual <- func_interp_3(
      year = c(2019, 2040, 2060), scale = c(1, 1, 1), 
      base = func_lastone(trans_nrgintst[["BY"]][["水路货运"]]$residual),
      "residual")$residual
  }
  
  # 航空
  trans_nrgintst[[set_scalc]][["航空"]] <- func_interp_3(
    year = c(2019, 2040, 2060), scale = c(1, 0.9, 0.8), 
    base = func_lastone(trans_nrgintst[["BY"]][["航空"]]$kerosene), "kerosene")
  
  ## Energy and emission ----
  trans_nrgsecfuel[[set_scalc]] <- 
    func_nrg_sum_ls(trans_nrgintst[[set_scalc]], trans_act[[set_scalc]])
  trans_nrgfuel[[set_scalc]] <- func_ls2df(trans_nrgsecfuel[[set_scalc]])
  trans_diremissum[[set_scalc]] <- 
    func_emissum(trans_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Commerce ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
    ori_comemployee <- func_cross(
      prj_global_population[c("year", "population")], 
      func_interp_2(
        year = c(2019, 2030, 2060),
        value = c(com_act[["BY"]]$com_employee[com_act[["BY"]]$year == 2019]/
                    global_population$"常住人口"[global_population$year == 2019], 
                  0.65, 0.75)))
    ori_comgdp <- prj_global_gdp[c("year", "comgdp")]
    com_act[[set_scalc]] <- func_merge_2(list(ori_comemployee, ori_comgdp))
    names(com_act[[set_scalc]]) <- c("year", "com_employee", "com_gdp")
  } else {
    # 如果已有缓存，就继承缓存内容
    com_act[[set_scalc]] <- com_act[[set_scalcs[1]]]
  }
  
  ## Energy intensity ----
  com_nrgintst[[set_scalc]] <- vector("list", 2)
  ### BR.ComNrgintst ----
  names(com_nrgintst[[set_scalc]]) <- global_com_subsector
  # 服务业用电强度略有增加
  # 服务业燃气强度略有增加后减少，且逐渐为电气替代
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    com_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.05, 0.93, 0.9), 
      base = func_lastone(com_nrgintst[["BY"]]$electricity$electricity), 
      "electricity")
    com_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.1, 1.08, 0.8), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$lpg), "lpg")
    com_nrgintst[[set_scalc]][[2]]$gas <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.15, 1.08, 0.8), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$gas), "gas")$gas
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    com_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.05, 1.04, 0.9), 
      base = func_lastone(com_nrgintst[["BY"]]$electricity$electricity), 
      "electricity")
    com_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.1, 1.2, 0.9), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$lpg), "lpg")
    com_nrgintst[[set_scalc]][[2]]$gas <- func_interp_3(
      year = c(2019, 2025, 2030, 2060), scale = c(1, 1.15, 1.2, 0.9), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$gas), "gas")$gas
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    com_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2060), scale = c(1, 1.05,1.07,1.03, 0.95), 
      base = func_lastone(com_nrgintst[["BY"]]$electricity$electricity), 
      "electricity")
    com_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2028,2030, 2060), scale = c(1, 1.11,1.24, 1.21, 1.0), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$lpg), "lpg")
    com_nrgintst[[set_scalc]][[2]]$gas <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2060), scale = c(1, 1.16, 1.27,1.26, 1.0), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$gas), "gas")$gas
  } else { 
    #### BAU ----
    com_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2035, 2060), scale = c(1, 1.2, 1), 
      base = func_lastone(com_nrgintst[["BY"]]$electricity$electricity), 
      "electricity")
    com_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2030, 2060), scale = c(1, 1.5, 1.0), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$lpg), "lpg")
    com_nrgintst[[set_scalc]][[2]]$gas <- func_interp_3(
      year = c(2019, 2025, 2060), scale = c(1, 1.5, 1.0), 
      base = func_lastone(com_nrgintst[["BY"]][[2]]$gas), "gas")$gas
  }
  
  ### Br.Elec4GasLpg ----
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    com_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = com_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2025, 2030, 2050, 2060), 
                      c(2019, 2025, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.3, 0.75, 1, 1), 
                      c(0, 0.3, 0.75, 1, 1)), 
      alterscales = list(0.8, 0.8))
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    com_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = com_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2025, 2030, 2050, 2060), 
                      c(2019, 2025, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.3, 0.6, 1, 1), 
                      c(0, 0.3, 0.6, 1, 1)), 
      alterscales = list(0.8, 0.8))
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    com_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = com_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2025, 2035, 2053, 2060), 
                      c(2019, 2025, 2035, 2053, 2060)), 
      propsubs = list(c(0, 0.25, 0.6, 1, 1), 
                      c(0, 0.25, 0.6, 1, 1)), 
      alterscales = list(0.9, 0.9))
  } else { 
    #### BAU ----
    com_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = com_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg", "gas"), 
      namenrgsubs = list("electricity", "electricity"), 
      yearsubs = list(c(2019, 2040, 2055, 2060), 
                      c(2019, 2040, 2055, 2060)), 
      propsubs = list(c(0, 0.6, 1, 1), 
                      c(0, 0.6, 1, 1)), 
      alterscales = list(1, 1))
  }
  
  ## Energy and emission ----
  com_nrgsecfuel[[set_scalc]] <- 
    func_nrg_sum_ls(com_nrgintst[[set_scalc]], com_act[[set_scalc]])
  com_nrgfuel[[set_scalc]] <- func_ls2df(com_nrgsecfuel[[set_scalc]])
  com_diremissum[[set_scalc]] <- 
    func_emissum(com_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Household ----
  ## Activity level ----
  if (set_calc_cache == FALSE) { ### Cache ----
    # hh_elec
    hh_ori_household <- prj_global_population[c("year", "household")]
    # hh_lpg
    hh_ori_lpguser <- 
      func_cross(prj_global_population[c("year", "household")], 
                 func_interp_3(year = c(2019, 2025, 2035, 2060), 
                               scale = c(1, 0.8, 0.75, 0.30), 
                               base = func_lastone(
                                 by_hh_ori_users_prop[c("year", "lpg")])))
    names(hh_ori_lpguser)[2] <- "lpg"
    # hh_gas
    hh_ori_gasuser <- 
      func_cross(prj_global_population[c("year", "household")], 
                 func_interp_3(year = c(2019, 2025, 2030, 2060), 
                               scale = c(1, 1.2, 1.8, 2), 
                               base = func_lastone(
                                 by_hh_ori_users_prop[c("year", "gas")])))
    names(hh_ori_gasuser)[2] <- "gas"
    # 合并
    hh_act[[set_scalc]] <- 
      func_merge_2(list(hh_ori_household, hh_ori_lpguser, hh_ori_gasuser))
  } else {
    # 如果已有缓存，就继承缓存内容
    hh_act[[set_scalc]] <- hh_act[[set_scalcs[1]]]
  }
  
  ## Energy intensity ----
  hh_nrgintst[[set_scalc]] <- vector("list", length(global_hh_subsector))
  names(hh_nrgintst[[set_scalc]]) <- global_hh_subsector
  ### BR.Elec ----
  # 生活用电强度
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    hh_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2035, 2060), 
      scale = c(1, 1.05, 1.06, 1.20, 1.30), 
      base = func_lastone(hh_nrgintst[["BY"]][["household"]][, "electricity"]))
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    hh_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2035, 2060), 
      scale = c(1.0, 1.09, 1.19, 1.15, 1.26, 1.4), 
      base = func_lastone(hh_nrgintst[["BY"]][["household"]][, "electricity"]))
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    hh_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2035, 2060), 
      scale = c(1.0, 1.11, 1.19, 1.19, 1.31, 1.45), 
      base = func_lastone(hh_nrgintst[["BY"]][["household"]][, "electricity"]))
  } else { 
    #### BAU ----
    hh_nrgintst[[set_scalc]][[1]] <- func_interp_3(
      year = c(2019, 2025, 2035, 2060), 
      scale = c(1.0, 1.19, 1.35, 1.6), 
      base = func_lastone(hh_nrgintst[["BY"]][["household"]][, "electricity"]))
  }
  names(hh_nrgintst[[set_scalc]][[1]])[2] <- "electricity"
  
  # 生活用煤强度
  hh_nrgintst[[set_scalc]][[1]]$rawcoal <- func_interp_2(
    year = c(2019, 2020, 2060), 
    value = c(func_lastone(hh_nrgintst$BY$household$rawcoal), 0, 0))$value
  
  ### BR.LPG ----
  # 生活液化石油气
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    hh_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2035, 2060), 
      scale = c(1, 0.97, 0.88, 0.88, 0.6), 
      base = func_lastone(hh_nrgintst[["BY"]][["lpg"]]$lpg), "lpg")
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    hh_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2030, 2035, 2060), 
      scale = c(1, 0.97, 0.97, 0.97, 0.6), 
      base = func_lastone(hh_nrgintst[["BY"]][["lpg"]]$lpg), "lpg")
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    hh_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2025, 2028, 2030, 2035, 2060), 
      scale = c(1.0, 1.07, 1.02, 0.95, 0.98, 0.65), 
      base = func_lastone(hh_nrgintst[["BY"]][["lpg"]]$lpg), "lpg")
  } else { 
    #### BAU ----
    hh_nrgintst[[set_scalc]][[2]] <- func_interp_3(
      year = c(2019, 2040, 2060), 
      scale = c(1, 1.3, 0.7), 
      base = func_lastone(hh_nrgintst[["BY"]][["lpg"]]$lpg), "lpg")
  }
  
  ### BR.Gas -----
  # 生活天然气
  if (grepl("PLUS", set_scalc)) { 
    #### PLUS ----
    hh_nrgintst[[set_scalc]][[3]] <- func_interp_3(
      year = c(2019, 2025, 2035, 2060), scale = c(1, 1.1, 0.66, 0.4), 
      base = func_lastone(hh_nrgintst[["BY"]][["gas"]]$gas), "gas")
  } else if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    hh_nrgintst[[set_scalc]][[3]] <- func_interp_3(
      year = c(2019, 2025, 2035, 2060), scale = c(1, 1.1, 0.9, 0.5), 
      base = func_lastone(hh_nrgintst[["BY"]][["gas"]]$gas), "gas")
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    hh_nrgintst[[set_scalc]][[3]] <- func_interp_3(
      year = c(2019, 2025, 2028,2035, 2060), scale = c(1, 1.15,1.06,0.85, 0.6), 
      base = func_lastone(hh_nrgintst[["BY"]][["gas"]]$gas), "gas")
  } else { 
    #### BAU ----
    hh_nrgintst[[set_scalc]][[3]] <- func_interp_3(
      year = c(2019, 2040, 2060), scale = c(1, 1.3, 0.7), 
      base = func_lastone(hh_nrgintst[["BY"]][["gas"]]$gas), "gas")
  }
  
  ### BR.Elec4LPG ----
  # LPG电气化
  if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    hh_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.6, 0.9, 0.9)), 
      alterscales = list(0.8))
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    hh_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2060)), 
      propsubs = list(c(0, 0.6)), 
      alterscales = list(0.8))
  } else { 
    #### BAU ----
    hh_nrgintst[[set_scalc]][[2]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[2]], 
      namenrgoris = list("lpg"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2060)), 
      propsubs = list(c(0, 0.5)), 
      alterscales = list(0.8))
  }
  
  ### BR.Elec4Gas ----
  # 天然气电气化
  if (grepl("SLC", set_scalc)) { 
    #### SLC ----
    hh_nrgintst[[set_scalc]][[3]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[3]], 
      namenrgoris = list("gas"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2030, 2050, 2060)), 
      propsubs = list(c(0, 0.6, 0.9, 0.9)), 
      alterscales = list(0.8))
  } else if (grepl("WLC", set_scalc)) { 
    #### WLC ----
    hh_nrgintst[[set_scalc]][[3]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[3]], 
      namenrgoris = list("gas"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2060)), 
      propsubs = list(c(0, 0.6)), 
      alterscales = list(0.8))
  } else { 
    #### BAU ----
    hh_nrgintst[[set_scalc]][[3]] <- func_nrgsub(
      nrgori = hh_nrgintst[[set_scalc]][[3]], 
      namenrgoris = list("gas"), 
      namenrgsubs = list("electricity"), 
      yearsubs = list(c(2019, 2060)), 
      propsubs = list(c(0, 0.5)), 
      alterscales = list(0.8))
  }
  
  
  ## Consumption and emission ----
  hh_nrgsecfuel[[set_scalc]] <- 
    func_nrg_sum_ls(hh_nrgintst[[set_scalc]], hh_act[[set_scalc]])
  hh_nrgfuel[[set_scalc]] <- func_ls2df(hh_nrgsecfuel[[set_scalc]])
  hh_diremissum[[set_scalc]] <- 
    func_emissum(hh_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Power generation ----
  ## Energy intensity ----
  # 逐渐下降
  tf_nrgintst[[set_scalc]] <- cbind(
    data.frame(year = c(2019: 2060)), 
    sapply(
      names(tf_nrgintst[["BY"]])[names(tf_nrgintst[["BY"]]) %in% "year" == FALSE], 
      function(i) {
        func_interp_3(year = c(2019, 2025, 2030, 2060), scale = c(1, 1, 1, 0.9), 
                      base = func_lastone(tf_nrgintst[["BY"]][, i]))$value}))
  
  ## Activity level ----
  # 全社会用电量
  tfres_act[[set_scalc]] <- func_ls2df(
    list(agri_nrgfuel[[set_scalc]], ind_nrgfuel[[set_scalc]], 
         const_nrgfuel[[set_scalc]], trans_nrgfuel[[set_scalc]], 
         com_nrgfuel[[set_scalc]], hh_nrgfuel[[set_scalc]]))
  tfres_act[[set_scalc]] <- tfres_act[[set_scalc]][c("year", "electricity")]
  names(tfres_act[[set_scalc]]) <- c("year", "elecuse")
  
  ### BR.LowerCoal ----
  # 本地发电量
  if  (grepl("DECOAL", set_scalc)) { 
    #### DECOAL ----
    # 2026年开始减煤，两年内减为原来的3/4
    tfres_act[[set_scalc]] <- 
      func_merge_2(list(
        tfres_act[[set_scalc]], 
        func_interp_3(
          year = c(2019, 2025, 2028, 2050, 2060), 
          scale = c(1.0, 1.00, 0.75, 0.5, 0.5), 
          base = tfres_act[["BY"]]$elecgen_thrm[tfres_act[["BY"]]$year == 2019],
          "elecgen_thrm")))
  } else { 
    #### BAU ----
    # 2030年开始减煤，十年内减为原来的一半，之后保持
    tfres_act[[set_scalc]] <- 
      func_merge_2(list(
        tfres_act[[set_scalc]], 
        func_interp_3(
          year = c(2019, 2030, 2040, 2060), scale = c(1, 1, 0.5, 0.5), 
          base = tfres_act[["BY"]]$elecgen_thrm[tfres_act[["BY"]]$year == 2019],  
          "elecgen_thrm")))
  }
  
  # 本地清洁发电量
  tfres_act[[set_scalc]]$elecgen_clean <- 
    # 太阳能发电量：刘洋预测
    func_interp_2(year = c(2019, global_solarelecgen_fut$year), 
                  value = c(func_lastone(global_elecgen$"#太阳能"), 
                            global_solarelecgen_fut$"潜在发电量"*10000))$value + 
    # 加上垃圾发电量：继续扩容
    # 环能公司：2025年满负荷运行将达到2019年的2.5倍，保守预测为2倍
    func_interp_3(year = c(2019, 2025, 2035, 2060), scale = c(1, 2, 2.5, 3.0), 
                  base = func_lastone(global_elecgen$"#垃圾发电"))$value + 
    # 加上水电：假设不变
    func_lastone(global_elecgen$"#水电")
  
  # 本地火力发电所用电量
  tfres_act[[set_scalc]]$tfelecuse <- 
    func_nrg_sum(tf_nrgintst[[set_scalc]][c("year", "electricity")], 
                 tfres_act[[set_scalc]], "elecgen_thrm")$electricity
  
  # 生成省电网发电结构
  tfres_provelecgenstr <- data.frame(
    year = c(2019: 2060), 
    thrm_prop = func_interp_2(
      year = c(2019, 2035, 2040, 2050, 2060),
      value = c(0.546504985, 0.291973506, 0.249449437, 0.161468824, 0))$value)
  tfres_provelecgenstr$clean_prop <- 1 - tfres_provelecgenstr$thrm_prop
  
  # 计算外调电力需求并将其分成煤电和清洁能源发电
  tfres_importelec <- data.frame(
    year = c(2019: 2060), 
    importelec = tfres_act[[set_scalc]]$elecuse + 
      tfres_act[[set_scalc]]$tfelecuse - 
      tfres_act[[set_scalc]]$elecgen_thrm - 
      tfres_act[[set_scalc]]$elecgen_clean)
  tfres_importelec <- 
    func_nrg_sum(tfres_provelecgenstr, tfres_importelec, "importelec")
  names(tfres_importelec) <- c("year", "importthrm", "importclean")
  # 合并活动水平
  tfres_act[[set_scalc]] <- 
    func_merge_2(list(tfres_act[[set_scalc]], tfres_importelec))
  
  ## Consumption and emission ----
  tf_nrgfuel[[set_scalc]] <- 
    func_nrg_sum(tf_nrgintst[[set_scalc]],tfres_act[[set_scalc]],"elecgen_thrm")
  tf_diremissum[[set_scalc]] <- 
    func_emissum(tf_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  # Imported elec ----
  ## Energy intensity ----
  # 省电网煤电发电效率保持不变
  res_nrgintst[[set_scalc]] <- cbind(
    data.frame(year = c(2019: 2060)), 
    sapply(names(res_nrgintst[["BY"]])[names(res_nrgintst[["BY"]]) != "year"], 
           function(i) {rep(func_lastone(res_nrgintst[["BY"]][, i]), 42)}))
  
  ## Consumption and emission ----
  res_nrgfuel[[set_scalc]] <- 
    func_nrg_sum(res_nrgintst[[set_scalc]],tfres_act[[set_scalc]],"importthrm")
  res_diremissum[[set_scalc]] <- 
    func_emissum(res_nrgfuel[[set_scalc]], prj_emisfac_df)
  
  
  # Result ----
  tot_results <- func_resultcalc(set_scalc)
  
  tot_nrgsumce[[set_scalc]] <- tot_results[[1]]
  tot_nrgaggfuel[[set_scalc]] <- tot_results[[2]]
  tot_nrgaggfuelce[[set_scalc]] <- tot_results[[3]]
  tot_nrgpropaggfuel[[set_scalc]] <- tot_results[[4]]
  tot_nrgsecce[[set_scalc]] <- tot_results[[5]]
  tot_emissum[[set_scalc]] <- tot_results[[6]]
  tot_emispergdp[[set_scalc]] <- tot_results[[7]]
  tot_emissec[[set_scalc]] <- tot_results[[8]]
  tot_emisaggfuel[[set_scalc]] <- tot_results[[9]]
}
# 查看运行时间
Sys.time() - global_starttime

# Output ----
# 该部分输出项目需要的结果
## Peak times ----
# 输出各情景总量和各部门排放量达峰时间
{
  # 作图：各情景排放总量图
  print(
    ggarrange(plotlist = list(
      func_scompplot(tot_emissum[set_scalcs], "co2") + labs(y = "Emission"), 
      func_scompplot(tot_nrgsumce[set_scalcs], "energyconsump") + 
        labs(y = "Energy")), nrow = 1, common.legend = TRUE)
  )
  
  # 输出各情景能耗和碳排放峰值年份
  idx_peakyear <- data.frame(scenarios = set_scalcs)
  idx_peakyear$nrg_peakyear <- sapply(set_scalcs, function(i) {
    func_peakyear(tot_nrgsumce[[i]], "energyconsump")})
  idx_peakyear$emis_peakyear <- sapply(set_scalcs, function(i) {
    func_peakyear(tot_emissum[[i]], "co2")})
  print(idx_peakyear)
  
  # 输出各情景各部门达峰时间
  idx_peakyearsec <- data.frame(scenarios = set_scalcs)
  for (i in set_scalcs) {
    for (j in global_sectors[1:6]) {
      idx_peakyearsec[which(idx_peakyearsec$scenarios == i), j] <- 
        func_peakyear(tot_emissec[[i]], j)
    }
  }
  print(idx_peakyearsec)
  # 输出时间轴图
  print(
    ggplot(melt(idx_peakyearsec, id = "scenarios"), 
           aes(x = value, y = variable)) + 
      geom_point(aes(color = variable), size = 2) + 
      geom_text(aes(label = value, hjust = -0.3), size = 3) + 
      geom_segment(aes(yend = variable, color = variable), xend = 0, size = 1) +
      xlim(c(2019, 2035)) + facet_wrap(~scenarios, ncol = 1) + 
      theme_bw()
  )
}

## Key index ----
# 输出关键指标
{
  # 各情景下服务业和生活部门电力消费量所占比例
  # 构建列表用于储存结果
  idx_all <- vector("list", length(set_scalcs))
  names(idx_all) <- set_scalcs
  
  for (i in set_scalcs) {
    # 计算服务业和家庭部门电力标准煤占比
    idx_all[[i]] <- data.frame(
      year = c(2019: 2060), 
      # 工业GDP化工占比
      高能耗传统行业增加值比例 = 
        ind_ori_act_prop[[i]]$"化学工业" + 
        ind_ori_act_prop[[i]]$"食品饮料及烟草制造业" +
        ind_ori_act_prop[[i]]$"非金属矿物制品业" +
        ind_ori_act_prop[[i]]$"金属加工制造业" +
        ind_ori_act_prop[[i]]$"石油及炼焦", 
      低能耗传统行业增加值比例 = 
        ind_ori_act_prop[[i]]$"纺织及服装制造业" +
        ind_ori_act_prop[[i]]$"木材及家具制造业" +
        ind_ori_act_prop[[i]]$"造纸及印刷" +
        ind_ori_act_prop[[i]]$"文体工美用品制造业" +
        ind_ori_act_prop[[i]]$"其他制造业",
      新兴行业增加值比例 = 
        ind_ori_act_prop[[i]]$"医药制造业" +
        ind_ori_act_prop[[i]]$"设备制造业" +
        ind_ori_act_prop[[i]]$"电子电气制造业", 
      # 私家车电动车比例
      私家电动车比例 = trans_carprop_ls[[i]]$elec*100, 
      # 家庭人均生活能耗
      人均生活能耗 = tot_nrgsecce[[i]]$hh / prj_global_population$population, 
      # 能源结构
      外调电力消费占比 = 
        tot_nrgaggfuelce[[i]]$"电力"/tot_nrgsumce[[i]]$energyconsump*100 + 3.5, 
      # 外调电力
      外调电力清洁能源占比 = 
        tfres_act[[i]]$importclean/(tfres_act[[i]]$importclean +
                                      tfres_act[[i]]$importthrm), 
      # 工业单位GDP能耗
      备用_工业单位GDP能耗 = tot_nrgsecce[[i]]$ind/prj_global_gdp$indgdp, 
      # 碳排放量
      碳排放量 = tot_emissum[[i]]$co2, 
      # 能耗量
      能耗量 = tot_nrgsumce[[i]]$energyconsump/10000, 
      # 单位GDP碳排放和能耗
      单位GDP碳排放 = func_cross(
        tot_emissum[[i]], prj_global_gdp[c("year", "GDP")], "rate")$co2, 
      单位GDP能耗 = func_cross(
        tot_nrgsumce[[i]], prj_global_gdp[c("year", "GDP")],"rate")$energyconsump
    )
  }
  
  # 输出特定年份结果
  idx_output <- vector("list", length(set_scalcs))
  names(idx_output) <- set_scalcs
  for (i in set_scalcs) {
    idx_output[[i]] <- idx_all[[i]][which(
      idx_all[[i]]$year %in% c(2019, 2020, 2025, 2030, 2035)), ]
    # 添加相对值
    for (j in c("备用_工业单位GDP能耗", "人均生活能耗")) {
      idx_output[[i]][, paste0(j, "变化率")] <- 
        func_conservrate(idx_output[[i]][, j])*100
    }
    # 添加五年变化率
    for (j in c("人均生活能耗")) {
      idx_output[[i]][, paste0(j, "五年变化率")] <- 
        func_ratecalc(idx_output[[i]], j)$rate * 100
    }
    # 添加五年下降率
    for (j in c("单位GDP碳排放", "单位GDP能耗")) {
      idx_output[[i]][, paste0(j, "五年下降率")] <- 
        func_ratecalc(idx_output[[i]], j)$rate * -100
    }
  }
  # 整理为目标格式
  idx_output_long <- 
    func_idxouput(idx_output, baseyear = 2020, digits = 1)
  
  # 增加指标计算并筛选出要用到的指标
  # 问题：本地能源中非化石能源比例均假设为1
  idx_output_long$清洁能源比例 <- 
    idx_output_long$外调电力消费占比 * idx_output_long$外调电力清洁能源占比 + 1
  idx_output_long <- idx_output_long[c(
    "year", "scenario", 
    "高能耗传统行业增加值比例", "低能耗传统行业增加值比例","新兴行业增加值比例",
    "私家电动车比例", "人均生活能耗", "人均生活能耗五年变化率",
    "碳排放量", "能耗量", "单位GDP碳排放", 
    "单位GDP碳排放五年下降率", "单位GDP能耗五年下降率", "清洁能源比例")]
  
  # 作图查看各指标五年变化趋势
  ggarrange(plotlist = list(
    ggplot(idx_output_long, aes(year, 高能耗传统行业增加值比例)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 低能耗传统行业增加值比例)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 新兴行业增加值比例)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 私家电动车比例)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 人均生活能耗五年变化率)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 清洁能源比例)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 能耗量)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 碳排放量)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 单位GDP能耗五年下降率)) + 
      geom_line(aes(color = scenario)), 
    ggplot(idx_output_long, aes(year, 单位GDP碳排放五年下降率)) + 
      geom_line(aes(color = scenario))), 
    nrow = 2, ncol = 5, common.legend = TRUE)
}
# 输出为Excel文件
func_dataexp("各情景下关键指标", mydata = idx_output_long)

## EmisNrgStr ~ aggfuel ----
{
  # 生成各情景下煤油气电占比
  idx_nrgaggfuel_str_ls <- vector("list", length(set_scalcs))
  names(idx_nrgaggfuel_str_ls) <- set_scalcs
  for (i in set_scalcs) {
    idx_nrgaggfuel_str_ls[[i]] <- func_nrg_intst(
      tot_nrgaggfuelce[[i]], tot_nrgsumce[[i]], "energyconsump")
    idx_nrgaggfuel_str_ls[[i]]$scenario <- i
  }
  # 整理数据为所需格式和内容
  idx_nrgpropaggfuel <- 
    func_idxouput(idx_nrgaggfuel_str_ls, baseyear = 2019, digits = 3)
  idx_nrgpropaggfuel[names(idx_nrgpropaggfuel) %in% 
                       c("year", "scenario") == FALSE] <-
    idx_nrgpropaggfuel[names(idx_nrgpropaggfuel) %in% 
                         c("year", "scenario") == FALSE]*100
  names(idx_nrgpropaggfuel)[names(idx_nrgpropaggfuel) %in% 
                              c("year", "scenario") == FALSE] <- 
    paste(names(idx_nrgpropaggfuel)[names(idx_nrgpropaggfuel) %in% 
                                      c("year", "scenario") == FALSE], 
          "能源占比", sep = "")
  
  # 生成各情景下煤油气电排放占比
  idx_emispropaggfuel <- vector("list", length(set_scalcs))
  names(idx_emispropaggfuel) <- set_scalcs
  for (i in set_scalcs) {
    idx_emispropaggfuel[[i]] <- func_nrg_intst(
      tot_emisaggfuel[[i]], tot_emissum[[i]], "co2")
    idx_emispropaggfuel[[i]]$scenario <- i
  }
  idx_emispropaggfuel <- 
    func_idxouput(idx_emispropaggfuel, baseyear = 2019, digits = 3)
  idx_emispropaggfuel[names(idx_emispropaggfuel) %in% 
                        c("year", "scenario") == FALSE] <-
    idx_emispropaggfuel[names(idx_emispropaggfuel) %in% 
                          c("year", "scenario") == FALSE]*100
  names(idx_emispropaggfuel)[names(idx_emispropaggfuel) %in% 
                               c("year", "scenario") == FALSE] <- 
    paste(names(idx_emispropaggfuel)[names(idx_emispropaggfuel) %in% 
                                       c("year", "scenario") == FALSE], 
          "碳排放占比", sep = "")
  
  
  # 合并能源结构和碳排放结构
  report_apptab6 <- cbind(idx_nrgpropaggfuel, idx_emispropaggfuel)
  func_dataexp("能源和碳排放结构", mydata = report_apptab6)
}

## For report ----
{
  # 输出主要结论报告所需表格
  # 表2：减煤情景下厦门市能源与碳排放预测
  report_tab2 <- 
    idx_output_long[c("scenario", "year", "碳排放量", "能耗量", 
                      "单位GDP碳排放五年下降率", "单位GDP能耗五年下降率" )]
  report_tab2 <- rbind(
    report_tab2[which(report_tab2$year == 2020), ],
    report_tab2[which(report_tab2$scenario == "BAU_SLC_DECOAL_OTHER"), ]
  )
  report_tab2 <- cbind(names(report_tab2), as.data.frame(t(report_tab2)))
  rownames(report_tab2) <- NULL
  names(report_tab2) <- as.character(report_tab2[2, ])
  names(report_tab2)[1] <- "item"
  report_tab2 <- report_tab2[which(
    report_tab2$item %in% c("scenario", "year") == FALSE), ]
  report_tab2[2:5] <- sapply(report_tab2[2:5], as.numeric)
  report_tab2[1:2, 2:5] <- round(report_tab2[1:2, 2:5], digits = 0)
  report_tab2[3:4, 2:5] <- round(report_tab2[3:4, 2:5], digits = 1)
  report_tab2[3:4, 2:3] <- "--"
  func_dataexp("主要结论报告表2", mydata = report_tab2)
  
  # 附表4：LEAP模型情景描述
  data.frame(
    新型行业行业比例年均增长量 = sapply(idx_output, function(x) {
      round((tail(x$"新兴行业增加值比例", 1) - 
               head(x$"新兴行业增加值比例", 1))/(2035-2019), digit = 1)
    }), 
    电动车年均增长率 = sapply(trans_act[-1], function(x) {
      round(((x$"纯电动私家车"[x$year == 2035] / 
                head(x$"纯电动私家车", 1))^(1/(2035-2019)) -1)*100)
    })
  )
  
  # 附表7：不同情景下分行业能源消费与碳排放量
  # 能源部分
  report_apptab7_1 <- tot_nrgsecce[-1]
  for (i in set_scalcs) {
    report_apptab7_1[[i]]$scenario <- i
  }
  report_apptab7_1 <- func_idxouput(report_apptab7_1, baseyear = 2020)
  report_apptab7_1[global_sectors[1:6]] <- 
    round(report_apptab7_1[global_sectors[1:6]]/10000)
  names(report_apptab7_1)[names(report_apptab7_1) %in% global_sectors] <- 
    paste(global_sectors[1:6], "nrg", sep = "_")
  
  # 碳排放部分
  report_apptab7_2 <- tot_emissec[-1]
  for (i in set_scalcs) {
    report_apptab7_2[[i]]$scenario <- i
  }
  report_apptab7_2 <- func_idxouput(report_apptab7_2, baseyear = 2020)
  names(report_apptab7_2)[names(report_apptab7_2) %in% global_sectors] <- 
    paste(global_sectors[1:6], "emis", sep = "_")
  
  # 合并两个部分
  report_apptab7 <- cbind(report_apptab7_1, report_apptab7_2)
  report_apptab7 <- report_apptab7[
    c("scenario", "year", 
    paste(rep(global_sectors[1:6], each = 2), c("nrg", "emis"), sep = "_"))]
  func_dataexp("附表7_不同情景下分行业能源消费与碳排放量", 
               mydata = report_apptab7)
}

## Tot emis & Emissec ----
{
  exp_var <- func_mrgcol(tot_emissum[set_scalcs], "co2", set_scalcs)
  func_dataexp("各情景总排放量", mydata = exp_var)
  
  func_dataexp("减煤情景各部门排放量", 
               mydata = tot_emissec$`BAU_SLC_DECOAL_OTHER`)
}

