# 工业用能
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
ind_subsector <- c("食品饮料及烟草制造业", 
                   "纺织及服装制造业", "木材及家具制造业", 
                   "造纸及印刷", "文体工美用品制造业",    
                   "石油及炼焦", "化学工业", "医药制造业",
                   "非金属矿物制品业", "金属加工制造业",   
                   "设备制造业",      
                   "电子电气制造业",   "其他制造业",
                   "电力、热力生产和供应业")
ind_nrgclass <- c("coal", "coalproduct", 
               "gasoline", "diesel", "residual", "lpg", 
               "gas", "electricity")

# 历史分析
# 活动水平：各行业GDP
ind_act <- func_read_trans("7TP7UDE6", "工业GDP")
ind_act <- func_ind_agg(ind_act)
func_show_trend(ind_act)
# 计算各子部门所占比例
ind_ori_act_prop <- ind_act[, -1]/rowSums(ind_act[, -1])*100
ind_ori_act_prop$year <- ind_act$year
ind_ori_act_prop <- ind_ori_act_prop[c("year", ind_subsector)]
# 读取各行业各类能耗总量
# 原本的数据是按能源分类的
ind_ori_nrgsum_ls <- vector("list", 8)
tbl_read <- c("煤", "煤制品", "汽油", "柴油", "燃料油", "液化石油气", 
              "天然气", "电力")
for (i in c(1: 8)) {
  ind_ori_nrgsum_ls[[i]] <- func_read_trans("7TP7UDE6", tbl_read[i])
  ind_ori_nrgsum_ls[[i]] <- func_ind_agg(ind_ori_nrgsum_ls[[i]])
}
names(ind_ori_nrgsum_ls) <- ind_nrgclass
# 转化为按行业分的能耗总量
ind_nrgsum_ls <- func_ls_transition(ind_ori_nrgsum_ls)
length(ind_nrgsum_ls)
# 活动强度：单位GDP能耗
ind_nrgintst_ls <- func_nrg_intst_ls(ind_nrgsum_ls, ind_act)
func_show_trend_ls(ind_nrgintst_ls)

# 预测未来
# 活动水平
# 先计算未来子部门GDP所占比重
proj_ind_ori_act_prop <- data.frame(year = c(2019:2050))
proj_ind_ori_act_prop[, ind_subsector[1]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(5, 5, 5))$value
proj_ind_ori_act_prop[, ind_subsector[2]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(6, 6, 6))$value
proj_ind_ori_act_prop[, ind_subsector[3]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(1.5, 1.5, 2))$value
proj_ind_ori_act_prop[, ind_subsector[4]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(1.5, 1.5, 0.5))$value
proj_ind_ori_act_prop[, ind_subsector[5]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(2, 2, 4))$value
proj_ind_ori_act_prop[, ind_subsector[6]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(0.3, 0.3, 0.1))$value
proj_ind_ori_act_prop[, ind_subsector[7]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(7.4, 5, 0))$value
proj_ind_ori_act_prop[, ind_subsector[8]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(1.6, 1, 0.7))$value
proj_ind_ori_act_prop[, ind_subsector[9]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(1.8, 1.3, 0.5))$value
proj_ind_ori_act_prop[, ind_subsector[10]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(7.6, 7.6, 7.6))$value
proj_ind_ori_act_prop[, ind_subsector[11]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(12, 10, 9))$value
proj_ind_ori_act_prop[, ind_subsector[12]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(49, 49, 49))$value
proj_ind_ori_act_prop[, ind_subsector[13]] <- 
  func_interp_2(year = c(2019, 2030, 2050), value = c(2.5, 2.5, 0.5))$value
proj_ind_ori_act_prop[, ind_subsector[14]] <- func_interp_2(
  year = c(2019, 2030, 2050), 
  value = c(100 - sum(proj_ind_ori_act_prop[2:14][proj_ind_ori_act_prop$year == 2019, ]), 
            100 - sum(proj_ind_ori_act_prop[2:14][proj_ind_ori_act_prop$year == 2030, ]), 
            100 - sum(proj_ind_ori_act_prop[2:14][proj_ind_ori_act_prop$year == 2050, ])
            ))$value
# 计算未来各子部门GDP
proj_ind_act <- func_nrg_sum(proj_ind_ori_act_prop, proj_gdp_ind, "value")
proj_ind_act[ind_subsector] <- proj_ind_act[ind_subsector]/100
func_show_trend(proj_ind_act)

# 活动强度：假设保持不变
proj_ind_nrgintst_ls <- vector("list", 14)
names(proj_ind_nrgintst_ls) <- ind_subsector
for (i in ind_subsector) {
  proj_ind_nrgintst_ls[[i]] <- data.frame(year = c(2019: 2050))
  for (j in ind_nrgclass) {
    proj_ind_nrgintst_ls[[i]][, j] <- func_lastone(ind_nrgintst_ls[[i]][, j])
  }
}

# 未来总能耗
proj_ind_nrgsum_ls <- func_nrg_sum_ls(proj_ind_nrgintst_ls, proj_ind_act)

# 检查结果
func_history_project_df(ind_act, proj_ind_act)
func_history_project_ls(ind_nrgintst_ls, proj_ind_nrgintst_ls)
func_history_project_ls(ind_nrgsum_ls, proj_ind_nrgsum_ls)

