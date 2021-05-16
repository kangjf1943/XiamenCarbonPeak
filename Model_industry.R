# 工业用能
# 活动水平：GDP
# 用于聚合行业
# 问题：如何强制转化为数字以避免计算错误
func_ind_agg <- function(input_df) {
  new_df <- data.frame(year = input_df[, "year"])
  ind_lookup <- data.frame(ind_agg = c("食品饮料及烟草制造业"), 
                           ind_ori = c("非金属矿采选业", 
                                       "农副食品加工业", 
                                       "食品制造业", 
                                       "酒、饮料和精制茶制造业", 
                                       "烟草制品业"))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("纺织及服装制造业"), 
                                 ind_ori = c("纺织业", 
                                             "纺织服装、服饰业", 
                                             "皮革、毛皮、羽毛及其制品和制鞋业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("木材及家具制造业"), 
                                 ind_ori = c("木材加工和木、竹、藤、棕、草制品业", 
                                             "家具制造业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("造纸及印刷"), 
                                 ind_ori = c("造纸和纸制品业", 
                                             "印刷和记录媒介复制业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("文体工美用品制造业"), 
                                 ind_ori = c("文教、工美、体育和娱乐用品制造业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("石油及炼焦"), 
                                 ind_ori = c("石油加工、炼焦和核燃料加工业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("化学工业"), 
                                 ind_ori = c("化学原料和化学制品制造业", 
                                             "化学纤维制造业", 
                                             "橡胶和塑料制品业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("医药制造业"), 
                                 ind_ori = c("医药制造业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("非金属矿物制品业"), 
                                 ind_ori = c("非金属矿物制品业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("金属加工制造业"), 
                                 ind_ori = c("黑色金属冶炼和压延加工业", 
                                             "有色金属冶炼和压延加工业", 
                                             "金属制品业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("设备制造业"), 
                                 ind_ori = c("通用设备制造业", 
                                             "专用设备制造业", 
                                             "交通运输设备制造业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("电子电气制造业"), 
                                 ind_ori = c("电气机械和器材制造业", 
                                             "计算机、通信和其他电子设备制造业", 
                                             "仪器仪表制造业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("其他制造业"), 
                                 ind_ori = c("其他制造业", 
                                             "废弃资源综合利用业", 
                                             "金属制品、机械和设备修理业", 
                                             "燃气生产和供应业", 
                                             "水的生产和供应业")))
  ind_lookup <- rbind(ind_lookup, 
                      data.frame(ind_agg = c("电力、热力生产和供应业"), 
                                 ind_ori = c("电力、热力生产和供应业")))
  ind_agg_list <- unique(ind_lookup$ind_agg)
  for (i in c(1:length(ind_agg_list))) {
    name_ind_agg <- ind_agg_list[i]
    name_ind_ori <- ind_lookup$ind_ori[ind_lookup$ind_agg == name_ind_agg]
    new_df[, name_ind_agg] <- 
      rowSums(input_df[names(input_df) %in% name_ind_ori], na.rm = TRUE)
  }
  new_df
}
# 聚合成的能源大类和行业大类
itemnames <- c("coal", "coal_product", "gasoline", "diesel", "residual", "lpg", "natural_gas", "electricity")


ind_gdp <- func_read_trans("7TP7UDE6", "工业GDP新")
ind_gdp_agg <- func_ind_agg(ind_gpd)
ind_gdp_agg
func_show_trend(ind_gdp_agg)
# 假设2018-2019年各行业GDP所占比重和2017年相同
ind_gdp_agg_prop <- ind_gdp_agg[, -1]/rowSums(ind_gdp_agg[, -1])
ind_gdp_agg_prop$year <- ind_gdp_agg$year
func_show_trend(ind_gdp_agg_prop)
ind_gdp_agg_prop[which(ind_gdp_agg_prop$year %in% c(2018, 2019)), 
                 -ncol(ind_gdp_agg_prop)] <- 
  ind_gdp_agg_prop[which(ind_gdp_agg_prop$year == 2017), 
                   -ncol(ind_gdp_agg_prop)]

func_merge_rate(ind_gdp_agg_prop[, c("year", i)], i, 
                proj_gdp_ind, "value", 
                method = "product")

# 那么各年份工业各行业GDP为
ind_gdp_agg_whole <- ind_gdp_agg_prop
for (i in names(ind_gdp_agg_whole)) {
  ind_gdp_agg_whole[, i] <- 
    func_merge_rate(ind_gdp_agg_prop[, c("year", i)], i, 
                    proj_gdp_ind, "value", 
                    method = "product", startyear = 2000, endyear = 2019)$Rate
  names(ind_gdp_agg_whole)[which(names(ind_gdp_agg_whole) == i)] <- i
}
func_show_trend(ind_gdp_agg_whole)

# 读取各行业各类能耗总量
# 原本的数据是按能源分类的
ind_nrgsum_ls <- vector("list", 8)
tbl_read <- c("煤新", "煤制品新", "汽油新", "柴油新", "燃料油新", "液化石油气新", 
              "天然气新", "电力新")
for (i in c(1: 8)) {
  ind_nrgsum_ls[[i]] <- func_read_trans("7TP7UDE6", tbl_read[i])
  ind_nrgsum_ls[[i]] <- func_ind_agg(ind_nrgsum_ls[[i]])
}

# 转化为按行业分的能耗总量
itemnames <- c("coal", "coal_product", "gasoline", "diesel", "residual", "lpg", "natural_gas", "electricity")
ind_nrgsum_ls_2 <- vector("list")
for (i in c(1: 14)) {
  ind_nrgsum_ls_2[[i]] <- data.frame(year = c(2009:2019))
  for (j in c(1: 8)) {
    ind_nrgsum_ls_2[[i]] <- cbind(ind_nrgsum_ls_2[[i]], ind_nrgsum_ls[[j]][, (i+1)])
  }
  names(ind_nrgsum_ls_2[[i]]) <- c("year", itemnames)
}

# 活动强度：单位GDP能耗
# 各行业多能源消费强度
ind_agg_list <- c("食品饮料及烟草制造业", "纺织及服装制造业", "木材及家具制造业", 
                  "造纸及印刷", "文体工美用品制造业",    
                  "石油及炼焦", "化学工业", "医药制造业",
                  "非金属矿物制品业", "金属加工制造业",   "设备制造业",      
                  "电子电气制造业",   "其他制造业", "电力、热力生产和供应业")
ind_nrgintst_ls <- vector("list", 14)
for (i in c(1: 14)) {
  ind_nrgintst_ls[[i]] <- data.frame(year = c(2009:2019))
  par(mfrow = c(3, 3))
  for (j in c(1: 8)) {
    ind_nrgintst_ls[[i]] <- cbind(ind_nrgintst_ls[[i]], 
                                  func_merge_rate(ind_nrgsum_ls_2[[i]], itemnames[j], 
                                                  ind_gdp_agg, ind_agg_list[i], "rate")$Rate)
  }
  names(ind_nrgintst_ls[[i]]) <- c("year", itemnames)
}
names(ind_nrgintst_ls) <- ind_agg_list


# 构建二级行业活动水平矩阵 - 预测
proj_ind_gdp <- data.frame(c(2005:2050), 
                           proj_gdp$value * proj_gdp_ind_prop$value)
proj_ind_gdp_agg_prop <- data.frame(c(2005:2050))
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[1]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 5, 5, 5)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[2]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 4, 4, 4)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[3]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1.5, 1.5, 2)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[4]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1, 1, 0.5)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[5]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 3, 3, 5)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[6]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 0.06, 0.06, 0.05)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[7]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1, 1, 0)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[8]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 10, 10, 12)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[9]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1, 1, 0.5)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[10]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 4, 4, 4)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[11]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 15, 15, 12)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[12]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 45, 45, 45)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[13]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1, 1, 0.5)))$value
proj_ind_gdp_agg_prop[, names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[14]] <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0, 1, 1, 0.5)))$value
dim(proj_ind_gdp_agg_prop)
names(proj_ind_gdp_agg_prop)[1] <- "year"
names(proj_ind_gdp)[1] <- "year"

proj_ind_gdp_agg <- func_nrg_sum(proj_ind_gdp_agg_prop, proj_ind_gdp, 
                                 "proj_gdp.value...proj_gdp_ind_prop.value")
func_show_trend(proj_ind_gdp_agg)

# 然后构建各子行业的各种能耗预测
# 第一个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[1]
input_ls_1 <- vector("list")
input_ls_1[[1]] <- data.frame(year = c(2005,2019,2030,2040,2050), 
                              value = c(0, 
                                        1.80924338673981E-5, 
                                        1.80924338673981E-5*0.8,
                                        1.80924338673981E-5*0.4,
                                        1.80924338673981E-5*0.3))
input_ls_1[[2]] <- data.frame(year = c(2005, 2050), 
                              value = c(2.55396298892317E-6, 2.55396298892317E-6))
input_ls_1[[3]] <- data.frame(year = c(2005, 2050), 
                              value = c(2.79812530149773E-7, 2.79812530149773E-7))
input_ls_1[[4]] <- data.frame(year = c(2005, 2050), 
                              value = c(5.39151768132654E-7, 5.39151768132654E-7))
input_ls_1[[5]] <- data.frame(year = c(2005, 2050), 
                              value = c(2.99998564052684E-7, 2.99998564052684E-7))
input_ls_1[[6]] <- data.frame(year = c(2005, 2050), 
                              value = c(8.9945677010286E-8, 8.9945677010286E-8))
input_ls_1[[7]] <- data.frame(year = c(2005, 2050), 
                              value = c(0.000222816383659193, 0.000222816383659193))
input_ls_1[[8]] <- data.frame(year = c(2005, 2019, 2030,2050), 
                              value = c(0, 
                                        0.0922413805542146, 
                                        0.0922413805542146*0.92, 
                                        0.0922413805542146*0.85))

# 第二个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[2]
input_ls_2 <- vector("list")
input_ls_2[[1]] <- data.frame(year = c(2005, 2050), 
                              value = c(8.46588638948107E-4, 8.46588638948107E-4))
input_ls_2[[2]] <- data.frame(year = c(2005, 2050), 
                              value = c(9.95789408980545E-3, 9.95789408980545E-3))
input_ls_2[[3]] <- data.frame(year = c(2005, 2050), 
                              value = c(2.37783999764108E-3, 2.37783999764108E-3))
input_ls_2[[4]] <- data.frame(year = c(2005, 2050), 
                              value = c(1.5860707371884E-3, 1.5860707371884E-3))
input_ls_2[[5]] <- data.frame(year = c(2005, 2050), 
                              value = c(1.66262559901535E-3, 1.66262559901535E-3))
input_ls_2[[6]] <- data.frame(year = c(2005, 2050), 
                              value = c(1.97555810901631E-3, 1.97555810901631E-3))
input_ls_2[[7]] <- data.frame(year = c(2005, 2050), 
                              value = c(0.00195237139153439, 0.00195237139153439))
input_ls_2[[8]] <- data.frame(year = c(2005, 2019, 2030, 2050), 
                              value = c(0,0.0707766044727504, 
                                        0.0707766044727504*0.92,
                                        0.0707766044727504*0.85))
# 第三个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[3]
input_ls_3 <- vector("list")
for (i in c(1: 8)) {
  input_ls_3[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(ind_nrgintst_ls[[3]][which(ind_nrgintst_ls[[3]]$year == 2015), ][1, i + 1]))
}
input_ls_3

# 第四个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[4]
func_show_trend(ind_nrgintst_ls[[4]])
input_ls_4 <- vector("list")
for (i in c(1: 8)) {
  input_ls_4[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(ind_nrgintst_ls[[4]][which(ind_nrgintst_ls[[4]]$year == 2015), ][1, i + 1]))
}
input_ls_4

# 第五个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[5]
func_show_trend(ind_nrgintst_ls[[5]])
input_ls_5 <- vector("list")
input_ls_5[[5]] <- data.frame(year = c(2005, 2050), 
                              value = c(ind_nrgintst_ls[[6]][which(ind_nrgintst_ls[[6]]$year == 2015), ][1,6], ind_nrgintst_ls[[6]][which(ind_nrgintst_ls[[6]]$year == 2015), ][1,6]*0.6))
for (i in c(1: 4, 6:8)) {
  input_ls_5[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(ind_nrgintst_ls[[5]][which(ind_nrgintst_ls[[5]]$year == 2015), ][1, i + 1]))
}
input_ls_5

# 第六个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[6]
func_show_trend(ind_nrgintst_ls[[6]])
input_ls_6 <- vector("list")
input_ls_6[[1]] <- data.frame(year = c(2005, 2050), 
                              value = c(
                                ind_nrgintst_ls[[6]][which(ind_nrgintst_ls[[6]]$year 
                                                           == 2015), ][1,2],
                                ind_nrgintst_ls[[6]][which(ind_nrgintst_ls[[6]]$year 
                                                           == 2015), ][1,2]*0.6))
for (i in c(2: 8)) {
  input_ls_6[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(
                                  ind_nrgintst_ls[[6]][which(ind_nrgintst_ls[[6]]$year 
                                                             == 2015), ][1, i + 1]))
}
input_ls_6

# 第七个行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[7]
func_show_trend(ind_nrgintst_ls[[7]])
input_ls_7 <- vector("list")
for (i in c(1: 8)) {
  input_ls_7[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(
                                  ind_nrgintst_ls[[7]][which(ind_nrgintst_ls[[7]]$year 
                                                             == 2015), ][1, i + 1]))
}
input_ls_7

# 行业
names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[8]
func_show_trend(ind_nrgintst_ls[[8]])
input_ls_8 <- vector("list")
input_ls_8[[8]] <- data.frame(year = c(2005, 2050), 
                              value = c(
                                ind_nrgintst_ls[[8]][which(ind_nrgintst_ls[[8]]$year 
                                                           == 2015), ][1,9],
                                ind_nrgintst_ls[[8]][which(ind_nrgintst_ls[[8]]$year 
                                                           == 2015), ][1,9]*0.6))
for (i in c(1: 7)) {
  input_ls_8[[i]] <- data.frame(year = c(2005, 2050), 
                                value = c(
                                  ind_nrgintst_ls[[8]][which(ind_nrgintst_ls[[8]]$year 
                                                             == 2015), ][1, i + 1]))
}
input_ls_8

# 行业九到行业十四
for (j in c(9: 14)) {
  names(ind_gdp_agg_prop[, -ncol(ind_gdp_agg_prop)])[j]
  func_show_trend(ind_nrgintst_ls[[j]])
  assign(paste0("input_ls_", j), vector("list"))
}
# 赋值函数
func_input_ls <- function(input_ls_k, k, ind_nrgintst_ls = ind_nrgintst_ls) {
  for (i in c(1: 8)) {
    input_ls_k[[i]] <- data.frame(year = c(2005, 2050), 
                                  value = c(
                                    ind_nrgintst_ls[[k]][which(ind_nrgintst_ls[[k]]$year 
                                                               == 2015), ][1, i + 1]))
  }
  input_ls_k
}
# 开始赋值
input_ls_9 <- func_input_ls(input_ls_10, 9, ind_nrgintst_ls)
input_ls_10 <- func_input_ls(input_ls_10, 10, ind_nrgintst_ls)
input_ls_11 <- func_input_ls(input_ls_10, 11, ind_nrgintst_ls)
input_ls_12 <- func_input_ls(input_ls_10, 12, ind_nrgintst_ls)
input_ls_13 <- func_input_ls(input_ls_10, 13, ind_nrgintst_ls)
input_ls_14 <- func_input_ls(input_ls_10, 14, ind_nrgintst_ls)

input_ls <- vector("list")
input_ls[[1]] <- input_ls_1
input_ls[[2]] <- input_ls_2
input_ls[[3]] <- input_ls_3
input_ls[[4]] <- input_ls_4
input_ls[[5]] <- input_ls_5
input_ls[[6]] <- input_ls_6
input_ls[[7]] <- input_ls_7
input_ls[[8]] <- input_ls_8
input_ls[[9]] <- input_ls_9
input_ls[[10]] <- input_ls_10
input_ls[[11]] <- input_ls_11
input_ls[[12]] <- input_ls_12
input_ls[[13]] <- input_ls_13
input_ls[[14]] <- input_ls_14

# 最后一起算预测的各行业各种能耗强度列表和预测总能耗列表
proj_ind_nrgintst_ls <- vector("list", 14)
for (i in c(1: 14)) {
  proj_ind_nrgintst_ls[[i]] <- func_proj(input_ls = input_ls[[i]], 
                                         itemnames = itemnames)
}

proj_ind_nrgsum_ls <- vector("list", 14)
for (i in c(1:14)) {
  proj_ind_nrgsum_ls[[i]] <- func_nrg_sum(proj_ind_nrgintst_ls[[i]], 
                                          proj_ind_gdp_agg, ind_agg_list[i])
}
func_show_trend(proj_ind_nrgsum_ls[[2]])