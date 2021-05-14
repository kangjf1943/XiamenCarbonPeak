## 交通
## 历史数据
# 活动水平：营运车辆和非营运车辆里程数
ori_operation_mileage <- func_read_trans("IZM9FWIY", "里程数")
ori_operation_mileage$year <- as.numeric(ori_operation_mileage$year)
ori_operation_mileage <- 
  ori_operation_mileage[, c("year", "常规公交", "BRT", "出租车", "农村客车")]

ori_nonoperation_number <- func_read_trans("Y3PGVSR7")
ori_nonoperation_number <- 
  ori_nonoperation_number[c("year", 
                            "摩托车",
                            "轿车", 
                            "轻型客车 ", "大型客车 ", 
                            "轻型货车 ", "中型货车 ", "重型货车 ", 
                            "农用运输车")]
ori_nonoperation_mileagepervehicle <- 
  func_read_trans("Y3PGVSR7", "非营运性车辆年均里程数")

ori_nonoperation_mileage <- ori_nonoperation_number
ori_nonoperation_mileage <- apply(ori_nonoperation_mileage, 2, as.numeric)
ori_nonoperation_mileage <- as.data.frame(ori_nonoperation_mileage)
for (i in c(2:nrow(ori_nonoperation_mileage))) {
  ori_nonoperation_mileage[i, -1] <- ori_nonoperation_mileage[i, -1] * 
    ori_nonoperation_mileagepervehicle[1,-1]
}

trans_act <- Reduce(func_merge, 
                    list(ori_operation_mileage, 
                         ori_nonoperation_mileage))
trans_act$"航空" <- c(1)

ori_water_act <- func_read_trans("P6KQQFUP")
trans_act <- Reduce(func_merge, 
                    list(trans_act, 
                         ori_water_act[c("year", "客运周转量")], 
                         ori_water_act[c("year", "货运周转量")]))
names(trans_act)[names(trans_act) == "客运周转量"] <- "水路客运周转量"
names(trans_act)[names(trans_act) == "货运周转量"] <- "水路货运周转量"

func_looknote(trans_act)
func_show_trend(trans_act[, c("year", "航空")])
func_show_trend(trans_act[, c("year", "轿车")])
func_show_trend(trans_act[names(trans_act) %in% "轿车" == FALSE])

# 能耗总量和能耗强度：营运车辆部分先输入总量后算强度，非营运车辆则相反
# 营运车辆部分
# 能耗总量
trans_nrgsum_ls <- vector("list")
# 各类车总能耗：按能源类型分
tbl_read <- c("汽油消费量", "柴油消费量", "天然气消费量")
for (i in c(1:length(tbl_read))) {
  trans_nrgsum_ls[[i]] <- func_read_trans("IZM9FWIY", tbl_read[i])
}
# 先筛选和计算数据
# 汽油消费量
trans_nrgsum_ls[[1]] <- 
  trans_nrgsum_ls[[1]][, c("year", "出租车合计", "常规公交", "BRT")]
names(trans_nrgsum_ls[[1]])[2] <- "出租车"
# 汽油密度0.72千克/升 = 7.2吨/万升
# 柴油密度0.85千克/升 = 8.5吨/万升
trans_nrgsum_ls[[1]]$"常规公交" <- trans_nrgsum_ls[[1]]$"常规公交" * 7.2
comment(trans_nrgsum_ls[[1]]$常规公交) <- "吨"
trans_nrgsum_ls[[1]]$BRT <- trans_nrgsum_ls[[1]]$BRT * 7.2
comment(trans_nrgsum_ls[[1]]$BRT) <- "吨"
func_looknote(trans_nrgsum_ls[[1]])
# 柴油消费量
trans_nrgsum_ls[[2]]
func_looknote(trans_nrgsum_ls[[2]])
trans_nrgsum_ls[[2]] <- 
  trans_nrgsum_ls[[2]][c("year", "常规公交新数据", "BRT新数据", "农村客车")]
names(trans_nrgsum_ls[[2]]) <- c("year", "常规公交", "BRT", "农村客车")
trans_nrgsum_ls[[2]]$常规公交 <- trans_nrgsum_ls[[2]]$常规公交 * 8.5
comment(trans_nrgsum_ls[[2]]$常规公交) <- "吨"
trans_nrgsum_ls[[2]]$BRT <- trans_nrgsum_ls[[2]]$BRT * 8.5
comment(trans_nrgsum_ls[[2]]$BRT) <- "吨"
# 天然气消费量
trans_nrgsum_ls[[3]]
func_looknote(trans_nrgsum_ls[[3]])
trans_nrgsum_ls[[3]] <- 
  trans_nrgsum_ls[[3]][c("year", "公交合计", "BRT", "出租车合计")]
names(trans_nrgsum_ls[[3]]) <- 
  c("year", "常规公交", "BRT", "出租车")

# 转化为按车辆类型分的能耗量列表
for (i in c(1:length(trans_nrgsum_ls))) {
  cat(i, "\n")
  print(names(trans_nrgsum_ls[[i]]))
  cat("\n")
}
trans_nrgsum_ls_2 <- vector("list")
operation_vehicle_list <- c("常规公交", "BRT", "出租车", "农村客车")
for (i in c(1:length(operation_vehicle_list))) {
  trans_nrgsum_ls_2[[i]] <- 
    data.frame(year = trans_nrgsum_ls[[1]]$year)
  if (operation_vehicle_list[i] %in% names(trans_nrgsum_ls[[1]])) {
    trans_nrgsum_ls_2[[i]]$gasoline = 
      trans_nrgsum_ls[[1]][, operation_vehicle_list[i]]
  }
  if (operation_vehicle_list[i] %in% names(trans_nrgsum_ls[[2]])) {
    trans_nrgsum_ls_2[[i]]$diesel = 
      trans_nrgsum_ls[[2]][, operation_vehicle_list[i]]
  }
  if (operation_vehicle_list[i] %in% names(trans_nrgsum_ls[[3]])) {
    trans_nrgsum_ls_2[[i]]$natural_gas = 
      trans_nrgsum_ls[[3]][, operation_vehicle_list[i]]
  }
}
names(trans_nrgsum_ls_2) <- operation_vehicle_list

# 能耗强度
trans_nrgintst_ls <- vector("list")
for (i in c(1:4)) {
  trans_nrgintst_ls[[i]] <- 
    func_nrg_intst(trans_nrgsum_ls_2[[i]], trans_act, operation_vehicle_list[i])
}
names(trans_nrgintst_ls) <- c("常规公交", "BRT", "出租车", "农村客车")
func_show_trend(trans_nrgintst_ls[[4]])

# 非营运车辆部分
# 每辆车的能耗强度
tbl_read <- c("摩托车",
              "轿车", 
              "轻型客车", "大型客车", 
              "轻型货车", "中型货车", "重型货车", 
              "农用运输车")
names(ori_nonoperation_mileagepervehicle) <- c("year", tbl_read)
for (i in tbl_read) {
  trans_nrgintst_ls[[i]] <- func_read_trans("Z9S24XNM", i)
  year <- c(2005: 2050)
  value <- rep(trans_nrgintst_ls[[i]][1, 2] *
                 ori_nonoperation_mileagepervehicle[1, i] * 0.1, 
               length(c(2005: 2050)))
  names <- c("year", names(trans_nrgintst_ls[[i]])[2])
  trans_nrgintst_ls[[i]] <- data.frame(year, value)
  names(trans_nrgintst_ls[[i]]) <- names
}
# 计算能耗总量
colnames(trans_act)[c(6:13)] <- tbl_read
trans_nrgsum_ls_2 <- c(trans_nrgsum_ls_2, 
                       func_nrg_sum_ls(trans_nrgintst_ls[c(5:12)], 
                                       trans_act[, c("year", tbl_read)]))
names(trans_nrgsum_ls_2)[5:12] <- tbl_read

# 航空部分
trans_nrgsum_ls_2$"航空" <- func_read_trans("JXG6KGSA")[, c("year", "国内")]

# 水运部分
trans_nrgsum_ls_2$"水路客运" <- func_read_trans("NTNZD6VV", "国内客运")
trans_nrgsum_ls_2$"水路货运" <- func_read_trans("NTNZD6VV", "国内货运")

## 预测未来
# 活动水平
proj_trans_act <- data.frame(year = c(2019: 2050))
proj_trans_act$"常规公交" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$常规公交[which(trans_act$year == 2017)], 
                          trans_act$常规公交[which(trans_act$year == 2017)]*1.05, 
                          trans_act$常规公交[which(trans_act$year == 2017)]*1.07))$value
proj_trans_act$"BRT" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$BRT[which(trans_act$year == 2017)], 
                          trans_act$BRT[which(trans_act$year == 2017)]*1.05, 
                          trans_act$BRT[which(trans_act$year == 2017)]*1.07*1.01))$value
proj_trans_act$"出租车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$出租车[which(trans_act$year == 2017)], 
                          trans_act$出租车[which(trans_act$year == 2017)]*1.05, 
                          trans_act$出租车[which(trans_act$year == 2017)]*1.07*1.01))$value
proj_trans_act$"农村客车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$农村客车[which(trans_act$year == 2014)], 
                          trans_act$农村客车[which(trans_act$year == 2014)]*1.05, 
                          trans_act$农村客车[which(trans_act$year == 2014)]*1.07*1.01))$value

proj_trans_act$"摩托车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$摩托车[which(trans_act$year == 2019)], 
                          trans_act$摩托车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$摩托车[which(trans_act$year == 2019)]*1.07*1.01))$value
func_history_project(trans_act, "摩托车", proj_trans_act, "摩托车")

proj_trans_act$"轿车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$轿车[which(trans_act$year == 2019)], 
                          trans_act$轿车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$轿车[which(trans_act$year == 2019)]*1.07*1.01))$value
func_history_project(trans_act, "轿车", proj_trans_act, "轿车")

proj_trans_act$"轻型客车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$轻型客车[which(trans_act$year == 2019)], 
                          trans_act$轻型客车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$轻型客车[which(trans_act$year == 2019)]*1.07*1.01))$value
func_history_project(trans_act, "轻型客车", proj_trans_act, "轻型客车")

proj_trans_act$"大型客车"  <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$大型客车[which(trans_act$year == 2019)], 
                          trans_act$大型客车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$大型客车[which(trans_act$year == 2019)]*1.07*1.01))$value
func_history_project(trans_act, "大型客车", proj_trans_act, "大型客车")

proj_trans_act$"轻型货车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$轻型货车[which(trans_act$year == 2019)], 
                          trans_act$轻型货车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$轻型货车[which(trans_act$year == 2019)]*1.07*1.01))$value
func_history_project(trans_act, "轻型货车", proj_trans_act, "轻型货车")

proj_trans_act$"中型货车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$中型货车[which(trans_act$year == 2019)], 
                          trans_act$中型货车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$中型货车[which(trans_act$year == 2019)]*1.07*1.01))$value

proj_trans_act$"重型货车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$重型货车[which(trans_act$year == 2019)], 
                          trans_act$重型货车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$重型货车[which(trans_act$year == 2019)]*1.07*1.01))$value

proj_trans_act$"农用运输车" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(trans_act$农用运输车[which(trans_act$year == 2019)], 
                          trans_act$农用运输车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$农用运输车[which(trans_act$year == 2019)]*1.07*1.01))$value

proj_trans_act$"航空" <- rep(1, length(c(2019: 2050)))

proj_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(10000, 10500, 11000))$value
comment(proj_trans_act$"水路客运") <- "万人公里"

proj_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(proj_trans_act$"水路货运") <- "万吨公里"
func_history_project(trans_act, "水路货运周转量", proj_trans_act, "水路货运")

# 活动强度
proj_trans_nrgintst_ls <- vector("list", 12)
# 常规公交，BRT，出租车，农村客车，摩托车，轿车，轻型客车，大型客车，轻型货车，重型货车，重型货车，农用运输车 - 均假设和最后一个有数值的年份一致，且假设这个数值是在2019年
for (j in c(1: 12)) {
  proj_trans_nrgintst_ls[[j]] <- data.frame(year = c(2019: 2050))
  for (i in names(trans_nrgintst_ls[[j]])[
    names(trans_nrgintst_ls[[j]]) %in% "year" == FALSE]) {
    cleaned_value <- trans_nrgintst_ls[[j]][, i][
      is.na(trans_nrgintst_ls[[j]][, i]) == FALSE]
    cleaned_value <- cleaned_value[cleaned_value != 0]
    proj_trans_nrgintst_ls[[j]][, i] <- tail(cleaned_value, 1)
  }
}

proj_trans_nrgintst_ls[["航空"]] <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(202e4, 202e4, 300e4, 320e4))
names(proj_trans_nrgintst_ls[["航空"]])[2] <- "kerosene"

proj_trans_nrgintst_ls[["水路客运"]] <- data.frame(year = c(2005: 2050))
proj_trans_nrgintst_ls[["水路客运"]]$residual <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(1.40, 1.40, 1.40*0.85, 1.40*0.80))$value
proj_trans_nrgintst_ls[["水路客运"]]$diesel <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.80, 0.80, 0.80*0.85, 0.80*0.80))$value

proj_trans_nrgintst_ls[["水路货运"]] <- data.frame(year = c(2005: 2050))
proj_trans_nrgintst_ls[["水路货运"]]$residual <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.04649123, 0.04649123, 0.04649123*0.85, 0.04649123*0.80))$value
proj_trans_nrgintst_ls[["水路货运"]]$diesel <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.00476, 0.00476, 0.00476*0.85, 0.00476*0.80))$value

# 能耗总量
proj_trans_nrgsum_ls <- func_nrg_sum_ls(proj_trans_nrgintst_ls, proj_trans_act)

# 对比历史和预测数据
func_history_project_df(trans_act, proj_trans_act)
func_history_project_ls(trans_nrgintst_ls, proj_trans_nrgintst_ls)
func_history_project_ls(trans_nrgsum_ls, proj_trans_nrgsum_ls)

func_history_project(trans_act, "水路货运周转量", proj_trans_act, "水路货运")
plot(trans_act$year, trans_act$水路货运周转量)