## 交通部门
# 子部门
trans_subsector <- c("常规公交", "快速公交", "出租车", "农村客车", 
                     "摩托车", "轿车", 
                     "轻型客车", "大型客车", 
                     "轻型货车", "中型货车", "重型货车", 
                     "农用运输车", 
                     "航空", 
                     "水路客运", 
                     "水路货运")

## 历史数据
# 活动水平
# 营运车辆里程数
trans_act_operation <- func_read_trans("IZM9FWIY", "里程数")
trans_act_operation <- 
  trans_act_operation[, c("year", "常规公交", "BRT", "出租车", "农村客车")]
names(trans_act_operation) <- c("year", "常规公交", "快速公交", "出租车", "农村客车")

# 非营运车辆数量
trans_act_nonoperation <- func_read_trans("Y3PGVSR7")
trans_act_nonoperation <- trans_act_nonoperation[, c("year", "摩托车", "轿车", 
                                         "轻型客车", "大型客车", 
                                         "轻型货车", "中型货车", "重型货车", 
                                         "农用运输车")]
# 航空活动水平
trans_act_aviation <- data.frame(year = c(2005: 2019), 
                                 航空活动水平 = c(1))
comment(trans_act_aviation$航空活动水平) <- "nounit"

# 水路客运周转量和水路货运周转量
trans_act_water <- func_read_trans("P6KQQFUP")
trans_act_water <- trans_act_water[, c("year", "客运周转量", "货运周转量")]
names(trans_act_water) <- c("year", "水路客运周转量", "水路货运周转量")
trans_act <- func_merge_2(list(trans_act_operation, trans_act_nonoperation, 
                               trans_act_aviation, trans_act_water))
func_looknote(trans_act)

# 能耗总量和能耗强度：营运车辆部分先输入总量后算强度，非营运车辆则相反
# 营运车辆部分能耗总量和能耗强度
# 能耗总量
trans_nrgsum_ls_ori <- vector("list")
# 各类车总能耗：按能源类型分
tbl_read <- c("汽油消费量", "柴油消费量", "天然气消费量")
for (i in c(1:length(tbl_read))) {
  trans_nrgsum_ls_ori[[i]] <- func_read_trans("IZM9FWIY", tbl_read[i])
}
names(trans_nrgsum_ls_ori) <- c("gasoline", "diesel", "gas")
# 先筛选和计算数据
# 汽油消费量
trans_nrgsum_ls_ori[[1]] <- 
  trans_nrgsum_ls_ori[[1]][, c("year", "常规公交", "出租车合计")]
names(trans_nrgsum_ls_ori[[1]])<- c("year", "常规公交", "出租车")
# 汽油密度0.72千克/升 = 7.2吨/万升
trans_nrgsum_ls_ori[[1]]$"常规公交" <- trans_nrgsum_ls_ori[[1]]$"常规公交" * 7.2
comment(trans_nrgsum_ls_ori[[1]]$常规公交) <- "吨"
# 柴油消费量
func_looknote(trans_nrgsum_ls[[2]])
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
trans_nrgsum_ls <- trans_nrgsum_ls[c("常规公交", "快速公交", "出租车", "农村客车")]
rm(trans_nrgsum_ls_ori)
# 营运性车辆的能耗强度
trans_nrgintst_ls <- 
  func_nrg_intst_ls(trans_nrgsum_ls, 
                    trans_act[c("year", "常规公交", "快速公交", "出租车", "农村客车")])

# 非营运车辆部分
# 按照全国各类非营运车辆平均数据计算，各年份一样
trans_ori_nonoperation_nrgintst <- 
  func_read_data("Y3PGVSR7", "非营运性车辆年均里程数和百公里能耗")
trans_ori_nonoperation_nrgintst <- 
  trans_ori_nonoperation_nrgintst[c("项目", "能耗类别", "年均能耗")]
trans_ori_nonoperation_nrgintst$项目 <- 
  gsub(" ", "", trans_ori_nonoperation_nrgintst$项目)
trans_ori_nonoperation_nrgintst$年均能耗 <- 
  gsub(" ", "", trans_ori_nonoperation_nrgintst$年均能耗)
# 将该数据框转化为列表
trans_nrgintst_ls[c(5:12)] <- vector("list", nrow(trans_ori_nonoperation_nrgintst))
names(trans_nrgintst_ls)[5:12] <- trans_ori_nonoperation_nrgintst$"项目"
for (i in names(trans_nrgintst_ls[c(5:12)])) {
  trans_nrgintst_ls[[i]] <- 
    data.frame(year = c(2005: 2019),
               value = c(trans_ori_nonoperation_nrgintst$年均能耗[trans_ori_nonoperation_nrgintst$项目 == i]))
  names(trans_nrgintst_ls[[i]])[2] <- 
    trans_ori_nonoperation_nrgintst$能耗类别[trans_ori_nonoperation_nrgintst$项目 == i]
}
# 计算非营运车辆能耗总量
# 将能耗强度转化为数字类型
trans_nrgintst_ls <- func_ls_asnumber(trans_nrgintst_ls)
trans_nrgsum_ls[c(5:12)] <- 
  func_nrg_sum_ls(trans_nrgintst_ls[c(5:12)], 
                  trans_act[c("year", trans_subsector[c(5:12)])])

# 航空和水运部分能耗总量和能耗强度
trans_nrgsum_ls$"航空" <- func_read_trans("JXG6KGSA")[, c("year", "国内")]
trans_nrgsum_ls$"水路客运" <- func_read_trans("NTNZD6VV", "国内客运")
trans_nrgsum_ls$"水路货运" <- func_read_trans("NTNZD6VV", "国内货运")
names(trans_nrgsum_ls)[13:15] <- trans_subsector[13:15]
trans_nrgintst_ls[trans_subsector[13:15]] <- 
  func_nrg_intst_ls(trans_nrgsum_ls[trans_subsector[13:15]], 
                 trans_act[, c(1, 14:16)])

## 预测未来
# 活动水平
# 营运和非营运车辆
proj_trans_act <- data.frame(year = c(2019: 2050))
for (i in trans_subsector[1:12]) {
  proj_trans_act[, i] <- 
    func_interp_2(year = c(2019, 2030, 2050), 
                  value = c(func_lastone(trans_act[, i]), 
                            func_lastone(trans_act[, i])*1.05, 
                            func_lastone(trans_act[, i])*1.07))$value
}
#航空和水运
proj_trans_act$"航空" <- rep(1, length(c(2019: 2050)))
proj_trans_act$"水路客运" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(10000, 10500, 11000))$value
comment(proj_trans_act$"水路客运") <- "万人公里"
proj_trans_act$"水路货运" <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(21538635, 21538635*1.5, 21538635*2.0))$value
comment(proj_trans_act$"水路货运") <- "万吨公里"

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
# 航空
proj_trans_nrgintst_ls[["航空"]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(36.73, 40, 50))
names(proj_trans_nrgintst_ls[["航空"]])[2] <- "kerosene"

# 水路客运
proj_trans_nrgintst_ls[["水路客运"]] <- data.frame(year = c(2019: 2050))
proj_trans_nrgintst_ls[["水路客运"]]$diesel <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(1.612026e-05, 1.612026e-05*0.85, 1.612026e-05*0.80))$value
proj_trans_nrgintst_ls[["水路客运"]]$燃料油 <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(3.224052e-07, 3.224052e-07*0.85, 3.224052e-07*0.80))$value
func_history_project_df(proj_trans_nrgintst_ls[["水路客运"]], 
                        trans_nrgintst_ls[["水路客运"]])

# 水路货运
proj_trans_nrgintst_ls[["水路货运"]] <- data.frame(year = c(2019: 2050))
proj_trans_nrgintst_ls[["水路货运"]]$diesel <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(6.964230e-09, 6.964230e-09*0.85, 6.964230e-09*0.80))$value
proj_trans_nrgintst_ls[["水路货运"]]$residual <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(1.392846e-10, 1.392846e-10*0.85, 1.392846e-10*0.80))$value

# 能耗总量
proj_trans_nrgsum_ls <- func_nrg_sum_ls(proj_trans_nrgintst_ls, proj_trans_act)

# 对比历史和预测数据
func_history_project_df(trans_act, proj_trans_act)
func_history_project_ls(trans_nrgintst_ls, proj_trans_nrgintst_ls)
func_history_project_ls(trans_nrgsum_ls, proj_trans_nrgsum_ls)