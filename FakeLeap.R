library(openxlsx)
library(ggplot2)
library(reshape2)
library(dplyr)
library(Hmisc)

# 全局函数
Sys.setlocale("LC_ALL", "chinese")

## 构建函数
# 查看列名对应的批注
func_looknote <- function(data) {
  notes <- character(0)
  for (i in  c(1: ncol(data))) {
    note <- attributes(data[, i])$comment
    if (is.null(note)) {note = "nounit"}
    notes <- c(notes, note)
  }
  notes_df <- data.frame(colnames = names(data), note = notes)
  notes_df
}

# 读取并转化带4列文件头的Excel数据
func_read_trans <- function(name_subdir, order_sht = 1) {
  data_dir <- "C:/Users/kangj/Documents/OneDrive/Zotero/storage/"
  data_name <- list.files(paste0(data_dir, "/", name_subdir, "/"))
  path <- paste0(data_dir, "/", name_subdir, "/", data_name)
  data_ori <- read.xlsx(path, sheet = order_sht)
  data_ori <- data_ori[, -c(1,2)]
  data_trans <- as.data.frame(t(data_ori[, -c(1,2)]))
  colnames(data_trans) <- data_ori[, 1]
  data_trans$year <- colnames(data_ori)[3:ncol(data_ori)]
  for (i in c(1: nrow(data_ori))) {
    comment(data_trans[, names(data_trans)[i]]) <- data_ori[i, 2]
  }
  data_trans <- data_trans[c("year", 
                             names(data_trans)[names(data_trans) %in% "year" == FALSE])]
  rownames(data_trans) <- NULL
  for (i in names(data_trans)) {
    print(i)
    print(attributes(data_trans[, i]))
  }
  data_trans
}


# 读取特定单元格
func_read <- function(name_subdir, name_sht, num_row, num_col) {
  data_dir <- "C:/Users/kangj/Documents/OneDrive/Zotero/storage/"
  data_name <- list.files(paste0(data_dir, "/", name_subdir, "/"))
  path <- paste0(data_dir, "/", name_subdir, "/", data_name)
  data_ori <- read.xlsx(path, sheet = name_sht, 
                        rows = num_row, cols = num_col, 
                        colNames = FALSE)
  data_ori[1, 1]
}

# 查看一个数据框中不同组的变化趋势
func_show_trend <- function(var_df) {
  long_df <- melt(var_df, id = "year")
  ggplot(long_df) + geom_point(aes(year, value, color = variable), 
                               size = 5, alpha = 0.5)
}

# 计算两个系列的比率或乘积：一对一
# method取值“rate”或者“product”
func_merge_rate <- function(var_1, name_1, var_2, name_2, method, 
                            startyear = 2005, endyear = 2050) {
  total <- data.frame(year = c(startyear:endyear), 
                      value = NA)
  var_1 <- var_1[var_1$year %in% total$year, ]
  var_2 <- var_2[var_2$year %in% total$year, ]
    names(var_1)[which(names(var_1) == name_1)] <- "name_x"
  names(var_2)[which(names(var_2) == name_2)] <- "name_y"
  total <- merge(var_1, var_2, by = "year", all.x = TRUE)
  if (method == "rate") {
    outcome <- data.frame("year" = total$year, 
                          "Rate" = total[, "name_x"]/total[, "name_y"])
  } else {
    outcome <- data.frame("year" = total$year, 
                          "Rate" = total[, "name_x"] * total[, "name_y"])
  }
  unit_1 <- attributes(var_1[, "name_x"])[[1]]
  unit_2 <- attributes(var_2[, "name_y"])[[1]]
  if (method == "rate") {
    name_new <- paste(name_1, unit_1, "/", name_2, unit_2)
  } else {
    name_new <- paste(name_1, unit_1, "*", name_2, unit_2)
  }
  plot(x = outcome$year, y = outcome$Rate, 
       main = name_new)
  outcome
}

func_merge_rate(ind_gdp_agg_prop[, c("year", i)], i, 
                proj_gdp_ind, "value", 
                method = "product")

# 计算乘积
func_merge_product <- function(var_1, name_1, var_2, name_2) {
  total <- merge(var_1, var_2, by = "year")
  product <- data.frame("year" = total$year, 
                        "Rate" = total[, name_1] * total[, name_2])
  plot(x = product$year, y = product$Rate)
  product
}

# 二级行业活动水平矩阵 + 某子行业各种能耗强度 = 该行业各种能耗总量
# 输入数据：
# 目标行业各种能源强度数据框
# 带有目标行业活动水平的数据数据框
# 两个数据框都要带有一个命名为“year”的列
# 活动水平名称
func_nrg_sum <- function(df.nrg.intst , df.actlvl, name.actlvl) {
  # 统一数据框的年份
  intersect_year <- intersect(df.nrg.intst$year, df.actlvl$year)
  df.nrg.intst <- df.nrg.intst [df.nrg.intst$year %in% intersect_year, ]
  df.actlvl <- df.actlvl[df.actlvl$year %in% intersect_year, ]
  df.actlvl <- df.actlvl[, c("year", name.actlvl)]
  # 构建输出结果
  total_df <- df.nrg.intst 
  for (i in names(total_df)[names(total_df) %in% "year" == FALSE]) {
    total_df[, i] <- df.nrg.intst [, i] * df.actlvl[, name.actlvl]
  }
  total_df$year <- intersect_year
  total_df <- total_df[c("year", 
                         names(total_df)[names(total_df) %in% "year" == FALSE])]
  total_df
}

# 比较两组数据的函数
func_datacomp <- function(var_1, name_source_1, var_2, name_source_2, name_comp) {
  var_1[, c("Source")] <- name_source_1
  var_2[, c("Source")] <- name_source_2
  mrg_data <- rbind(var_1[, c("year", name_comp, "Source")], 
                    var_2[, c("year", name_comp, "Source")])
  ggplot(mrg_data) + 
    geom_point(aes(year, mrg_data[, c(name_comp)], color = Source, alpha = 0.5))
}

# 将计算结果统一到一个表头下的函数
func_supple_colnames <- function(var_output){
  supple_colnames <- account_names[account_names %in% names(var_output) == FALSE]
  for (i in supple_colnames) {
    var_output[, i] <- ""
  }
  var_output[is.na(var_output)] <- ""
  var_output[, c(account_names, 
                 names(var_output)[names(var_output) %in% account_names == FALSE])]
}

# 构建插值函数
func_interp <- function(mydata) {
  total_df <- data.frame(year = c(2005:2050))
  for (j in c(1:(nrow(mydata) - 1))) {
    start_year <- mydata$year[j]
    end_year <- mydata$year[j + 1]
    start_value <- mydata$value[which(mydata$year == start_year)]
    end_value <- mydata$value[which(mydata$year == end_year)]
    for (i in seq(from = start_year, to = end_year, by = 1)) {
      total_df$value[which(total_df$year == i)] <- 
        start_value + 
        (end_value - start_value) * (i - start_year) /
        (end_year - start_year)
    }
  }
  plot(total_df$year, total_df$value)
  total_df
}

func_interp_2 <- function(year, value) {
  total_df <- data.frame(year = c(year[1]: year[length(year)]))
  for (j in c(1:(length(year) - 1))) {
    start_year <- year[j]
    end_year <- year[j + 1]
    start_value <- value[j]
    end_value <- value[j + 1]
    for (i in seq(from = start_year, to = end_year, by = 1)) {
      total_df$value[which(total_df$year == i)] <- 
        start_value + 
        (end_value - start_value) * (i - start_year) /
        (end_year - start_year)
    }
  }
  plot(total_df$year, total_df$value)
  total_df
}

# 预测函数：基于增长率
func_proj_incr_rate <- function(mydatanow, name_mydata, proj_rate, name_rate) {
  total_df <- data.frame(year = c(2005:2050), 
                         value = NA)
  mydatanow <- mydatanow[mydatanow$year %in% total_df$year, ]
  proj_rate <- proj_rate[proj_rate$year %in% total_df$year, ]
  total_df$value[total_df$year %in% mydatanow$year] <- mydatanow[, name_mydata]
  for (i in 
       mydatanow$year[nrow(mydatanow)]:(proj_rate$year[nrow(proj_rate)] - 1)) {
    total_df$value[which(total_df$year == (i + 1))] <- 
      total_df$value[which(total_df$year == i)] * 
      (1 + proj_rate[, name_rate][which(proj_rate$year == (i + 1))] / 100)
  }
  plot(total_df$value)
  total_df
}

# 结果计算函数
func_result <- function(var_aclevel, var_int) {
  total_df <- merge(var_aclevel, var_int, by = "year", all.x = TRUE)
  total_df$value <- total_df[, 1] * total_df[, 2]
  plot(total_df$value)
  total_df
}

# 通过能源总量和活动水平计算活动强度
func_nrg_intst <- function(df_nrg_sum, df_actlvl, name) {
  intersect_year <- intersect(df_nrg_sum$year, df_actlvl$year)
  df_nrg_sum <- df_nrg_sum[df_nrg_sum$year %in% intersect_year, ]
  df_actlvl <- df_actlvl[df_actlvl$year %in% intersect_year, ]
  df_actlvl <- df_actlvl[, c("year", name)]
  total_df <- df_nrg_sum
  for (i in names(total_df[, -1])) {
    total_df[, i] <- df_nrg_sum[, i] / df_actlvl[, name]
  }
  total_df
}

# 生成预测数据的函数
func_proj <- function(input_ls, itemnames, startyear = 2005, endyear = 2050) {
  out_df <- data.frame(year = c(startyear: endyear))
  for (i in c(1:length(itemnames))) {
    out_df[, itemnames[i]] <- func_interp(input_ls[[i]])$value
  }
  out_df
}


## 全局变量
## GDP
gdp <- func_read_trans("2VHEE264", "GDP")
func_show_trend(gdp)
# GDP预测
mydata <- data.frame(year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050), 
                     value = c(8, 7.90, 6.00, 6.00, 5.00, 4.00, 3.00))
proj_gdp_incr_rate <- func_interp(mydata)
proj_gdp <- func_proj_incr_rate(gdp, "GDP", proj_gdp_incr_rate, "value")

# 工业所占比例（需求：暂时算的是第二产业）
mydata <- data.frame(year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050), 
                     value = c(41, 41.60, 41, 30, 26, 23, 19))
proj_gdp_ind_prop <- func_interp(mydata)
proj_gdp_ind <- data.frame(year = c(2005:2050), 
                           value = proj_gdp$value * 
                             proj_gdp_ind_prop$value / 100)

## 人口
population <- func_read_trans("2VHEE264")


## 家庭和建筑业
## 备选1：电器拥有量*户数*使用强度
data_name <- "厦门城乡耐用品数量.xlsx"
app_urban <- read.xlsx("C:/Users/kangj/Documents/OneDrive/Zotero/storage/CZDN5XJN/厦门城乡耐用品数量.xlsx", sheet = 1, rows = c(2:25))
year <- colnames(app_urban)[3:ncol(app_urban)]
col_name <- app_urban$"电器"

app_urban_trans <- t(app_urban[, -c(1,2)])
app_urban_trans <- as.data.frame(app_urban_trans)
dim(app_urban_trans)
app_urban_trans$year <- year
colnames(app_urban_trans) <- col_name

app_urban_trans_perhouse <- app_urban_trans[, -ncol(app_urban_trans)]/100
app_urban_trans_perhouse$year <- year

app_urban_trans_perhouse_tar <- app_urban_trans_perhouse[, c("洗衣机", 
                                                             "电冰箱", 
                                                             "空调器", 
                                                             "脱排油烟机", 
                                                             "微波炉", 
                                                             "淋浴热水器", 
                                                             "彩色电视机", 
                                                             "家用电脑", 
                                                             "year"
)]
app_urban_trans_perhouse_tar_long <- melt(app_urban_trans_perhouse_tar, id = c("year"))
ggplot(app_urban_trans_perhouse_tar_long) + geom_point(aes(x = year, y = value, color = variable))
# 结论：在2013年各项突降，感觉数据不太说得通

## 备选2：每户用电量*户数
# 全市用电量
# 能源平衡表：生活用电数据，亿千瓦时
temp_ls <- vector("list", 1)
for (i in c(2005:2019)) {
  temp_ls[[1]] <- c(temp_ls[[1]], func_read("D4J2KVSW", paste(i), 16, 15))
}
electricity_living <- data.frame(year = c(2005:2019), 
                                 Electricity = temp_ls[[1]])

# 人口数据：户数等
# 假设2016-2019年家庭规模维持稳定，为2.5人/户
population$"调查城镇家庭规模"[which(population$year %in% 
                              c("2016", "2017", "2018", "2019"))] <- 2.5
# 那么常驻人口户数为
population$Household <- population$"常住人口" / population$"调查城镇家庭规模"

# 计算每户用电量
electricity_perhouse <- func_merge_rate(electricity_living, "Electricity", 
                                        population, "Household", 
                                        method = "product")
# 结论：逐渐升高
fit <- lm(electricity_perhouse$Rate ~ electricity_perhouse$year)
a <- summary(fit)
abline(fit)

proj_electricity_perhouse <- data.frame("year" = c(2005:2050))
proj_electricity_perhouse$Value <- a$coefficients[2,1] * proj_electricity_perhouse$year + a$coefficients[1, 1]
plot(proj_electricity_perhouse$year, proj_electricity_perhouse$Value)

# 和广州数据比较
electricity_living_guangzhou <- func_read_trans("QSPLFZXP", 3)
population_guangzhou <- func_read_trans("QSPLFZXP")
func_merge_rate(electricity_living_guangzhou, "#生活用电", 
                population_guangzhou, "总户数")
# 单从数据而言，厦门市还是有很大的增长空间的

## 农业
ag_ori <- func_read_trans("4NJ97NS9")
func_merge_rate(ag_ori[, c("year", "农用柴油使用量")], "农用柴油使用量", 
                ag_ori[, c("year", "全年农作物总播种面积")], "全年农作物总播种面积", 
                method = "product")
func_merge_rate(ag_ori[, c("year", "农村用电量")], "农村用电量", 
                ag_ori[, c("year", "全年农作物总播种面积")], "全年农作物总播种面积", 
                method = product)
# 农村用电量应该不是农业生产用电量
# 问题：柴油没有算入清单中，如何处理？

# 读取能源平衡表中的农业用电量
temp_ls <- vector("list", 1)
for (i in c(2005:2019)) {
  temp_ls[[1]] <- c(temp_ls[[1]], func_read("D4J2KVSW", paste(i),9, 15))
}
electricity_ag <- data.frame(year = c(2005:2019), 
                             Electricity = temp_ls[[1]])
plot(electricity_ag$Electricity)

# 农业用电强度
ag_elec_intnst <- func_merge_rate(electricity_ag, "Electricity", 
                                  ag_ori, "全年农作物总播种面积", 
                                  method = "product")

# 播种面积趋势
func_show_trend(ag_ori)
# 问题：农作物播种面积在下降，但用电量是波动的，柴油好像是上升的？

## 建模
# 预测
# 活动水平
proj_population <- data.frame("year" = c(2019:2050), 
                              "population" = read.xlsx("temp.xlsx")$Population * 1000/2.5)

proj_other_act <- data.frame(year = c(2005: 2050))
proj_other_act <- merge(proj_other_act, proj_population, by = "year")
proj_other_act$population <- proj_other_act$population / 2.5
names(proj_other_act)[2] <- "household"

proj_other_act$household_lpg <- 
  proj_other_act$household * func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                                                    value = c(48, 48, 30, 8)))$value
proj_other_act$household_natural_gas <- 
  proj_other_act$household * func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                                                    value = c(52, 52, 70, 96)))$value

proj_other_act$construction_gdp <- 
  proj_gdp$value * 
  func_interp(data.frame(year = c(2005, 2019,2030,2050), 
                         value = c(39.05*15.30/100, 
                                   39.05*15.30/100, 
                                   34.7*15.30/100,
                                   29.7*5/100)))$value

proj_other_act$agriculture_area <- 
  func_interp(
    data.frame(year = c(2005, 2019, 2050), 
               value = c(0, 
                         ag_ori[nrow(ag_ori), 3], 
                         ag_ori[nrow(ag_ori), 3] * 0.5)))$value

func_show_trend(proj_other_act)

# 活动强度
proj_other_nrgintst_ls <- vector("list")
proj_other_nrgintst_ls[[1]] <- 
  data.frame(year = c(2005: 2050),
             electricity = proj_electricity_perhouse$Value)

proj_other_nrgintst_ls[[2]] <- 
  data.frame(year = c(2005: 2050), 
             lpg = c(588.5))

proj_other_nrgintst_ls[[3]] <- 
  data.frame(year = c(2005: 2050), 
             natural_gas = c(74.5))

proj_other_nrgintst_ls[[4]] <- data.frame(year = c(2005: 2050), 
                                          electricity = c(0.09))

proj_other_nrgintst_ls[[5]] <- 
  data.frame(year = c(2005: 2050),
             electricity = func_interp(
               data.frame(year = c(2005, 2019, 2050), 
                          value = c(0, 
                                    ag_elec_intnst[1, 2],
                                    ag_elec_intnst[1, 2] * 0.5)))$value)

func_show_trend(proj_other_nrgintst_ls[[5]])

# 则能耗总量为
proj_other_nrgsum_ls <- vector("list")
for (i in c(1:5)) {
  proj_other_nrgsum_ls[[i]] <- func_nrg_sum(proj_other_nrgintst_ls[[i]], 
                                            proj_other_act, 
                                            names(proj_other_act)[i])
}
proj_other_nrgsum_ls
func_show_trend(proj_other_nrgsum_ls[[5]])


## 交通
## 公路交通
# 车辆数量
# 来自统计年鉴的数据
num_vehicle_stat <- func_read_trans("E5AU338C")
# 对比一下运营车辆保有量
# 来自交通局的数据
num_bus_tb <- func_read_trans("IZM9FWIY", "保有量")
num_taxi_tb <- func_read_trans("FUM5GGXX", order_sht = "保有量")
num_ruralbus_tb <- func_read_trans("2KMQZKN2", "保有量")
num_vehicle_tb <- Reduce(merge, 
                         list(num_bus_tb[, c("公交合计", "year")], 
                                     num_taxi_tb[, c("出租车合计", "year")], 
                                     num_ruralbus_tb[, c("农村客车", "year")]))

# 来自车管所的数据
num_vehicle_vao <- func_read_trans("E66EF3SQ")

# 数据比较
func_datacomp(num_vehicle_tb, "tb", num_vehicle_vao, "vao", "公交合计")
func_datacomp(num_vehicle_tb, "tb", num_vehicle_vao, "vao", "出租车合计")

# 非运营车辆数据
# 来自车管所的数据
num_vehicle_private <- func_read_trans("Y3PGVSR7")
num_vehicle_private$year <- as.numeric(num_vehicle_private$year)
# 和统计年鉴数据比较
names(num_vehicle_stat)[which(names(num_vehicle_stat) == "#摩托车")] <- "摩托车"
names(num_vehicle_stat)[which(names(num_vehicle_stat) == "#拖拉机")] <- "拖拉机"
func_datacomp(num_vehicle_private, "vao", num_vehicle_stat, "stat", "摩托车")
func_datacomp(num_vehicle_private, "vao", num_vehicle_stat, "stat", "拖拉机")

# 暂时营运车辆取交通局数据，非营运车辆取车管所数据
# 看看非营运性车辆的情况
func_show_trend(num_vehicle_private)

## 分析历史数据
# 活动水平数据框
ori_mileage <- func_read_trans("IZM9FWIY", "里程数")
ori_mileage$year <- as.numeric(ori_mileage$year)
ori_operation_mileage <- 
  ori_mileage[, c("year", "常规公交", "BRT", "出租车", "农村客车")]

ori_nonoperation_number <- func_read_trans("Y3PGVSR7")
ori_nonoperation_number <- 
  ori_nonoperation_number[
    names(ori_nonoperation_number) %in% c("year", 
                                          "摩托车", 
                                          "轿车", 
                                          "轻型客车 ", 
                                          "大型客车 ", 
                                          "轻型货车 ", 
                                          "中型货车 ", 
                                          "重型货车 ", 
                                          "农用运输车")]
ori_nonoperation_mileagepervehicle <- 
  func_read_trans("Y3PGVSR7", "非营运性车辆年均里程数")
dim(ori_nonoperation_number)
dim(ori_nonoperation_mileagepervehicle)
ori_nonoperation_mileage <- ori_nonoperation_number
ori_nonoperation_mileage <- apply(ori_nonoperation_mileage, 2, as.numeric)
ori_nonoperation_mileage <- as.data.frame(ori_nonoperation_mileage)
for (i in c(2:nrow(ori_nonoperation_mileage))) {
  ori_nonoperation_mileage[i, -1] <- ori_nonoperation_mileage[i, -1] * 
    ori_nonoperation_mileagepervehicle[1,-1]
}

func_merge <- function(x, y) {
  merge(x, y, by = "year", all = TRUE)
}
trans_act <- Reduce(func_merge, 
                    list(ori_operation_mileage, 
                         ori_nonoperation_mileage))

func_show_trend(trans_act[names(trans_act) %in% "出租车" == FALSE])

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
func_show_trend(trans_nrgintst_ls[[4]])

## 预测未来
# 活动水平
proj_trans_act <- data.frame(year = c(2005: 2050))
proj_trans_act$"常规公交" <- 
  func_interp_2(year = c(2005, 2017, 2030, 2050), 
                value = c(0,
                          trans_act$常规公交[which(trans_act$year == 2017)], 
                          trans_act$常规公交[which(trans_act$year == 2017)]*1.05, 
                          trans_act$常规公交[which(trans_act$year == 2017)]*1.07*1.01))$value
proj_trans_act$"BRT" <- 
  func_interp_2(year = c(2005, 2017, 2030, 2050), 
                value = c(0,
                          trans_act$BRT[which(trans_act$year == 2017)], 
                          trans_act$BRT[which(trans_act$year == 2017)]*1.05, 
                          trans_act$BRT[which(trans_act$year == 2017)]*1.07*1.01))$value
proj_trans_act$"出租车" <- 
  func_interp_2(year = c(2005, 2017, 2030, 2050), 
                value = c(0,
                          trans_act$出租车[which(trans_act$year == 2017)], 
                          trans_act$出租车[which(trans_act$year == 2017)]*1.05, 
                          trans_act$出租车[which(trans_act$year == 2017)]*1.07*1.01))$value
proj_trans_act$"农村客车" <- 
  func_interp_2(year = c(2005, 2017, 2030, 2050), 
                value = c(0,
                          trans_act$农村客车[which(trans_act$year == 2014)], 
                          trans_act$农村客车[which(trans_act$year == 2014)]*1.05, 
                          trans_act$农村客车[which(trans_act$year == 2014)]*1.07*1.01))$value
proj_trans_act$"轿车" <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0,
                          trans_act$轿车[which(trans_act$year == 2019)], 
                          trans_act$轿车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$轿车[which(trans_act$year == 2019)]*1.07*1.01))$value
proj_trans_act$"摩托车" <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0,
                          trans_act$摩托车[which(trans_act$year == 2019)], 
                          trans_act$摩托车[which(trans_act$year == 2019)]*1.05, 
                          trans_act$摩托车[which(trans_act$year == 2019)]*1.07*1.01))$value

proj_trans_act$"航空" <- rep(1, length(c(2005: 2050)))

proj_trans_act$"水路客运" <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(10000, 10000, 10500, 11000))$value
comment(proj_trans_act$"水路客运") <- "万人公里"

proj_trans_act$"水路货运" <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(14247920, 14247920, 15000000, 22000000))$value
comment(proj_trans_act$"水路货运") <- "万吨公里"

# 活动强度
proj_trans_nrgintst_ls <- vector("list", 9)
names(proj_trans_nrgintst_ls) <- c("常规公交", 
                                   "BRT", 
                                   "出租车", 
                                   "农村客车", 
                                   "轿车", 
                                   "摩托车", 
                                   "航空", 
                                   "水路客运", 
                                   "水路货运")

proj_trans_nrgintst_ls[[1]] <- data.frame(year = c(2005: 2050))
for (i in c(1: (ncol(trans_nrgsum_ls_2[[1]]) - 1))) {
  proj_trans_nrgintst_ls[[1]][, names(trans_nrgsum_ls_2[[1]])[i + 1]] <- 
    trans_nrgsum_ls_2[[1]][, names(trans_nrgsum_ls_2[[1]])[i + 1]][nrow(trans_nrgsum_ls_2[[1]])]
}

proj_trans_nrgintst_ls[[2]] <- data.frame(year = c(2005: 2050))
for (i in c(1: (ncol(trans_nrgsum_ls_2[[2]]) - 1))) {
  proj_trans_nrgintst_ls[[2]][, names(trans_nrgsum_ls_2[[2]])[i + 1]] <- 
    trans_nrgsum_ls_2[[2]][, names(trans_nrgsum_ls_2[[2]])[i + 1]][nrow(trans_nrgsum_ls_2[[2]])]
}

proj_trans_nrgintst_ls[[3]] <- data.frame(year = c(2005: 2050))
for (i in c(1: (ncol(trans_nrgsum_ls_2[[3]]) - 1))) {
  proj_trans_nrgintst_ls[[3]][, names(trans_nrgsum_ls_2[[3]])[i + 1]] <- 
    trans_nrgsum_ls_2[[3]][, names(trans_nrgsum_ls_2[[3]])[i + 1]][5]
}

proj_trans_nrgintst_ls[[4]] <- data.frame(year = c(2005: 2050))
for (i in c(1: (ncol(trans_nrgsum_ls_2[[4]]) - 1))) {
  proj_trans_nrgintst_ls[[4]][, names(trans_nrgsum_ls_2[[4]])[i + 1]] <- 
    trans_nrgsum_ls_2[[4]][, names(trans_nrgsum_ls_2[[4]])[i + 1]][4]
}

proj_trans_nrgintst_ls[["航空"]] <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(202e4, 202e4, 300e4, 320e4))
names(proj_trans_nrgintst_ls[["航空"]])[2] <- "kerosene"

proj_trans_nrgintst_ls[["水路客运"]] <- data.frame(year = c(2005: 2050))
proj_trans_nrgintst_ls[["水路客运"]]$redidual <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(1.40, 1.40, 1.40*0.85, 1.40*0.80))$value
proj_trans_nrgintst_ls[["水路客运"]]$diesel <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.80, 0.80, 0.80*0.85, 0.80*0.80))$value

proj_trans_nrgintst_ls[["水路货运"]] <- data.frame(year = c(2005: 2050))
proj_trans_nrgintst_ls[["水路货运"]]$redidual <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.04649123, 0.04649123, 0.04649123*0.85, 0.04649123*0.80))$value
proj_trans_nrgintst_ls[["水路货运"]]$diesel <- 
  func_interp_2(year = c(2005, 2019, 2030, 2050), 
                value = c(0.00476, 0.00476, 0.00476*0.85, 0.00476*0.80))$value

# 则能耗总量为
proj_trans_nrgsum_ls <- vector("list")
for (i in c(1:9)) {
  proj_trans_nrgsum_ls[[i]] <- func_nrg_sum(proj_trans_nrgintst_ls[[i]], 
                                            proj_trans_act, names(proj_trans_act)[i])
}
func_show_trend(proj_trans_nrgsum_ls[[7]])

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

## 服务业
## 现状分析
working_population <- func_read_trans("2VHEE264", "从业人口")
com_act <- data.frame(year = c(2005:2019))
com_act <- merge(com_act, working_population[c("year", "第三产业")], by = "year")
names(com_act)[2] <- "work_pop"
com_act
com_act <- merge(com_act, gdp[c("year", "#第三产业")], by = "year")
names(com_act)[3] <- "com_gdp"
com_act

com_nrgsum_ls <- vector("list")
com_nrgsum_ls[[1]] <- data.frame(year = c(2010: 2015), 
                                 electricity = func_read_trans("2I4DKY2A")$"##第三产业用电")
com_nrgsum_ls[[2]] <- func_read_trans("HV4JBQTQ")

com_nrgintst_ls <- vector("list")
for (i in c(1:2)) {
  com_nrgintst_ls[[i]] <- func_nrg_intst(com_nrgsum_ls[[i]], 
                                         com_act, 
                                         names(com_act)[[i]])
}
func_show_trend(com_nrgintst_ls[[1]])
func_show_trend(com_nrgintst_ls[[2]])

## 预测分析
# 未来活动水平
# 补全一下总人口预测的数据
names(proj_population) <- c("year", "population")
proj_population <- rbind(data.frame(year = c(2005:2018), population = c(0)), 
                         proj_population)
proj_com_act <- data.frame(year = c(2005: 2050))
proj_com_act$work_pop <- proj_population$population * 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(45, 45, 48, 73)))$value / 100
proj_com_act$com_gdp <- proj_gdp$value * 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(60, 60, 65, 70)))$value / 100
proj_com_act

## 未来活动强度
proj_com_nrgintst_ls <- vector("list")
proj_com_nrgintst_ls[[1]] <- data.frame(year = c(2005: 2050))
proj_com_nrgintst_ls[[1]]$electricity <- 
  func_interp(data.frame(year = c(2005, 2019, 2030, 2050), 
                         value = c(0.2757e4, 0.2757e4, 0.2757*0.97e4, 0.2757*0.90e4)))$value
proj_com_nrgintst_ls[[2]] <- data.frame(year = c(2005: 2050))
proj_com_nrgintst_ls[[2]]$lpg <- func_interp(
  data.frame(year = c(2005, 2019, 2030, 2050),
             value = c(22.3*0.4/10000*0.45, 
                       22.3*0.4/10000*0.45, 
                       22.3*0.4/10000*0.4, 
                       22.3*0.4/10000*0.25)))$value
proj_com_nrgintst_ls[[2]]$natural_gas <- func_interp(
  data.frame(year = c(2005, 2019, 2030, 2050),
             value = c(1.4678/10000+2.23*0.35*1.1/10000, 
                       1.4678/10000+2.23*0.35*1.1/10000, 
                       1.4678/10000+2.23*0.35*1.1/10000*0.8, 
                       1.4678/10000+2.23*0.35*1.1/10000*0.6)))$value
proj_com_nrgintst_ls

# 则未来能耗总量
proj_com_nrgsum_ls <- vector("list")
for (i in c(1:2)) {
  proj_com_nrgsum_ls[[i]] <- func_nrg_sum(proj_com_nrgintst_ls[[i]], 
                                          proj_com_act, names(proj_com_act)[i])
}
func_show_trend(proj_com_nrgsum_ls[[1]])
func_show_trend(proj_com_nrgsum_ls[[2]])
