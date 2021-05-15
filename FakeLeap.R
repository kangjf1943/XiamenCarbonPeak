library(openxlsx)
library(ggplot2)
library(reshape2)
library(ggpubr)

# 全局函数
Sys.setlocale("LC_ALL", "chinese")

## 构建函数
# 给数据框添加备注
func_addnote <- function(data, note) {
  for (i in c(1: ncol(data))) {
    comment(data[, names(data)[i]]) <- note[i]
  }
  data
}

# 查看列名对应的批注：数据框版
func_looknote <- function(data) {
  notes <- character(0)
  for (i in  c(1: ncol(data))) {
    note <- attributes(data[, i])$comment
    if (is.null(note)) {note = "nounit"}
    notes <- c(notes, note)
  }
  notes_df <- data.frame(colnames = names(data), note = notes)
  print(notes_df)
}

# 查看列名对应的批注：列表版
func_looknote_ls <- function(var_ls) {
  for (i in c(1: length(var_ls))) {
    print(names(var_ls[i]))
    print(func_looknote(var_ls[[i]]))
    cat("\n")
  }
}

# 合并2个数据框，并且保留所有观察
func_merge <- function(x, y) {
  merge(x, y, by = "year", all = TRUE)
}

# 合并多个数据框
func_merge_2 <- function(ls_var) {
  # 如果输入列表为空则输出为NULL
  if (length(ls_var) == 0) {
    NULL
  } else {
    df_out <- data.frame(year = ls_var[[1]][, "year"])
    notes <- c("nounit")
    for (i in c(1: length(ls_var))) {
      df_out <- merge(df_out, ls_var[[i]], by = "year", all = TRUE)
      notes <- c(notes, func_looknote(ls_var[[i]])[-1, "note"])
    }
    df_out <- func_addnote(df_out, notes)
    df_out
  }
}

# 构建列表分类方式转换的函数：类别1到类别2
# 比如将按照能源类别分的各部门能耗列表转换为按照部门分的各类能耗列表
# 前提：原列表的每个数据框都要命名 - 即类别1
# 目标列表的各数据框名称来自原列表的各个数据框的列名 - 即类别2
func_ls_transition <- function(ls_ori) {
  # 提取类别2的名称
  class_trans <- character()
  for (k in names(ls_ori)) {
    class_trans <- c(class_trans, names(ls_ori[[k]]))
  }
  class_trans <- class_trans[class_trans %in% "year" == FALSE]
  class_trans <- unique(class_trans)
  # 构建目标列表
  ls_trans <- vector("list", length(class_trans))
  names(ls_trans) <- class_trans
  for (i in names(ls_trans)) {
    # 构建一个列表，由和能源类型等长度数量的数据框组成
    temp_ls <- vector("list", length(ls_ori))
    names(temp_ls) <- names(ls_ori)
    # 遍历每个按能源类型分类的各类车能耗数据框
    # 如果包含所需车型，则提取出年份和该车型列所组成的数据框
    # 最后将这些数据框组合，即得到目标车型的各类能源消费数据框
    for (j in names(ls_ori)) {
      if (i %in% names(ls_ori[[j]])) {
        temp_ls[[j]] <- ls_ori[[j]][c("year", i)]
        names(temp_ls[[j]])[2] <- j
      } else {
        temp_ls[[j]] <- NULL
      }
    }
    ls_trans[[i]] <- func_merge_2(temp_ls)
  }
  ls_trans
}

# 获得变量名称
func_varname <- function(variable) {
  deparse(substitute(variable))
}

# 读取普通表格
func_read_data <- function(name_subdir, order_sht = 1) {
  data_dir <- "C:/Users/kangj/Documents/OneDrive/Zotero/storage/"
  data_name <- list.files(paste0(data_dir, "/", name_subdir, "/"))
  path <- paste0(data_dir, "/", name_subdir, "/", data_name)
  data <- read.xlsx(path, sheet = order_sht)
  data
}

# 读取并转化带4列文件头的Excel数据
func_read_trans <- function(name_subdir, order_sht = 1) {
  data_ori <- func_read_data(name_subdir, order_sht = 1)
  data_ori <- data_ori[, -c(1,2)]
  data_trans <- as.data.frame(t(data_ori[, -c(1,2)]))
  colnames(data_trans) <- data_ori[, 1]
  data_trans$year <- colnames(data_ori)[3:ncol(data_ori)]
  for (i in c(1: nrow(data_ori))) {
    comment(data_trans[, names(data_trans)[i]]) <- data_ori[i, 2]
  }
  data_trans <- 
    data_trans[c("year", names(data_trans)[names(data_trans) %in% "year" == FALSE])]
  rownames(data_trans) <- NULL
  # 删除列名中的空格
  columnnames <- gsub(" ", "", names(data_trans))
  names(data_trans) <- columnnames
  # 传递备注信息
  notes <- func_looknote(data_trans)[, "note"]
  data_trans <- as.data.frame(lapply(data_trans, as.numeric))
  data_trans <- func_addnote(data_trans, notes)
  func_looknote(data_trans)
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

# 转化列表中所有数据为数字
func_ls_asnumber <- function(ls) {
  for (i in c(1: length(ls))) {
    ls[[i]] <- as.data.frame(lapply(ls[[i]], as.numeric))
  }
  ls
}

# 查看一个数据框中不同数据的变化趋势：数据框版本
func_show_trend <- function(var_df) {
  names_var_df <- names(var_df)[names(var_df) %in% "year" == FALSE]
  names_unit <- 
    func_looknote(var_df[names_var_df])
  
  var_df_ls <- vector("list", nrow(names_unit))
  names(var_df_ls) <- unique(names_unit$note)
  for (i in c(1: length(unique(names_unit$note)))) {
    var_df_ls[[i]] <- 
      var_df[, c("year", 
                 names_unit$colnames[names_unit$note == unique(names_unit$note)[i]])]
    var_df_ls[[i]] <- melt(var_df_ls[[i]], id = "year")
    var_df_ls[[i]] <- var_df_ls[[i]][is.na(var_df_ls[[i]]$value) == FALSE,]
    plot <- ggplot(var_df_ls[[i]]) + 
      geom_point(aes(year, value, color = variable), na.rm = T,size = 2, alpha = 0.5)
    print(plot)
  }
}

# 查看一个数据框中不同数据的变化趋势：列表版本
func_show_trend_ls <- function(var_ls) {
  for (i in c(1: length(var_ls))) {
    func_show_trend(var_ls[[i]])
  }
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

# 计算乘积：可能跟上面的函数重复了
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
  # 检查数据，如果年份有重复，就输出警告
  if (sum(duplicated(df.nrg.intst$year)) + sum(duplicated(df.actlvl$year))) {
    print("warning: 重复数据")
  } else {
    # 统一数据框的年份：取交集
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
}
# 基于活动水平和活动强度计算活动总量：列表版
func_nrg_sum_ls <- function(ls_nrgintst, df_actlvl) {
  ls_nrgsum <- vector("list", length(ls_nrgintst))
  for (i in c(1: (length(ls_nrgintst)))) {
    ls_nrgsum[[i]] <- func_nrg_sum(ls_nrgintst[[i]], 
                                   df_actlvl, names(df_actlvl)[i + 1])
  }
  ls_nrgsum
}

# 比较历史数据和预测数据：两个数据框的版本 - 比较某一列
func_history_project <- function(var_his, name_his, var_proj, name_proj) {
  var_his <- var_his[, c("year", name_his)]
  var_proj <- var_proj[, c("year", name_proj)]
  var_his$color <- "history"
  var_proj$color <- "project"
  names(var_his)[names(var_his) == name_his] <- name_proj
  total_df <- rbind(var_his[, c("year", name_proj, "color")], 
                    var_proj[, c("year", name_proj, "color")])
  total_df <- total_df[is.na(total_df[, name_proj]) == FALSE, ]
  # 将作图数据强制转化为数字，否则可能会出现坐标轴重叠
  total_df$year <- as.numeric(total_df$year)
  total_df[, name_proj] <- as.numeric(total_df[, name_proj])
  # 作图
  plot <- ggplot(total_df) + 
    geom_point(aes(year, total_df[, name_proj], color = color), alpha = 0.5, size = 3) +
    labs(y = name_his)
  print(plot)
}

# 比较历史数据和预测数据：两个数据框的版本 - 比较每一列
# 要保证输入两个数据框列数一致
func_history_project_df <- function(var_his, var_proj) {
  names_varhis <- names(var_his)[names(var_his) %in% "year" == FALSE]
  names_varproj <- names(var_proj)[names(var_proj) %in% "year" == FALSE]
  plot_ls <- vector("list")
  for (i in c(1: length(names_varhis))) {
    plot_ls[[i]] <- func_history_project(var_his, names_varhis[i], 
                                         var_proj, names_varproj[i])
  }
  plot_arrange <- ggarrange(plotlist = plot_ls, nrow = 2, ncol = 2, common.legend = TRUE)
  print(plot_arrange)
}

# 比较历史数据和预测数据：两个格式一致的列表的版本
func_history_project_ls <- function(ls_his, ls_proj) {
  for (i in c(1: length(ls_his))) {
    func_history_project_df(ls_his[[i]], ls_proj[[i]])
  }
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
func_supple_colnames <- function(var_output, account_names){
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
func_rate <- function(baseyear, basevalue, rate_df) {
  names(rate_df) <- c("year", "rate")
  # 先统一年度为基准年到增长率数据框的最后一年
  rate_df <- rate_df[rate_df$year %in% c(baseyear: max(rate_df$year)), ]
  # 更改增长率的单位为1
  rate_df$rate <- rate_df$rate / 100
  proj_df <- rbind(data.frame(year = baseyear, rate = basevalue), 
                   rate_df)
  names(proj_df) <- c("year", "value")
  for (i in c(2: nrow(proj_df))) {
    proj_df$value[i] <- proj_df$value[i - 1] * (1 + proj_df$value[i])
  }
  plot(proj_df$value)
  proj_df <- proj_df[-1, ]
}

# 结果计算函数
func_result <- function(var_aclevel, var_int) {
  total_df <- merge(var_aclevel, var_int, by = "year", all.x = TRUE)
  total_df$value <- total_df[, 1] * total_df[, 2]
  plot(total_df$value)
  total_df
}

# 通过能源总量和活动水平计算活动强度：数据框对某一列的版本
func_nrg_intst <- function(df_nrg_sum, df_actlvl, name) {
  # 先将年份转换为数字类型
  df_nrg_sum$year <- as.numeric(df_nrg_sum$year)
  df_actlvl$year <- as.numeric(df_actlvl$year)
  df_actlvl <- df_actlvl[, c("year", name)]
  # 统一年份：并集
  allyear <- union(df_nrg_sum$year, df_actlvl$year)
  df_nrg_sum <- merge(data.frame(year = allyear), df_nrg_sum, by = "year", all = TRUE)
  df_actlvl <- merge(data.frame(year = allyear), df_actlvl, by = "year", all = TRUE)
  # 构建输出数据框
  total_df <- df_nrg_sum
  for (i in names(total_df)[names(total_df) %in% "year" == FALSE]) {
    total_df[, i] <- total_df[, i] / df_actlvl[, name]
  }
  total_df
}
# 通过能源总量和活动水平计算活动强度：列表对数据框的版本
# 输入列表的长度应等于数据框列数减1
# 列表中数据框的排列顺序应和数据框严格对应
# 计算结果应为一个列表
func_nrg_intst_ls <- function(ls_nrgsum, df_actlvl) {
  ls_nrgintst <- vector("list", length(ls_nrgsum))
  for (i in c(1: length(ls_nrgsum))) {
    ls_nrgintst[[i]] <- func_nrg_intst(ls_nrgsum[[i]], df_actlvl, 
                                             names(df_actlvl)[i + 1])
    names(ls_nrgintst)[i] <- names(df_actlvl)[i + 1]
  }
  ls_nrgintst
}

# 生成预测数据的函数
func_proj <- function(input_ls, itemnames, startyear = 2005, endyear = 2050) {
  out_df <- data.frame(year = c(startyear: endyear))
  for (i in c(1:length(itemnames))) {
    out_df[, itemnames[i]] <- func_interp(input_ls[[i]])$value
  }
  out_df
}

# 根据历史趋势线性外推未来趋势的函数
func_linear <- function(df_history, col_dependent, endyear) {
  df_history[, "year"] <- as.numeric(df_history[, "year"])
  fit <- lm(df_history[, col_dependent] ~ df_history[, "year"])
  fit_result <- summary(fit)
  proj_df <- data.frame(year = c((max(df_history[, "year"]) + 1): endyear))
  proj_df[, col_dependent] <- 
    fit_result$coefficients[2, 1] * proj_df[, "year"] + 
    fit_result$coefficients[1, 1]
  proj_df[, "color"] <- "predicted"
  df_history[, "color"] <- "history"
  proj_df <- rbind(df_history, proj_df)
  ggplot() + geom_point(aes(proj_df[, "year"], proj_df[, col_dependent], 
                            color = proj_df[, "color"]))
  proj_df
}

## 常用名称变量
# 能源类别
nrg_names <- c("coal", "coal_product", 
               "gasoline", "diesel", "kerosene", "residual", "lpg", 
               "natural_gas", 
               "electricity")


## 全局变量
## GDP
gdp <- func_read_trans("2VHEE264", "GDP")

# GDP预测
proj_gdp_rate <- func_interp_2(
  year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050),
  value = c(8, 7.90, 6.00, 6.00, 5.00, 4.00, 3.00))
proj_gdp <- func_rate(baseyear = 2019, basevalue = 59950422, 
                      rate_df = proj_gdp_rate)
names(proj_gdp)[2] <- "GDP"
func_history_project(gdp, "GDP", proj_gdp, "GDP")
comment(proj_gdp$value) <- "万元当年价"

# 工业GDP（需求：暂时算的是第二产业）
proj_gdp_ind_prop <- func_interp_2(
  year = c(2005, 2019, 2020, 2025, 2030, 2035, 2050), 
  value = c(41, 41.60, 41, 30, 26, 23, 19))
comment(proj_gdp_ind_prop$value) <- "%"
proj_gdp_ind <- data.frame(year = c(2005:2050), 
                           value = proj_gdp$value * 
                             proj_gdp_ind_prop$value / 100)
comment(proj_gdp_ind$value) <- "万元当年价"

# 人口和户数
population <- func_read_trans("2VHEE264")
population$year <- as.numeric(population$year)
# 假设2016-2017的城镇家庭户数和2015年一致
population$调查城镇家庭规模[population$year > 2015] <- 
  population$调查城镇家庭规模[population$year == 2015]
population$household <- population$"常住人口" / population$"调查城镇家庭规模"
# 计算户数
proj_population <- func_interp_2(
  year = c(2019, 2025, 2030, 2035, 2050), 
  value = c(population$"常住人口"[population$year == 2019], 
            550, 737, 730, 800))
proj_household <- proj_population
names(proj_household)[2] <- "household"
proj_household$household <- proj_household$household/2.5
comment(proj_household$household) <- "万户"

## 其他部门：家庭，建筑业和农业
names_other_act <- c("household", "lpg_user", "gas_user", 
                     "construct_gdp", "agriculture_area")
names_other_ls <- c("household_electricity", "household_lpg", "household_gas", 
                           "construct_electricity", "agriculture_electricity")

# 历史数据
# 构建其他部门活动水平数据框
# 家庭户数
ori_other_act_house <- data.frame(year = population$year, 
                              household = population$常住人口 / 
                                population$调查城镇家庭规模)
plot(ori_other_act_house$year, ori_other_act_house$household)
comment(ori_other_act_house$household) <- "万户"

# 用液化石油气的户数
ori_other_act_house_lpg <- func_read_trans("S32RZEF7", "瓶装液化气总用户数")
ori_other_act_house_lpg <- ori_other_act_house_lpg[, c("year", "民用")]
ori_other_act_house_lpg$民用 <- ori_other_act_house_lpg$民用 / 10000
ori_other_act_house_lpg <- merge(population[, c("year", "household")],
                                 ori_other_act_house_lpg, by = "year")
names(ori_other_act_house_lpg)[2] <- "total_household"
names(ori_other_act_house_lpg)[3] <- "lpg_user"
plot(ori_other_act_house_lpg$lpg_user / ori_other_act_house_lpg$total_household)

# 用管道天然气的用户数
ori_other_act_house_gas <- func_read_trans("S32RZEF7", "管道天然气总用户数")
ori_other_act_house_gas <- ori_other_act_house_gas[, c("year", "民用")]
ori_other_act_house_gas$民用 <- ori_other_act_house_gas$民用 / 10000
ori_other_act_house_gas <- merge(population[, c("year", "household")],
                                 ori_other_act_house_gas, by = "year")
names(ori_other_act_house_gas)[2] <- "total_household"
names(ori_other_act_house_gas)[3] <- "gas_user"
plot(ori_other_act_house_gas$gas_user / ori_other_act_house_gas$total_household)
# 比例上：液化石油气下降，管道天然气可能是上升

# 建筑业的GDP
ori_other_act_construct_gdp <- gdp[, c("year", "##建筑业")]

# 农业的播种面积
ori_other_agriculture_area <- func_read_trans("4NJ97NS9")
ori_other_agriculture_area <- 
  ori_other_agriculture_area[, c("year", "全年农作物总播种面积")]
ori_other_agriculture_area[, "全年农作物总播种面积"] <- 
  ori_other_agriculture_area[, "全年农作物总播种面积"]/1500
ori_other_agriculture_area
comment(ori_other_agriculture_area$全年农作物总播种面积) <- "平方公里"

# 合并成一个活动水平数据框
other_act <- Reduce(func_merge, list(ori_other_act_house, 
                                     ori_other_act_house_lpg[, c("year", "lpg_user")], 
                                     ori_other_act_house_gas[, c("year", "gas_user")],
                                     ori_other_act_construct_gdp, 
                                     ori_other_agriculture_area))
comment(other_act$household) <- attributes(ori_other_act_house$household)[[1]]
comment(other_act$lpg_user) <- attributes(ori_other_act_house_lpg$lpg_user)[[1]]
comment(other_act$gas_user) <- attributes(ori_other_act_house_gas$gas_user)[[1]]
comment(other_act$"##建筑业") <- attributes(ori_other_act_construct_gdp$"##建筑业")[[1]]
comment(other_act$全年农作物总播种面积) <- attributes(ori_other_agriculture_area$全年农作物总播种面积)[[1]]
func_looknote(other_act)
func_show_trend(other_act)

# 构建其他部门的能耗总量列表
other_nrgsum_ls <- vector("list", 5)
# 家庭用电部分
names(other_nrgsum_ls)[1] <- "家庭用电"
other_nrgsum_ls[[1]] <- func_read_trans("2I4DKY2A")
other_nrgsum_ls[[1]] <- other_nrgsum_ls[[1]][, c("year", "#城乡居民生活用电")]
names(other_nrgsum_ls[[1]]) <- c("year", "household_electricity")
# 家庭液化石油气部分
names(other_nrgsum_ls)[2] <- "家庭液化石油气"
other_nrgsum_ls[[2]] <- func_read_trans("HHKVE85Q", "瓶装液化气")
other_nrgsum_ls[[2]] <- other_nrgsum_ls[[2]][, c("year", "家庭")]
names(other_nrgsum_ls[[2]]) <- c("year", "household_lpg")
# 家庭天然气部分
names(other_nrgsum_ls)[3] <- "家庭天然气"
other_nrgsum_ls[[3]] <- func_read_trans("HHKVE85Q", "管道天然气")
other_nrgsum_ls[[3]] <- other_nrgsum_ls[[3]][, c("year", "家庭")]
names(other_nrgsum_ls[[3]]) <- c("year", "household_gas")
# 建筑用电部分
names(other_nrgsum_ls)[4] <- "建筑用电"
other_nrgsum_ls[[4]] <- func_read_trans("2I4DKY2A", "全市电力消费情况表分具体行业")
other_nrgsum_ls[[4]] <- other_nrgsum_ls[[4]][, c("year", "建筑业")]
names(other_nrgsum_ls[[4]]) <- c("year", "construct_electricity")
# 农业用电部分
names(other_nrgsum_ls)[5] <- "农业用电"
other_nrgsum_ls[[5]] <- ori_global_electricity[, c("year", "##第一产业")]
names(other_nrgsum_ls[[5]]) <- c("year", "agriculture_electricity")

# 检查各部分的单位
func_looknote_ls(other_nrgsum_ls)
func_show_trend_ls(other_nrgsum_ls)

# 计算各行业用能强度
other_nrgintst_ls <- func_nrg_intst_ls(other_nrgsum_ls, other_act)
func_show_trend_ls(other_nrgintst_ls)

# 和广州家庭用电数据比较
electricity_living_guangzhou <- func_read_trans("QSPLFZXP", 3)
population_guangzhou <- func_read_trans("QSPLFZXP")
func_merge_rate(electricity_living_guangzhou, "#生活用电", 
                population_guangzhou, "总户数")
# 单从数据而言，厦门市还是有很大的增长空间的

# 未来活动水平
# 家庭用电
proj_other_act <- proj_household
func_history_project(population, "household", proj_other_act, "household")
comment(proj_other_act$household) <- "万户"
# 家庭液化石油气部分
proj_other_act$lpg_user <- 
  func_nrg_sum(proj_other_act[, c("year", "household")], 
               func_interp_2(year = c(2019, 2030, 2050), value = c(0.30, 0.15, 0.08)), 
               "value")$household
func_history_project(other_act, "lpg_user", proj_other_act, "lpg_user")
comment(proj_other_act$lpg_user) <- "万户"
# 家庭天然气部分
proj_other_act$gas_user <- 
  func_nrg_sum(proj_other_act[, c("year", "household")], 
               func_interp_2(year = c(2019, 2030, 2050), value = c(0.35, 0.70, 0.96)), 
               "value")$household
func_history_project(other_act, "gas_user", proj_other_act, "gas_user")
comment(proj_other_act$gas_user) <- "万户"
# 建筑物用电部分
proj_other_act$construction_gdp <- 
  func_nrg_sum(proj_gdp, 
               func_interp_2(year = c(2019,2030,2050), 
                             value = c(0.10, 
                                       0.08,
                                       0.03)), 
               "value")$GDP
func_history_project(gdp[gdp$year > 2000, ], "##建筑业", proj_other_act, "construction_gdp")
comment(proj_other_act$construction_gdp) <- "万元"
# 农业用电部分
proj_other_act$agriculture_area <- 
  func_interp_2(year = c(2019, 2030, 2050), 
               value = c(221, 
                         221 * 0.6, 
                         221 * 0.5))$value
comment(proj_other_act$agriculture_area) <- "平方千米"

func_show_trend(proj_other_act)

# 活动强度
proj_other_nrgintst_ls <- vector("list")
# 家庭用电部分
proj_other_nrgintst_ls[[1]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
              value = c(3900, 3900*1.2, 3900*1.4))
names(proj_other_nrgintst_ls[[1]])[2] <- "家庭用电强度"
func_history_project(other_nrgintst_ls[[1]], "household_electricity", 
                     proj_other_nrgintst_ls[[1]], "家庭用电强度")

# 家庭液化石油气部分
proj_other_nrgintst_ls[[2]] <- 
  data.frame(year = c(2019: 2050), 
             lpg = 588.52)
names(proj_other_nrgintst_ls[[2]])[2] <- "每户液化石油气"
func_history_project(other_nrgintst_ls[[2]], "household_lpg", 
                     proj_other_nrgintst_ls[[2]], "每户液化石油气")

# 家庭天然气部分
proj_other_nrgintst_ls[[3]] <- 
  data.frame(year = c(2019: 2050), 
             natural_gas = 74.50)
names(proj_other_nrgintst_ls[[3]])[2] <- "每户天然气"
func_history_project(other_nrgintst_ls[[3]], "household_gas", 
                     proj_other_nrgintst_ls[[3]], "每户天然气")

# 建筑业用电部分
proj_other_nrgintst_ls[[4]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(0.0081, 
                          0.0081 * 1.2, 
                          0.0081))
names(proj_other_nrgintst_ls[[4]])[2] <- "单位建筑GDP用电"
func_history_project(other_nrgintst_ls[[4]], "construct_electricity", 
                     proj_other_nrgintst_ls[[4]], "单位建筑GDP用电")

# 农业用电
proj_other_nrgintst_ls[[5]] <- 
  func_interp_2(year = c(2019, 2030, 2050), 
                value = c(59, 
                          59 * 0.8,
                          59 * 0.7))
names(proj_other_nrgintst_ls[[5]])[2] <- "农业单位面积用电量"
func_history_project(other_nrgintst_ls[[5]], "agriculture_electricity", 
                     proj_other_nrgintst_ls[[5]], "农业单位面积用电量")

# 则能耗总量为
proj_other_nrgsum_ls <- func_nrg_sum_ls(proj_other_nrgintst_ls, proj_other_act)

func_history_project_ls(other_nrgintst_ls, proj_other_nrgintst_ls)
func_history_project_df(other_act, proj_other_act)
func_history_project_ls(other_nrgsum_ls, proj_other_nrgsum_ls)

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
proj_com_act <- data.frame(year = c(2005: 2050))
proj_com_act$work_pop <- proj_household$population * 
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

## 将结果合并
names(proj_trans_nrgsum_ls) <- trans_names
names(proj_ind_nrgsum_ls) <- ind_agg_list
names(proj_com_nrgsum_ls) <- c("用电", "用气")
names(proj_other_nrgsum_ls) <- c("household", 
                                 "household_lpg", 
                                 "household_natural_gas", 
                                 "construction_gdp", 
                                 "agriculture_area")

# 给各个数据框添加部门名称和活动水平名称
for (i in c(1:length(proj_trans_nrgsum_ls))) {
  proj_trans_nrgsum_ls[[i]]$act_name <- names(proj_trans_nrgsum_ls)[i]
  proj_trans_nrgsum_ls[[i]]$sector <- func_varname(proj_trans_nrgsum_ls)
  proj_trans_nrgsum_ls[[i]] <- func_supple_colnames(proj_trans_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_ind_nrgsum_ls))) {
  proj_ind_nrgsum_ls[[i]]$act_name <- names(proj_ind_nrgsum_ls)[i]
  proj_ind_nrgsum_ls[[i]]$sector <- func_varname(proj_ind_nrgsum_ls)
  proj_ind_nrgsum_ls[[i]] <- func_supple_colnames(proj_ind_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_com_nrgsum_ls))) {
  proj_com_nrgsum_ls[[i]]$act_name <- names(proj_com_nrgsum_ls)[i]
  proj_com_nrgsum_ls[[i]]$sector <- func_varname(proj_com_nrgsum_ls)
  proj_com_nrgsum_ls[[i]] <- func_supple_colnames(proj_com_nrgsum_ls[[i]], nrg_names)
}

for (i in c(1:length(proj_other_nrgsum_ls))) {
  proj_other_nrgsum_ls[[i]]$act_name <- names(proj_other_nrgsum_ls)[i]
  proj_other_nrgsum_ls[[i]]$sector <- func_varname(proj_other_nrgsum_ls)
  proj_other_nrgsum_ls[[i]] <- func_supple_colnames(proj_other_nrgsum_ls[[i]], nrg_names)
}

# 分部门合并
# 交通
proj_trans_nrgsum_df <- proj_trans_nrgsum_ls[[1]]
for (i in c(2: length(proj_trans_nrgsum_ls))) {
  proj_trans_nrgsum_df <- rbind(proj_trans_nrgsum_df, proj_trans_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_trans_nrgsum_df[, i] <- as.numeric(proj_trans_nrgsum_df[, i])
}
proj_trans_nrgsum_agg_df <- aggregate(proj_trans_nrgsum_df[, nrg_names], 
                                      by = list(proj_trans_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_trans_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_trans_nrgsum_agg_df[, c("year", "diesel")])

# 工业
proj_ind_nrgsum_df <- proj_ind_nrgsum_ls[[1]]
for (i in c(2: length(proj_ind_nrgsum_ls))) {
  proj_ind_nrgsum_df <- rbind(proj_ind_nrgsum_df, proj_ind_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_ind_nrgsum_df[, i] <- as.numeric(proj_ind_nrgsum_df[, i])
}
proj_ind_nrgsum_agg_df <- aggregate(proj_ind_nrgsum_df[, nrg_names], 
                                    by = list(proj_ind_nrgsum_df$year), 
                                    function(x) {sum(x, na.rm = TRUE)})
names(proj_ind_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_ind_nrgsum_agg_df)

# 服务业
proj_com_nrgsum_df <- proj_com_nrgsum_ls[[1]]
for (i in c(2: length(proj_com_nrgsum_ls))) {
  proj_com_nrgsum_df <- rbind(proj_com_nrgsum_df, proj_com_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_com_nrgsum_df[, i] <- as.numeric(proj_com_nrgsum_df[, i])
}
proj_com_nrgsum_agg_df <- aggregate(proj_com_nrgsum_df[, nrg_names], 
                                    by = list(proj_com_nrgsum_df$year), 
                                    function(x) {sum(x, na.rm = TRUE)})
names(proj_com_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_com_nrgsum_agg_df)

# 其他
proj_other_nrgsum_df <- proj_other_nrgsum_ls[[1]]
for (i in c(2: length(proj_other_nrgsum_ls))) {
  proj_other_nrgsum_df <- rbind(proj_other_nrgsum_df, proj_other_nrgsum_ls[[i]])
}
for (i in nrg_names) {
  proj_other_nrgsum_df[, i] <- as.numeric(proj_other_nrgsum_df[, i])
}
proj_other_nrgsum_agg_df <- aggregate(proj_other_nrgsum_df[, nrg_names], 
                                      by = list(proj_other_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_other_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_other_nrgsum_agg_df)

# 汇总
proj_total_nrgsum_df <- Reduce(rbind, list(proj_trans_nrgsum_agg_df, 
                                           proj_ind_nrgsum_agg_df, 
                                           proj_com_nrgsum_agg_df, 
                                           proj_other_nrgsum_agg_df))
proj_total_nrgsum_agg_df <- aggregate(proj_total_nrgsum_df[, nrg_names], 
                                      by = list(proj_total_nrgsum_df$year), 
                                      function(x) {sum(x, na.rm = TRUE)})
names(proj_total_nrgsum_agg_df)[1] <- "year"
func_show_trend(proj_total_nrgsum_agg_df[names(proj_total_nrgsum_agg_df) %in% c("kerosene") == F])


## 构建排放因子列表
ef_ls <- func_read_trans("8C8EDJVH")
ef_ls_2 <- ef_ls[-1, ]
for (i in c(1:3)) {
  ef_ls_2[i, -1] <- ef_ls_2[i, -1]* ef_ls[1, -1]
}
ef_ls_2
names(ef_ls_2) <- c("ghg", "coal", "coal_product", 
                    "gasoline", "kerosene", "diesel", "residual", "lpg", "natural_gas", 
                    "electricity")

## 计算排放量
# 通过时间序列的各类能耗 + 各类能源的各类排放因子 = 计算时间序列的各类排放的函数
# 测试数据
var_nrgsum <- proj_trans_nrgsum_agg_df
var_emifac <- ef_ls_2
func_emisum <- function(var_nrgsum, var_emifac) {
  # 输入的时间序列各类能耗中能源的名字和排放因子中能源的名字要相同
  # 建立一个列表用于存放二氧化碳、甲烷和氧化亚氮排放量的列表
  emission_ls <- vector("list", 3)
  names(emission_ls) <- var_emifac[, 1]
  # 填充排放列表
  for (j in c(1:3)) {
    for (i in names(var_nrgsum)[names(var_nrgsum) %in% "year" == FALSE]) {
      emission_ls[[j]] <- cbind(emission_ls[[j]], 
                                var_nrgsum[, i] * var_emifac[j, i])
    }
  }
  # 构建输出数据框：观察为时间序列，列名为各类温室气体
  out_df <- data.frame(year = var_nrgsum[, "year"], 
                       co2 = rowSums(emission_ls[[1]]), 
                       ch4 = rowSums(emission_ls[[2]]), 
                       n2o = rowSums(emission_ls[[3]]))
  out_df
}
proj_trans_emission <- func_emisum(proj_trans_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_trans_emission)
proj_com_emission <- func_emisum(proj_com_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_com_emission)
proj_ind_emission <- func_emisum(proj_ind_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_ind_emission)
proj_other_emission <- func_emisum(proj_other_nrgsum_agg_df, ef_ls_2)
func_show_trend(proj_other_emission)

proj_total_emission <- rbind(proj_trans_emission, 
                             proj_com_emission, 
                             proj_ind_emission, 
                             proj_other_emission)
proj_total_emission_agg <- aggregate(proj_total_emission[, c("co2", "ch4", "n2o")], 
                                     list(proj_total_emission$year), 
                                     function(x) sum(x, na.rm = TRUE))
names(proj_total_emission_agg)[1] <- "year"
func_show_trend(proj_total_emission_agg)

