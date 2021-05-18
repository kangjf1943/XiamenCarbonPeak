library(openxlsx)
library(ggplot2)
library(reshape2)
library(ggpubr)

# 全局函数
Sys.setlocale("LC_ALL", "chinese")

## 构建函数
# 取一列数据最后一个有效数值
func_lastone <- function(numbers) {
  # 去除零值
  numbers <- numbers[numbers != 0]
  # 去除NA
  numbers <- numbers[is.na(numbers) == FALSE]
  # 去除NaN
  numbers <- numbers[is.nan(numbers) == FALSE]
  # 去除Inf
  numbers <- numbers[is.infinite(numbers) == FALSE]
  # 取最后一个值
  numbers <- tail(numbers, 1)
  numbers
}

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
name_subdir <- "2VHEE264"
order_sht <- "GDP"
func_read_trans <- function(name_subdir, order_sht = 1) {
  data_ori <- func_read_data(name_subdir, order_sht = order_sht)
  # 删去前两列，即数据来源和数据备注
  data_ori <- data_ori[, -c(1,2)]
  # 组合不含标头的数据，第一列为年份，其他列为数据
  data_trans <- cbind(colnames(data_ori)[3:ncol(data_ori)], 
                      as.data.frame(t(data_ori[, -c(1,2)])))
  # 将所有数据转换成数字类型
  data_trans <- as.data.frame(lapply(data_trans, as.numeric))
  # 设置列名，并删除列名中的空格
  colnames(data_trans) <- c("year", gsub(" ", "", data_ori[, 1]))
  # 传递备注单位信息
  # 年份的单位设置为“year”
  comment(data_trans$year) <- "year"
  # 其他列单位则来自所读取的数据表
  for (i in c(1: nrow(data_ori))) {
    comment(data_trans[, (i + 1)]) <- data_ori[i, 2]
  }
  rownames(data_trans) <- NULL
  # 输出的时候查看对应单位
  func_looknote(data_trans)
  cat("\n")
  # 输出结果
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
var_df <- com_act
func_show_trend <- function(var_df) {
  names_var_df <- names(var_df)[names(var_df) %in% "year" == FALSE]
  names_unit <- 
    func_looknote(var_df[names_var_df])
  
  var_df_ls <- vector("list", nrow(names_unit))
  names(var_df_ls) <- unique(names_unit$note)
  # 构建列表以储存图像数据
  plot_ls <- vector("list")
  for (i in c(1: length(unique(names_unit$note)))) {
    var_df_ls[[i]] <- 
      var_df[, c("year", 
                 names_unit$colnames[names_unit$note == unique(names_unit$note)[i]])]
    var_df_ls[[i]] <- melt(var_df_ls[[i]], id = "year")
    var_df_ls[[i]] <- var_df_ls[[i]][is.na(var_df_ls[[i]]$value) == FALSE,]
    plot_ls[[i]] <- ggplot(var_df_ls[[i]]) + 
      geom_point(aes(year, value, color = variable), na.rm = T, 
                              size = 2, alpha = 0.5)
  }
  plot <- ggarrange(plotlist = plot_ls, nrow = 2, ncol = 2)
  plot
}

# 查看一个数据框中不同数据的变化趋势：列表版本
func_show_trend_ls <- function(var_ls) {
  for (i in c(1: length(var_ls))) {
    print(func_show_trend(var_ls[[i]]))
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
func_history_project <- function(var_his, name_his, var_proj, name_proj, 
                                 figureout = TRUE) {
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
  plot_data <- ggplot(total_df) + 
    geom_point(aes(year, total_df[, name_proj], color = color), alpha = 0.5, size = 3) +
    labs(y = name_his)
  if (figureout == TRUE) {print(plot_data)} else {plot_data}
}
# 比较历史数据和预测数据：两个数据框的版本 - 比较每一列
# 要保证输入两个数据框列数一致
var_his <- ls_his[[i]]
var_proj <- ls_proj[[i]]
func_history_project_df <- function(var_his, var_proj, 
                                    commontitle = NULL) {
  names_varhis <- names(var_his)[names(var_his) %in% "year" == FALSE]
  names_varproj <- names(var_proj)[names(var_proj) %in% "year" == FALSE]
  plot_ls <- vector("list")
  for (i in c(1: length(names_varhis))) {
    plot_ls[[i]] <- func_history_project(var_his, names_varhis[i], 
                                         var_proj, names_varproj[i],
                                         figureout = FALSE)
  }
  plot_arrange <- ggarrange(plotlist = plot_ls, 
                            nrow = 3, ncol = 2, 
                            common.legend = TRUE, labels = commontitle)
  print(plot_arrange)
}
# 比较历史数据和预测数据：两个格式一致的列表的版本
ls_his <- ind_nrgintst_ls
ls_proj <- proj_ind_nrgintst_ls
func_history_project_ls <- function(ls_his, ls_proj) {
  for (i in c(1: length(ls_his))) {
    func_history_project_df(ls_his[[i]], ls_proj[[i]], 
                            commontitle = names(ls_his[i]))
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
func_interp_2 <- function(year, value, name_value = "value") {
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
  names(total_df)[2] <- name_value
  plot(total_df$year, total_df[, name_value])
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

