## 载入包和设置
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(ggpubr)
library(RColorBrewer)
Sys.setlocale("LC_ALL", "chinese")


## 数据读取函数
# 来自Zotero的普通Excel数据
func_read_data <- function(name_subdir, order_sht = 1) {
  data_dir <- "C:/Users/kangj/Documents/OneDrive/Zotero/storage/"
  data_name <- list.files(paste0(data_dir, "/", name_subdir, "/"))
  path <- paste0(data_dir, "/", name_subdir, "/", data_name)
  data <- read.xlsx(path, sheet = order_sht)
  data
}

# 读取并转化带4列文件头的Excel数据
# 问题：如果Excel数据中“数据来源”有数据，而其他无数据，就会导致列明出现“NA”
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
# 读取多个工作表输出为列表
# 输入变量：待读取的工作表名称
func_read_multitable <- function(name_subdir, names_tbl, names_ls) {
  out_ls <- vector("list", length(names_tbl))
  for (i in c(1: length(names_tbl))) {
    out_ls[[i]] <- func_read_trans(name_subdir, names_tbl[i])
  }
  names(out_ls) <- names_ls
  out_ls
}


## 转化列表中所有数据为数字
func_ls_asnumber <- function(ls) {
  for (i in c(1: length(ls))) {
    ls[[i]] <- as.data.frame(lapply(ls[[i]], as.numeric))
  }
  ls
}

## 构建列表分类方式转换的函数：类别1到类别2
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

## 将计算结果统一到一个表头下的函数
func_supple_colnames <- function(var_output, account_names){
  # 对于原本没有的列，填充为0
  supple_colnames <- account_names[account_names %in% names(var_output) == FALSE]
  for (i in supple_colnames) {
    var_output[, i] <- 0
  }
  # 重新排列
  var_output[, c(account_names, 
                 names(var_output)[names(var_output) %in% account_names == FALSE])]
}

## 获得变量名称
func_varname <- function(variable) {
  deparse(substitute(variable))
}

## 判断峰值年份
func_peakyear <- function(nrg_df, name_peak) {
  nrg_df$year[which(nrg_df[, name_peak] == max(nrg_df[, name_peak], na.rm = TRUE))]
}

## 取一列数据最后一个有效数值
func_lastone <- function(numbers, zero.rm = TRUE) {
  # 是否去除零值
  if (zero.rm) {
    numbers <- numbers[numbers != 0]
  }
  # 去除NA
  numbers <- numbers[is.na(numbers) == FALSE]
  # 去除NaN
  numbers <- numbers[is.nan(numbers) == FALSE]
  # 去除Inf
  numbers <- numbers[is.infinite(numbers) == FALSE]
  # 取最后一个值
  numbers <- tail(numbers, 1)
  if (length(numbers) == 0) {
    numbers <- 0
  }
  numbers
}

## 给数据框添加备注信息
func_addnote <- function(data, note) {
  for (i in c(1: ncol(data))) {
    comment(data[, names(data)[i]]) <- note[i]
  }
  data
}

## 查看列名对应的批注
# 数据框版
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
# 列表版
func_looknote_ls <- function(var_ls) {
  for (i in c(1: length(var_ls))) {
    print(names(var_ls[i]))
    print(func_looknote(var_ls[[i]]))
    cat("\n")
  }
}

## 合并多个时间序列数据框
# 该函数合并后输出为数据框，并跨越所有组成元素数据框的年份
func_merge_2 <- function(ls_var) {
  # 如果输入列表为空则输出为NULL
  if (length(ls_var) == 0) {
    NULL
  } else {
    # 输出数据框第一列为年份
    df_out <- data.frame(year = ls_var[[1]][, "year"])
    notes <- c("nounit")
    # 其他列为输入列表各元素数据框除年份外的内容
    for (i in c(1: length(ls_var))) {
      df_out <- merge(df_out, ls_var[[i]], by = "year", all = TRUE)
      notes <- c(notes, func_looknote(ls_var[[i]])[-1, "note"])
    }
    df_out <- func_addnote(df_out, notes)
    df_out
  }
}

# 合并多个年份不同数据框同名列并重命名各列
func_mrgcol <- function(ls_var, namemrg, namesnew) {
  # 判断新名字和要合并的列数长度是否一致
  if (length(namesnew) != length(ls_var)) {
    print("warning: wrong length of new names.")
    out_df <- NULL
  } else {
    for (i in c(1: length(ls_var))) {
      # 提取year列和目标列，存储，并改列名
      ls_var[[i]] <- ls_var[[i]][c("year", namemrg)]
      names(ls_var[[i]])[2] <- namesnew[i]
    }
    # 合并各个数据框成为一个新数据框
    out_df <- func_merge_2(ls_var)
  }
  out_df
}
comment(func_mrgcol) <- "合并多个年份不同数据框同名列并重命名各列"

## 计算两个系列的比率或乘积
# 两列版本
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
# 升级版
# 输入：两个时间序列数据框
# 可选计算方式：“sum”，“product”，“rate”和“difference”，默认为“product”
# 注意：该函数计算的时候顺序很重要！
func_cross <- function(df1, df2, method = "product") {
  # 判断是否包含“year”列
  if ("year" %in% names(df1) & "year" %in% names(df2) == FALSE) {
    print("warning: no year data in the dataframe.")
  } else {
    # 取出第一个数据框除了“year”外的列名作为输出列名
    dfout_names <- names(df1)[names(df1) %in% "year" == FALSE]
    # 先重命名输入的数据框以避免同名冲突
    df1_names <- paste0(1, c(1: (length(df1)-1)))
    df2_names <- paste0(2, c(1: (length(df2)-1)))
    names(df1) <- c("year", df1_names)
    names(df2) <- c("year", df2_names)
    # 基于year列合并数据框，作为储存结果的数据框
    dfout <- merge(df1, df2, by = "year")
    for (i in c(1: length(dfout_names))) {
      # 根据不同method参数选择不同计算方式
      if (method == "sum") {
        dfout[, dfout_names[i]] <- dfout[, df1_names[i]] + dfout[, df2_names[i]]
      }
      if (method == "difference") {
        dfout[, dfout_names[i]] <- dfout[, df1_names[i]] - dfout[, df2_names[i]]
      }
      if (method == "product") {
        dfout[, dfout_names[i]] <- dfout[, df1_names[i]] * dfout[, df2_names[i]]
      }
      if (method == "rate") {
        dfout[, dfout_names[i]] <- dfout[, df1_names[i]] / dfout[, df2_names[i]]
      }
    }
    dfout <- dfout[c("year", dfout_names)]
    dfout
  }
}
comment(func_cross) <- 
  "可选计算方式：“sum”，“product”，“rate”和“difference”，默认为“product”"


## 构建插值函数
func_interp_2 <- function(year, value, name_value = "value", showplot = FALSE) {
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
  if (showplot == TRUE) {
    plot(total_df$year, total_df[, name_value])
  }
  total_df
}

## 基于特定数值和比率插值
func_interp_3 <- function(year, scale, base, name_value = "value") {
  total_df <- data.frame(year = c(year[1]: year[length(year)]))
  basevalue <- base
  for (j in c(1:(length(year) - 1))) {
    start_year <- year[j]
    end_year <- year[j + 1]
    start_value <- basevalue * scale[j]
    end_value <- basevalue * scale[j + 1]
    for (i in seq(from = start_year, to = end_year, by = 1)) {
      total_df$value[which(total_df$year == i)] <- 
        start_value + 
        (end_value - start_value) * (i - start_year) /
        (end_year - start_year)
    }
  }
  names(total_df)[2] <- name_value
  if (sum(is.na(total_df[, name_value])) < nrow(total_df)) {
    plot(total_df$year, total_df[, name_value])
  }
  total_df
}

# 可指定初始增长率和最高值时间点
# 增速逐渐减慢直至饱和：抛物线加水平直线
func_curve_1 <- function(baseyear, basevalue, maxyear, endyear,  init_rate) {
  # 将增长率转化成斜率
  initrate <- init_rate*basevalue
  # 生成方程各项系数
  a <- (initrate)/(baseyear-maxyear)
  b <- -maxyear*a
  c <- basevalue-0.5*a*baseyear^2-b*baseyear
  out_df <- data.frame(year = c(baseyear:endyear))
  # 储存计算结果因变量
  # 计算抛物线段因变量值：达到最大值之前持续上升
  out_df$value <- 0.5*a*(out_df$year^2) + b*out_df$year + c
  # 计算水平直线段因变量值：达到最大值之后保持不变
  out_df[which(out_df$year > maxyear), "value"] <- 
    0.5*a*maxyear^2+b*maxyear + c
  # 画图检验
  plot(out_df$year, out_df$value)
  # 输出结果
  out_df
}

## 平滑插值函数
func_smooth <- function(year, value, name_value = "value") {
  # 步骤1：拆分成几段并计算每段斜率
  # 假设第一个斜率为0，方便后面比较
  ls_slop <- vector("list")
  ls_slop[[1]] <- 0
  for (i in c(2: length(year))) {
    ls_slop[[i]] <- 
      (value[i] - value[i-1])/(year[i] - year[i-1])
  }
  # 步骤2：构建级数
  ls_series_sub <- vector("list")
  ls_series <- vector("list")
  for (i in c(1: (length(ls_slop)-1))) {
    # 如果后一段比当前段斜率小，则级数顺序为正，曲线为凸
    if (ls_slop[[i+1]] < ls_slop[[i]]) {
      ls_series_sub[[i]] <- 
        c((year[i+1] - year[i] -1): 
            (2*year[i+1] - 2*year[i] -3))
    } else {
      # 如果后一段比前一段斜率大，则级数顺序为负
      ls_series_sub[[i]] <- 
        c((2*year[i+1] - 2*year[i] -3): 
            (year[i+1] - year[i] -1))
    }
    ls_series[[i]] <- 1/ls_series_sub[[i]]
  }
  # 步骤3：计算差值
  # 第一个差值为0
  ls_diff <- vector("list")
  ls_diff[[1]] <- 0
  for (i in c(2: length(year))) {
    ls_diff[[i]] <- 
      value[i] - value[i-1]
  }
  # 步骤4：插值
  ls_out <- vector("list")
  for (i in c(1:length(year))) {
    ls_out[[i]] <- value[i]
  }
  for (i in c(1:(length(year)-1))) {
    for (j in c(1: length(ls_series[[i]]))) {
      ls_out[[i]] <- 
        c(ls_out[[i]],
          tail(ls_out[[i]], 1) + 
            ls_diff[[i+1]] * ls_series[[i]][j])
    }
  }
  ls_out
  df_out <- data.frame(year = c(year[1]:tail(year, 1)), 
                       value = Reduce(c, ls_out))
  plot(df_out$year, df_out$value)
  name_value <- name_value
  df_out
}

# 阶梯式插值函数
func_stage <- function(year, value, name_value = "value") {
  total_df <- data.frame(year = c(year[1]: year[length(year)]))
  for (i in c(1: length(year))) {
    total_df$value[total_df$year >= year[i]] <- value[i]
  }
  names(total_df)[2] <- name_value
  total_df
}

## 预测函数：基于增长率
func_rate <- function(baseyear, basevalue, rate_df) {
  names(rate_df) <- c("year", "rate")
  # 先统一年度为基准年到增长率数据框的最后一年
  rate_df <- rate_df[which(rate_df$year > baseyear), ]
  # 更改增长率的单位为1
  rate_df$rate <- rate_df$rate / 100
  proj_df <- rbind(data.frame(year = baseyear, rate = basevalue), 
                   rate_df)
  names(proj_df) <- c("year", "value")
  for (i in c(2: nrow(proj_df))) {
    proj_df$value[i] <- proj_df$value[i - 1] * (1 + proj_df$value[i])
  }
  # plot(proj_df$value)
  proj_df
}

## 补足比例数据
# 输入：时间序列数据
func_saturate <- function(in_df, name_new = "value") {
  in_df_noyear <- in_df[names(in_df) %in% "year" == FALSE]
  in_df[, name_new] <- 100 - rowSums(in_df_noyear)
  if (sum(in_df[, name_new] < 0) > 0) {
    print("warning: less then 0%.")
  } else {
    in_df <- in_df[c("year", name_new)]
    in_df
  }
}

## 根据历史趋势线性外推未来趋势的函数
func_linear <- function(df_history, col_dependent, startyear, endyear) {
  # 保留目标数据
  df_history <- df_history[c("year", col_dependent)]
  df_history[, "year"] <- as.numeric(df_history[, "year"])
  # 拟合：因变量中有NA也可以计算
  fit <- lm(df_history[, col_dependent] ~ df_history[, "year"])
  fit_result <- summary(fit)
  proj_df <- data.frame(year = c(startyear: endyear))
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

## 转化能源类型
func_alter <- function(nrg_in, name_in, name_out) {
  # 输入折标煤系数
  factors <- 
    data.frame(nrg = c("coal", "coalproduct", 
                       "gasoline", "diesel", "kerosene", "residual", "lpg", 
                       "gas", "electricity", "ce"), 
               factor = c(0.7143, 0.6072, 
                          1.4714, 1.4571, 1.4714, 1.4286, 1.7143, 
                          13.3, 1.229, 1))
  alter_factor <- factors$factor[which(factors$nrg == name_in)] / 
    factors$factor[which(factors$nrg == name_out)]
  nrg_out <- nrg_in * alter_factor
  nrg_out
}

## 计算电力等价值
# 需要输入本地发电各类能耗物理量，外调电力对应发电投入量物理量，全社会电力使用量
func_elecequalfac <- function(nrg_input, elecgen_df) {
  # 转化成标准量
  nrg_input_ce <- func_toce(nrg_input)
  # 计算标准量之和
  tot_nrg_df_ce <- data.frame(year = nrg_input_ce$year)
  tot_nrg_df_ce[, "nrg_input"] <- 
    rowSums(nrg_input_ce[, -1])
  # 计算电力等价值
  elecequalfac <- func_cross(tot_nrg_df_ce, elecgen_df, method = "rate")
  elecequalfac
}


## 转化成标准煤
# 输入能源数据框单位：吨/万立方米/百万千焦/万千瓦时
# 输出单位：吨标准煤
func_toce <- function(nrg_df, agg = FALSE) {
  out_df <- data.frame(year = nrg_df[, "year"])
  factors <- 
    data.frame(nrg = c("rawcoal", "coalproduct", 
                       "gasoline", "diesel", "kerosene", "residual", "lpg", 
                       "gas"), 
               factor = c(0.7143, 0.6072, 
                          1.4714, 1.4571, 1.4714, 1.4286, 1.7143, 
                          13.3))
  # 提取共同的列名
  name_nrg <- names(nrg_df)[names(nrg_df) %in% "year" == FALSE]
  name_factor <- factors$nrg
  name_scope <- intersect(name_nrg, name_factor)
  # 重塑两个数据框
  nrg_df <- nrg_df[name_scope]
  factors <- factors[which(factors$nrg %in% name_scope), ]
  # 将能源数据框能耗转化为标准煤
  for (i in name_scope) {
    out_df[, i] <- nrg_df[, i] * factors$factor[which(factors$nrg == i)]
  }
  # 是否加和各类能源标准煤量
  if (agg == TRUE) {
    out_df_mid <- data.frame(year = out_df$year)
    out_df_mid$stdcoal <- rowSums(out_df[names(out_df) != "year"])
    out_df <- out_df_mid
  }
  out_df
}

# 能源替代
# 一半是对能耗强度数据框进行替代，当然对能耗总量进行替代亦可
func_nrgsub <- 
  function(nrgori, namenrgoris, namenrgsubs, yearsubs, propsubs, alterscales) {
    out_df <- vector("list", length(namenrgsubs))
    for (i in c(1: length(namenrgsubs))) {
      # 构造替代比例数据框
      prop_df <- data.frame(year = nrgori$year)
      # 有多少比例原能源被新能源替代
      prop_df[, namenrgsubs[[i]]] <- 
        func_interp_2(year = yearsubs[[i]], value = propsubs[[i]])$value
      prop_df[, namenrgoris[[i]]] <- 1 - prop_df[, namenrgsubs[[i]]]
      # 调整下顺序使之和下面的计算需求一致
      prop_df <- prop_df[c("year", namenrgoris[[i]], namenrgsubs[[i]])]
      # 构造新数据框：替代后的能耗强度
      out_df[[i]] <- nrgori[c("year", namenrgoris[[i]])]
      out_df[[i]][, namenrgsubs[[i]]] <- 
        func_alter(
          nrgori[, namenrgoris[[i]]], 
          name_in = namenrgoris[[i]], 
          name_out = namenrgsubs[[i]]) * alterscales[[i]]
      out_df[[i]] <- func_cross(
        out_df[[i]][c("year", namenrgoris[[i]], namenrgsubs[[i]])], 
        prop_df)
    }
    out_df <- func_ls2df(out_df)
  }


# # 计算除年份外的各行之和
# func_rowsums <- function(df, namevalue = "value") {
#   new_df <- data.frame(year = df$year)
#   df <- df[names(df) %in% year == FALSE]
#   new_df$value <- rowSums(df)
#   names(new_df)[2] <- namevalue
#   new_df
# }


# 计算生长率
func_ratecalc <- function(in_df, name_value) {
  out_df <- data.frame(year = in_df$year)
  out_df$now <- in_df[, name_value]
  # 将待计算数据错位放在另一列
  out_df$before <- 
    c(NA, head(in_df[, name_value], length(in_df[, name_value])-1))
  out_df$rate <- (out_df$now - out_df$before) / out_df$before
  out_df <- out_df[c("year", "rate")]
  out_df
}

# 生成方案组合名称的函数
func_sgen <- function(basescenario, measures) {
  # 生成组合矩阵
  # 该代码来自网络
  BitMatrix <- function(n) {
    # 方案选择矩阵
    set <- 0:(2^n-1)
    rst <- matrix(0, ncol = n, nrow = 2^n)
    for (i in 1:n){
      rst[, i] = ifelse((set-rowSums(rst*rep(c(2^((n-1):0)), each=2^n)))/(2^(n-i))>=1, 1, 0)
    }
    rst
  }
  selectmatrix <- BitMatrix(length(measures))
  # 看措施是否入选
  out_df <- data.frame(sid = c(1: nrow(selectmatrix)))
  out_df$scenario <- basescenario
  for (i in c(1: nrow(selectmatrix))) {
    for (j in c(1: ncol(selectmatrix))) {
      if (selectmatrix[i, j] == 1) {
        # 如果对应的选择矩阵的数值是1，则将对应措施添加到情景中
        out_df[i, "scenario"] <- paste0(out_df[i, "scenario"], "_", measures[j])
      }
    }
  }
  out_df$scenario
}

## 通过能源总量和活动水平计算活动强度
# 数据框对某一列的版本
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
# 列表对数据框的版本
# 输入列表的长度应等于数据框列数减1
# 列表中数据框的排列顺序应和数据框严格对应
# 计算结果应为一个列表
# 问题：生成的能耗强度列表继承活动水平的列名可能未必好，虽然通常活动水平是唯一的
func_nrg_intst_ls <- function(ls_nrgsum, df_actlvl) {
  ls_nrgintst <- vector("list", length(ls_nrgsum))
  for (i in c(1: length(ls_nrgsum))) {
    ls_nrgintst[[i]] <- func_nrg_intst(ls_nrgsum[[i]], df_actlvl, 
                                       names(df_actlvl)[i + 1])
    names(ls_nrgintst)[i] <- names(df_actlvl)[i + 1]
  }
  ls_nrgintst
}

## 基于活动水平和能耗强度计算能耗总量
# 数据框版本
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
# 列表版
func_nrg_sum_ls <- function(ls_nrgintst, df_actlvl) {
  ls_nrgsum <- vector("list", length(ls_nrgintst))
  # 如果原来的列表各元素有名称，则继承名称
  names(ls_nrgsum) <- names(ls_nrgintst)
  for (i in c(1: (length(ls_nrgintst)))) {
    ls_nrgsum[[i]] <- func_nrg_sum(ls_nrgintst[[i]], 
                                   df_actlvl, names(df_actlvl)[i + 1])
  }
  ls_nrgsum
}

## 基于两个数据框计算碳排放的函数
# 本函数根据列名对应能耗总量和排放因子
func_emissum <- function(nrgsum_df, emisfac_df, agg = TRUE) {
  emissum_df <- data.frame(year = nrgsum_df[, "year"])
  # 只计算两个数据框共有的能耗类型
  nameori_nrgsum <- names(nrgsum_df)[names(nrgsum_df) %in% "year" == FALSE]
  nameori_emisfac <- names(emisfac_df)[names(emisfac_df) %in% "year" == FALSE]
  nrg_scope <- intersect(nameori_nrgsum, nameori_emisfac)
  if (length(nrg_scope) == 0) {
    emissum_df <- data.frame(year= nrgsum_df[, "year"])
    for (i in names(emissum_ls)) {
      emissum_df[, i] <- 0
    }
  } else {
    # 如果输入数据框有共同的能源类型则计算排放量
    nrgsum_df <- nrgsum_df[c("year", nrg_scope)]
    emisfac_df <- emisfac_df[c("year", nrg_scope)]
    # 构建一个列表用来暂存结果
    emissum_df_ori <- func_cross(nrgsum_df, emisfac_df, method = "product")
    if (agg == TRUE) {
      emissum_df_ori$temp <- 0
      emissum_df <- data.frame(year = emissum_df_ori$year)
      emissum_df$co2 <- rowSums(emissum_df_ori[, -1])
    } else {
      emissum_df <- emissum_df_ori
    }
  }
  # 有些能源出现在能耗数据中但没有对应的排放因子
  name_nofac <- setdiff(nameori_nrgsum, nameori_emisfac)
  if (length(name_nofac) != 0) {
    cat("warning: no emission faction for", name_nofac, "\n\n")
  }
  emissum_df
}


## 对部门进行加和合并
# 数据框版本
func_secagg <- function(input_df, tbl_lookup) {
  new_df <- data.frame(year = input_df[, "year"])
  ind_subsector <- unique(tbl_lookup$ind_agg)
  for (i in c(1:length(ind_subsector))) {
    name_ind_agg <- ind_subsector[i]
    name_ind_ori <- tbl_lookup$ind_ori[tbl_lookup$ind_agg == name_ind_agg]
    new_df[, name_ind_agg] <- 
      rowSums(input_df[names(input_df) %in% name_ind_ori], na.rm = TRUE)
  }
  new_df
}
# 列表版本
func_secagg_ls <- function(input_ls, tbl_lookup) {
  new_ls <- vector("list")
  for (i in c(1:length(input_ls))) {
    new_ls[[i]] <- func_secagg(input_ls[[i]], tbl_lookup)
  }
  names(new_ls) <- names(input_ls)
  new_ls
}

## 合并各部门能耗列表为数据框的函数
# 输入的列表应满足：
# 列表各元素有独特的名称
# 列表各元素数据框列名排列相同
func_nrgsum_ls_to_df <- function(nrgsum_ls) {
  # 如果输入列表的各元素名称不全，则输出警告
  if (length(unique(names(nrgsum_ls))) != length(nrgsum_ls)) {
    print("warning: not enough list names.") 
  } else {
    # 取输出列名
    names_nrgsum_df <- names(nrgsum_ls[[1]])[names(nrgsum_ls[[1]]) %in% "year" == FALSE]
    # 给各个数据框添加部门名称和活动水平名称
    for (i in names(nrgsum_ls)) {
      
      nrgsum_ls[[i]] <- func_supple_colnames(nrgsum_ls[[i]], names_nrgsum_df)
    }
    # 将列表各元素数据框组成一个大数据框
    nrgsum_df <- Reduce(rbind, nrgsum_ls)
    # 将大数据框各列转换为数字类型
    nrgsum_df[, c("year", names_nrgsum_df)] <- 
      lapply(nrgsum_df[, c("year", names_nrgsum_df)], as.numeric)
    # 根据年份进行加和
    nrgsum_df <- aggregate(nrgsum_df[, names_nrgsum_df], by = list(nrgsum_df$year), 
                           function(x) {sum(x, na.rm = TRUE)})
    names(nrgsum_df)[1] <- "year"
    # 对数据框各列重新排序
    nrgsum_df <- nrgsum_df[c("year", names_nrgsum_df)]
    nrgsum_df
  }
}
# 升级版
func_ls2df <- function(ls) {
  # 生成目标数据框的列名
  names_column <- character(0)
  for (i in c(1: length(ls))) {
    names_column <- c(names_column, names(ls[[i]]))
  }
  names_column <- unique(names_column)
  names_column <- names_column[names_column %in% "year" ==FALSE]
  # 将列表各元素放入统一的列名下
  for (i in c(1: length(ls))) {
    ls[[i]] <- func_supple_colnames(ls[[i]], names_column)
  }
  # 合并列表各元素数据框
  df <- Reduce(rbind, ls)
  # 将各列转化为数字类型
  df[names_column] <- lapply(df[names_column], as.numeric)
  # 按照年份合并
  df <- aggregate(df[, names_column], by = list(df$year), sum)
  names(df)[1] <- "year"
  df
}

## 比较历史数据和预测数据
# 比较数据框的两列版本
# 提供两种作图风格：“base”为基础作图，“ggplot”为高级作图
func_history_project <- 
  function(var_his, name_his, var_proj, name_proj, style = "ggplot", 
           xlab = "year", ylab = name_proj, main = NULL) {
  var_his <- var_his[, c("year", name_his)]
  var_proj <- var_proj[, c("year", name_proj)]
  var_his$attr <- "history"
  var_his$color <- "blue"
  var_his$Data <- "历史数据"
  var_his$cex <- 1.5
  var_proj$attr <- "project"
  var_proj$color <- "red"
  var_proj$Data <- "预测数据"
  var_proj$cex <- 1
  names(var_his)[names(var_his) == name_his] <- name_proj
  total_df <- rbind(var_his[, c("year", name_proj, "attr", "color", "cex", "Data")], 
                    var_proj[, c("year", name_proj, "attr", "color", "cex", "Data")])
  total_df <- total_df[is.na(total_df[, name_proj]) == FALSE, ]
  # 将作图数据强制转化为整数，否则可能会出现坐标轴重叠
  total_df$year <- as.integer(total_df$year)
  total_df[, name_proj] <- as.numeric(total_df[, name_proj])
  # 作图
  if (style == "base") {
    legend_df <- data.frame(attr = c("history", "project"), 
                            color = c("blue", "red"))
    plot(total_df$year, total_df[, name_proj], 
         ylim = c(0, max(total_df[, name_proj])), 
         xlab = xlab, ylab = ylab, main = main, 
         col = total_df$color, 
         cex = total_df$cex)
    #legend("topleft", legend = legend_df$attr, pch = 1, col = legend_df$color)
    plot_data <- recordPlot()
  } else {
    if (style == "ggline") {
      plot_data <- ggplot(total_df) + 
        geom_line(aes(year, total_df[, name_proj], color = Data), 
                  size = 1.5) + 
        labs(x = "", y = ylab) +
        theme_bw() + 
        theme(axis.title = element_text(size = 10))
    } else {
      if (style == "ggpoint") {
        plot_data <- ggplot(total_df) + 
          geom_point(aes(year, total_df[, name_proj], color = Data), 
                     size = 1.5) + 
          labs(x = "", y = ylab) +
          theme_bw() + 
          theme(axis.title = element_text(size = 10))
      } else {
        plot_data <- ggplot(total_df) + 
          geom_smooth(aes(year, total_df[, name_proj], color = Data), 
                      se = FALSE, size = 1.5) + 
          labs(x = "", y = ylab) +
          theme_bw() + 
          theme(axis.title = element_text(size = 10))
      }
    }
  }
  plot_data
}
# 两个数据框的每一列
# 要保证输入两个数据框列数一致
# 问题：对除了“year”外列名少于3列的会报错
func_history_project_df <- function(var_his, var_proj, 
                                    commontitle = NULL, basetitle = NULL, 
                                    style = "base") {
  names_varhis <- names(var_his)[names(var_his) %in% "year" == FALSE]
  names_varproj <- names(var_proj)[names(var_proj) %in% "year" == FALSE]
  if (style == "base") {
    par(mfrow = c(4, 2))
    # 小图是否共享标题
    if (is.null(basetitle) == FALSE) {
      plot.new()
      text(0.5, 0.5, basetitle)
      plot.new()
    }
    # 画小图
    for (i in names_varproj[1:length(names_varproj)]) {
      func_history_project(var_his, i, var_proj, i)
    }
  } else {
    plot_ls <- vector("list")
    for (i in c(1: length(names_varhis))) {
      plot_ls[[i]] <- invisible(func_history_project(var_his, names_varhis[i], 
                                                     var_proj, names_varproj[i]))
    }
    plot_arrange <- invisible(ggarrange(plotlist = plot_ls, 
                                        nrow = 3, ncol = 2, 
                                        common.legend = TRUE, labels = commontitle))
    plot_arrange
  }
}
# 两个列表的版本
func_history_project_ls <- function(ls_his, ls_proj) {
  for (i in c(1: length(ls_his))) {
    print(func_history_project_df(ls_his[[i]], ls_proj[[i]], 
                                  commontitle = names(ls_his[i]), 
                                  basetitle = names(ls_his[i])))
  }
}

# 查看数据框中百分比的变化
func_propplot <- function(in_df) {
  in_df_long <- melt(in_df, id = "year")
  ggplot(in_df_long) + 
    geom_bar(aes(year, value, fill = variable), stat = "identity")
}

# 对比不同情景
func_scenarios <- 
  function(var_his, name_his, var_proj, name_proj, style = "ggplot", 
           xlab = "year", ylab = name_proj, main = NULL) {
    var_his <- var_his[, c("year", name_his)]
    var_proj <- var_proj[, c("year", name_proj)]
    var_his$attr <- "history"
    var_his$color <- "blue"
    var_his$Data <- "惯性情景"
    var_his$cex <- 1.5
    var_proj$attr <- "project"
    var_proj$color <- "red"
    var_proj$Data <- "规划情景"
    var_proj$cex <- 1
    names(var_his)[names(var_his) == name_his] <- name_proj
    total_df <- rbind(var_his[, c("year", name_proj, "attr", "color", "cex", "Data")], 
                      var_proj[, c("year", name_proj, "attr", "color", "cex", "Data")])
    total_df <- total_df[is.na(total_df[, name_proj]) == FALSE, ]
    # 将作图数据强制转化为整数，否则可能会出现坐标轴重叠
    total_df$year <- as.integer(total_df$year)
    total_df[, name_proj] <- as.numeric(total_df[, name_proj])
    # 作图
    if (style == "ggpoint") {
      plot_data <- ggplot(total_df) + 
        geom_point(aes(year, total_df[, name_proj], color = Data), 
                   size = 1.5) + 
        labs(x = "", y = ylab) +
        theme_bw() + 
        theme(axis.title = element_text(size = 10))
    } else {
      if (style == "ggline") {
        plot_data <- ggplot(total_df) + 
          geom_line(aes(year, total_df[, name_proj], color = Data), 
                    size = 1.5) + 
          labs(x = "", y = ylab) +
          theme_bw() + 
          theme(axis.title = element_text(size = 10))
      } else {
        plot_data <- ggplot(total_df) + 
          geom_smooth(aes(year, total_df[, name_proj], color = Data), 
                      se = FALSE, size = 1.5) + 
          labs(x = "", y = ylab) +
          theme_bw() + 
          theme(axis.title = element_text(size = 10))
      }
    }
    plot_data
}
# 升级版
# 输入列表查看同名列差异
func_scompplot <- function(ls_var, namecol) {
  # 挑出内容非空的列表元素
  ls_var <- ls_var[vapply(ls_var, Negate(is.null), NA)]
  # 转化成长数据框
  df_var <- 
    func_mrgcol(ls_var = ls_var, namemrg = namecol, namesnew = names(ls_var))
  df_var_long <- 
    melt(df_var, id = "year")
  # 画图
  ggplot(df_var_long) + geom_line(aes(year, value, color = variable))
}

# 模仿Excel的作图风格
# 输入原始图像，输出增加特定主题的图像
func_excelplot <- function(oriplot, name_legend_position = "right") {
  oriplot + 
    theme(axis.title = element_text(size = 10, family = "STSongti"), 
          legend.text = element_text(size = 9, family = "STSongti"), 
          legend.title = element_text(size = 9, family = "STSongti"), 
          legend.background = element_blank(),
          legend.position = name_legend_position, 
          axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"), 
          panel.grid.minor=element_blank(), 
          panel.background = element_blank(), 
          axis.ticks.length=unit(-0.25, "cm"), 
          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))
}


## 查看数据框中不同数据的变化趋势
# 数据框版本
func_show_trend <- function(var_df, commontitle = NULL) {
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
                              size = 2, alpha = 0.5) + 
      scale_color_brewer(palette = "Set1")
  }
  plot <- ggarrange(plotlist = plot_ls, nrow = 2, ncol = 2, labels = commontitle)
  plot
}
# 列表版本
func_show_trend_ls <- function(var_ls) {
  for (i in c(1: length(var_ls))) {
    print(func_show_trend(var_ls[[i]], commontitle = names(var_ls)[i]))
  }
}

## 比较两组数据的函数
func_datacomp <- function(var_1, name_source_1, var_2, name_source_2, name_comp) {
  var_1[, c("Source")] <- name_source_1
  var_2[, c("Source")] <- name_source_2
  mrg_data <- rbind(var_1[, c("year", name_comp, "Source")], 
                    var_2[, c("year", name_comp, "Source")])
  ggplot(mrg_data) + 
    geom_point(aes(year, mrg_data[, c(name_comp)], color = Source, alpha = 0.5))
}



# # 考虑废弃的函数
# # 计算乘积：可能跟上面的函数重复了 - 废弃？
# func_merge_product <- function(var_1, name_1, var_2, name_2) {
#   total <- merge(var_1, var_2, by = "year")
#   product <- data.frame("year" = total$year,
#                         "Rate" = total[, name_1] * total[, name_2])
#   plot(x = product$year, y = product$Rate)
#   product
# }
# 
# ## 合并2个数据框，并且保留所有观察 - 废弃？
# # func_merge <- function(x, y) {
# #   merge(x, y, by = "year", all = TRUE)
# # }
# 
# ## 旧版插值函数 - 废弃？
# func_interp <- function(mydata) {
#   total_df <- data.frame(year = c(2005:2050))
#   for (j in c(1:(nrow(mydata) - 1))) {
#     start_year <- mydata$year[j]
#     end_year <- mydata$year[j + 1]
#     start_value <- mydata$value[which(mydata$year == start_year)]
#     end_value <- mydata$value[which(mydata$year == end_year)]
#     for (i in seq(from = start_year, to = end_year, by = 1)) {
#       total_df$value[which(total_df$year == i)] <- 
#         start_value + 
#         (end_value - start_value) * (i - start_year) /
#         (end_year - start_year)
#     }
#   }
#   plot(total_df$year, total_df$value)
#   total_df
# }
# 
# ## 生成预测数据的函数-废弃？
# func_proj <- function(input_ls, itemnames, startyear = 2005, endyear = 2050) {
#   out_df <- data.frame(year = c(startyear: endyear))
#   for (i in c(1:length(itemnames))) {
#     out_df[, itemnames[i]] <- func_interp(input_ls[[i]])$value
#   }
#   out_df
# }
# 
# ## 结果计算函数-废弃？
# func_result <- function(var_aclevel, var_int) {
#   total_df <- merge(var_aclevel, var_int, by = "year", all.x = TRUE)
#   total_df$value <- total_df[, 1] * total_df[, 2]
#   plot(total_df$value)
#   total_df
# }
# 
# ## 临时测试函数
# func_test <- function(x, y) {
#   print(x[1])
#   print(x[1]/y > 0.8 & x[1]/y < 1.2)
#   print(min(x)/y)
#   print(max(x)/y)
# }
