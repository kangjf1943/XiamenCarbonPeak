library(nsga2R)
# 测试数据：各年份各参数设置
x <- read.xlsx("Yuxianmodel.xlsx", "S2")
# 重命名各列
names(x) <- c("year", "pop", "gdp", "comprop", "coalprop", 
              "nrgintst", "imelecprop", "emisfacimelec")

# 看看值都怎么设置的
par(mfrow = c(3, 3))
for (i in names(x)) {
  plot(x[, 1], x[, i], ylab = i)
}

# 理想曲线值
idea <- read.xlsx("Yuxianmodel.xlsx", "S3")
names(idea) <- c("year", "co2")

# 计算排放量的函数
yuxian_model <- function(x) {
  Emis <- data.frame(
    year = c(2019: 2035),
    co2 = exp(1)^(0.294 + 0.006 * log(x$pop) + 
                    0.936 * log(x$gdp) + 
                    0.202 * log(x$comprop) + 
                    0.042 * log(x$coalprop) + 
                    1.005 * log(x$nrgintst) - 
                    0.061 * log(x$imelecprop) + 
                    0.363 * log(x$emisfacimelec)))
  Emis
}


# 原本的设置下的排放量
emisori <- yuxian_model(x)
par(mfrow = c(1, 1))
plot(emisori$year, emisori$co2)
lines(idea$year, idea$co2)
func_peakyear(emisori, "co2")

# 优化目标函数
# 优化变量：2035年的值
yuxian_opt <- function(z) {
  pop <- x$pop
  gdp <- x$gdp
  comprop <- seq(from = x$comprop[1], to = z[1], length.out = 17)
  coalprop <- seq(from = x$coalprop[1], to = z[2], length.out = 17)
  # 允许2026年煤炭占比突变
  coalprop[8: 17] <- coalprop[8: 17]*0.8
  # 能耗强度前半段下降较快，后半段较慢
  nrgintst <- seq(from = x$nrgintst[1], to = z[3], length.out = 17)
  imelecprop <- seq(from = x$imelecprop[1], to = z[4], length.out = 17)
  emisfacimelec <- x$emisfacimelec
  Emis <- data.frame(year = c(2019: 2035))
  Emis$co2 <- exp(1)^(0.294 + 0.006 * log(pop) + 
                    0.936 * log(gdp) + 
                    0.202 * log(comprop) + 
                    0.042 * log(coalprop) + 
                    1.005 * log(nrgintst) - 
                    0.061 * log(imelecprop) + 
                    0.363 * log(emisfacimelec))
  # 备选约束条件
  # 和理想曲线之间差的绝对值之和最小
  diff_all <- sum(abs(Emis$co2 - idea$co2))
  # 和理想曲线之间变化率之差的绝对值之和最小
  diff_rate <- sum(abs(func_ratecalc(Emis, "co2")$rate[7:17] - 
                        func_ratecalc(idea, "co2")$rate[7:17]))
  # 达峰年份尽量接近2025年
  diff_peakyear <- abs(func_peakyear(Emis, "co2") - 2025)
  # 入选约束条件
  return(c(diff_all, diff_peakyear))
}

garesult <- 
  nsga2R(fn = yuxian_opt, 
         varNo = 4, objDim = 2, generations = 20, popSize = 50, 
         lowerBounds = c(62, 2,  0.13, 60), 
         upperBounds = c(68, 15, 0.20, 84))
  
# 查看优化结果
garesult$objectives
plot(garesult$objectives[, 1], garesult$objectives[, 2])

# 取优化自变量值
zopt <- as.numeric(garesult$parameters[49, ])
yuxian_opt(zopt)

# 生成最佳参数集合
xopt <- data.frame(
  year = c(2019: 2035), 
  pop = x$pop, 
  gdp = x$gdp, 
  comprop = seq(from = x$comprop[1], to = zopt[1], length.out = 17), 
  coalprop = seq(from = x$coalprop[1], to = zopt[2], length.out = 17),
  nrgintst = seq(from = x$nrgintst[1], to = zopt[3], length.out = 17), 
  imelecprop = seq(from = x$imelecprop[1], to = zopt[4], length.out = 17), 
  emisfacimelec = x$emisfacimelec
)
xopt$coalprop[8: 17] <- xopt$coalprop[8: 17]*0.8
# 对应的排放量
emisopt <- yuxian_model(xopt)
plot(emisopt$year, emisopt$co2, ylim = c(2250, 2600))
lines(idea$year, idea$co2)
func_peakyear(emisopt, "co2")

# 看看调整后各种值的设置
par(mfrow = c(3, 3))
for (i in names(xopt)) {
  plot(xopt[, 1], xopt[, i], ylab = i)
}

# 导出最佳参数集
write.xlsx(xopt, "参数集最优解-基于十四五人口规划.xlsx")

# 手动调整
xman <- xopt
xman$nrgintst <- func_interp_2(
  year = c(2019, 2029, 2032, 2035), 
  value = c(0.2531015, 0.1761717, 0.156, 0.142))$value

# 对应的排放量
emisopt <- yuxian_model(xman)
plot(emisopt$year, emisopt$co2, ylim = c(2250, 2600))
lines(idea$year, idea$co2)
func_peakyear(emisopt, "co2")

# 看看调整后各种值的设置
par(mfrow = c(3, 3))
for (i in names(xman)) {
  plot(xman[, 1], xman[, i], ylab = i)
}

# 导出最佳参数集
write.xlsx(xman, "参数集最优解-基于十四五人口规划-手动优化.xlsx")
