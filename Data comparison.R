## 其他部门数据对照
## 家庭用能
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

## 交通部门数据比较
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

# 对比1千克标煤不同能源的排放量
standardcoal_fac <- data.frame(nrg = c("coal", "coalproduct", 
                                       "gasoline", "diesel", "kerosene", "residual", "lpg", 
                                       "gas", "electricity"), 
                               factor = c(0.7143, 0.6072, 
                                          1.4714, 1.4571, 1.4714, 1.4286, 1.7143, 
                                          13.3, 1.229))
standardcoal_fac$emisfac <- as.numeric(emisfac_df[1, -1])
standardcoal_fac$ce_emis <- 1 / standardcoal_fac$factor * standardcoal_fac$emisfac
ggplot(standardcoal_fac) + geom_bar(aes(nrg, ce_emis), stat = "identity")
# 结果发现油品比煤炭等排放高？可见燃料替代不应该直接用燃气热值算，因为燃烧效率也有差异

# 对比本地发电和外调电力排放因子
# 未计算本地发电中电力投入的排放
emisfac_elecgen <- func_emissum(tf_nrgintst, emisfac_df)
emisfac_elecgen <- emisfac_elecgen[c("year", "co2")]
res_emifac_df
func_cross(emisfac_elecgen, res_emifac_df, method = "rate")
# 外调电力的排放因子更低





