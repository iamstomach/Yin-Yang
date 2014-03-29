#############################
# 基本思想

# 用机票订单adv控制不可知突发因素的影响(包括长期趋势)
# 用holiday控制节假日影响
# 用day(星期几)的虚拟变量控制week的周期变化
# 用ios/adr的虚拟变量控制发版的影响(在模型中不显著)

# 采用线性回归模型 + AR 模型，在预测值上反映则是：最终预测值 = 线性模型预测值 + 上一期的残差

# 周一的预测值 =  线性模型预测值 + 上一期(上周日)的预测值偏差
# 周二至周日的预测值 = 周一的预测值 * day虚拟变量的差值

# 基于周一预测值预测周二的例子, d1和d2分别为周一和周二虚拟变量的估计值：
# 周一：log(y1) = x + d1    + e
# 周二：log(y2) = x +    d2 + e
# 假设X相同, 已知周一的值y1, 周二的预测值y2 = y1 * exp(d2-d1)

# 留出最后最后三周的数据作为测试集，其他数据作为训练集构建线性模型，最后三周的误差分别为
# 第一周：5.867458%
# 第二周：5.720324%
# 第三周：3.168405%

# 可以改进的地方：预测变量的选择；AR项的阶数，现在只用了上一期的预测残差，相当于只有AR(1), 
# 可以进行自相关检验，引入高阶的AR项，如前面三期残差的加权平方


## step1: 导入数据----
#setwd("E:\\CP\\predict")
setwd("N:\\qunar\\Predict")

# 导入酒店
raw00 <- read.csv("tt.csv", header = T, stringsAsFactors = F)
head(raw00)
raw00$check_date <- as.Date(raw00$check_date)
raw00$order_date <- as.Date(raw00$order_date)
raw00$vol <- as.numeric(raw00$vol)
tt <- aggregate(vol ~ city_code + order_date, raw00, sum)

# 导入机票
airorder <- read.csv("airorder.csv", header = T, stringsAsFactors = F)

airorder[is.na(airorder$adv), 'adv'] <- 0
airorder$DATE <- as.Date(airorder$DATE)
airorder$dep <- as.Date(airorder$dep)

airorder1 <- subset(airorder, adv<=3, select = c(adv,  dep, arrCity, COUNT...))

library(reshape2)

gg <- head(airorder1, 30)
names(gg)[4] <- "value"
row.names(gg) <- 1:30
gg$dep <- as.character(gg$dep)
gg$adv <- as.character(gg$adv)
gg <- gg[order(gg$dep), ]
airorder2 <- acast(arrCity + dep ~ adv, gg)
head(airorder1)
summary(gg)



# 导入节假日


order_n <- read.csv("order.csv", header = T, stringsAsFactors = F)
# 将字符型日期转变成日期格式
order_n$date <- as.Date(order_n$date)
# 生成星期几的变量day, 取值1-6分别表示周一到周六，周天取值为0
order_n$day <- format(order_n$date, "%w")
head(order_n)
summary(order_n)

# 生成训练集和测试集
# 测试集为最近三周的数据(2014-03-03开始)
order_len <- dim(order_n)[1]
(test_num <- (order_len-22):order_len)
model_data <- order_n[-test_num, ]
test_data  <- order_n[ test_num, ]





## step2: 回归----

# 逐步回归得到模型，注意订单量进行了对数化
m0 <- lm(log(hotel_order) ~ log(adv_1) + log(adv_2) + holiday + adr_app + ios_app + day, 
         data = model_data)
m1 <- step(m0)
summary(m1)

# 评估模型预测效果
# 预测1：pre_vol---模型估计值
# 预测2：pre_vol2---模型估计值+上一期的估计偏差
order_vol <- model_data[, "hotel_order"]
pre_vol <- exp(fitted(m1))
res <- order_vol - pre_vol
names(res) <- model_data[, "date"]
mod_len <- length(res)
# 生成滞后一期的残差，第一期没有滞后偏差，赋值为缺失值NA
lagres <- c( NA, res[-mod_len])
pre_vol2 <- pre_vol + lagres

# 预测值与实际值对比图
fit_value <- cbind(1:mod_len, pre_vol, "模型估计值")
fit_value2 <- cbind(1:mod_len, pre_vol2, "模型估计值+偏差调整")
raw_value <- cbind(1:mod_len, order_vol, "原始值")
raw_fit <- as.data.frame(rbind(raw_value, fit_value, fit_value2), stringsAsFactors = F)
names(raw_fit) <- c("id", "vol", "type")
raw_fit$id <- as.numeric(raw_fit$id)
raw_fit$vol <- as.numeric(raw_fit$vol)
summary(raw_fit)
library(ggplot2)
# 整段时间区间的对比
ggplot(raw_fit, aes(x = id, y= vol, color = type)) + geom_line()
# 第400天以后的对比
ggplot(raw_fit[raw_fit$id>400, ], aes(x = id, y= vol, color = type)) + geom_line()


## step3: 测试集的预测---

######## 测试集第一周的预测  #################################

test_data[1:7, ]


# step3.1：预测星期一即2014-03-03的订单量
# 星期一的预测变量包括 adv1, adv2, holiday, day, 可以根据系数得出预测值，并利用上一期(2014-03-02)的预测偏差进行调整

# 生成公式，选取部分内容黏贴到下面，避免手写
para <- coef(m1)
par_day <- c(para[5:10], 0)
names(par_day) <- c(1:6,0)
paste(para, names(para), sep = "*", collapse="+")

# 输入变量取值
id <- 1
pre_date <- test_data[id, "date"]
adv_1 <- test_data[id, "adv_1"]
adv_2 <- test_data[id, "adv_2"]
holiday <- test_data[id, "holiday"]
day <- par_day[1] # 周一的虚拟变量取值
# 模型预测值
vol1_0 <- exp(3.86788569690671+0.204331344354474*log(adv_1)+0.467433253005349*log(adv_2)+0.243177801879599*holiday+day)
# 上期预测偏差
vol1_adj <- res[as.character(pre_date-1)]
# 最终预测值
vol1 <- vol1_0 + vol1_adj

# step3.2：以星期一的预测值作为基础，根据day取值的差距估计周二到周天的订单量,计算预测值pp_vol和预测误差pp_err
pp_vol <- vol1*exp(par_day-day)
rr_id <- which(test_data[, "date"] >= as.Date(pre_date) & test_data[, "date"] <= as.Date(pre_date) + 6)
rr_vol <- test_data[rr_id, "hotel_order"]  # 原始值
pp_err <- (pp_vol/rr_vol-1)*100
mean(abs(pp_err))
# 第一周的平均预测误差为5.87%


########## 测试集第二周的预测 #############################################

## 现在已经得到了第一周的真实值，以真实值和模型预测偏差为基础进行分析
## 注意区分模型预测值和最终预测值：模型预测值是预测变量和参数的加和，最终预测值是模型预测值和偏差调整值之和
## 偏差调整值为：上一期真实指 - 上一期模型预测值；
test_data[1:14, ]

# 这里定义了一个函数forcasting()计算某天的模型预测
forcasting <- function(date, pre_data = test_data){
  pre_date <- as.Date(date)
  id <- which(pre_data[, "date"] == pre_date)
  adv_1 <- pre_data[id, "adv_1"]
  adv_2 <- pre_data[id, "adv_2"]
  holiday <- pre_data[id, "holiday"]
  day <- par_day[as.character(pre_data[id, "day"])]
  # 模型预测值
  vol1_0 <- exp(3.86788569690671+0.204331344354474*log(adv_1)+0.467433253005349*log(adv_2)+0.243177801879599*holiday+day)
  return(vol1_0)
}

# 模型预测值
pre_date <- "2014-03-10"
vol1_0 <- forcasting(pre_date)
# 上一期预测偏差
last_date <- as.Date(pre_date) - 1
vol1_adj <- test_data[test_data$date==last_date, "hotel_order"] - forcasting(last_date)
# 最终预测值
vol1 <- vol1_0 + vol1_adj

# 第二步：以星期一的预测值作为基础，根据day取值的差距估计周二到周天的订单量,计算预测误差
pp_vol <- vol1*exp(par_day-day)
rr_id <- which(test_data[, "date"] >= as.Date(pre_date) & test_data[, "date"] <= as.Date(pre_date) + 6)
rr_vol <- test_data[rr_id, "hotel_order"]
pp_err <- (pp_vol/rr_vol-1)*100
mean(abs(pp_err))
# 第二周平均预测误差5.72%



########## 测试集第三周的预测 #############################################
# 这里定义了一个函数pp_test()计算整周的最终预测值

pp_test <- function(date_day1, pre_data=test_data){
  # 模型预测值
  pre_data <- test_data
  pre_date <- date_day1
  vol1_0 <- forcasting(pre_date, pre_data = pre_data)
  # 上周同期预测偏差
  last_date <- as.Date(pre_date) - 1
  vol1_adj <- pre_data[pre_data$date==last_date, "hotel_order"] - forcasting(last_date)
  # 最终预测值
  vol1 <- vol1_0 + vol1_adj
  
  # 第二步：以星期一的预测值作为基础，根据day取值的差距估计周二到周天的订单量,计算预测误差
  id_base <- which(pre_data[, "date"] == pre_date)
  day_base <- par_day[as.character(pre_data[id_base, "day"])]
  pp_vol <- vol1*exp(par_day-day)
  rr_id <- which(pre_data[, "date"] >= as.Date(pre_date) & pre_data[, "date"] <= as.Date(pre_date) + 6)
  rr_vol <- pre_data[rr_id, "hotel_order"]
  pp_err <- (pp_vol/rr_vol-1)*100
  list(star_date=pre_date,
       avg_err=mean(abs(pp_err)),
       compare=cbind(pp_vol, rr_vol, pp_err))
}

## 用自定义的预测函数pp_test预测未来一周的订单量，输入值为预测周的周一时间，输出值为开始时间；平均的绝对误差；真实值预测值对比情况
pp_test("2014-03-17")
pp_test("2014-03-10")


## 重新定义test_data的开始时间，可以往前预测更多周的订单量
(test_num <- (order_len-50):order_len)
test_data  <- order_n[ test_num, ]
pp_test("2014-03-03")
pp_test("2014-02-24")
pp_test("2014-02-17")
