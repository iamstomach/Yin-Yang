setwd("N:\\qunar\\Predict")
raw0 <- read.csv("tt.csv", header = T)
head(raw0)
raw0$check_date <- as.Date(raw0$check_date)
raw0$order_date <- as.Date(raw0$order_date)
raw0$vol <- as.numeric(raw0$vol)
summary(raw0)

raw1 <- subset(raw0, order_date  < as.Date("2014-03-01") & order_date >= as.Date("2013-01-01"))
order_sum <- aggregate( vol  ~ order_date, data=raw1, sum)
order_sum1 <- subset(order_sum, order_date  < as.Date("2014-02-15") )
library(ggplot2)
ggplot(order_sum1, aes(x = order_date, y = log10(vol), color = "blue")) + geom_line()

ggplot(order_sum, aes(x = order_date, y = vol, color = "blue")) + geom_line()



order_vol <- ts(order_sum1$vol, start = c(2013, 1, 1))

library(forecast)
fit <- auto.arima(order_vol)
plot(forecast(fit,h=7))

fit_value <- cbind(1:410, fitted(fit), "fit")
raw_value <- cbind(1:410, order_vol, "raw")
raw_fit <- as.data.frame(rbind(raw_value, fit_value), stringsAsFactors = F)
names(raw_fit) <- c("id", "vol", "type")
raw_fit$id <- as.numeric(raw_fit$id)
raw_fit$vol <- as.numeric(raw_fit$vol)
summary(raw_fit)

write.csv(raw_fit, "forecast.csv")
write.csv(order_sum1, "order_sum1.csv")

ggplot(raw_fit, aes(x = id, y= vol, color = type)) + geom_line()

tail(fit_value)
tail(order_sum1)
tail(raw_fit)
forecast(fit,h=7)


#############################

## step1: 导入数据----
setwd("E:\\CP\\predict")
order_n <- read.csv("order.csv", header = T, stringsAsFactors = F)
order_n$date <- as.Date(order_n$date)
# 生成星期几
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

# 逐步回归得到模型
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
ggplot(raw_fit, aes(x = id, y= vol, color = type)) + geom_line()
ggplot(raw_fit[raw_fit$id>400, ], aes(x = id, y= vol, color = type)) + geom_line()


## step3: 测试集的预测---

######## 测试集第一周的预测  #################################

test_data[1:7, ]


# 第一步：预测星期一即2014-03-03
# 星期一的预测变量包括 adv1, adv2, holiday, day, 可以根据系数得出预测值，并利用上一个周一(2014-02-24)的预测偏差进行调整

# 生成公式，选取部分内容黏贴到下面
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
day <- par_day[1]
# 模型预测值
vol1_0 <- exp(3.86788569690671+0.204331344354474*log(adv_1)+0.467433253005349*log(adv_2)+0.243177801879599*holiday+day)
# 上周同期预测偏差
vol1_adj <- res[as.character(pre_date-1)]
# 最终预测值
vol1 <- vol1_0 + vol1_adj

# 第二步：以星期一的预测值作为基础，根据day取值的差距估计周二到周天的订单量,计算预测误差
pp_vol <- vol1*(1+par_day-day)
pp_err <- (pp_vol/test_data[1:7, "hotel_order"]-1)*100
mean(abs(pp_err))
# 第一周的平均预测误差为6.33%


########## 测试集第二周的预测 #############################################

## 现在已经得到了第一周的真实值，以真实值和模型预测偏差为基础进行分析
## 注意区分模型预测值和最终预测值：模型预测值是预测变量和参数的加和，最终预测值是模型预测值和偏差调整值之和
## 偏差调整值为：上周同期真实指 - 上周同期模型预测值；
test_data[1:14, ]

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
# 上周同期预测偏差
last_date <- as.Date(pre_date) - 1
vol1_adj <- test_data[test_data$date==last_date, "hotel_order"] - forcasting(last_date)
# 最终预测值
vol1 <- vol1_0 + vol1_adj

# 第二步：以星期一的预测值作为基础，根据day取值的差距估计周二到周天的订单量,计算预测误差
pp_vol <- vol1*(1+par_day-day)
rr_id <- which(test_data[, "date"] >= as.Date(pre_date) & test_data[, "date"] <= as.Date(pre_date) + 6)
rr_vol <- test_data[rr_id, "hotel_order"]
pp_err <- (pp_vol/rr_vol-1)*100
mean(abs(pp_err))



########## 测试集第三周的预测 #############################################

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
  pp_vol <- vol1*(1+par_day-day)
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









## ARIMA
library(forecast)
head(order_n)
order_len <- dim(order_n)[1]
order_vol <- ts( order_n[ - ((order_len-21):order_len), "hotel_order"], start = c(1, 2), frequency = 7)
print( order_vol, calendar = TRUE)

head(order_vol)
m <- HoltWinters(order_vol)
plot(m)
plot(fitted(m))

forecasts2 <- forecast.HoltWinters(m, h=21)
plot.forecast(forecasts2) 



