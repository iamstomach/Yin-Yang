# setwd("E:\\CP\\predict")
setwd("N:\\qunar\\Predict")
raw0 <- read.csv("tt.csv", header = T, stringsAsFactors = F)
head(raw0)
raw0$check_date <- as.Date(raw0$check_date)
raw0$order_date <- as.Date(raw0$order_date)
raw0$vol <- as.numeric(raw0$vol)

raw1 <- raw0[-which(raw0$check_date<raw0$order_date), ]
raw1$pre_days <- as.numeric(raw1$check_date - raw1$order_date)
summary(raw1)

# 仅选取入住日期在2014年数据，计算不同提前天数的比例
raw2 <- subset(raw1, check_date  < as.Date("2014-02-27") & check_date >= as.Date("2014-01-01"))
check_sum <- aggregate( vol  ~ check_date, data=raw2, sum)
names(check_sum)[2] <- "check_sum"

raw3 <- merge(raw2, check_sum, by = "check_date")
raw3$pre_pct <- raw3$vol/raw3$check_sum


# 从图形可以看出不同提前天数的预定比例基本一致
library(ggplot2)
ggplot(raw3, aes(x = pre_days, y = vol, color = factor(check_date))) + geom_line()
ggplot(raw3, aes(x = pre_days, y = pre_pct, color = factor(check_date))) + geom_line()


## 计算不同提前天数预定量的预定比例
pre_pct <- aggregate( pre_pct  ~ pre_days, data=raw3, mean)

## 选取预定日期在2014年的数据进行预测测试
raw4 <- subset(raw1, order_date >= as.Date("2014-01-01"))
raw4 <- raw4[order(raw4$order_date), ]



## 预测未来7天的订单量

#############################################################################
# 列名-预定日期
# 行名-入住日期
# R(1,4)-入住日期为04，提前1天预定的量
入住/预定01-01    01-02    01-03    01-04    01-05    01-06    01-07    .....
[01-01] "R(0,1)"
[01-02] "R(1,2)-> R(0,2)"
[01-03] "R(2,3)-> R(1,3)-> R(0,3)"
[01-04] "R(3,4)-> R(2,4)-> R(1,4)-> R(0,4)"
[01-05] "R(4,5)-> R(3,5)-> R(2,5)-> R(1,5)-> R(0,5)"
[01-06] "R(5,6)-> R(4,6)-> R(3,6)-> R(2,6)-> R(1,6)-> R(0,6)"
[01-07] "R(6,7)-> R(5,7)-> R(4,7)-> R(3,7)-> R(2,7)-> R(1,7)-> R(0,7)"
[01-08] "R(7,8)-> R(6,8)-> R(5,8)-> R(4,8)-> R(3,8)-> R(2,8)-> R(1,8)-> R(0,8)"
[01-09] "R(8,9)-> R(7,9)-> R(6,9)-> R(5,9)-> R(4,9)-> R(3,9)-> R(2,9)-> R(1,9)-> R(0,9)"
[01-10] ...........
[01-11] ...........
...........
...........
...........


# p0表示当天预定的比例，p1表示提前1天预定的比例
入住/预定01   02   03   04   05   06   07  ......
[01-01] "p0"
[01-02] "p1   p0"
[01-03] "p2   p1   p0"
[01-04] "p3   p2   p1   p0"
[01-05] "p4   p3   p2   p1   p0"
[01-06] "p5   p4   p3   p2   p1   p0"
[01-07] "p6   p5   p4   p3   p2   p1   p0"
[01-08] "p7   p6   p5   p4   p3   p2   p1   p0"
[01-09] "p8   p7   p6   p5   p4   p3   p2   p1   p0"
[01-10] ...........
[01-11] ...........
...........
...........
...........


01-01的预定量=R(0,1)+R(1,2)+R(2,3)+R(3,4)+R(4,5)+R(5,6)+R(6,7)+R(7,8)+R(8,9)+R(0,10)+....
01-02的预定量=       R(0,2)+R(1,3)+R(2,4)+R(3,5)+R(4,6)+R(5,7)+R(6,8)+R(7,9)+R(8,10)+R(0,11)+....
01-03的预定量=              R(0,3)+R(1,4)+R(2,5)+R(3,6)+R(4,7)+R(5,8)+R(6,9)+R(7,10)+R(8,11)+R(0,12)+....
...........
...........

# 已知01-01的预定量及其预定结构R，提前天数的比例p(i)
# 预测未来的预定量
01-01的预定量    =R(0,1)  +R(1,2)        +R(2,3)        +R(3,4)        +R(4,5)+R(5,6)+R(6,7)+R(7,8)+R(8,9)+R(0,10)+....
01-02的预定量预测=R(0,1)*0+R(1,2)*(p0/p1)+R(2,3)*(p1/p2)+R(3,4)*(p2/p3)+......
01-02的预定量预测=R(0,1)*0+R(1,2)*0      +R(2,3)*(p0/p2)+R(3,4)*(p1/p3)+......

#############################################################################



## 以当天为base, 分别预测后7天的订单量
for (i in 1:9){
     print(paste("R(", paste((i:1)-1, i, sep=","), ")", sep = "", collapse="-> "))
   }
for (i in 1:9){
  print(paste("R(", paste( (1:9) -1, i:(9+i), sep=","), ")", sep = "", collapse="+"))
}
for (i in 1:9){
  print(paste("p", (i:1)-1, sep = "", collapse="   "))
}
  
  
pre_pct2 <- pre_pct[order(-pre_pct$pre_days), ]

for (i in 1:7){
  pre_pct2[, 2+i] <- c(pre_pct2[-(1:i), "pre_pct"], rep(0, i) ) / pre_pct2[, "pre_pct"]
}
names(pre_pct2)[3:9] <- paste("rr", 1:7, sep="")
raw5 <- merge(raw4, pre_pct2, by = "pre_days")
raw5 <- raw5[order(raw5$order_date), ]

raw5$predict_vol1 <- raw5$vol * raw5$rr1
raw5$predict_vol2 <- raw5$vol * raw5$rr2
raw5$predict_vol3 <- raw5$vol * raw5$rr3
raw5$predict_vol4 <- raw5$vol * raw5$rr4
raw5$predict_vol5 <- raw5$vol * raw5$rr5
raw5$predict_vol6 <- raw5$vol * raw5$rr6
raw5$predict_vol7 <- raw5$vol * raw5$rr7


compare_vol <- aggregate( cbind(vol, predict_vol1,predict_vol2,predict_vol3,predict_vol4,predict_vol5,predict_vol6,predict_vol7)  ~ order_date, data=raw5, sum)
write.csv(compare_vol, "compare_vol.csv")
















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



