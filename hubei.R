setwd("E:\\SupStat\\Project\\CTCC\\2.data")

library(ggplot2)
# part1: 数据清洗
testdata <- read.csv("E:\\SupStat\\Project\\CTCC\\2.data\\80万用户清单\\201404月80万清单.csv", 
                     header = T)
summary(testdata2[, c(1:11,36:37,113:126)])
names(testdata)
library(ggplot2)
qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否参加终补促销 , position="dodge")

write.csv(table(testdata[, c("本地网标识")]),"本地网.csv")
write.csv(table(testdata[, c("本地网名称")]),"本地网ming.csv")
table(testdata[, c("本地网名称")])

names(testdata) <- names(testdata2)
set.seed(70)
testdata2 <- testdata[sample(1:dim(testdata)[1], 80000), ]
save(testdata2, file = "testdata2.RData")
save(testdata, file = "testdata.RData")


# 重排数据
# 名义指标和二分指标
## 增加新变量：武汉标识
summary(testdata2[, 127])
testdata2[testdata2[, "本地网标识"]==1001, "是否武汉"] <- TRUE
testdata2[testdata2[, "本地网标识"]!=1001, "是否武汉"] <- FALSE
class_var <- c(1:2, 123:127, 3:7, 9:14, 22:30, 94:95, 101:104, 114)
testdata <- testdata2[, c(class_var, which(! 1:126 %in% class_var))]
summary(testdata)

# 检查指标缺失情况
names(testdata[, 1:34])
summary(testdata[, 1:34])


for (g in c(10:16,18:20,22:24,26:34)){
  print(table(testdata[,g])/nrow(testdata))
}

write.csv(names(testdata[, 1:34]), "缺失检查名.csv")

# 去掉异常值

apply(testdata[, 35:36], 2, function(x){
  c(median=median(x), iqr=quantile(x, 0.75)-quantile(x, 0.25))
})
n <- 17; fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
summary(testdata[, 36:107])


# 去掉取值没有具体含义的变量(取值为"未知"的比例太高)
testdata <- testdata[, ! names(testdata) %in% c("用户促销实例到期时间","第二级套餐名称", "定价计划标识", "大类偏好","大类偏好.1", "内容偏好")]

# 设定基本筛选标准
qplot(稳定用户标识, 捆绑销售终端价格, data = testdata, geom = "jitter", alpha = I(1/5))

qplot(捆绑销售终端价格, data = subset(testdata, 捆绑销售终端价格==0), 
      geom = "histogram", fill = 是否捆绑终端)
testdata <- subset(testdata, 捆绑销售终端价格<8000) ## 大部分为0，捆绑的价格中60%以上的价格为0, 该变量可以去除

qplot(log(预存款准备率), data = subset(testdata, 预存款准备率<200 & 预存款准备率 >0), 
      geom = "histogram", fill = 稳定用户标识)
testdata[testdata[, "预存款准备率"]<0, "预存款准备率"] <- min(testdata[testdata[, "预存款准备率"]>0, "预存款准备率"])

qplot(log(用户ARPU), data = subset(testdata, 用户ARPU<2000 & 用户ARPU >-2279), 
      geom = "histogram", fill = 稳定用户标识)
testdata[testdata[, "用户ARPU"]<0, "用户ARPU"] <- min(testdata[testdata[, "用户ARPU"]>0, "用户ARPU"])

qplot(用户ARPU趋势, data = subset(testdata, 用户ARPU趋势<5 & 用户ARPU趋势 >=0), 
      geom = "histogram", fill = 稳定用户标识)
testdata <- subset(testdata, 用户ARPU趋势<5 & 用户ARPU趋势 >=0)
testdata[testdata[, "用户ARPU趋势"]==0, "用户ARPU趋势"] <- 0.0001

qplot(增值业务费用, data = subset(testdata, 增值业务费用<300 & 增值业务费用 >=0), 
      geom = "histogram", fill = 稳定用户标识)
qplot(稳定用户标识, 增值业务费用, data = testdata, geom = "jitter", alpha = I(1/5))
testdata <- subset(testdata, 增值业务费用<300 & 增值业务费用 >=0)
testdata[testdata[, "增值业务费用"]==0, "增值业务费用"] <- 0.0001

testdata <- subset(testdata, 增值业务费用占比<=1 & 增值业务费用 >=0)
testdata[testdata[, "增值业务费用占比"]==0, "增值业务费用占比"] <- 0.0001

qplot(稳定用户标识, 套餐保底消费金额, data = testdata, geom = "boxplot", alpha = I(1/5))
qplot(套餐保底消费金额, data = subset(testdata, 套餐保底消费金额<750 & 套餐保底消费金额 >=0), 
      geom = "histogram", fill = 稳定用户标识)
testdata <- subset(testdata, 套餐保底消费金额<750) 

qplot(稳定用户标识, 当月赠送款余额, data = testdata, geom = "jitter", alpha = I(1/5))
qplot(稳定用户标识, 终端换机次数, data = testdata, geom = "jitter", alpha = I(1/5))
testdata <- subset(testdata, 终端换机次数<=50) 

qplot(稳定用户标识, 流量包限额, data = testdata, geom = "boxplot", alpha = I(1/5))
q
qplot(当月赠送款余额, data = subset(testdata, 当月赠送款余额<500 & 当月赠送款余额 >=-350), 
      geom = "histogram", fill = 稳定用户标识)


# 把0设定为0.0001, 进行对数化处理
names(testdata)
summary(testdata)
testdata_log <- testdata
for (i in 35:107){
  testdata_log[testdata_log[, i] < 0, i] <- 0.0001
  testdata_log[testdata_log[, i] == 0, i] <- 0.001
}

testdata_log[, 35:107] <- log(testdata_log[, 35:107])
summary(testdata_log)
qplot(总通话次数 , data = testdata_log, geom = "density", color = 稳定用户标识)

# 相关性分析
library(corrplot)
names(testdata)
M <- cor(testdata[, c(6, 45:57)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 3)

M <- cor(testdata[, c(6, 45:57)])

# 去掉占比指标
aa <- names(testdata)
aa1 <- !grepl("*.占比",aa) 
testdata <- testdata[, aa1]
summary(testdata)

names(testdata)
M <- cor(testdata[, c(6, 45:59,63:69)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 5)

# 计算相关系数
## 对数相关系数
aa <- testdata_log[, c(7, 13:14, 18, 22:24, 26:27, 35:107)]
summary(aa)
cor_var <- cor(testdata_log[, c(7, 13:14, 18, 22:24, 26:27, 35:107)], testdata_log[, 6])
cor_var <- cor_var[ order(-abs(cor_var[, 1])), ]
write.csv(cor_var, "cor_var.csv")
## 原始相关系数
cor_var1 <- cor(testdata[, c(7, 13:14, 18, 22:24, 26:27, 35:107)], testdata[, 6])
cor_var1 <- cor_var1[ order(-abs(cor_var1[, 1])), ]
write.csv(cor_var1, "cor_var1.csv")

paste(names(head(cor_var, 20)), collapse = " + ")

cor_matrix <- cor(testdata_log[, names(head(cor_var, 20))])


## 随机森林挑选重要变量
library(randomForest)
rm(x)
rm(y)
set.seed(71)
names(testdata)
set.seed(17)
iid <- sample(1:80000,10000)
x <- as.matrix(testdata[iid, c(7, 13:14, 18, 22:24, 26:27, 35:107)])
y <- factor(testdata[iid,6])
iris.rf <- randomForest(x, y ,
                        importance=TRUE,
                        proximity=TRUE, ntree = 50)
## Look at variable importance:
write.csv(round(importance(iris.rf, type=2), 2), "rf_impt.csv")
varImpPlot(iris.rf, main="随机森林重要度-原始值")

iid <- sample(1:80000,10000)
x <- as.matrix(testdata_log[iid, c(7, 13:14, 18, 22:24, 26:27, 35:107)])
y <- factor(testdata_log[iid,6])
iris.rf <- randomForest(x, y ,
                        importance=TRUE,
                        proximity=TRUE, ntree = 50)
## Look at variable importance:
write.csv(round(importance(iris.rf), 2), "rf_impt_log.csv")
varImpPlot(iris.rf, main="随机森林重要度-取对数")

testdata <- subset(testdata, 终端换机次数<=50 & 套餐保底消费金额<=750 & 流量包限额<=5000 & 捆绑销售终端价格<8000)


# 选取重要指标

select_name <- readLines(textConnection(
"稳定交往圈个数
本地被叫次数
本地被叫时长
被叫号码个数
网内号码个数
网间通话次数
终端换机次数
总通话次数
总通话时长
当月赠送款余额
近六个月平均缴费金额
距离协议到期时长
近六月累计短厅查询次数
剩余积分
用户ARPU
套餐保底消费金额
流量包限额
捆绑销售终端价格
预存款准备率
约定协议时长
本地主叫时长
语音交往圈个数趋势
增值业务费用
零通话天数"))


testdata_mod <- subset(testdata, select = c("稳定用户标识",select_name))
summary(testdata_mod)

library(ggplot2)
testdata_mod <- subset(testdata_mod, 终端换机次数<=50 & 套餐保底消费金额<=750 & 流量包限额<=5000 & 捆绑销售终端价格<8000)
testdata_mod$稳定用户标识 <- factor(testdata_mod$稳定用户标识)
qplot(稳定用户标识,  流量包限额 , data = testdata, geom = "jitter", alpha = I(1/3))
qplot(log(剩余积分), data = testdata, geom = "histogram", fill = 稳定用户标识)

testdata_mod_log <- testdata_mod
names(testdata_mod_log)
for (i in 2:25){
  testdata_mod_log[testdata_mod_log[, i] < 0, i] <- 0.0001
  testdata_mod_log[testdata_mod_log[, i] == 0, i] <- 0.001
  testdata_mod_log[, i] <- log(testdata_mod_log[, i])
}
summary(testdata_mod_log)

# 区分测试集和数据集
mod <- sample(2, nrow(testdata_mod), replace = TRUE, prob=c(0.8, 0.2))
       

paste(select_name, collapse = " + ")
fit <- glm(稳定用户标识 ~ ., family=binomial(link=logit), data=testdata_mod_log[mod==1,])
summary(fit) # display results
summary(fit) # display results
pp <- predict.glm(fit, type="response") # predicted values
summary(pp)
pp1 <- predict.glm(fit, newdata =testdata_mod_log[mod==2, ], type="response") # predicted values
(a <- table(pp>0.5, testdata_mod_log[mod==1, "稳定用户标识"])/length(pp))
(b <- table(pp1>0.5, testdata_mod_log[mod==2, "稳定用户标识"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")
# ROC曲线
library(ROCR)
pred <- prediction( pp, testdata_mod_log$稳定用户标识)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Logit回归算法ROC曲线")

## LIFT曲线
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)



# 决策树
library(rpart)
library(rpart.plot)

fit_tree <- rpart(稳定用户标识 ~ ., data=testdata_mod[mod==1,])
rpart.plot(fit_tree, cex=0.8)

pp_tree <- predict(fit_tree, type = "prob")
summary(pp_tree)

pp <- predict(fit_tree, type = "class") # predicted values
pp1 <- predict(fit_tree, newdata =testdata_mod[mod==2, ], type="class") # predicted values
(a <- table(pp, testdata_mod[mod==1, "稳定用户标识"])/length(pp))
(b <- table(pp1, testdata_mod[mod==2, "稳定用户标识"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")


head(pp_tree[,2])
head(testdata_mod$稳定用户标识)
# ROC曲线
pred <- prediction( pp_tree[,2], testdata_mod$稳定用户标识)
perf <- performance(pred,"tpr","fpr")
plot(perf, main = "决策树算法ROC曲线")

## LIFT曲线
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)



# 随机森林
rm(predict_all)
library(randomForest)
set.seed(80)
iris.rf <- randomForest(稳定用户标识 ~ ., 
                        data = testdata_mod[mod == 1, ],  ntree=100)
pp_forest <- predict(iris.rf, type = "prob")
summary(pp_forest)
pp <- predict(iris.rf, type = "class") # predicted values
pp1 <- predict(iris.rf, newdata =testdata_mod[mod==2, ], type="class") # predicted values
(a <- table(pp, testdata_mod[mod==1, "稳定用户标识"])/length(pp))
(b <- table(pp1, testdata_mod[mod==2, "稳定用户标识"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")


head(pp_forest[,2])
head(testdata_mod$稳定用户标识)
# ROC曲线
pred <- prediction( pp_forest[,2], y)
perf <- performance(pred,"tpr","fpr")
plot(perf, main = "随机森林算法ROC曲线")

## LIFT曲线
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)


# 利用随机森林结果进行打分及分层
predict_all <- predict(iris.rf, testdata_mod, type = "prob")

testdata_mod$稳定性打分 <- predict_all[, 2]

high_q <- quantile(testdata_mod[ testdata_mod$稳定用户标识=="TRUE", "稳定性打分"], c(0.2, 0.8))
low_q <- quantile(testdata_mod[ testdata_mod$稳定用户标识=="FALSE", "稳定性打分"], c(0.2, 0.8))
summary(testdata_mod[ testdata_mod$稳定用户标识=="FALSE", "稳定性打分"])

testdata_mod[testdata_mod$稳定用户标识=="TRUE" & testdata_mod$稳定性打分 > high_q[2], "稳定性分层"] <- "A稳定用户-高等级"
testdata_mod[testdata_mod$稳定用户标识=="TRUE" & testdata_mod$稳定性打分 >= high_q[1] & 
               testdata_mod$稳定性打分 <= high_q[2] , "稳定性分层"] <- "B稳定用户-中等级"
testdata_mod[testdata_mod$稳定用户标识=="TRUE" & testdata_mod$稳定性打分 <= high_q[1] , "稳定性分层"] <- "C稳定用户-低等级"
testdata_mod[testdata_mod$稳定用户标识=="FALSE" & testdata_mod$稳定性打分 > low_q[2] , "稳定性分层"] <- "D非稳定用户-高等级"
testdata_mod[testdata_mod$稳定用户标识=="FALSE" & testdata_mod$稳定性打分 > low_q[1] & 
               testdata_mod$稳定性打分 < low_q[2] , "稳定性分层"] <- "E非稳定用户-中等级"
testdata_mod[testdata_mod$稳定用户标识=="FALSE" & testdata_mod$稳定性打分 <= low_q[1], "稳定性分层"] <- "F非稳定用户-低等级"
table(testdata_mod$稳定用户标识, testdata_mod$稳定性分层)



names(testdata_mod)
summary(testdata_mod)
qplot(稳定性打分, data = testdata_mod, geom = "density", color = 稳定用户标识, alpha = I(1/3))

qplot(log(用户ARPU), data = testdata_mod, geom = "density", color = 稳定性分层, alpha = I(1/3), main = "各层用户对数ARPU分布情况")
qplot(log(总通话次数), data = testdata_mod, geom = "density", color = 稳定性分层)
qplot(log(总通话时长), data = testdata_mod, geom = "density", color = 稳定性分层)
qplot(log(套餐保底消费金额), data = testdata_mod, geom = "density", fill = 稳定性分层, alpha = I(1/3))
qplot(约定协议时长, data = testdata_mod, geom = "density", fill = 稳定性分层, alpha = I(1/3))
qplot(约定协议时长, data = testdata_mod, geom = "density", fill = 稳定用户标识, alpha = I(1/3))

# 分层特征刻画

names(test_aa)
test_class <- testdata[, c(1:5, 7:34)]
head(test_class)
test_class$id <- rownames(test_class)
test_wd <- testdata_mod[, c("稳定性打分","稳定性分层")]
test_wd$id <- rownames(test_wd)
test_aa <- merge(test_class, test_wd)

paste(select_name, collapse = ",")
ind_stat<-function(x)(c(IQR=paste(quantile(x, 0.25), quantile(x, 0.75),sep="~")))
summary1 <- aggregate(cbind(稳定交往圈个数,距离协议到期时长,剩余积分,用户ARPU,套餐保底消费金额,流量包限额,
                            捆绑销售终端价格,约定协议时长)~ 稳定用户标识 + 稳定性分层,data=testdata_mod,ind_stat)
summary2 <- aggregate(cbind(是否捆绑终端,是否vpn,是否融合,是否快消品,是否参加终补促销,是否参加话补促销)~ 稳定性分层,data=test_aa,mean)
write.csv(summary1, "summary1.csv")
write.csv(summary2, "summary2.csv")

summary(test_aa)
qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否参加终补促销 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否参加终补促销 , position="fill")

qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否校园用户 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否校园用户 , position="fill")

qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否捆绑终端 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =是否捆绑终端 , position="fill")

qplot(稳定性分层, data = test_aa, geom = "bar", fill =付费模式 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =付费模式 , position="fill")


qplot(稳定性分层, data = test_aa, geom = "bar", fill =套餐第一级名称 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =套餐第一级名称 , position="fill")

qplot(稳定性分层, data = test_aa, geom = "bar", fill =大类偏好 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =大类偏好 , position="fill")

qplot(稳定性分层, data = test_aa, geom = "bar", fill =缴费方式偏好 , position="dodge")
qplot(稳定性分层, data = test_aa, geom = "bar", fill =缴费方式偏好 , position="fill")
test


















testdata <- testdata2
class(testdata2[,17])
names(testdata)
# 去掉缺失较多的变量
apply(testdata[1,13:20], 2 , function(x) is.numeric(x))

aa <- as.data.frame(summary(testdata2))
write.table(aa, "aa.csv")

# 去掉取值没有具体含义的变量(取值为"未知"的比例太高)
summary(testdata)
testdata <- testdata[, ! names(testdata) %in% c("用户促销实例到期时间","第二级套餐名称", "定价计划标识", "大类偏好","大类偏好.1", "内容偏好")]

# 统计取值为0的情况
apply(testdata[, 30:34], 2, function(x) sum(x==0))
test4 <- sum(testdata$总通话次数==0)

# 异常值
library(ggplot2)
qplot(稳定用户标识, log(捆绑销售终端价格), data = testdata, geom = "jitter", alpha = I(1/5))

qplot(捆绑销售终端价格, data = testdata, geom = "histogram", fill = 是否捆绑终端)

# 初步设定
testdata <- subset(testdata, 捆绑销售终端价格<10000) ## 大部分为0，捆绑的价格中60%以上的价格为0, 该变量可以去除

test2 <- subset(testdata, 预存款准备率<10 & 预存款准备率 >-10) 

test3 <- subset(testdata2, 当月赠送款余额<1000) 

qplot( log(当月赠送款余额) , data = testdata2, geom = "histogram", fill = 稳定用户标识, )
exp(10)

library(ggplot2)
summary(testdata2)

qplot(用户ARPU , data = testdata, geom = "histogram")
testdata <- testdata2[(testdata2$用户ARPU <500) & (testdata2$用户ARPU >-50), ]
testdata <- testdata[(testdata$当月赠送款余额.元.<5000), ]
summary(testdata)
summary(testdata2)
rm(testdata2)
summary(testdata$用户ARPU..元.)
qplot(稳定用户标识, 总通话次数, data = testdata, geom = "jitter", alpha = I(1/5))
par(mfrow = c(1, 2))
qplot(总通话次数, data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(总通话次数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(本地主叫次数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(本地被叫次数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(零通话天数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(漫游主叫次数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(漫游被叫次数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(点对点短信发送条数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(异地上网流量.1X.EV..M. ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(主叫号码个数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(被叫号码个数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(X1X.EV流量.M.), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(网内号码个数+网外号码个数), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(主叫号码个数 ), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(流量包限额.M.), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(总通话时长.分钟.), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(距离协议到期时长), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(异地上网流量.1X.EV..M.), data = testdata, geom = "density", color = 稳定用户标识)


qplot(log(近三个月平均缴费金额..元.), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(缴费次数), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(X10000号客服查询次数), data = testdata, geom = "histogram", fill = 稳定用户标识)
qplot(log(X10000号客服查询次数), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(X10000号客服咨询次数), data = testdata, geom = "density", color = 稳定用户标识)

qplot(log(网厅登录次数), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(掌厅登录次数), data = testdata, geom = "density", color = 稳定用户标识)
qplot(log(短厅查询次数), data = testdata, geom = "density", color = 稳定用户标识)




qplot(是否参加终补促销, data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")
qplot(是否参加终补促销, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")

qplot(是否捆绑终端, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(套餐第一级名称, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")

qplot(操作系统, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(操作系统, data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")

qplot(大类偏好, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(大类偏好, data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")

qplot(终端档位变化.提升.稳定.降低., data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(factor(拨打竞争对手客服偏好), data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(factor(拨打竞争对手客服偏好), data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")
qplot(缴费方式偏好, data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(缴费方式偏好, data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")


qplot(DPI客户端应用软件.社交类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(DPI客户端应用软件.社交类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")

qplot(DPI客户端应用软件.银行类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(DPI客户端应用软件.银行类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")

qplot(DPI客户端应用软件.电商购物类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(DPI客户端应用软件.电商购物类网站或软件使用次数., data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")


qplot(factor(稳定交往圈个数), data = testdata, geom = "bar", fill = 稳定用户标识, position="fill")
qplot(factor(稳定交往圈个数), data = testdata, geom = "bar", fill = 稳定用户标识, position="dodge")




X1X.EV流量.M.

qplot(testdata$网内通话次数)

log(0)
library(car)
scatterplotMatrix(testdata2[, c("用户ARPU..元.", "总通话次数", "总通话时长.分钟.", "点对点短信发送条数", "X1X.EV流量.M.")])
names(testdata)
testdata3 <- testdata[, c("总通话次数","稳定用户标识")]
testdata3$log总通话次数 <- testdata3$总通话次数
testdata3[testdata3$log总通话次数==0, "log总通话次数"] <- 0.000001

cor(log(testdata3$log总通话次数), testdata3$稳定用户标识)
cor(log(testdata3$总通话次数), testdata3$稳定用户标识, use = "pairwise.complete.obs")

testdata3 <- testdata[, c(34:46, 126)]
head(testdata3)
summary(testdata3)

testdata4 <- testdata3[(testdata3[, 1] == 0), ]
summary(testdata4)

testdata3[testdata3[, 1] == 0, 1] <- 0.00001
testdata3[testdata3[, 1] == 0, 1] <- min((testdata3[testdata3[, 1] > 0, 1]), na.rm = T)
qplot(log(总通话次数 ), data = testdata3, geom = "density", color = 稳定用户标识)

head(testdata4[testdata4[, 2] != 0, ],20)

names(testdata)
test <- subset( testdata, 总通话次数==0 & 本地主叫次数==0 & 本地被叫次数==0 & 长途主叫次数==0 & 长途被叫次数==0)
test2 <- subset( testdata, 总通话次数==0 & 本地主叫次数==0 & 本地被叫次数==0 & 长途主叫次数==0 & 长途被叫次数==0 &
                  点对点短信发送条数==0)
test3 <- subset( testdata, 总通话次数==0 & 本地主叫次数==0 & 本地被叫次数==0 & 长途主叫次数==0 & 长途被叫次数==0 &
                  点对点短信发送条数==0 & X1X.EV流量.M.==0)
test <- subset( testdata, 总通话次数==0 & 本地主叫次数==0 & 本地被叫次数==0 & 长途主叫次数==0 & 长途被叫次数==0)



log(0.0001)
















# 相关性分析
library(corrplot)
names(testdata)
M <- cor(testdata[, c(6, 45:57)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 3)

# 主成分分析--将多个相关性较强的指标融合为少数指标
(pc.cr <- princomp(USArrests))  # inappropriate
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)

summary(pc.cr <- princomp(USArrests, cor = TRUE))
loadings(pc.cr)  ## note that blank entries are small but not zero
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)

# 筛选重要变量
# 随机森林筛选重要变量



# part2: 建模

# logit模型
gg <- mtcars
gg$vs <- factor(gg$vs)
summary(gg)
fit <- glm(vs ~ mpg + cyl, family=binomial(link=logit), data=gg)
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
pp <- predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals


# 决策树



# 随机森林



# 神经网络


# ROC曲线
library(ROCR)
data(ROCR.simple)
pred <- prediction( pp, testdata_lo$稳定性标识)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## LIFT曲线
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)
