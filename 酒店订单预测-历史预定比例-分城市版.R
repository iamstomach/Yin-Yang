# setwd("E:\\CP\\predict")
setwd("N:\\qunar\\Predict\\清明预测")
raw00 <- read.csv("tt.csv", header = T, stringsAsFactors = F)
head(raw00)
raw00$check_date <- as.Date(raw00$check_date)
raw00$order_date <- as.Date(raw00$order_date)
raw00$vol <- as.numeric(raw00$vol)
summary(raw00)
citylist <- unique(raw00$city_code)


date0 <- "2014-03-25"  # 基础日期
date1 <- "2014-03-24"  # 预测日期

for (city_id in 1:length(citylist) ){
  city_id <- 4
  city <- citylist[city_id]
  ## 选择城市数据并汇总提前天数
  raw0 <- subset(raw00, city_code == city)
  raw1 <- raw0[-which(raw0$check_date<raw0$order_date), ]
  raw1$pre_days <- as.numeric(raw1$check_date - raw1$order_date)
  summary(raw1)
  raw1[raw1$pre_days>=10, "pre_days"] <- 10  # 将提前天数大于15天的统一标示成15
  raw11 <- aggregate( vol  ~ pre_days + check_date, data=raw1, sum)
  
  # 选择入住日期在最近15天的数据，计算不同提前天数的比例
  current_date <- as.Date(date0)   # 指定当前日期
  
  raw12 <- subset(raw11, check_date >= current_date-28 & check_date <= current_date) # 如包含假期，需特殊处理
  check_sum <- aggregate( vol  ~ check_date, data=raw12, sum)
  names(check_sum)[2] <- "check_sum"
  raw2 <- merge(raw12, check_sum, by = "check_date")
  raw2$pre_pct <- raw2$vol/raw2$check_sum
  pre_pct <- aggregate( pre_pct  ~ pre_days , data=raw2, mean)  # 计算不同提前天数预定量的预定比例
  
  
  ## 预测函数, 用三天的平均预测值作为最终预测值
  
  pd <- numeric()
  for (i in 1:5){
    ct_date <- current_date+1-i
    predict_date <- as.Date(date1)
    gap <- as.numeric(predict_date - ct_date)
    raw13 <- aggregate( vol  ~ pre_days + order_date, data=raw1, sum)
    current_order <- subset(raw13, order_date == ct_date, select = c(order_date, vol, pre_days))
    predict_pct <- pre_pct
    predict_pct$pre_days <- as.numeric(predict_pct$pre_days)
    predict_pct <- predict_pct[order(-predict_pct$pre_days), ]
    predict_pct[, "rr"] <- c(predict_pct[-(1:gap), "pre_pct"], rep(0, gap) ) / predict_pct[, "pre_pct"]
    predict_order <- merge(current_order, predict_pct, by = "pre_days", order = F)
    predict_order$predict_vol <- predict_order$vol * predict_order$rr
    pd <- c(pd, sum(predict_order$predict_vol))
  }
  
  actual_vol <- subset(raw13, order_date == predict_date, select = c(order_date, vol, pre_days))
  pp0 <- sum(actual_vol$vol)
  pp1 <- format(mean(pd), digits = 1)
  pperr <- format((mean(pd)/pp0-1)*100, digits = 2)
  
  print(paste(city, date1, "的订单量为", pp1, "预测误差", pperr, "%"))
}

















# 导入假期标示
# holiday <- read.csv("holiday.csv", header = T, stringsAsFactors = F)
# holiday$check_date <- as.Date(holiday$check_date)
# 
# raw3 <- merge(raw2, holiday, by = "check_date", all.x = T, sort = F)
# 
# raw3[is.na(raw3$holiday), "holiday"] <- 0
# summary(raw3)
raw3 <- raw2

write.csv(raw2, "raw3.csv")



# 从图形可以看出不同提前天数的预定比例基本一致
library(ggplot2)
ggplot(pre_pct, aes(x = pre_days, y = vol, color = factor(holiday))) + geom_line()
ggplot(pre_pct, aes(x = pre_days, y = pre_pct)) + geom_line()


## 预测函数

current_date <- as.Date("2014-03-15")
predict_date <- as.Date("2014-03-17")

current_order <- subset(raw1, order_date == current_date, select = c(check_date, order_date, vol, pre_days))
current_order$pre_days <- as.numeric(current_order$pre_days)
current_order <- current_order[order(current_order$pre_days), ]
predict_pct <- pre_pct
predict_pct$pre_days <- as.numeric(predict_pct$pre_days)
predict_pct <- predict_pct[order(-predict_pct$pre_days), ]
gap <- as.numeric(predict_date - current_date)


predict_pct[, "rr"] <- c(predict_pct[-(1:gap), "pre_pct"], rep(0, gap) ) / predict_pct[, "pre_pct"]

predict_order <- merge(current_order, predict_pct, by = "pre_days", order = F)

predict_order$predict_vol <- predict_order$vol * predict_order$rr

sum(predict_order$predict_vol)

actual_vol <- subset(raw1, order_date == predict_date, select = c(check_date, order_date, vol, pre_days))
sum(actual_vol$vol)





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
