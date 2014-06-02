setwd("E:\\SupStat\\Project\\CTCC\\2.data")

library(ggplot2)
# part1: ������ϴ
testdata <- read.csv("E:\\SupStat\\Project\\CTCC\\2.data\\80���û��嵥\\201404��80���嵥.csv", 
                     header = T)
summary(testdata2[, c(1:11,36:37,113:126)])
names(testdata)
library(ggplot2)
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ�μ��ղ����� , position="dodge")

write.csv(table(testdata[, c("��������ʶ")]),"������.csv")
write.csv(table(testdata[, c("����������")]),"������ming.csv")
table(testdata[, c("����������")])

names(testdata) <- names(testdata2)
set.seed(70)
testdata2 <- testdata[sample(1:dim(testdata)[1], 80000), ]
save(testdata2, file = "testdata2.RData")
save(testdata, file = "testdata.RData")


# ��������
# ����ָ��Ͷ���ָ��
## �����±������人��ʶ
summary(testdata2[, 127])
testdata2[testdata2[, "��������ʶ"]==1001, "�Ƿ��人"] <- TRUE
testdata2[testdata2[, "��������ʶ"]!=1001, "�Ƿ��人"] <- FALSE
class_var <- c(1:2, 123:127, 3:7, 9:14, 22:30, 94:95, 101:104, 114)
testdata <- testdata2[, c(class_var, which(! 1:126 %in% class_var))]
summary(testdata)

# ���ָ��ȱʧ���
names(testdata[, 1:34])
summary(testdata[, 1:34])


for (g in c(10:16,18:20,22:24,26:34)){
  print(table(testdata[,g])/nrow(testdata))
}

write.csv(names(testdata[, 1:34]), "ȱʧ�����.csv")

# ȥ���쳣ֵ

apply(testdata[, 35:36], 2, function(x){
  c(median=median(x), iqr=quantile(x, 0.75)-quantile(x, 0.25))
})
n <- 17; fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
summary(testdata[, 36:107])


# ȥ��ȡֵû�о��庬��ı���(ȡֵΪ"δ֪"�ı���̫��)
testdata <- testdata[, ! names(testdata) %in% c("�û�����ʵ������ʱ��","�ڶ����ײ�����", "���ۼƻ���ʶ", "����ƫ��","����ƫ��.1", "����ƫ��")]

# �趨����ɸѡ��׼
qplot(�ȶ��û���ʶ, ���������ն˼۸�, data = testdata, geom = "jitter", alpha = I(1/5))

qplot(���������ն˼۸�, data = subset(testdata, ���������ն˼۸�==0), 
      geom = "histogram", fill = �Ƿ������ն�)
testdata <- subset(testdata, ���������ն˼۸�<8000) ## �󲿷�Ϊ0������ļ۸���60%���ϵļ۸�Ϊ0, �ñ�������ȥ��

qplot(log(Ԥ���׼����), data = subset(testdata, Ԥ���׼����<200 & Ԥ���׼���� >0), 
      geom = "histogram", fill = �ȶ��û���ʶ)
testdata[testdata[, "Ԥ���׼����"]<0, "Ԥ���׼����"] <- min(testdata[testdata[, "Ԥ���׼����"]>0, "Ԥ���׼����"])

qplot(log(�û�ARPU), data = subset(testdata, �û�ARPU<2000 & �û�ARPU >-2279), 
      geom = "histogram", fill = �ȶ��û���ʶ)
testdata[testdata[, "�û�ARPU"]<0, "�û�ARPU"] <- min(testdata[testdata[, "�û�ARPU"]>0, "�û�ARPU"])

qplot(�û�ARPU����, data = subset(testdata, �û�ARPU����<5 & �û�ARPU���� >=0), 
      geom = "histogram", fill = �ȶ��û���ʶ)
testdata <- subset(testdata, �û�ARPU����<5 & �û�ARPU���� >=0)
testdata[testdata[, "�û�ARPU����"]==0, "�û�ARPU����"] <- 0.0001

qplot(��ֵҵ�����, data = subset(testdata, ��ֵҵ�����<300 & ��ֵҵ����� >=0), 
      geom = "histogram", fill = �ȶ��û���ʶ)
qplot(�ȶ��û���ʶ, ��ֵҵ�����, data = testdata, geom = "jitter", alpha = I(1/5))
testdata <- subset(testdata, ��ֵҵ�����<300 & ��ֵҵ����� >=0)
testdata[testdata[, "��ֵҵ�����"]==0, "��ֵҵ�����"] <- 0.0001

testdata <- subset(testdata, ��ֵҵ�����ռ��<=1 & ��ֵҵ����� >=0)
testdata[testdata[, "��ֵҵ�����ռ��"]==0, "��ֵҵ�����ռ��"] <- 0.0001

qplot(�ȶ��û���ʶ, �ײͱ������ѽ��, data = testdata, geom = "boxplot", alpha = I(1/5))
qplot(�ײͱ������ѽ��, data = subset(testdata, �ײͱ������ѽ��<750 & �ײͱ������ѽ�� >=0), 
      geom = "histogram", fill = �ȶ��û���ʶ)
testdata <- subset(testdata, �ײͱ������ѽ��<750) 

qplot(�ȶ��û���ʶ, �������Ϳ����, data = testdata, geom = "jitter", alpha = I(1/5))
qplot(�ȶ��û���ʶ, �ն˻�������, data = testdata, geom = "jitter", alpha = I(1/5))
testdata <- subset(testdata, �ն˻�������<=50) 

qplot(�ȶ��û���ʶ, �������޶�, data = testdata, geom = "boxplot", alpha = I(1/5))
q
qplot(�������Ϳ����, data = subset(testdata, �������Ϳ����<500 & �������Ϳ���� >=-350), 
      geom = "histogram", fill = �ȶ��û���ʶ)


# ��0�趨Ϊ0.0001, ���ж���������
names(testdata)
summary(testdata)
testdata_log <- testdata
for (i in 35:107){
  testdata_log[testdata_log[, i] < 0, i] <- 0.0001
  testdata_log[testdata_log[, i] == 0, i] <- 0.001
}

testdata_log[, 35:107] <- log(testdata_log[, 35:107])
summary(testdata_log)
qplot(��ͨ������ , data = testdata_log, geom = "density", color = �ȶ��û���ʶ)

# ����Է���
library(corrplot)
names(testdata)
M <- cor(testdata[, c(6, 45:57)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 3)

M <- cor(testdata[, c(6, 45:57)])

# ȥ��ռ��ָ��
aa <- names(testdata)
aa1 <- !grepl("*.ռ��",aa) 
testdata <- testdata[, aa1]
summary(testdata)

names(testdata)
M <- cor(testdata[, c(6, 45:59,63:69)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 5)

# �������ϵ��
## �������ϵ��
aa <- testdata_log[, c(7, 13:14, 18, 22:24, 26:27, 35:107)]
summary(aa)
cor_var <- cor(testdata_log[, c(7, 13:14, 18, 22:24, 26:27, 35:107)], testdata_log[, 6])
cor_var <- cor_var[ order(-abs(cor_var[, 1])), ]
write.csv(cor_var, "cor_var.csv")
## ԭʼ���ϵ��
cor_var1 <- cor(testdata[, c(7, 13:14, 18, 22:24, 26:27, 35:107)], testdata[, 6])
cor_var1 <- cor_var1[ order(-abs(cor_var1[, 1])), ]
write.csv(cor_var1, "cor_var1.csv")

paste(names(head(cor_var, 20)), collapse = " + ")

cor_matrix <- cor(testdata_log[, names(head(cor_var, 20))])


## ���ɭ����ѡ��Ҫ����
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
varImpPlot(iris.rf, main="���ɭ����Ҫ��-ԭʼֵ")

iid <- sample(1:80000,10000)
x <- as.matrix(testdata_log[iid, c(7, 13:14, 18, 22:24, 26:27, 35:107)])
y <- factor(testdata_log[iid,6])
iris.rf <- randomForest(x, y ,
                        importance=TRUE,
                        proximity=TRUE, ntree = 50)
## Look at variable importance:
write.csv(round(importance(iris.rf), 2), "rf_impt_log.csv")
varImpPlot(iris.rf, main="���ɭ����Ҫ��-ȡ����")

testdata <- subset(testdata, �ն˻�������<=50 & �ײͱ������ѽ��<=750 & �������޶�<=5000 & ���������ն˼۸�<8000)


# ѡȡ��Ҫָ��

select_name <- readLines(textConnection(
"�ȶ�����Ȧ����
���ر��д���
���ر���ʱ��
���к������
���ں������
����ͨ������
�ն˻�������
��ͨ������
��ͨ��ʱ��
�������Ϳ����
��������ƽ���ɷѽ��
����Э�鵽��ʱ��
�������ۼƶ�����ѯ����
ʣ�����
�û�ARPU
�ײͱ������ѽ��
�������޶�
���������ն˼۸�
Ԥ���׼����
Լ��Э��ʱ��
��������ʱ��
��������Ȧ��������
��ֵҵ�����
��ͨ������"))


testdata_mod <- subset(testdata, select = c("�ȶ��û���ʶ",select_name))
summary(testdata_mod)

library(ggplot2)
testdata_mod <- subset(testdata_mod, �ն˻�������<=50 & �ײͱ������ѽ��<=750 & �������޶�<=5000 & ���������ն˼۸�<8000)
testdata_mod$�ȶ��û���ʶ <- factor(testdata_mod$�ȶ��û���ʶ)
qplot(�ȶ��û���ʶ,  �������޶� , data = testdata, geom = "jitter", alpha = I(1/3))
qplot(log(ʣ�����), data = testdata, geom = "histogram", fill = �ȶ��û���ʶ)

testdata_mod_log <- testdata_mod
names(testdata_mod_log)
for (i in 2:25){
  testdata_mod_log[testdata_mod_log[, i] < 0, i] <- 0.0001
  testdata_mod_log[testdata_mod_log[, i] == 0, i] <- 0.001
  testdata_mod_log[, i] <- log(testdata_mod_log[, i])
}
summary(testdata_mod_log)

# ���ֲ��Լ������ݼ�
mod <- sample(2, nrow(testdata_mod), replace = TRUE, prob=c(0.8, 0.2))
       

paste(select_name, collapse = " + ")
fit <- glm(�ȶ��û���ʶ ~ ., family=binomial(link=logit), data=testdata_mod_log[mod==1,])
summary(fit) # display results
summary(fit) # display results
pp <- predict.glm(fit, type="response") # predicted values
summary(pp)
pp1 <- predict.glm(fit, newdata =testdata_mod_log[mod==2, ], type="response") # predicted values
(a <- table(pp>0.5, testdata_mod_log[mod==1, "�ȶ��û���ʶ"])/length(pp))
(b <- table(pp1>0.5, testdata_mod_log[mod==2, "�ȶ��û���ʶ"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")
# ROC����
library(ROCR)
pred <- prediction( pp, testdata_mod_log$�ȶ��û���ʶ)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Logit�ع��㷨ROC����")

## LIFT����
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)



# ������
library(rpart)
library(rpart.plot)

fit_tree <- rpart(�ȶ��û���ʶ ~ ., data=testdata_mod[mod==1,])
rpart.plot(fit_tree, cex=0.8)

pp_tree <- predict(fit_tree, type = "prob")
summary(pp_tree)

pp <- predict(fit_tree, type = "class") # predicted values
pp1 <- predict(fit_tree, newdata =testdata_mod[mod==2, ], type="class") # predicted values
(a <- table(pp, testdata_mod[mod==1, "�ȶ��û���ʶ"])/length(pp))
(b <- table(pp1, testdata_mod[mod==2, "�ȶ��û���ʶ"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")


head(pp_tree[,2])
head(testdata_mod$�ȶ��û���ʶ)
# ROC����
pred <- prediction( pp_tree[,2], testdata_mod$�ȶ��û���ʶ)
perf <- performance(pred,"tpr","fpr")
plot(perf, main = "�������㷨ROC����")

## LIFT����
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)



# ���ɭ��
rm(predict_all)
library(randomForest)
set.seed(80)
iris.rf <- randomForest(�ȶ��û���ʶ ~ ., 
                        data = testdata_mod[mod == 1, ],  ntree=100)
pp_forest <- predict(iris.rf, type = "prob")
summary(pp_forest)
pp <- predict(iris.rf, type = "class") # predicted values
pp1 <- predict(iris.rf, newdata =testdata_mod[mod==2, ], type="class") # predicted values
(a <- table(pp, testdata_mod[mod==1, "�ȶ��û���ʶ"])/length(pp))
(b <- table(pp1, testdata_mod[mod==2, "�ȶ��û���ʶ"])/length(pp1))
write.csv(a, "a.csv")
write.csv(b, "b.csv")


head(pp_forest[,2])
head(testdata_mod$�ȶ��û���ʶ)
# ROC����
pred <- prediction( pp_forest[,2], y)
perf <- performance(pred,"tpr","fpr")
plot(perf, main = "���ɭ���㷨ROC����")

## LIFT����
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)


# �������ɭ�ֽ�����д�ּ��ֲ�
predict_all <- predict(iris.rf, testdata_mod, type = "prob")

testdata_mod$�ȶ��Դ�� <- predict_all[, 2]

high_q <- quantile(testdata_mod[ testdata_mod$�ȶ��û���ʶ=="TRUE", "�ȶ��Դ��"], c(0.2, 0.8))
low_q <- quantile(testdata_mod[ testdata_mod$�ȶ��û���ʶ=="FALSE", "�ȶ��Դ��"], c(0.2, 0.8))
summary(testdata_mod[ testdata_mod$�ȶ��û���ʶ=="FALSE", "�ȶ��Դ��"])

testdata_mod[testdata_mod$�ȶ��û���ʶ=="TRUE" & testdata_mod$�ȶ��Դ�� > high_q[2], "�ȶ��Էֲ�"] <- "A�ȶ��û�-�ߵȼ�"
testdata_mod[testdata_mod$�ȶ��û���ʶ=="TRUE" & testdata_mod$�ȶ��Դ�� >= high_q[1] & 
               testdata_mod$�ȶ��Դ�� <= high_q[2] , "�ȶ��Էֲ�"] <- "B�ȶ��û�-�еȼ�"
testdata_mod[testdata_mod$�ȶ��û���ʶ=="TRUE" & testdata_mod$�ȶ��Դ�� <= high_q[1] , "�ȶ��Էֲ�"] <- "C�ȶ��û�-�͵ȼ�"
testdata_mod[testdata_mod$�ȶ��û���ʶ=="FALSE" & testdata_mod$�ȶ��Դ�� > low_q[2] , "�ȶ��Էֲ�"] <- "D���ȶ��û�-�ߵȼ�"
testdata_mod[testdata_mod$�ȶ��û���ʶ=="FALSE" & testdata_mod$�ȶ��Դ�� > low_q[1] & 
               testdata_mod$�ȶ��Դ�� < low_q[2] , "�ȶ��Էֲ�"] <- "E���ȶ��û�-�еȼ�"
testdata_mod[testdata_mod$�ȶ��û���ʶ=="FALSE" & testdata_mod$�ȶ��Դ�� <= low_q[1], "�ȶ��Էֲ�"] <- "F���ȶ��û�-�͵ȼ�"
table(testdata_mod$�ȶ��û���ʶ, testdata_mod$�ȶ��Էֲ�)



names(testdata_mod)
summary(testdata_mod)
qplot(�ȶ��Դ��, data = testdata_mod, geom = "density", color = �ȶ��û���ʶ, alpha = I(1/3))

qplot(log(�û�ARPU), data = testdata_mod, geom = "density", color = �ȶ��Էֲ�, alpha = I(1/3), main = "�����û�����ARPU�ֲ����")
qplot(log(��ͨ������), data = testdata_mod, geom = "density", color = �ȶ��Էֲ�)
qplot(log(��ͨ��ʱ��), data = testdata_mod, geom = "density", color = �ȶ��Էֲ�)
qplot(log(�ײͱ������ѽ��), data = testdata_mod, geom = "density", fill = �ȶ��Էֲ�, alpha = I(1/3))
qplot(Լ��Э��ʱ��, data = testdata_mod, geom = "density", fill = �ȶ��Էֲ�, alpha = I(1/3))
qplot(Լ��Э��ʱ��, data = testdata_mod, geom = "density", fill = �ȶ��û���ʶ, alpha = I(1/3))

# �ֲ������̻�

names(test_aa)
test_class <- testdata[, c(1:5, 7:34)]
head(test_class)
test_class$id <- rownames(test_class)
test_wd <- testdata_mod[, c("�ȶ��Դ��","�ȶ��Էֲ�")]
test_wd$id <- rownames(test_wd)
test_aa <- merge(test_class, test_wd)

paste(select_name, collapse = ",")
ind_stat<-function(x)(c(IQR=paste(quantile(x, 0.25), quantile(x, 0.75),sep="~")))
summary1 <- aggregate(cbind(�ȶ�����Ȧ����,����Э�鵽��ʱ��,ʣ�����,�û�ARPU,�ײͱ������ѽ��,�������޶�,
                            ���������ն˼۸�,Լ��Э��ʱ��)~ �ȶ��û���ʶ + �ȶ��Էֲ�,data=testdata_mod,ind_stat)
summary2 <- aggregate(cbind(�Ƿ������ն�,�Ƿ�vpn,�Ƿ��ں�,�Ƿ����Ʒ,�Ƿ�μ��ղ�����,�Ƿ�μӻ�������)~ �ȶ��Էֲ�,data=test_aa,mean)
write.csv(summary1, "summary1.csv")
write.csv(summary2, "summary2.csv")

summary(test_aa)
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ�μ��ղ����� , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ�μ��ղ����� , position="fill")

qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ�У԰�û� , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ�У԰�û� , position="fill")

qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ������ն� , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�Ƿ������ն� , position="fill")

qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =����ģʽ , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =����ģʽ , position="fill")


qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�ײ͵�һ������ , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�ײ͵�һ������ , position="fill")

qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =����ƫ�� , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =����ƫ�� , position="fill")

qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�ɷѷ�ʽƫ�� , position="dodge")
qplot(�ȶ��Էֲ�, data = test_aa, geom = "bar", fill =�ɷѷ�ʽƫ�� , position="fill")
test


















testdata <- testdata2
class(testdata2[,17])
names(testdata)
# ȥ��ȱʧ�϶�ı���
apply(testdata[1,13:20], 2 , function(x) is.numeric(x))

aa <- as.data.frame(summary(testdata2))
write.table(aa, "aa.csv")

# ȥ��ȡֵû�о��庬��ı���(ȡֵΪ"δ֪"�ı���̫��)
summary(testdata)
testdata <- testdata[, ! names(testdata) %in% c("�û�����ʵ������ʱ��","�ڶ����ײ�����", "���ۼƻ���ʶ", "����ƫ��","����ƫ��.1", "����ƫ��")]

# ͳ��ȡֵΪ0�����
apply(testdata[, 30:34], 2, function(x) sum(x==0))
test4 <- sum(testdata$��ͨ������==0)

# �쳣ֵ
library(ggplot2)
qplot(�ȶ��û���ʶ, log(���������ն˼۸�), data = testdata, geom = "jitter", alpha = I(1/5))

qplot(���������ն˼۸�, data = testdata, geom = "histogram", fill = �Ƿ������ն�)

# �����趨
testdata <- subset(testdata, ���������ն˼۸�<10000) ## �󲿷�Ϊ0������ļ۸���60%���ϵļ۸�Ϊ0, �ñ�������ȥ��

test2 <- subset(testdata, Ԥ���׼����<10 & Ԥ���׼���� >-10) 

test3 <- subset(testdata2, �������Ϳ����<1000) 

qplot( log(�������Ϳ����) , data = testdata2, geom = "histogram", fill = �ȶ��û���ʶ, )
exp(10)

library(ggplot2)
summary(testdata2)

qplot(�û�ARPU , data = testdata, geom = "histogram")
testdata <- testdata2[(testdata2$�û�ARPU <500) & (testdata2$�û�ARPU >-50), ]
testdata <- testdata[(testdata$�������Ϳ����.Ԫ.<5000), ]
summary(testdata)
summary(testdata2)
rm(testdata2)
summary(testdata$�û�ARPU..Ԫ.)
qplot(�ȶ��û���ʶ, ��ͨ������, data = testdata, geom = "jitter", alpha = I(1/5))
par(mfrow = c(1, 2))
qplot(��ͨ������, data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(��ͨ������ ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(�������д��� ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(���ر��д��� ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(��ͨ������ ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(�������д��� ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(���α��д��� ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(��Ե���ŷ������� ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(�����������.1X.EV..M. ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(���к������ ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(���к������ ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(X1X.EV����.M.), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(���ں������+����������), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(���к������ ), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(�������޶�.M.), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(��ͨ��ʱ��.����.), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(����Э�鵽��ʱ��), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(�����������.1X.EV..M.), data = testdata, geom = "density", color = �ȶ��û���ʶ)


qplot(log(��������ƽ���ɷѽ��..Ԫ.), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(�ɷѴ���), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(X10000�ſͷ���ѯ����), data = testdata, geom = "histogram", fill = �ȶ��û���ʶ)
qplot(log(X10000�ſͷ���ѯ����), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(X10000�ſͷ���ѯ����), data = testdata, geom = "density", color = �ȶ��û���ʶ)

qplot(log(������¼����), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(������¼����), data = testdata, geom = "density", color = �ȶ��û���ʶ)
qplot(log(������ѯ����), data = testdata, geom = "density", color = �ȶ��û���ʶ)




qplot(�Ƿ�μ��ղ�����, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")
qplot(�Ƿ�μ��ղ�����, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")

qplot(�Ƿ������ն�, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(�ײ͵�һ������, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")

qplot(����ϵͳ, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(����ϵͳ, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")

qplot(����ƫ��, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(����ƫ��, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")

qplot(�ն˵�λ�仯.����.�ȶ�.����., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(factor(���������ֿͷ�ƫ��), data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(factor(���������ֿͷ�ƫ��), data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")
qplot(�ɷѷ�ʽƫ��, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(�ɷѷ�ʽƫ��, data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")


qplot(DPI�ͻ���Ӧ������.�罻����վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(DPI�ͻ���Ӧ������.�罻����վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")

qplot(DPI�ͻ���Ӧ������.��������վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(DPI�ͻ���Ӧ������.��������վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")

qplot(DPI�ͻ���Ӧ������.���̹�������վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(DPI�ͻ���Ӧ������.���̹�������վ������ʹ�ô���., data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")


qplot(factor(�ȶ�����Ȧ����), data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="fill")
qplot(factor(�ȶ�����Ȧ����), data = testdata, geom = "bar", fill = �ȶ��û���ʶ, position="dodge")




X1X.EV����.M.

qplot(testdata$����ͨ������)

log(0)
library(car)
scatterplotMatrix(testdata2[, c("�û�ARPU..Ԫ.", "��ͨ������", "��ͨ��ʱ��.����.", "��Ե���ŷ�������", "X1X.EV����.M.")])
names(testdata)
testdata3 <- testdata[, c("��ͨ������","�ȶ��û���ʶ")]
testdata3$log��ͨ������ <- testdata3$��ͨ������
testdata3[testdata3$log��ͨ������==0, "log��ͨ������"] <- 0.000001

cor(log(testdata3$log��ͨ������), testdata3$�ȶ��û���ʶ)
cor(log(testdata3$��ͨ������), testdata3$�ȶ��û���ʶ, use = "pairwise.complete.obs")

testdata3 <- testdata[, c(34:46, 126)]
head(testdata3)
summary(testdata3)

testdata4 <- testdata3[(testdata3[, 1] == 0), ]
summary(testdata4)

testdata3[testdata3[, 1] == 0, 1] <- 0.00001
testdata3[testdata3[, 1] == 0, 1] <- min((testdata3[testdata3[, 1] > 0, 1]), na.rm = T)
qplot(log(��ͨ������ ), data = testdata3, geom = "density", color = �ȶ��û���ʶ)

head(testdata4[testdata4[, 2] != 0, ],20)

names(testdata)
test <- subset( testdata, ��ͨ������==0 & �������д���==0 & ���ر��д���==0 & ��;���д���==0 & ��;���д���==0)
test2 <- subset( testdata, ��ͨ������==0 & �������д���==0 & ���ر��д���==0 & ��;���д���==0 & ��;���д���==0 &
                  ��Ե���ŷ�������==0)
test3 <- subset( testdata, ��ͨ������==0 & �������д���==0 & ���ر��д���==0 & ��;���д���==0 & ��;���д���==0 &
                  ��Ե���ŷ�������==0 & X1X.EV����.M.==0)
test <- subset( testdata, ��ͨ������==0 & �������д���==0 & ���ر��д���==0 & ��;���д���==0 & ��;���д���==0)



log(0.0001)
















# ����Է���
library(corrplot)
names(testdata)
M <- cor(testdata[, c(6, 45:57)])
corrplot(M, method = "circle")
corrplot(M, method = "ellipse")
corrplot.mixed(M)
corrplot(M, order = "hclust", addrect = 3)

# ���ɷַ���--���������Խ�ǿ��ָ���ں�Ϊ����ָ��
(pc.cr <- princomp(USArrests))  # inappropriate
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)

summary(pc.cr <- princomp(USArrests, cor = TRUE))
loadings(pc.cr)  ## note that blank entries are small but not zero
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)

# ɸѡ��Ҫ����
# ���ɭ��ɸѡ��Ҫ����



# part2: ��ģ

# logitģ��
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


# ������



# ���ɭ��



# ������


# ROC����
library(ROCR)
data(ROCR.simple)
pred <- prediction( pp, testdata_lo$�ȶ��Ա�ʶ)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## LIFT����
perf1 <- performance(pred, "lift", "rpp")
plot(perf1)