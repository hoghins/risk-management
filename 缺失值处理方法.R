##################
##缺失值处理方法##
##################

##数据准备及数据预处理
library(klaR)#GermanCredit数据在这个包中
data(GermanCredit)

#缺失值处理方法
d<-GermanCredit[,2]
d[10]<-NA
plot(d,type="l",main="原数据分布")


#平均数填补
d[10]<-mean(d,na.rm = TRUE)
plot(d,type = "l",main = "平均数填补")

#中位数填补
d[10]<-median(d,na.rm = TRUE)
plot(d,type = "l",main="中位数填补")

#众数填补
d[10]<-table(d)[table(d)==max(table(d))]
plot(d,type="l",main="众数填充")

#将第100行、第2列赋值为NA，以演示用线性回归替换缺失值的方法
GermanCredit[100,2]=NA
x<-GermanCredit
du<-lm(duration~.,x[-100,])#剔除含有缺失值的样本
x[100,2]=predict(du,x[100,-2])#使用线性回归模型预测缺失值
plot(x[,2],type="l",main="使用线性回归预测值填补")

#########################
##附件：table函数的用法##
#########################
ct <- data.frame(Vote.for.X = factor(c("Yes", "Yes", "No", "Not Sure", "No"), levels = c("Yes", "No", "Not Sure")),Vote.for.X.Last.Time =  factor(c("Yes", "No", "No", "Yes", "No"), levels = c("Yes", "No")))  
ct
(cttab<-table(ct))
#一个必须要掌握的操作，addmargins#
addmargins(cttab)
#取出各维度的名字，也就是各个的水平
dimnames(cttab)
