#实用的数据预处理方法及R源代码#
#第一步：明确要解决的问题
#大数据方法在欺诈交易侦测领域应用
#大数据技术得到的结果并非是直接给出哪些交易存在欺诈行为，而是提供某种欺诈概率排序作为输出结果

#第二步：数据集描述及探索性数据分析
install.packages("DMwR")
library(DMwR)
data(sales)
head(sales)
dim(sales) #获取数据集大小，输出结果如下
summary(sales) #获取数据集的概览
#由结果可见，Quant和Val变量中存在较大的缺失值
#检查是否存在这种现象
sum(is.na(sales$Quant&is.na(sales$Val)))
#探索数据集中每一列的属性
table(sales$Insp)/nrow(sales)*100
#获取每个销售人员的交易数量和每个产品的交易数量
total_Sales<-table(sales$ID)
total_Product<-table(sales$Prod)
#每人的交易数量
barplot(total_Sales,main="每人的交易数量",xlab="销售人员",ylab="数量")
#每个产品的交易数量
barplot(total_Product,main ="每个产品的交易数量",xlab="产品",ylab="数量")
#原数据集中添加新的列，产品的单位价格
sales$UnitPrice<-sales$Val/sales$Quant
summary(sales$UnitPrice)
nlevels(sales$Prod)
nlevels(sales$ID)
attach(sales)
#获取所有交易中价格最高和最低的几个产品
(up<-aggregate(UnitPrice,list(Prod),median,na.rm=T))
(top_Product<-sapply(c(T,F),function(x)up[order(up[,2],decreasing = x)[1:5],1])) #这个代码写的太棒了！！(*＾-＾*)
colnames(top_Product)<-c("价格最高","价格最低")
top_Product
detach(sales)
#获得哪些销售人员给公司带来更多的销售收入
attach(sales)
comp<-aggregate(Val,list(ID),sum,na.rm=T)
sS<-sapply(c(T,F),function(m) comp[order(comp$x,decreasing = m)[1:5],1])
colnames(sS)<-c("最多","最少")
#获取前一百名销售人员的销售收入占总公司总收入的比重
sum(comp[order(comp$x,decreasing = T)[1:100],2])/sum(Val,na.rm = T)*100
#获取后2000名销售人员的销售收入占公司总收入的比重
sum(comp[order(comp$x,decreasing = F)[1:2000],2])/sum(Val,na.rm = T)*100

#应用在产品分析上
attach(sales)
qunts<-aggregate(Quant,list(Prod),sum,na.rm=T)
scoreQs<-sapply(c(T,F),function(m) qunts[order(qunts$x,decreasing = m)[1:5],1])
colnames(scoreQs)<-c("最多","最少")
#获取所销售的数量最多和最少的产品
scoreQs
#获取销售量最高的100个产品占所有销量的比重
sum(as.double(qunts[order(qunts$x,decreasing = T)[1:100],2]))/sum(as.double(Quant),na.rm = T)*100
#获取销售量后4000个产品占所有销量的比重
sum(as.double(qunts[order(qunts$x,decreasing = F)[1:4000],2]))/sum(as.double(Quant),na.rm = T)*100

#第三步：缺失值处理
#从销售人员角度
(tots<-table(sales$ID))
NAs<-sales[which(is.na(sales$Quant)&is.na(sales$Val)),c("ID","Prod")]
propS<-100*table(NAs$ID)/tots
propS[order(propS,decreasing = T)[1:10]]#获取Quant和Val同时都有缺失值的交易占最大的前10个销售人员的信息
#从结果可以看出，就销售人员的角度，直接删除变量Quant和Val同时缺失的交易是基本合理的，因为他们的占比都比较小，最高也不过14%
#所以直接删除变量是合理的
#从产品角度来看
totP<-table(sales$Prod)
propP<-100*table(NAs$Prod)/totP
#获取变量Quant和Val都缺失的交易占比最大的前10个产品
propP[order(propP,decreasing = T)[1:10]]
#从结果可以看出，直接删除Quant和Val都缺失的交易，将会出现多只产品交易数据的超过20%的数据被删除
#特别是p2689有近40%的交易数据将被删除
#如果填补的话，以p2689为例，意味着用剩余60%的交易信息去填补该产品40%的交易数据

#填补思路：
#有些产品的交易量很大，有些产品的交易量很小
#但从产品单位价格分布之间的相似性来看，可以用交易量大的产品来代替交易量小的产品？
#如果大交易量的产品与小交易量的产品之间的单位价格非常相似，就可以使用上述的替代方法。

#绘制产品单位价格分布图
#为计算单位价格，先删除缺失值
sales<-sales[-which(is.na(sales$Quant)&is.na(sales$Val)),]
sales$UnitPrice<-sales$Val/sales$Quant
attach(sales)
notF<-which(Insp!="fraud")
#获取非欺诈交易的统计量
ms<-tapply(UnitPrice[notF],list(Prod[notF]),function(x){bp<-boxplot.stats(x)$stats})
#获取中位数和四分位间距统计量
ms<-tapply(UnitPrice[notF],list(Prod[notF]),function(x){bp<-boxplot.stats(x)$stats;c(median=bp[3],iqr=bp[4]-bp[2])})
ms<-matrix(unlist(ms),length(ms),2,byrow=T,dimnames = list(names(ms),c("median","iqr")))
par(mfrow=c(1,2))
#绘图显示产品单位价格的分布
plot(ms[,1],ms[,2],xlab="中位数",ylab="四分位间距，IQR",main="")
#绘制取对数后的产品价格分布
plot(ms[,1],ms[,2],xlab="中位数",ylab="四分位间距，IQR",mian="",col="grey",log="xy")
smalls<-which((table(Prod))<20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch="+")
detach(sales)

#总结
#从以上画出的图可以看出
#绝大多数的产品的单位价格分布是非常相似的
#图形分布多集中在一点上，只有极个别的额产品的单位价格分布比较分散
#在对数单位产品价格尺度下，分布仍然比较集中
#因此，在交易数据比较少的产品跟其他产品非常相似，我们可以用其他相似产品的交易数据来代替这些产品

#删除缺失值后的数据集
salesClean<-sales[-which(is.na(sales$Quant)&is.na(sales$Val)),]
#添加单位产品列
salesClean$UnitPrice<-salesClean$UnitPrice/salesClean$Quant

#第四步：数据抽样
#无放回简单随机抽取10个样本的行数
(smp2<-sample(nrow(GermanCredit),10,replace = F))
library(klaR)
data(GermanCredit)
#显示抽取的10个样本，假设该10个样本为测试集
GermanCredit[smp2,]
#样本集可以表示为
train_data=GermanCredit[-smp2,]
#测试集表示为
test_data=GermanCredit[smp2,]
#数据预处理完成，可用得到的样本集开发信用风险欺诈评分表模型
