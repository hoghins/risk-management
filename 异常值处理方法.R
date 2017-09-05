##异常值处理方法##
#1.单变量离群值检测
#2.局部离群值因子检验
#3.基于聚类方法的离群值检测

#方法1：单变量离群值检测
#原理：通过求解单变量数值的第1个和第3个四分位数的值，将数值小于第1个和大于第3个四分位数的值认定为离群值
#使用grDevices包中的boxplot()、stats()函数
install.packages("grDevices")
library(grDevices)
#用随机数来演示获取异常值的方法
set.seed(3147) #设置获取随机数的种子
x<-rnorm(100) #生成100个随机数
summary(x)
boxplot.stats(x)$out #检测并输出异常值
boxplot(x) #用箱图表现异常值

set.seed(3147)
x<-rnorm(100)
y<-rnorm(100)
df<-data.frame(x,y) #将x,y两个随机序列组成数据框
rm(x,y)  #删除x,y两个变量
attach(df)
(a<-which(x %in%boxplot.stats(x)$out))
(b<-which(y %in%boxplot.stats(y)$out))
detach(df)                                                                                                                                                                                                                                                                                                                                                                                  
(outlier1<-intersect(a,b)) #intersect()功能相当于取交集
plot(df)
points(df[outlier1,],col="red",pch="+",cex=2.0)
#也可以将变量x和y的离群值作为整个数据框的离群值，用“*”表示
(outlier2<-union(a,b))
plot(df)
points(df[outlier2,],col="green",pch="*",cex=2.0)

#方法二：局部离群值因子检测
#基于密度的局部离群值的算法：将一个点的局部密度与其他相邻区域进行比较，如果前者远远小于后者，则该点相对于相邻区域位于一个更稀疏的区域，则认为其为离散点
#使用DMwR包中的lofactor()函数中的局部离群值检测算法
install.packages("DMwR")
library(DMwR)
data("iris")
View(iris)
iris2<-iris[,1:4] #以DMwR包中的自带数据集iris为例，并去掉字符型变量

outlier.scores<-lofactor(iris2,k=5) #??k为什么等于5呢，检测k个相邻区域以获得离群值
plot(density(outlier.scores)) #画出离群值得分的密度图
#概率密度函数：在数学中，连续型随机变量的概率密度函数（在不至于混淆时可以简称为密度函数）是一个描述这个随机变量的输出值，在某个确定的取值点附近的可能性的函数。

outliers<-order(outlier.scores,decreasing = T)[1:5] #选出得分最高的5个离群值
print(outliers) #输出得分最高的5个离群值的分数
print(iris2[outliers,])#输出离群值及其对应的分数，选取iris2中离群值的行数

#绘制基于前两个主成分绘制离群值的双标图
n<-nrow(iris2)
labels<-1:n
labels[-outliers]<-"."
biplot[prcomp(iris2),cex=0.8,xlabs=labels]#无法实现，报错
 
#方法三：基于聚类方法的离群值检测
#根据样本的分布将样本聚为若干个群簇，远离这些群簇中心点的值被定义为离群值
library(DMwR)
iris2<-iris[,1:4] #删除字符型变量
kmeans.result<-kmeans(iris2 ,centers = 3)  #进行k-means聚类，并将样本聚为3类
kmeans.result$centers #输出3个聚类的中心
kmeans.result$cluster #标出样本属于哪一类的ID
centers<-kmeans.result$centers[kmeans.result$cluster,]#分别确定每个样本的中心点
distances<-sqrt(rowSums((iris2-centers)^2)) #计算每个样本到3个聚类中心的距离
outliers<-order(distances,decreasing = T)[1:5] #选出距离聚类中心最远的5个样本，作为离群值
print(outliers) #输出离群值与聚类中心的距离
print(iris2[outliers,]) #输出离群值
#画出离群值，用“*”表示3个聚类的中心，用“+”表示离群值
plot(iris2[,c("Sepal.Length","Sepal.Width")],pch="o",col=kmeans.result$cluster,cex=0.3)
points(kmeans.result$centers[,c("Sepal.Length","Sepal.Width")],col=1:3,pch=8,cex=1.5) #将聚类中心分别用红色、绿色、黑色的“*”表示
points(iris2[outliers,c("Sepal.Length","Sepal.Width")],pch="+",col=4,cex=1.5) #将离群值用蓝色的“+”表示

library(klaR)
data("GermanCredit")
summary(GermanCredit)
                                                                                                                       
