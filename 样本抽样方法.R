#样本抽样方法
#1.简单随机抽样
#2.分层抽样
#3.整群抽样

#方法1：简单随机抽样
#分为：有放回抽样和无放回抽样
#均可用sample()函数

smp1<-sample(nrow(GermanCredit),10,replace = T) #有放回简单随机抽取10个样本(replace=T),取行数
smp1 #输出简单随机抽取的10个样本的行数
GermanCredit[smp1,] #显示抽取的10个样本，假设其为测试集
train_data=GermanCredit[-smp1,] #样本集表示
test_data=GermanCredit[smp1,] #测试集表示
#用抽样的样本集开发模型，用测试集来测试模型


smp2<-sample(nrow(GermanCredit),10,replace = F) #无放回简单随机抽取10个样本(replace=F),取行数
smp2 #输出简单随机抽取的10个样本的行数
GermanCredit[smp2,] #显示抽取的10个样本，假设其为测试集
train_data=GermanCredit[-smp2,] #样本集表示
test_data=GermanCredit[smp2,] #测试集表示
#用抽样的样本集开发模型，用测试集来测试模型

#方法2：分层抽样
#根据样本总体的某些属性将样本总体分为若干的层次，然后在每个层次中实施相同的随机抽样策略
#使用strata()函数，在使用该函数前，首先对数据集按照该变量进行升序排序，method参数取决于选择列式的方法，分别是无放回、有放回、泊松、系统抽样，默认取srswor(无放回)
install.packages("sampling") #分层抽样函数strata在sampling包
library(sampling)
x<-GermanCredit[order(GermanCredit[,"housing"]),] #首先按照housing属性升序排序
sub_set<-strata(x,stratanames = "housing",size=c(5,5,5),method = "srswor") #以无放回的形式抽样
result<-getdata(GermanCredit,sub_set) #获取抽样结果
View(result)


#方法3：整群抽样
#以样本总体中某个变量分群为依据，对样本进行随机抽样
#要求各群对数据总体有较好的代表性，群内各样本的差异较大，群间差异较小
#使用cluster()函数实现
sub_cluster<-cluster(GermanCredit,clustername = "housing",size = 1,method = "srswor",description = T) #以housing属性分群抽样，并以有放回随机抽样的方法抽取1个群
cluster_data<-getdata(GermanCredit,sub_cluster) #提起抽取的样本
View(cluster_data) #查看抽取的样本


##无放回随机抽样的五折交叉验证##
#五折交叉验证：我们将样本总体随机分为5份，每次都取其中的4份做模型开发，另一份做模型验证，连续这样做5次，并对这5次的模型验证的统计指标取平均值，作为模型的最终验证结果。
#交叉验证：在样本总体较少，经常采用的模型开发和验证方法。优点在于得到的评级模型有较好的提高模型的区分能力、预测准确性和稳定性。

train_kfold<-sample(nrow(GermanCredit),800,replace = F) #无放回随机抽取5份的4份，即抽取80%的样本作为样本集用作模型开发
train_kfolddata<-GermanCredit[train_kfold,] #提取样本集数据
train_kfolddata<-GermanCredit[-train_kfold,] #提取测试集数据
