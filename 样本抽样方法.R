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
