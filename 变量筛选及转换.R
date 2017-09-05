##变量筛选及转换##
#定量指标的筛选方法（获取自变量中对违约状态影响最显著的指标）#
#1.随机森林
#2.计算变量间的相对重要性
#3.通过自变量间的广义交叉验证
#4.通过自变量的逐步回归法
#5.采用“Boruta”法
library(InformationValue)
library(klaR)
credit_risk<-ifelse(train_kfolddata[,"credit_risk"]=="good",0,1) #将违约样本用“1”表示，正常样本用”0“表示
tmp<-train_kfolddata[,-21] #将原来的good类的删掉
data<-cbind(tmp,credit_risk)

quant_vars<-c("duration","amount","installment_rate","present_residence","age","number_credits","people_liable","credit_risk") #获取定量指标
quant_GermanCredit<-data[,quant_vars] #提取定量指标

#方法1：随机森林
library(party)
cf1<-cforest(credit_risk~.,data = quant_GermanCredit,controls = cforest_unbiased(mtry=2,ntree=50))
varimp(cf1) #基于变量均值的精度下降，获取自变量的重要性
varimp(cf1,conditional = TRUE) #经过变量间的相关系数调整后，获取自变量的重要性
varimpAUC(cf1) #经过变量间的不平衡性调整后，获取自变量的重要性

#方法2：计算变量间的相对重要性,进行相对性排序，获取自变量中对违约状态影响最显著的指标
library(relaimpo)
lmMod<-lm(credit_risk~.,data=quant_GermanCredit) #线性回归
relImportance<-calc.relimp(lmMod,type="lmg",rela=TRUE) #计算自变量间的相对重要性
sort(relImportance$lmg,decreasing = TRUE) #排序并输出自变量间的相对重要性

#方法3：通过自变量间的广义交叉验证法，获取自变量中对违约状态影响最显著的指标
library(earth)
marsModel<-earth(credit_risk~.,data=quant_GermanCredit)
ev<-evimp(marsModel)
ev


#方法4：通过自变量的逐步回归法
base.mod<-lm(credit_risk~1,data=quant_GermanCredit) #获取线性回归模型的截距，为什么是1呢？
all.mod<-lm(credit_risk~.,data =quant_GermanCredit) #获取完整的线性回归模型
stepMod<-step(base.mod,scope = list(lower=base.mod,upper=all.mod),direction = "both",trace = 0,steps = 1000) #采用双向逐步回归法，筛选变量
shortlistedVars<-names(unlist(stepMod[[1]])) #获取逐步回归得到的变量列表
shortlistedVars<-shortlistedVars[!shortlistedVars%in%"[Intercept]"] #删除逐步回归的截距
print(shortlistedVars) #输出逐步回归后得到的变量
