##变量筛选及转换##
#定量指标的筛选方法（获取自变量中对违约状态影响最显著的指标）#
#1.随机森林
#2.计算变量间的相对重要性
#3.通过自变量间的广义交叉验证
#4.通过自变量的逐步回归法
#5.采用“Boruta”法

library(InformationValue)
library(klaR)
data(GermanCredit)

train_kfold<-sample(nrow(GermanCredit),800,replace = F) #无放回随机抽取5份的4份，即抽取80%的样本作为样本集用作模型开发
train_kfolddata<-GermanCredit[train_kfold,] #提取样本集数据
train_kfolddata<-GermanCredit[-train_kfold,] #提取测试集数据

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


#方法5：“Boruta”方法
#Boruta是一种特征选择算法。精确地说，它是随机森林周围的一种包装算法。这个包的名字来源是斯拉夫神话中一个居住在松林的恶魔。
#我们知道，特征选择是预测模型中很关键的一步。当构建一个数据集包含多个变量的模型时，这个步骤尤为重要。
#当你有兴趣了解变量相关性的价值，而不是只局限于建立一个具有良好的预测精度黑盒的预测模型时候，用boruta算法来处理这些数据集无疑是最佳选择。
library(Boruta)
boruta_output<-Boruta(credit_risk~.,data=na.omit(quant_GermanCredit),doTrace=2) #采用“Boruta”法，搜索自变量中对违约状态影响最显著的指标
boruta_signif<-names(boruta_output$finalDecision[boruta_output$finalDecision %in%c("Confirmed","Tentative")]) #获取自变量中确定的和试验性的指标
print(boruta_signif) #输出自变量中对违约状态影响最显著的排序指标
plot(boruta_output,cex.axis=.7,las=2,xlab = "",main="Variable Importance") #绘制变量显著性表示的箱图



#定性指标的筛选方法
library(InformationValue)
library(klaR)
credit_risk<-ifelse(train_kfolddata[,"credit_risk"]=="good",0,1) #违约状态变量用0和1表示，0表示正常，1表示违约
tmp<-train_kfolddata[,-21]
data<-cbind(tmp,credit_risk) #列合并
#提取数据集中全部的定性指标
factor_vars<-c("status","credit_history","purpose","savings","employment_duration","personal_status_sex","other_debtors","property","other_installment_plans","housing","job","telephone","foreign_worker") #获取所有名义自变量
all_iv<-data.frame(VARS=factor_vars,IV=numeric(length(factor_vars)),STRENGTH=character(length(factor_vars)),stringsAsFactors = F) #初始化待输出的数据框
 
factor_var<-c()
for(factor_var in factor_vars)
{
  all_iv[all_iv$VARS==factor_var,"IV"]<-InformationValue::IV(X=data[,factor_var],Y=data$credit_risk) #计算每一个指标的IV值
  all_iv[all_iv$VARS==factor_var,"STRENGTH"]<-attr(InformationValue::IV(X=data[,factor_var],Y=data$credit_risk),"howgood") #提取每个IV指标的描述
}
all_iv<-all_iv[order(-all_iv$IV),] #排序IV,-all_iv$IV表示降序排列



