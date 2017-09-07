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

#对入模的定量和定性指标，进行连续变量分段（对定量指标进行分段）
#计算定量指标的WOE和离散指标必要的降维


##连续变量分段方法2种
#1.等距分段：将连续指标分为等距离的若干区间，然后分别计算每个区间上的WOE值
#2.最优分段：根据变量的分布属性，结合该变量对违约状态变量预测能力的变化，按照一定规则，将属性相接近的数值聚在一起，形成距离不相等的若干区间，最终得到对违约变量预测能力最强的最优分段
#采取的逻辑：先进行最优分段，在连续变量的分布不满足最优分段的要求时，再考虑对连续变量进行等距分段

#最优分段采用的分段算法是条件推理树的递归分割法
#基本原理：根据自变量的连续分布和因变量的二元分布之间的关系，采用递归的回归分析方法，逐层递归满足给定的显著性水平，此时获取的分段结果就是连续变量的最优分段
#该算法采用ctree()函数

library(party)
#对定量入模指标进行最优分段算法
#对变量"duration“进行最优分段
library(smbinning)
result=smbinning(df=data,y="credit_risk",x="duration",p=0.05)
result$ivtable
#对变量“amount”进行最优分段
result=smbinning(df=data,y="credit_risk",x="amount",p=0.05)
result$ivtable
#对变量“age”进行最优分段
result=smbinning(df=data,y="credit_risk",x="age",p=0.05)
result$ivtable
#由于变量“installment_rate”的取值只有四个值，不适用最优分段算法，只能用等级算法
#总结：根据变量的值来确定，如果取值不够多，就只能采用等距分段了
library(klaR)
data("GermanCredit")
install_data<-GermanCredit[,c("installment_rate","credit_risk")]
tbl<-table(install_data)
total<-list()
for(i in 1:nrow(tbl))
{
        total[i]<-sum(tbl[i,]) #一行一行的来
}
t.tbl<-cbind(tbl,total)
GoodRate<-as.numeric(t.tbl[,"good"])/as.numeric(t.tbl[,"total"])
BadRate<-as.numeric(t.tbl[,"bad"])/as.numeric(t.tbl[,"total"])
gb.tbl<-cbind(t.tbl,GoodRate,BadRate)
Odds<-GoodRate/BadRate
LnOdds<-log(Odds)
tt.tbl<-cbind(gb.tbl,Odds,LnOdds)
WOE<-log((as.numeric(tt.tbl[,"good"])/700)/(as.numeric(tt.tbl[,"bad"])/300))
all.tbl<-cbind(tt.tbl,WOE) #WOE列合并贴到tt.tbl上面
all.tbl

##对离散变量做必要的降维处理和WOE值的计算
#首先要看下入模定性指标的概况
library(klaR)
data("GermanCredit")
discrete_data<-GermanCredit[,c("status","credit_history","savings","purpose","credit_risk")]
summary(discrete_data) #查看输入入模定性指标的概况


#吊轨的书上说purpose的维数为10，我自己的电脑跑出来是维数为7，但是明显也比其他定性指标要高很多了
#为了避免造成“维度灾难”，需要进行降维处理
#在评级模型开发中的降维处理方法中，通常采用的是将属性相似的合并处理

#对purpose进行降维处理
library(klaR)
data("GermanCredit")

discrete_data<-GermanCredit[,c("status","credit_history","savings","purpose","property","credit_risk")]
#对指标进行降维
x<-discrete_data[,c("purpose","credit_risk")]
d<-as.matrix(x)
for(i in 1:nrow(d))
{
        #合并car(new)、car(used)
        if(as.character(d[i,"purpose"])=="car(new)")
        {
                d[i,"purpose"]<-as.character("car(new/used)")
        }
        if(as.character(d[i,"purpose"])=="car(used)")
        {
                d[i,"purpose"]<-as.character("car(new/used)")
        }
        #合并radio/television\furniture/equipment
        if(as.character(d[i,"purpose"])=="radio/television")
        {
                d[i,"purpose"]<-as.character("radio/television/furniture/equipment")
        }
        if(as.character(d[i,"purpose"])=="furniture/equipment")
        {
                d[i,"purpose"]<-as.character("radio/television/furniture/equipment")
        }
        #合并others\repairs\business
        if(as.character(d[i,"purpose"])=="others")
        {
                d[i,"purpose"]<-as.character("others/repairs/business")        
        }
        if(as.character(d[i,"purpose"])=="repairs")
        {
                d[i,"purpose"]<-as.character("others/repairs/business")
        }
        if(as.character(d[i,"purpose"])=="business")
        {
                d[i,"purpose"]<-as.character("others/repairs/business")
        }
        #合并retraing、education
        if(as.character(d[i,"purpose"])=="retraining")
        {
                d[i,"purpose"]<-as.character("retraining/education")
        }
        if(as.character(d[i,"purpose"])=="education")
        {
                d[i,"purpose"]<-as.character("retraining/education")
        }
}

new_data<-cbind(discrete_data[,c(-4,-6)],d) #将第四列和第六列删去后，再补上一个d的列
woemodel<-woe(credit_risk~.,data = new_data,zeroadj=0.5,applyontrain=TRUE) #计算WOE
woemodel$woe #输出WOE


