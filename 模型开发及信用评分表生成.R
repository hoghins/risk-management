library(klaR)
library(InformationValue)
data("GermanCredit")
train_kfold<-sample(nrow(GermanCredit),800,replace = T)
#提取样本集数据
train_kfolddata<-GermanCredit[train_kfold,]
#提取测试集数据
test_kfolddata<-GermanCredit[-train_kfold,]
#将违约样本用“1”表示，正常样本用“0”表示
credit_risk<-ifelse(train_kfolddata[,"credit_risk"]=="good",0,1)
tmp<-train_kfolddata[,-21]
data<-cbind(tmp,credit_risk)
#获取定量数据
quant_vars<-c("duration","amount","installment_rate","present_residence","age","number_credits","people_liable","credit_risk")
#提取定量数据
quant_GermanCredit<-data[,quant_vars]

#采用通过自变量的逐步回归法，获取自变量中对违约状态影响最显著的指标
#获取线性回归模型的截距
(base.mod<-lm(credit_risk~1,data=quant_GermanCredit))
#获取完整的线性回归模型
(all.mod<-lm(credit_risk~.,data=quant_GermanCredit))
#采取逐步回归得到的变量列表
(shortlistedVars<-names(unlist(stepMod[[1]])))
#删除逐步回归的截距,同时输出逐步回归后得到的变量，获取定量入模指标
(shortlistedVars<-shortlistedVars[!shortlistedVars%in%"(Intercept)"])
#定量入模指标（注：这里是根据输出系数决定的）
quant_model_vars<-c("duration","amount","installment_rate","age")
#提取数据集中全部的定性指标
factor_vars<-c("status","credit_history","purpose","savings","employment_duration","personal_status_sex","other_debators","property","other_installment_plans","housing","job","telephone","foreign_worker")
#初始化待输出的数据框
all_iv<-data.frame(VARS=factor_vars,IV=numeric(length(factor_vars)),STRENGTH=character(length(factor_vars)),stringAsFactors=F)
factor_var<-c()
for(factor_var in factor_vars)
{
        #计算每个指标的IV值
        all_iv[all_iv$VARS==factor_var,"IV"]<-InformationValue::IV(X=data[,factor_var],Y=data$credit_risk)
        #提取每个IV指标的描述
        all_iv[all_iv$VARS==factor_var,"STRENGTH"]<-attr(InformationValue::IV(X=data[,factor_var],Y=data$credit_risk),"howgood")
}
#降序排列IV
all_iv<-all_iv[order(-all_iv$IV),]
#获取定性入模指标
(qual_model_vars<-subset(all_iv,STRENGTH=="Highly Predictive"))
#入模定性指标
qual_model_vars<-c("status","credit_history","savings","purpose","property")

#连续变量分段和离散变量降维
#1.变量duration
library(smbinning)
result=smbinning(df=data,y="credit_risk",x="duration",p=0.05)
result$ivtable

duration_Cutpoint<-c()
duration_WoE<-c()
duration<-data[,"duration"]
for(i in 1:length(duration))
{
        if(duration[i]<=8)
        {
                duration_Cutpoint[i]<-"<=8"
                duration_WoE[i]<--1.4382
        }
        if(duration[i]>8 && duration[i]<=26)
        {
                duration_Cutpoint[i]<-">8 &&<=26"
                duration_WoE[i]<--0.1319
        }
        if(duration[i]>26 && duration[i]<=72)
        {
                duration_Cutpoint[i]<-">26 &&<=72"
                duration_WoE[i]<-0.7048
        }
}
#2.变量：amount
result=smbinning(df=data,y="credit_risk",x="amount",p=0.05)
result$ivtable

amount_Cutpoint<-c()
amount_WoE<-c()
amount<-data[,"amount"]
for(i in 1:length(amount))
{
        if(amount[i]<=3499)
        {
                amount_Cutpoint[i]<-"<=3499"
                amount_WoE[i]<--0.1731
        }
        if(amount[i]>3499 && amount[i]<=3913)
        {
                amount_Cutpoint[i]<-">3499 &&<=3913"
                amount_WoE[i]<--2.1850
        }
        if(amount[i]>3913 && amount[i]<=7678)
        {
                amount_Cutpoint[i]<-">3913 &&<=7678"
                amount_WoE[i]<-0.3884
        }
        if(amount[i]>7678 && amount[i]<=18424)
        {
                amount_Cutpoint[i]<-">7678 &&<=18424"
                amount_WoE[i]<-1.1408
        }
}

#3.变量age
result=smbinning(df=data,y="credit_risk",x="age",p=0.05)
result$ivtable
age_Cutpoint<-c()
age_WoE<-c()
age<-data[,"age"]
for(i in 1:length(age))
{
        if(age[i]<=25)
        {
                age_Cutpoint[i]<-"<=25"
                age_WoE[i]<-0.5555
        }
        if(age[i]>25 && age[i]<=75)
        {
                age_Cutpoint[i]<-">25 && <=75"
                age_WoE[i]<--0.1454
        }
}

#4.变量installment_rate 等距分段
install_data<-data[,c("installment_rate","credit_risk")]
(tbl<-table(install_data))
total<-list()
for(i in 1:nrow(tbl))
{
        total[i]<-sum(tbl[i,1])
}
t.tbl<-cbind(tbl,total)
GoodRate<-as.numeric(t.)
BadRate<-as.numeric(t.tbl[,"1"])/as.numeric(t.tbl[,"total"])
gb.tbl<-cbind(t.tbl,Odds,BadRate)
Odds<-GoodRate/BadRate
LnOdds<-log(Odds)
(tt.tbl<-cbind(gb.tbl,Odds,LnOdds))
WoE<-log((as.numeric(tt.tbl[,"0"])/700)/(as.numeric(tt.tbl[,"1"])/300))
all.tbl<-cbind(tt.tbl,WoE)
all.tbl
install_rate_Cutpoint<-c()
install_rate_WoE<-c()
installment_rate<-data[,"installment_rate"]
for(i in 1:length(installment_rate))
{
        if(installment_rate[i]==1)
        {
                install_rate_Cutpoint[i]<-"=1"
                install_rate_WoE[i]<-0.2513144
        }
        if(installment_rate[i]==2)
        {
                install_rate_Cutpoint[i]<-"=2"
                install_rate_WoE[i]<-0.1554665
        }
        if(installment_rate[i]==3)
        {
                install_rate_Cutpoint[i]<-"=3"
                install_rate_WoE[i]<-0.06453852
        }
        if(installment_rate[i]==4)
        {
                install_rate_Cutpoint[i]<-"=4"
                install_rate_WoE[i]<-0.1573003
        }
}

#定性指标的降维和WoE
discrete_data<-data[,c("status","credit_history","savings","purpose","property","credit_risk")]
summary(discrete_data)
#对指标purpose进行降维
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
        #合并radio/television、furniture/equipment
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
        #合并retraining、education
        if(as.character(d[i,"purpose"])=="retraining")
        {
                d[i,"purpose"]<-as.character("retraining/education")
        }
        if(as.character(d[i,"purpose"])=="education")
        {
                d[i,"purpose"]<-as.character("retraining/education")
        }
}
(new_data<-cbind(discrete_data[,c(-4,-6)],d))

#替换原数据集中的“purpose”指标的值
#计算WOE
woemodel<-woe(credit_risk~.,data=new_data,zeroadj=0.5,applyontrain=TRUE)
#输出WOE
woemodel$woe
#1.status
status<-as.matrix(new_data[,"status"])
colnames(status)<-"status"
status_WoE<-c()
for (i in 1:length(status))
{
        if(status[i]=="... < 100 DM")
        {
                status_WoE[i]<--0.7553785
        }
        if(status[i]=="0 <= ... < 200 DM")
        {
                status_WoE[i]<--0.4260422
        }
        if(status[i]=="... >= 200 DM / salary for at least 1 year")
        {
                status_WoE[i]<-0.3541628
        }
        if(status[i]=="no checking account")
        {
                status_WoE[i]<-1.1964794
        }
}
#credit_history
credit_history<-as.matrix(new_data[,"credit_history"])
colnames(credit_history)<-"credit_history"
credit_history_WoE<-c()
for(i in 1:length(credit_history))
{
        if(credit_history[i]=="no credits taken/all credits paid back duly")
        {
                credit_history_WoE[i]<--1.43325824
        }
        if(credit_history[i]=="all credits at this bank paid back duly")
        {
                credit_history_WoE[i]<--0.83542124
        }
        if(credit_history[i]=="existing credits paid back duly till now")
        {
                credit_history_WoE[i]<--0.11018574
        }
        if(credit_history[i]=="delay in paying off in the past")
        {
                credit_history_WoE[i]<-0.07328118
        }
        if(credit_history[i]=="critical account/other credits existing")
        {
                credit_history_WoE[i]<-0.70502380
        }
}
#3.savings
savings<-as.matrix(new_data[,"savings"])
colnames(savings)<-"savings"
savings_WoE<-c()
for(i in 1:length(savings))
{
        if(savings[i]=="... < 100 DM")
        {
                savings_WoE[i]<--0.26762879
        }
        if(savings[i]=="100 <= ... < 500 DM")
        {
                savings_WoE[i]<--0.08820683
        }
        if(savings[i]=="500 <= ... < 1000 DM")
        {
                savings_WoE[i]<-0.60494035
        }
        if(savings[i]=="... >= 1000 DM")
        {
                savings_WoE[i]<-1.02087675
        }
        if(savings[i]=="unknown/no savings account")
        {
                savings_WoE[i]<-0.68902346
        }
}
#4.property
property<-as.matrix(new_data[,"property"])
colnames(property)<-"property"
property_WoE<-c()
for(i in 1:length(property))
{
        if(property[i]=="real estate")
        {
                property_WoE[i]<-0.53478699
        }
        if(property[i]=="buiding society saving agreement/life insurance")
        {
                property_WoE[i]<--0.07558268
        }
        if(property[i]=="car or other")
        {
                property_WoE[i]<--0.03137336
        }
        if(property[i]=="unknown/no property")
        {
                property_WoE[i]<--0.64126522
        }
}
#5.purpose
purpose<-as.matrix(new_data[,"purpose"])
colnames(purpose)<-"purpose"
purpose_WoE<-c()
for(i in 1:length(purpose))
{
        if(purpose[i]=="car(new/used)")
        {
                purpose_WoE[i]<--0.1200509
        }
        if(purpose[i]=="domestic appliances")
        {
                purpose_WoE[i]<-0.4657153
        }
        if(purpose[i]=="others/repairs/business")
        {
                purpose_WoE[i]<--0.1585346
        }
        if(purpose[i]=="radio/television/furniture/equipment")
        {
                purpose_WoE[i]<--0.1022687
        }
        if(purpose[i]=="retraining/education")
        {
                purpose_WoE[i]<--0.5169675
        }
}
#入模定量和定性指标
model_data<-cbind(data[,quant_model_vars],data[,qual_model_vars])
credit_risk<-as.matrix(data[,"credit_risk"])
colnames(credit_risk)<-"credit_risk"
#入模定量和定性指标的WoE
model_data_WoE<-as.data.frame(cbind(duration_WoE,amount_WoE,age_WoE,install_rate_WoE,status_WoE,credit_history_WoE,savings_WoE,property_WoE,purpose_WoE,credit_risk))
#入模定量和定性指标的“分段"
model_data_Cutpoint<-cbind(duration_Cutpoint,amount_Cutpoint,age_Cutpoint,install_rate_Cutpoint,status,credit_history,savings,property,purpose)
#逻辑回归
m<-glm(credit_risk~.,data = model_data_WoE,family = binomial())
alpha_beta<-function(basepoints,baseodds,pdo)
{
        beta<-pdo/log(2)
        alpha<-basepoints+beta*log(baseodds)
        return(list(alpha,beta=beta))
}
coefficients<-m$coefficients
#特定比率（1/20）的特定分值（50）和比率翻番的分数（10），计算评分卡的系数alpha和beta
(x<-alpha_beta(50,0.05,10))
#计算基础分值
basepoint<-round(x$alpha-x$beta*coefficients[1])
#1.duration_score
duration_score<-round(as.matrix(-(model_data_WoE[,"duration_WoE"]*coefficients["duration_WoE"]*x$beta)))
colnames(duration_score)<-"duration_score"
#2.amount_score
amount_score<-round(as.matrix(-(model_data_WoE[,"amount_WoE"]*coefficients["amount_WoE"]*x$beta)))
colnames(amount_score)<-"amount_score"
#3.age_score
age_score<-round(as.matrix(-(model_data_WoE[,"age_WoE"]*coefficients["age_WoE"]*x$beta)))
colnames(age_score)<-"age_score"
#4.install_rate_score
(install_rate_score<-round(as.matrix(-(model_data_WoE[,"install_rate_WoE"]*coefficients["install_rate_WoE"]*x$beta))))
colnames(install_rate_score)<-"install_rate_score"
#5.status_score
status_score<-round(as.matrix(-(model_data_WoE[,"status_WoE"]*coefficients["status_WoE"]*x$beta)))
colnames(status_score)<-"status_score"
#6.credit_history
credit_history_score<-round(as.matrix(-(model_data_WoE[,"credit_history_WoE"]*coefficients["credit_history_WoE"]*x$beta)))
colnames(credit_history_score)<-"credit_history_score"
#7.savings_score
savings_score<-round(as.matrix(-(model_data_WoE[,"savings_WoE"]*coefficients["savings_WoE"]*x$beta)))
colnames(savings_score)<-"savings_score"
#8.property_score
property_score<-round(as.matrix(-(model_data_WoE[,"property_WoE"]*coefficients["property_WoE"]*x$beta)))
colnames(property_score)<-"property_score"
#9.purpose_score
purpose_score<-round(as.matrix(-(model_data_WoE[,"purpose_WoE"]*coefficients["purpose_WoE"]*x$beta)))
colnames(purpose_score)<-"purpose_score"
#输出最终的csv格式的打分卡
#1.基础分值
r1<-c("","basepoint",19)
(m1<-matrix(r1,nrow=1))
colnames(m1)<-c("Basepoint","Basepoint","Score")
m1
#2.duration的分值
(duration_scoreCard<-cbind(as.matrix(c("Duration","",""),ncol=1),unique(cbind(duration_Cutpoint,duration_score)))) #unique()去重
#3.age的分值
(age_scoreCard<-cbind(as.matrix(c("Installment_Rate","","",""),ncol=1),unique(cbind(install_rate_Cutpoint,install_rate_score))))
#4.install_rate的分值
(install_rate_scoreCard<-cbind(as.matrix(c("installment_Rate","","",""),ncol=1),unique(cbind(install_rate_Cutpoint,install_rate_score))))
#5.status的分值
(status_scoreCard<-cbind(as.matrix(c("Status","","",""),ncol=l),unique(cbind(status,status_score))))
#6.credit_history的分值
(credit_history_scoreCard<-cbind(as.matrix(c("Credit_History","","","",""),ncol=1),unique(cbind(credit_history,credit_history_score))))
#7.savings的分值
(savings_scoreCard<-cbind(as.matrix(c("Savings","","","",""),ncol=1),unique(cbind(savings,savings_score))))
#8.property的分值
(property_scoreCard<-cbind(as.matrix(c("Property","","",""),ncol=1),unique(cbind(property,property_score))))
#9.purpose的分值
(purpose_scoreCard<-cbind(as.matrix(c("Purpose","","","",""),ncol=1),unique(cbind(purpose,purpose_score))))

scoreCard_CSV<-rbind(m1,
                     duration_scoreCard,
                     age_scoreCard,
                     install_rate_scoreCard,
                     status_scoreCard,
                     credit_history_scoreCard,
                     savings_scoreCard,
                     property_scoreCard,
                     purpose_scoreCard)
write.csv(scoreCard_CSV,"D:/Users/caiyue/Documents/scoreCard.csv")
