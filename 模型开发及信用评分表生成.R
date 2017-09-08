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
