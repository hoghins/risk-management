library(Hmisc)
mdatas<-as.matrix(GermanCredit[,1:6]) #为方便显示，取前6列



#######################################################################
#前4个预测力指标，均用来判断同类型的变量之间的关联性关系
#通俗来说：连续变量与连续变量、名义变量与名义变量之间的相关关系和关联性
#######################################################################

#1.皮尔森相关系数
pcor<-rcorr(mdatas,type = "pearson")#mdatas书上的数据没有说明，无法实现
#2.斯皮尔曼相关系数
scor<-rcorr(mdatas,type = "spearman")
#3.卡方统计量
library(gmodels)
CrossTable(datas$CHK_ACCT,datas$RESPONSE,expected = TRUE)
#4.概率比统计量
library(mosaic)
t<-CrossTable(datas$EDUCATION,datas$RESPINSE,expected = TRUE)
m<-as.matrix(t$t)
oddsRatio(m)

########################################################
#以下指标可以用来衡量不同类别变量之间的相关性和关联关系#
########################################################

#5.F检验
var.test(datas$CHK_ACCT,datas$RESPONSE)

#6.基尼方差
library(Hmisc)
rcorr.cens(datas$DURATION,datas$RESPONSE)

#7.信息值
library(InformationValue)
library(klaR)
#将因变量转变为二元变量
credit_risk<-ifelse(GermanCredit[,"credit_risk"]=="good",0,1)
tmp<-GermanCredit[,-21]
data<-cbind(tmp,credit_risk)
#将因变量转换为二元变量_End
factor_vars<-c("status","credit_history","purpose","savings","employment_duration","personal_status_sex","other_debtors","property","other_installment_plans","housing","job","telephone","foreign_worker")#获取所有名义因变量
all_iv=data.frame(vars=factor_vars,IV=numeric(length(factor_vars)),STRENGTH=character(length(factor_vars),stringAsFactors=F)#初始化待输出的数据框
                  
for(factor_var in factor_vars)
{
  all_iv[all_iv$VARS==factor_var,"IV"]<-InformationValue::IV
  (X=data[,factor_var],Y=data$credit_risk)
  all_iv[all_iv$VARS==factor_var,"STRENTH"]<-attr
  (InformationValue::IV(X=data[,factor_var],Y=data$credit_risk),"howgood")
}
all_iv<-all_iv[order(-all_iv$IV)] #排序IV
