##############################
#####个人主体违约概率计量#####
###信用风险评价——层次分析法###
#信用风险标准评分卡——逻辑回归#
##############################

##############################
#个人、机构信用风险评级开发流程：
#1.数据预处理
#1.1 数据获取
#1.2 EDA
#1.3 相关指标定义
#1.4 数据校准
#2.指标选择
#3.模型开发
# 4.模型验证
# 5.模型实施
# 6.检测与反馈
###############################

################################################################
#影响模型区分能力和预测准确度的步骤主要在于数据预处理和指标选择#
################################################################

########################
# 模型开发的计量方法：
# 1.回归分析（逻辑与概率）
# 2.判别分析
# 3.数据挖掘方法
##########################

###########################
# AHP法开发个人主题评级模型
###########################

#4.入模指标权重计算
#step1:计算目标层——准则层的权重
##目标层——准则层，权重计算
#构建相对重要性判别矩阵
#根据书中P73页中表5.2中数据构建矩阵
d<-c(1,0.5,2,1) 
fa<-matrix(d,ncol = 2,nrow = 2,byrow = TRUE,dimnames = list(c("A1","A2"),c("A1","A2")))
#加载几何平均数函数psych，psych 包里的 geometric.mean
install.packages("psych")
library("psych")
#计算几何平均值并求解权重
gi_a1<-geometric.mean(as.data.frame(fa[1,]))
gi_a2<-geometric.mean(as.data.frame(fa[2,]))
total_gi_a1_a2<-gi_a1+gi_a2
wi_a1<-gi_a1/total_gi_a1_a2
wi_a2<-gi_a2/total_gi_a1_a2
#求解判别矩阵的最大特征值
aw_fa1<-sum(fa[1,]*c(wi_a1,wi_a2))
aw_fa2<-sum(fa[2,]*c(wi_a1,wi_a2))

tmp1<-aw_fa1/wi_a1
tmp2<-aw_fa2/wi_a2
lamda_max_fa<-mean(tmp1,tmp2) #最大特征值

#判别矩阵的一致性检验
#一致性检验：一致性检验是为了检验各元素重要度之间的协调性，避免出现A比B重要，B比C重要，而C又比A重要这样的矛盾情况出现。
ci_fa<-(lamda_max_fa-2)/(2-1)
RI=0.01 #查书中P59表5.5中获得同阶平均随机一致性指标,n=2
CR<-ci_fa/RI
if(CR<CI){
  print("一致性较好，接受。")
}else{
  print("一致性较差，拒绝。")
}
#目标层——准则层，权重确定


#step2:准则层（A1）——定量指标层的权重
#构建相对重要性判别矩阵
d<-c(1,4,2,3,3,4,2,4,2,0.25,1,0.3333333,0.5,0.5,1,0.33333,1,0.33333,0.5,3,1,2,2,3,1,3,1,0.333333,2,0.5,1,1,2,0.5,2,0.5,0.33333333,2,0.5,1,1,2,0.5,2,0.5,0.25,1,1,0.5,0.5,1,0.333333,1,0.333333,0.5,3,0.333333,2,2,3,1,3,1,0.25,1,0.333333,0.5,0.5,1,0.333333,1,0.333333,0.5,3,1,2,2,3,1,3,1)
#根据表5.3中数据，构建矩阵
alp<-matrix(d,ncol = 9,nrow = 9,byrow = TRUE,dimnames = list(c("P1","P2","P3","P4","P5","P6","P7","P8","P9"),c("P1","P2","P3","P4","P5","P6","P7","P8","P9")))
#计算几何平均数并求解权重
alp_p1<-geometric.mean(as.data.frame(alp[1,]))
alp_p2<-geometric.mean(as.data.frame(alp[2,]))
alp_p3<-geometric.mean(as.data.frame(alp[3,]))
alp_p4<-geometric.mean(as.data.frame(alp[4,]))
alp_p5<-geometric.mean(as.data.frame(alp[5,]))
alp_p6<-geometric.mean(as.data.frame(alp[6,]))
alp_p7<-geometric.mean(as.data.frame(alp[7,]))
alp_p8<-geometric.mean(as.data.frame(alp[8,]))
alp_p9<-geometric.mean(as.data.frame(alp[9,]))

total_gi_alp<-sum(alp_p1,alp_p2,alp_p3,alp_p4,alp_p5,alp_p6,alp_p7,alp_p8,alp_p9)

wi_alp_p1<-alp_p1/total_gi_alp
wi_alp_p2<-alp_p2/total_gi_alp
wi_alp_p3<-alp_p3/total_gi_alp
wi_alp_p4<-alp_p4/total_gi_alp
wi_alp_p5<-alp_p5/total_gi_alp
wi_alp_p6<-alp_p6/total_gi_alp
wi_alp_p7<-alp_p7/total_gi_alp
wi_alp_p8<-alp_p8/total_gi_alp
wi_alp_p9<-alp_p9/total_gi_alp

#求解判别矩阵的最大特征值
wi_alp<-c(wi_alp_p1,wi_alp_p2,wi_alp_p3,wi_alp_p4,wi_alp_p5,wi_alp_p6,wi_alp_p7,wi_alp_p8,wi_alp_p9)
aw_alp_p1<-sum(alp[1,]*wi_alp)
aw_alp_p2<-sum(alp[2,]*wi_alp)
aw_alp_p3<-sum(alp[3,]*wi_alp)
aw_alp_p4<-sum(alp[4,]*wi_alp)
aw_alp_p5<-sum(alp[5,]*wi_alp)
aw_alp_p6<-sum(alp[6,]*wi_alp)
aw_alp_p7<-sum(alp[7,]*wi_alp)
aw_alp_p8<-sum(alp[8,]*wi_alp)
aw_alp_p9<-sum(alp[9,]*wi_alp)

tmp1<-aw_alp_p1/wi_alp_p1
tmp2<-aw_alp_p2/wi_alp_p2
tmp3<-aw_alp_p3/wi_alp_p3
tmp4<-aw_alp_p4/wi_alp_p4
tmp5<-aw_alp_p5/wi_alp_p5
tmp6<-aw_alp_p6/wi_alp_p6
tmp7<-aw_alp_p7/wi_alp_p7
tmp8<-aw_alp_p8/wi_alp_p8
tmp9<-aw_alp_p9/wi_alp_p9

lamda_max_alp<-mean(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9)#最大特征值
#判别矩阵的一致性检验
ci_alp<-(lamda_max_alp-9)/(9-1)
RI_alp=1.45 #查表5.5获得同阶平均随机一致性指标，n=9
CR<-ci_alp/RI_alp
if(CR<RI_alp){
  print("一致性较好，接受。")
}else{
  print("一致性较差，拒绝。")
}
#准则层A1——指标层P权重确定。

#step3：准则层（A2）——定性指标层的权重

