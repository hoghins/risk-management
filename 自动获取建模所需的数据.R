#对应《基于R语言的证券公司信用风险计量和管理》P28-P34
#需要电脑上有wind账号才能够连上WindR包
#还需要建立好sql数据库
 
# :)我们开始吧~
#测试WindR是否安装成功
library(WindR)
w.start()
#测试数据库是否连接成功
#安装“RODBC”
install.packages("RODBC")
library("RODBC")
#使用odbcConnect()函数进行连接：
test=odbcConnect('credit')
test

#查看数据库中是否已有数据库表（应当是空）
d<-sqlTables(test)
d

#安装“stringr”包
install.packages("stringr")

##################
##获取并存储数据##
##################
library(WindR)
library(RODBC)
library(stringr)
#弹出windR窗口
w.start()
#使用ODBC连接图2.28中配置的数据库
test=odbcConnect("credit")
#将获取的数据强制转换为数据框格式(dataframe)，目的在于数据往数据库中存储
quant_stock_cashflows15<-as.data.frame(w_wss_data$Data)
#获取数据库中已有的数据库表
d<-sqlTables(test)
#提取数据库表的名字
txt<-d[,"TABLE_NAME"]
is_exists<-str_detect(txt,"quant_stock_cashflows15")
#判断将要建立的数据库表(quant_stock_cashflows15)是否在数据库中已经存在
if(sum(is_exists)>=1){#如果将要新建的数据库表已经存在，就将其删除
  sqlDrop(test,"quant_stock_cashflows15")
}
#将自动获取的数据存储到数据表中
sqlSave(test,quant_stock_cashflows15,rownames = "index".addPK=TRUE)
#关闭数据库的远程连接
odbcClose(test)
