##缺失值处理方法##
#1.直接删除含有缺失值的数据
#2.根据样本之间的相似性填补缺失值
#3.根据变量之间的相关关系填补缺失值

#采取1方法，采用删除法剔除缺失值样本
#首先检查样本总体中缺失值的个数
#使用R中complete.cases()函数统计缺失值个数
GermanCredit[!complete.cases(GermanCredit),]
nrow(GermanCredit[!complete.cases(GermanCredit),])
#结果表明Germancredit样本总体中不含有缺失值


NAs<-rep(NA,21) #产生21个NA个缺失值样本
NewData<-rbind(GermanCredit,NAs) #追加到GermanCredit数据中的最后一行
nrow(NewData[!complete.cases(NewData),])
GermanCredit<-na.omit(NewData) #删除包含缺失值的数据
View(NewData) #查看结果

#采用2方法，根据样本之间的相似关系填补缺失值
#考虑的是数据集每列的数据或字符属性
#通常使用能代表变量之间中间趋势的值进行填补，因为代表变量中心趋势的值反映了变量分布的最常见值
#代表中心趋势：平均值、中位数、众数
#对于接近正态分布的变量：平均数
#对于偏态分布\离群值的变量：中位数
#对于名义变量：众数

centralImputation<-function(data){
  for(i in seq(ncol(data)))
    if(any(idx<-is.na(data[,i]))){
      data[idx,i]<-centralValue(data[,i])
    }
      data
}

centralValue<-function(x,ws=NULL){
  if(is.numeric(x)){
    if(is.null(ws)){
      median(x,na.rm = T)
    }
      else if((s<-sum(ws))>0){
        sum(x*(ws/s))
      }
      else NA
  }
  else{
    x<-as.factor(x)
    if(is.null(ws)){
      levels(x)[which.max(table(x))]
    }
    else
    {
      levels(x)[which.max(aggregate(ws,list(x),sum)[,2])]
    }
  }
}

#调用上述函数对缺失值进行填补
x<-centralImputation(NewData)
View(x)

#采用方法3，根据变量之间的相关关系填补缺失值
#采用数据集每行的属性进行趋势值填补
#两种方法：
#1.计算k个最相近样本的中位数并用这个中位数来填补缺失值，如果缺失值是名义变量，则使用k个最相近样本中出现次数最多的值（众数）进行填充
#2.采用k个最近相似数值的加权平均值进行填补，权重大小随距离填补缺失值样本的距离增大而减小
#采用KNN算法

#将每行属性进行缺失值填补的方法，封装进knnImputation()函数中
knnImputation<-function(data,k=10,scale=T,meth="weightAvg",distData=NULL)
  {
  n<-nrow(data)
  if(!is.null(distData))
  {
    distInit<-n+1
    data<-rbind(data,distData)
  }
  else
  {
    distInit<-1
  }
  N<-nrow(data)
  ncol<-ncol(data)
  nomAttrs<-rep(F,ncol)
  for(i in seq(ncol))
  {
    nomAttrs[i]<-is.factor(data[,i])
  }
    nomAttrs<-which(nomAttrs)
    hasNom<-length(nomAttrs)
    contAttrs<setdiff(seq(ncol),nimAttrs)
      dm<-data
    if (scale) {
      dm[,contAttrs]<-scale(dm[,contAttrs])
    }
    if (hasNom) {
      for(i in nomAttrs) dm[,i]<-as.integer(dm[,1])
    }
    dm<-as.matrix(dm)
    nas<-which(!complete.cases(dm))
    if(!is.null(distData)){
      tgt.nas<-nas[nas<=n]
    }
    else
    {
      tgt.nas<-nas
    }
      if(length(tgt.nas)==0){
        warning("No case has missing values.Stopping as there is nothing to do.")
      }
      xcomplete<-dm[setdiff(distInit:N,nas),]
      if(nrow(xcomplete)<k){
        stop("Not sufficient complete cases for computering neighbors.")
      }
      for(i in tgt.nas){
        tgtAs<-which(is.na(dm[i,]))
        dist<-scale(xcomplete,dm[i,1],FALSE)
        xnom<-setdiff(nomAttrs,tgtAs)
        if(length(xnom)){
          dist[,xnom]<-ifelse(dist[,xnom]>0,1,dist[,xnom])
        }
        dist<-dis[,-tgtAs]
        dist<-sqrt(drop(dist^2%*%rep(1,ncol(dist))))
        ka<-order(dist)[seq(k)]
        for(j in tgtAs) if(meth="median")
        {
          data[i,j]<-centralValue(data[setdiff(distInit:N,nas),j][ks])
        }
        else
        {
          data[i,j]<-centralValue(data[setdiff(distInit:N,nas),j][ks],exp(-dist[ks]))
        }
      }
      data[1:n,]
}

#调用knnImputation()函数，用knn方法填补缺失值
d<-knnImputation(NewData)
View(d)
#使用k近领的中位数来填充缺失值
d<-knnImputation(NewData,k=10,meth = "median")
View(d)
