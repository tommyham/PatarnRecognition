library(tidyverse)
library(psych) #psychパッケージの読み込み
library(MASS)

data=Pima.tr

columnList <- c('glu','bmi') 
data.pos<-data[data$type=='Yes',][,columnList]
data.neg<-data[data$type=='No',][,columnList]

# Pima Indianデータセットのプロット
p<-ggplot(data = data, mapping = aes(x = glu, y = bmi,color=type))+geom_point()
ggsave(file='PimaFig/pimaTrain.png')
p

data.describe<-describeBy(data,data$type)
data.pos.mean<-sapply(data.pos,mean)
data.neg.mean<-sapply(data.neg,mean)
data.pos.cov<-cov(data.pos)
data.neg.cov<-cov(data.neg)

S<-solve(data.pos.cov)-solve(data.neg.cov)
c=data.neg.mean%*%solve(data.neg.cov)-data.pos.mean%*%solve(data.pos.cov)
F=data.pos.mean%*%solve(data.pos.cov)%*%data.pos.mean-data.neg.mean%*%solve(data.neg.cov)%*%data.neg.mean-2*log(data.pos.cov)