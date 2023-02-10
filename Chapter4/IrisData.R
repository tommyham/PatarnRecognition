library(tidyverse)

iris.data <- iris[,-5] #Speacies の列を削除
iris.data.mean <- sapply(iris.data, mean)
iris.data.sd <- sapply(iris.data, sd)
print(c(iris.data.mean, iris.data.sd)) #平均と標準偏差を出力

# あわせてdataとaesも指定
p<-ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length,color=Species))+geom_point()
p<-p+layer(geom = "point", stat = "identity", position = "identity")
p<-p+geom_point(aes(x=mean(Sepal.Length),y=mean(Petal.Length)),stat="identity",position="identity",colour="red", size=3) 
ggsave(file='IrisFig/iris.png')
p

###### 標準化 ######
iris.scale_data<-as.data.frame(scale(iris.data)) #標準化+データフレーム化（値がmatrixで返ってくるので。）
iris.scale_data.mean <- sapply(iris.scale_data, mean)
iris.scale_data.sd <- sapply(iris.scale_data, sd)
print(c(iris.scale_data.mean, iris.scale_data.sd)) #平均と標準偏差を出力
iris.scale_data$Species<-iris[,'Species']

# あわせてdataとaesも指定
p<-ggplot(data = iris.scale_data, mapping = aes(x = Sepal.Length, y = Petal.Length,color=Species))+geom_point()
p<-p+layer(geom = "point", stat = "identity", position = "identity")
p<-p+ geom_point(aes(x=mean(Sepal.Length),y=mean(Petal.Length)),stat="identity",position="identity",colour="red", size=3)
p
ggsave(file='IrisFig/irisScale.png',plot=p)

####### 無相関化 ########
iris.scale_data.cov<-as.data.frame(cov(iris.scale_data[,-5])) # 共分散行列の計算
iris.scale_data.eigen<-eigen(iris.scale_data.cov) # 固有値解析
iris.scale_data.eigen$values<-as.data.frame(iris.scale_data.eigen$values) # 固有値のデータフレーム化
iris.scale_data.eigen$vectors<-as.data.frame(iris.scale_data.eigen$vectors) # 固有ベクトルのデータフレーム化
iris.scale_data.Lambda<-t(as.matrix(iris.scale_data.eigen$vectors))%*%as.matrix(iris.scale_data.cov)%*%as.matrix(iris.scale_data.eigen$vectors) # 無相関後の共分散行列
iris.nocorr_data<-as.data.frame(t(t(iris.scale_data.eigen$vectors)%*%t(as.matrix(iris.scale_data[,-5])))) # 無相関化
colnames(iris.nocorr_data)<-names(iris[,-5]) # 列名の変更
iris.nocorr_data$Species<-iris[,'Species'] # 品種列の追加

# あわせてdataとaesも指定
p<-ggplot(data = iris.nocorr_data, mapping = aes(x = Sepal.Length, y = Petal.Length,color=Species))+geom_point()
p<-p+layer(geom = "point", stat = "identity", position = "identity")
p<-p+ geom_point(aes(x=mean(Sepal.Length),y=mean(Petal.Length)),stat="identity",position="identity",colour="red", size=3)
p
ggsave(file='IrisFig/irisNocorr.png',plot=p)

##### 白色化 #####
data_svd=svd(iris.scale_data.Lambda) # 特異値分解
Lambda_sqrt=data_svd$u%*%diag(sqrt(data_svd$d))%*%t(data_svd$v) # Lambdaの平方根
iris.whitening_data=as.data.frame(t(Lambda_sqrt%*%t(as.matrix(iris.scale_data.Lambda))%*%t(as.matrix(iris.nocorr_data[,-5]-sapply(iris.nocorr_data[,-5], mean)))))
colnames(iris.whitening_data)<-names(iris[,-5]) # 列名の変更
iris.whitening_data$Species<-iris[,'Species'] # 品種列の追加

# あわせてdataとaesも指定
p<-ggplot(data = iris.whitening_data, mapping = aes(x = Sepal.Length, y = Petal.Length,color=Species))+geom_point()
p<-p+layer(geom = "point", stat = "identity", position = "identity")
p<-p+ geom_point(aes(x=mean(Sepal.Length),y=mean(Petal.Length)),stat="identity",position="identity",colour="red", size=3)
ggsave(file='IrisFig/irisWhitening.png',plot=p)