library(tidyverse)

iris.data <- iris[,-5] #Speacies の列を削除
iris.data.mean <- sapply(iris.data, mean)
iris.data.sd <- sapply(iris.data, sd)
print(c(iris.data.mean, iris.data.sd)) #平均と標準偏差を出力

x<-c(iris[,'Petal.Length'])
y<-c(iris[,'Petal.Width'])
# あわせてdataとaesも指定
p<-ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length,color=Species))+geom_point()
p<-p+layer(geom = "point", stat = "identity", position = "identity")
ggsave(file='iris.pdf',plot=p,device = 'pdf')
p

png('iris.png')
plot(x,y)
plot(mean(x),mean(y))
dev.off()

iris.data.new<-as.data.frame(scale(iris.data)) #標準化+データフレーム化（値がmatrixで返ってくるので。）
iris.data.new.mean <- sapply(iris.data.new, mean)
iris.data.new.sd <- sapply(iris.data.new, sd)
print(c(iris.data.new.mean, iris.data.new.sd)) #平均と標準偏差を出力

x<-c(iris.data.new[,'Petal.Length'])
y<-c(iris.data.new[,'Petal.Width'])
png('irisScale.png')
plot(x,y)
dev.off()