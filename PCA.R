#PCA 图绘制
library(factoextra)
library(FactoMineR)

data<-read.csv("E:/Code/R/PCA.csv")
res.pca<-PCA(data,graph = FALSE)
bb<-as.factor(c(rep("A",9),rep("B",(ncol(data)-9))))
fviz_pca_var(res.pca,col.var=bb,palette=c("red","blue"),repel = TRUE, legend.title = "Group")
