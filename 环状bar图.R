#调用R包
library(ggplot2)
library(RColorBrewer)
#读取数据

df1<- read.csv("E:\\linshi\\0309\\示例数据.csv",header = T)
# df1$value <- log2(df1$value)#对value值取对数，因为原始数据范围太大
# 
# df11 <- df1[which(grepl("R",df1[[1]])),]
# 
# temp <- data.frame(id=paste0("R",seq(1:8)))
# df11 <- merge(df11,temp,by.x = "individual",by.y = "id")
#设置新罗马字体
windowsFonts(A=windowsFont("Times New Roman"),
             B=windowsFont("Arial"))
#绘图

ggplot(data=df1,aes(type,value,fill=group))+  #画图使用的数据
  geom_bar(stat="identity", color="black", position=position_dodge(),width=0.7,size=0.25)+ #bar图柱子设置
  coord_polar(theta = "x",start=0) +   #设置成圆形
  # ylim(c(-1,6))+                      # y轴范围设置，这里设置时候要大于表中value值的最大最小的范围
  # scale_fill_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF","#F39B7FFF","#91D1C2FF", "#8491B4FF"))+ # group分类颜色设置
  theme_light()+                       # ggplot的画图主题设置
  theme( panel.background = element_blank(), #去除原始的背景颜色
         panel.grid.major = element_line(colour = "grey80",size=.25),#设置放射状的网格线粗细和颜色
         axis.text.y = element_text(size = 10,colour="black"),      ## 设置y轴字体大小和颜色，图中的value旁边的数字0,5,10等
         axis.line.y = element_line(size=0.25),                     ## 设置左侧边框竖线的粗细
         axis.text.x=element_text(size = 13,colour="black"))+       ## 设置x轴字体大小和颜色，图中的L 和R
  theme(text=element_text(family="A",size=16))                     ## 设置图例group字体的大小

