
df<-read.csv("E:/linshi/0407/123.csv",header=T,
             stringsAsFactors = F)
head(df)
dim(df)
df$group<-ifelse(df$logFC>=2&df$P.Value<=0.05,"Up",
                 ifelse(df$logFC<=-2&df$P.Value<=0.05,
                        "Down","Not sig"))
table(df$group)
library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)
# 设置pvalue和logFC的阈值
cut_off_pvalue = 0.05
cut_off_logFC = 1
# 根据阈值分别为上调基因设置‘up’，下调基因设置‘Down’，无差异设置‘Stable’，保存到change列
# 这里的change列用来设置火山图点的颜色
df$change = ifelse(df$P.Value < cut_off_pvalue & abs(df$logFC) >= cut_off_logFC, 
                   ifelse(df$logFC> cut_off_logFC ,'Up','Down'), 'NoSig')

df_label <- subset(df, df$P.Value < 0.000001 & abs(df$logFC) >= 4)
# 绘制火山图====================================
p <- ggplot(df, 
  aes(x = logFC, 
      y = -log10(P.Value), 
      colour=change)) +
  geom_point(alpha=0.4, size=3.5) +
  xlim(-10,10)+
  scale_color_manual(values=c("#00AFBB", "#999999", "#FC4E07"))+   #"#546de5", "#d2dae2","#ff4757"
  
  # 辅助线
  geom_vline(xintercept=c(-1,1),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(cut_off_pvalue),lty=4,col="black",lwd=0.8) +
  
  # 坐标轴
  labs(x="log2(fold change)",
       y="-log10 (p-value)")+
  theme_bw()+
  
  # 图例
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position="right", 
        legend.title = element_blank()
  )

p+
  geom_text_repel(
    data = df_label,
    aes(label = gene_name),
    size = 3,
    box.padding = unit(0.5, "lines"),
    point.padding = unit(0.8, "lines"), segment.color = "black", show.legend = FALSE )

#########################################################################################
# 方法2. 适合大数据量数据
p +
  geom_point(size = 3, shape = 1, data = df_label) +
  ggrepel::geom_label_repel(
    aes(label = gene_name),
    data = df_label,
    color="black"
  )

##########################################################################################
# 方法3. 有现成的参数“label.select”来选择标记的标签
# 由于ggpubr写纵坐标时直接写-log10（P.value）不识别，可采取迂回策略，改列名，完事再在图上改纵轴标签。

df$tran_pvalue <- -log10(df$P.Value)

library(ggpubr)

ggscatter(df, 
          x = "logFC", 
          y = "tran_pvalue", 
          ylab="-log10(P.value)",
          color = "change",
          size = 1.5,
          label = "gene_name", 
          repel = T,
          palette = c("#00AFBB", "#999999", "#FC4E07") ,
          #label.select = dat$symbol[1:10] ,                       #选取向量子集
          label.select = c("FIBCD1","AFAP1-AS1","PPP1R14D","AC023154.1","STAB2","NKX2-5" )  # 手动挑选
)

