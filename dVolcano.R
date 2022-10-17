#!/usr/bin/env Rscript

#火山图绘制
library(hyplot)
ARGV<-commandArgs(TRUE)

if(length(ARGV)!=4){
  cat("Usage: Rscript dVolcano.R input fccutoff pcutoff output_prefix")
  q()
}

input<-as.character(ARGV[1])
fc <-as.numeric(ARGV[2])
pvalue<-as.numeric(ARGV[3])
pn <-as.character(ARGV[4])

if(FALSE){
  input<- 'F:\\ProteinAccessionAnalysis\\test\\hsa3\\HSA3\\DEP\\ttest_input.txt'
  pn<-'F:\\ProteinAccessionAnalysis\\test\\hsa3\\HSA3\\DEP\\'
  fc<-1.5
  pvalue<-0.05
}


dd<-read.delim(input,check.names = FALSE)
colnames(dd)
colnames(dd)[1:2]<-c("ID","Gene.names")
dd$NegLog10p<- -log10(dd$ttestPvalue)

#log2 fold change 散点图
cat("draw FC scatter chart")
write.csv(dd[,c("ID","Log2FC")],"testtmp.csv")
plotFC(dd[,c("ID","Log2FC")], FCcutoff=fc, outimage=paste0(pn,"/FoldChange.scatter.png"), labelTop=FALSE, topNumber=10, xlab="Index")
plotFC(dd[,c("ID","Log2FC")], FCcutoff=fc, outimage=paste0(pn,"/FoldChange.scatter.pdf"), labelTop=TRUE, topNumber=10, xlab="Index")

#p value 散点图
cat("draw T-test scatter chart")
plotTT(dd[,c("ID","NegLog10p")], pcutoff=pvalue, outimage=paste0(pn,"/pvalue.scatter.png"), labelTop=FALSE, topNumber=10,xlab="Index")
plotTT(dd[,c("ID","NegLog10p")], pcutoff=pvalue, outimage=paste0(pn,"/pvalue.scatter.pdf"), labelTop=TRUE, topNumber=10,xlab="Index")


#火山图
#plotVC(dd[,c("Gene.names","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano.png"),size=2, alpha=0.9,labelTop=FALSE,topNumber=10,width=8,height=6)
#plotVC(dd[,c("Gene.names","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano.pdf"),size=2, alpha=0.9,labelTop=TRUE,topNumber=10,width=8,height=6)
cat("draw Volcano style 1\n")
plotVolcano(dd[,c("ID","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano.png"),size=2, alpha=1,labelTop=FALSE,topNumber=5,width=8,height=4)
plotVolcano(dd[,c("ID","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano.pdf"),size=2, alpha=1,labelTop=TRUE,topNumber=5,width=8,height=4)

cat("draw Volcano style 2\n")
plotVC(dd[,c("ID","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano_style2.png"),size=2, alpha=0.9,labelTop=FALSE,topNumber=10,width=8,height=6)
plotVC(dd[,c("ID","Log2FC","NegLog10p")], pcutoff=pvalue, fccutoff=fc, outimage=paste0(pn,"/Volcano_style2.pdf"),size=2, alpha=0.9,labelTop=TRUE,topNumber=10,width=8,height=6)


#差异标志物的统计柱状图
library(ggsci)
library(ggplot2)
library(cowplot)

tmpdf<-as.data.frame(table(dd$threshold))
bardf<-tmpdf[tmpdf$Var1!="NoSig",]

#mycol<-pal_jama("default")(7)
###上下调蛋白的柱状图
p<-ggplot(bardf,aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(width=.5,colour="black",stat="identity")+theme_bw()+
  geom_text(aes(label=Freq),stat="identity",vjust=-.3,size=3,colour="black")+
  theme(panel.border = element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x = element_blank(),legend.position = "none",axis.line = element_line(colour = "black",size=1))+
  #scale_fill_manual(values=c(Up="#A50026",Down="#006837"))+
  scale_y_continuous(expand=c(0,0),limits=c(0,max(bardf$Freq)*1.3))+
  xlab("")+ylab("Count")+
  scale_fill_manual(values=c("Down"="#3853A4","Up"="#ED1B24"))
ggsave(filename=paste0(pn,"/updown.bar.png"),plot=p,dpi=300,width=4,height=4,units="in")
ggsave(filename=paste0(pn,"/updown.bar.pdf"),plot=p,dpi=300,width=4,height=4,units="in")

colnames(bardf)<-c("Term","Count")
write.table(bardf,paste0(pn,"/updown.stat.txt"), sep='\t', row.names=FALSE, quote=FALSE)


#差异标志物的百分比环形图
colnames(tmpdf)<-c("Term","Count")
tmpdf$percentage<- round(tmpdf$Count / sum(tmpdf$Count) *100, 2)

tmpdf$fraction<-tmpdf$Count/sum(tmpdf$Count)
tmpdf$ymax<-cumsum(tmpdf$fraction)
tmpdf$ymin<-c(0,head(tmpdf$ymax,n=-1))
tmpdf$label<- paste0(tmpdf$Term,"(",tmpdf$Count,", ",tmpdf$percentage,"%)")

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
p2<-ggplot(tmpdf,aes(ymax=ymax,ymin=ymin,
                   xmax=4,xmin=3))+
  geom_rect(aes(fill=label),colour='white')+
  xlim(2,4)+
  theme_bw()+
  coord_polar(theta="y")+
  #scale_fill_brewer(palette='Paired')+
  scale_fill_manual(values=c("#3853A4","grey","#ED1B24"))+
  blank_theme+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"in"))+
  guides(fill=guide_legend(title=""))
ggsave(filename=paste0(pn,"/DEP.Donut.png"),plot=p2,dpi=300,width=8,height=4,units="in")
ggsave(filename=paste0(pn,"/DEP.Donut.pdf"),plot=p2,dpi=300,width=8,height=4,units="in")

